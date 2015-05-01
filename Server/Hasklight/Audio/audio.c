#include "audio.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <alsa/asoundlib.h>
#include <math.h>
#include <pthread.h>
#include <fftw3.h>

//FFTW things
fftw_complex *inl, *outl, *inr, *outr;
fftw_plan planl, planr;

pthread_mutex_t storageLock = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t fftbufLock = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t onbeatLock = PTHREAD_MUTEX_INITIALIZER;

SAMPLE *readbufs[2];
SAMPLE readbufl[BUFSIZE];
SAMPLE readbufr[BUFSIZE];

SAMPLE *storages[2];
SAMPLE storagel[BUFSIZE];
SAMPLE storager[BUFSIZE];

SAMPLE fftbufl[BUFSIZE];
SAMPLE fftbufr[BUFSIZE];

SAMPLE *fftreadbufs[2];
SAMPLE fftreadbufl[BUFSIZE];
SAMPLE fftreadbufr[BUFSIZE];

SAMPLE energyHistories[32][43];

int onbeat;

double PI = 3.141592653;

float hann_window(int sample, int num_samples)
{
        return 0.5 * (1 - cos((2 * PI * sample) / (num_samples - 1)));
}

SAMPLE **getSoundBuffer()
{
    pthread_mutex_lock(&storageLock);
    for(int i = 0; i < BUFSIZE; i++)
    {
        memcpy(readbufl, storagel, sizeof(SAMPLE) * BUFSIZE);
        memcpy(readbufr, storager, sizeof(SAMPLE) * BUFSIZE);
    }
    pthread_mutex_unlock(&storageLock);
    return readbufs;
}

SAMPLE **getFFTBuffer()
{
    pthread_mutex_lock(&fftbufLock);
    for(int i = 0; i < BUFSIZE; i++)
    {
        memcpy(fftreadbufl, fftbufl, sizeof(SAMPLE) * BUFSIZE);
        memcpy(fftreadbufr, fftbufr, sizeof(SAMPLE) * BUFSIZE);
    }
    pthread_mutex_unlock(&fftbufLock);
    return fftreadbufs;
}

int getOnBeat()
{
    int tmp;
    pthread_mutex_lock(&onbeatLock);
    tmp = onbeat;
    pthread_mutex_unlock(&onbeatLock);
    return tmp;
}

void runFFT()
{
    pthread_mutex_lock(&storageLock);
    for(int i = 0; i < BUFSIZE; i++)
    {
        inl[i][0] = storagel[i] * hann_window(i, BUFSIZE);
        inr[i][0] = storager[i] * hann_window(i, BUFSIZE);
    }
    pthread_mutex_unlock(&storageLock);
    fftw_execute(planl);
    fftw_execute(planr);
    pthread_mutex_lock(&fftbufLock);
    for(int i = 0; i < BUFSIZE; i++)
    {
        fftbufl[i] = sqrt(outl[i][0] * outl[i][0] + outl[i][1] * outl[i][1]);
        fftbufr[i] = sqrt(outr[i][0] * outr[i][0] + outr[i][1] * outr[i][1]);
    }
    pthread_mutex_unlock(&fftbufLock);
}

void beatDetect()
{
    int tmp;
    SAMPLE avgen;
    SAMPLE es[32];
    SAMPLE ei[32];
    memset(es, 0, sizeof(SAMPLE) * 32);
    pthread_mutex_lock(&fftbufLock);
    for(int i = 0; i < 32; i++)
    {
        tmp = 0;
        for(int j = 0; j < 32; j++)
        {
            tmp += fftbufl[i*32+j];
        }
        es[i] = tmp / 32;
    }
    pthread_mutex_unlock(&fftbufLock);
    for(int i = 0; i < 32; i++)
    {
        tmp = 0;
        for(int j = 0; j < 43; j++)
        {
            tmp += energyHistories[i][j];
        }
        ei[i] = tmp / 43;
        memmove(energyHistories[i] + 1, energyHistories[i], sizeof(SAMPLE) * 42);
        energyHistories[i][0] = es[i];
    }
    pthread_mutex_lock(&onbeatLock);
    for(int i = 0; i < 32; i++)
    {
        if(es[i] > 250 * ei[i]) {
            onbeat = 1;
            pthread_mutex_unlock(&onbeatLock);
            return;
        }
    }
    onbeat = 0;
    pthread_mutex_unlock(&onbeatLock);
}

/**
 * Update loop for recording audio
 */
void *processAudio(void *arg) {
    snd_pcm_t *capture_handle = (snd_pcm_t *)arg;
    short bufl[BUFSIZE];
    short bufr[BUFSIZE];
    void *bufs[2] = { bufl, bufr };
    int frames;
    int tomove;
    while(1) {
        frames = snd_pcm_readi (capture_handle, bufs, BUFSIZE);
        tomove = BUFSIZE - frames;
        pthread_mutex_lock(&storageLock);
        if (frames < 0) {
            fprintf (stderr, "read from audio interface failed (%s)\n", 
                snd_strerror (frames)); 
            pthread_mutex_unlock(&storageLock);
            return NULL;
        } else if (frames < BUFSIZE) {
            fprintf (stderr, "Buffer underrun (Only read %d)\n", frames);
            for(int i = 0; i < tomove; i++) {
                storagel[i] = storagel[frames + i];
                storager[i] = storager[frames + i];
            }
        }
        for(int i = 0; i < frames; i++) {
            storagel[tomove + i] = (SAMPLE) bufl[i] / 32768.0;
            storager[tomove + i] = (SAMPLE) bufr[i] / 32768.0;
        }
        pthread_mutex_unlock(&storageLock);
        runFFT();
        beatDetect();
    }
    snd_pcm_close (capture_handle);
    return NULL;
}

void *audioInitialization() {
    for(int i = 0; i < BUFSIZE; i++)
    {
        storager[i] = 0;
        storagel[i] = 0;
    }

    readbufs[0] = readbufl;
    readbufs[1] = readbufr;
    storages[0] = storagel;
    storages[1] = storager;
    fftreadbufs[0] = fftreadbufl;
    fftreadbufs[1] = fftreadbufr;

    inr = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * BUFSIZE);
    outr = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * BUFSIZE);
    inl = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * BUFSIZE);
    outl = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * BUFSIZE);

    for(int i = 0; i < BUFSIZE; i++)
    {
        inr[i][0] = 0;
        inr[i][1] = 0;
        inl[i][0] = 0;
        inl[i][1] = 0;
    }

    planl = fftw_plan_dft_1d(BUFSIZE, inr, outr, FFTW_FORWARD, FFTW_MEASURE);
    planr = fftw_plan_dft_1d(BUFSIZE, inl, outl, FFTW_FORWARD, FFTW_MEASURE);

    int err;
    char *device = "hw:1,0";
    unsigned int rate = 44100;
    snd_pcm_t *capture_handle;
    snd_pcm_hw_params_t *hw_params;

    if ((err = snd_pcm_open (&capture_handle, device, SND_PCM_STREAM_CAPTURE, 0)) < 0) {
        fprintf (stderr, "cannot open audio device %s (%s)\n",
        device,
        snd_strerror (err));
        exit (1);
    }
     
    if ((err = snd_pcm_hw_params_malloc (&hw_params)) < 0) {
        fprintf (stderr, "cannot allocate hardware parameter structure (%s)\n",
        snd_strerror (err));
        exit (1);
    }
     
    if ((err = snd_pcm_hw_params_any (capture_handle, hw_params)) < 0) {
        fprintf (stderr, "cannot initialize hardware parameter structure (%s)\n",
        snd_strerror (err));
        exit (1);
    }
    if ((err = snd_pcm_hw_params_set_access (capture_handle, hw_params, SND_PCM_ACCESS_RW_INTERLEAVED)) < 0) {
        fprintf (stderr, "cannot set access type (%s)\n", 
        snd_strerror (err)); 
        exit (1); 
    } 

    if ((err = snd_pcm_hw_params_set_format (capture_handle, hw_params, SND_PCM_FORMAT_S16_LE)) < 0) {
        fprintf (stderr, "cannot set sample format (%s)\n", 
        snd_strerror (err)); 
        exit (1); 
    } 

    if ((err = snd_pcm_hw_params_set_rate_near (capture_handle, hw_params, &rate, 0)) < 0) {
        fprintf (stderr, "cannot set sample rate (%s)\n", 
        snd_strerror (err)); 
        exit (1); 
    } 

    if ((err = snd_pcm_hw_params_set_channels (capture_handle, hw_params, 1)) < 0) {
        fprintf (stderr, "cannot set channel count (%s)\n", 
        snd_strerror (err)); 
        exit (1); 
    } 

    if ((err = snd_pcm_hw_params (capture_handle, hw_params)) < 0) {
        fprintf (stderr, "cannot set parameters (%s)\n",
        snd_strerror (err)); 
        exit (1); 
    } 

    snd_pcm_hw_params_free (hw_params);

    if ((err = snd_pcm_prepare (capture_handle)) < 0) {
        fprintf (stderr, "cannot prepare audio interface for use (%s)\n", 
        snd_strerror (err)); 
        exit (1); 
    } 

    pthread_t al;
    pthread_create(&al,NULL,processAudio,capture_handle);
}
