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
fftw_complex *in, *out;
fftw_plan plan;

pthread_mutex_t storageLock = PTHREAD_MUTEX_INITIALIZER;

SAMPLE storage[BUFSIZE];
SAMPLE readbuf[BUFSIZE];
SAMPLE fftbuf[BUFSIZE];

double PI = 3.141592653;

float hann_window(int sample, int num_samples) {
        return 0.5 * (1 - cos((2 * PI * sample) / (num_samples - 1)));
}

SAMPLE *getSoundBuffer() {
    pthread_mutex_lock(&storageLock);
    for(int i = 0; i < BUFSIZE; i++)
        readbuf[i] = storage[i] * hann_window(i, BUFSIZE);
    pthread_mutex_unlock(&storageLock);
    return readbuf;
}

SAMPLE *runFFT()
{
    pthread_mutex_lock(&storageLock);
    for(int i = 0; i < BUFSIZE; i++)
        in[i][0] = storage[i];
    pthread_mutex_unlock(&storageLock);
    fftw_execute(plan);
    for(int i = 0; i < BUFSIZE; i++)
    {
        fftbuf[i] = sqrt(out[i][0] * out[i][0] + out[i][1] * out[i][1]);
    }
    return fftbuf;
}

/**
 * Update loop for recording audio
 */
void *processAudio(void *arg) {
    snd_pcm_t *capture_handle = (snd_pcm_t *)arg;
    short buf[BUFSIZE];
    int err;
    while(1) {
        if ((err = snd_pcm_readi (capture_handle, buf, BUFSIZE)) != BUFSIZE) {
            fprintf (stderr, "read from audio interface failed (%s)\n", 
            snd_strerror (err)); 
            exit (1); 
        } 
        pthread_mutex_lock(&storageLock);
        for(int i = 0; i < BUFSIZE; i++) {
            storage[i] = (SAMPLE) buf[i] / 32768.0;
        }
        pthread_mutex_unlock(&storageLock);
    }

    snd_pcm_close (capture_handle);
}

void audioInitialization() {
    for(int i = 0; i < BUFSIZE; i++)
        storage[i] = 0;

    in = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * BUFSIZE);
    out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * BUFSIZE);
    for(int i = 0; i < BUFSIZE; i++)
    {
        in[i][0] = 0;
        in[i][1] = 0;
    }

    plan = fftw_plan_dft_1d(BUFSIZE, in, out, FFTW_FORWARD, FFTW_MEASURE);

    int err;
    char *device = "hw:1,0";
    unsigned int rate = 48000;
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
