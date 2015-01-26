#ifndef Audio
#define Audio

//Audio related stuff
#define BUFSIZE (1024)
typedef float SAMPLE;

void audioInitialization();
SAMPLE *getSoundBuffer();
SAMPLE *runFFT();

#endif
