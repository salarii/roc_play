#include <stdio.h>
#include <math.h>
#include <portaudio.h>

#define SAMPLE_RATE 44100
#define FREQUENCY 12000
#define DURATION 5

typedef struct {
    double phase;
    double phase_increment;
} paTestData;

static int patestCallback(const void *inputBuffer, void *outputBuffer,
                          unsigned long framesPerBuffer,
                          const PaStreamCallbackTimeInfo* timeInfo,
                          PaStreamCallbackFlags statusFlags,
                          void *userData) {
    /* Cast data passed through stream to our structure. */
    paTestData *data = (paTestData*)userData;
    float *out = (float*)outputBuffer;
    unsigned long i;

    (void) timeInfo; // Prevent unused variable warnings.
    (void) statusFlags;
    (void) inputBuffer;

    for (i = 0; i < framesPerBuffer; i++) {
        *out++ = sin(data->phase);  // Left channel
        *out++ = sin(data->phase);  // Right channel
        data->phase += data->phase_increment;
        if (data->phase >= 2.0 * M_PI) data->phase -= 2.0 * M_PI;
    }

    return 0;
}

int runAudioInternal(void) {
    PaStream *stream;
    PaError err;
    paTestData data;

    data.phase = 0;
    data.phase_increment = 2.0 * M_PI * FREQUENCY / SAMPLE_RATE;

    err = Pa_Initialize();
    if (err != paNoError) goto error;

    err = Pa_OpenDefaultStream(&stream,
                               0,          /* no input channels */
                               2,          /* stereo output */
                               paFloat32,  /* 32 bit floating point output */
                               SAMPLE_RATE,
                               256,        /* frames per buffer */
                               patestCallback,
                               &data);
    if (err != paNoError) goto error;

    err = Pa_StartStream(stream);
    if (err != paNoError) goto error;

    /* Play for several seconds. */
    Pa_Sleep(DURATION * 1000);

    err = Pa_StopStream(stream);
    if (err != paNoError) goto error;

    err = Pa_CloseStream(stream);
    if (err != paNoError) goto error;

    Pa_Terminate();
    printf("Test finished.\n");

    return err;

error:
    Pa_Terminate();
    fprintf(stderr, "An error occured while using the portaudio stream\n");
    fprintf(stderr, "Error number: %d\n", err);
    fprintf(stderr, "Error message: %s\n", Pa_GetErrorText(err));
    return err;
}


#ifdef __cplusplus
extern "C" {
#endif

int runAudio(void)
{
    return  runAudioInternal();

}

#ifdef __cplusplus
}
#endif