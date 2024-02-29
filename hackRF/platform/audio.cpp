#include <stdio.h>
#include <math.h>
#include <cassert>
#include <portaudio.h>

#include "hackRF.h"

#define SAMPLE_RATE 44100
#define FREQUENCY 12000
#define DURATION 5



static int patestCallback(const void *inputBuffer, void *outputBuffer,
                          unsigned long framesPerBuffer,
                          const PaStreamCallbackTimeInfo* timeInfo,
                          PaStreamCallbackFlags statusFlags,
                          void *userData) {
    /* Cast data passed through stream to our structure. */

    float *out = (float*)outputBuffer;


    (void) timeInfo; // Prevent unused variable warnings.
    (void) statusFlags;
    (void) inputBuffer;
    auto buffer = giveBuffer();
    if (buffer)
    {
        if (buffer->hasProcessed())
        {
            float div =  256.0;
            auto buff = buffer->getProcessedBatch();
            assert(framesPerBuffer == buff.size());
            for (size_t i = 0; i < buff.size(); ++i) {
                *out++ = buff[i]/div;
                *out++ = buff[i]/div;
            }
            return 0;
        }
    }

    for (unsigned i = 0; i < framesPerBuffer; i++) {
        *out++ = 0;  // Left channel
        *out++ = 0;  // Right channel
    }

    return 0;
}
PaStream *stream;
int stopAudioInternal(void) {
    /* Play for several seconds. */
    PaError err;
    err = Pa_StopStream(stream);
    if (err != paNoError) goto error;

    err = Pa_CloseStream(stream);
    if (err != paNoError) goto error;

    Pa_Terminate();
    printf("Test finished.\n");

    if (err != paNoError) goto error;

    return err;

error:
    Pa_Terminate();
    fprintf(stderr, "An error occured while using the portaudio stream\n");
    fprintf(stderr, "Error number: %d\n", err);
    fprintf(stderr, "Error message: %s\n", Pa_GetErrorText(err));
    return err;
}

int runAudioInternal(void) {

    PaError err;

    err = Pa_Initialize();
    if (err != paNoError) goto error;

    err = Pa_OpenDefaultStream(&stream,
                               0,          /* no input channels */
                               2,          /* stereo output */
                               paFloat32,  /* 32 bit floating point output */
                               SAMPLE_RATE,
                               SAMPLE_RATE,        /* frames per buffer */
                               patestCallback,
                               nullptr);
    if (err != paNoError) goto error;

    err = Pa_StartStream(stream);
    if (err != paNoError) goto error;



    return err;

error:
    Pa_Terminate();
    fprintf(stderr, "An error occured while using the portaudio stream\n");
    fprintf(stderr, "Error number: %d\n", err);
    fprintf(stderr, "Error message: %s\n", Pa_GetErrorText(err));
    return err;
}


// #ifdef __cplusplus
// extern "C" {
// #endif

int runAudio(void)
{
    return  runAudioInternal();

}

// #ifdef __cplusplus
// }
// #endif