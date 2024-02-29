#ifndef HACK_RF_H
#define HACK_RF_H

#include <iostream>
#include <vector>
#include <deque>
#include <list>
#include <algorithm> // For std::copy
#include <hackrf.h>
#include <mutex>
#include <functional>
#include <memory>


int startHackRF(float _freq, int _sampleRate);
void stopHackRF();

void process(std::function<std::vector<float>(std::vector<float>)> _callback);
std::vector<float> getBatch();

    // processData(myData, [](const std::vector<float>& vec) -> float {
    //     float sum = 0.0f;
    //     for (auto val : vec) {
    //         sum += val;
    //     }
    //     return sum; // Return the sum of the vector's elements
    // });

class SampleBuffer {
private:
    std::deque<float> buffer; // Using deque for efficient front removal
    std::list<std::vector<float>> processedBatches; // Store processed batches
    size_t batchSize; // Size of the batch to process
    mutable std::mutex bufferMtx;
    mutable std::mutex batchesMtx;
public:
    SampleBuffer(size_t batchSz);
    bool hasProcessed() const;
    void addChunk(float* chunk, size_t chunkSize);
    std::vector<float> getBatch();
    bool hasBatch() const;
    void processAndStoreBatch(std::function<std::vector<float>(std::vector<float>)> _callback);
    std::vector<float> getProcessedBatch();

};

std::shared_ptr<SampleBuffer> giveBuffer();
#endif // HACK_RF_H
