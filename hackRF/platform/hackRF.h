#ifndef HACK_RF_H
#define HACK_RF_H

#include <iostream>
#include <vector>
#include <deque>
#include <list>
#include <algorithm> // For std::copy
#include <hackrf.h>

int startHackRF();

class SampleBuffer {
private:
    std::deque<float> buffer; // Using deque for efficient front removal
    std::list<std::vector<float>> processedBatches; // Store processed batches
    size_t batchSize; // Size of the batch to process

public:
    SampleBuffer(size_t batchSz);

    void addChunk(float* chunk, size_t chunkSize);
    std::vector<float> getBatch();
    bool hasBatch() const;
    void processAndStoreBatch();
    void forwardOneProcessedBatch();
};

void processBatch(const std::vector<float>& batch);
void forwardBatch(const std::vector<float>& batch);

#endif // HACK_RF_H
