#include <iostream>

#include <chrono>
#include <thread>



#include "audio.h"
#include "hackRF.h"


std::shared_ptr<SampleBuffer> sampleBuffer;
hackrf_device* device = nullptr;


SampleBuffer::SampleBuffer(size_t batchSz) : batchSize(batchSz) {}

std::shared_ptr<SampleBuffer> giveBuffer()
{
    return sampleBuffer;
}

void SampleBuffer::addChunk(float* chunk, size_t chunkSize) {
    std::lock_guard<std::mutex> mutex(bufferMtx);
    for (size_t i = 0; i < chunkSize; ++i) {
        buffer.push_back(chunk[i]);
    }
    //delete[] chunk; // Properly delete the C-style array after copying the data
}

std::vector<float> SampleBuffer::getBatch() {
    std::lock_guard<std::mutex> mutex(bufferMtx);
    std::vector<float> batch;
    if (buffer.size() < batchSize) {
        return batch; // Return empty if not enough data
    }

    // Copy the data from the buffer to the batch
    std::copy(buffer.begin(), buffer.begin() + batchSize, std::back_inserter(batch));
    // Remove the data from the buffer
    buffer.erase(buffer.begin(), buffer.begin() + batchSize);
    return batch;
}

bool SampleBuffer::hasBatch() const {
    std::lock_guard<std::mutex> mutex(bufferMtx);
    return buffer.size() >= batchSize;
}

bool SampleBuffer::hasProcessed() const {
    std::lock_guard<std::mutex> mutex(batchesMtx);
    return !processedBatches.empty();
}

void SampleBuffer::processAndStoreBatch(std::function<std::vector<float>(std::vector<float>)> _callback) {
    while (hasBatch()) {
        std::vector<float> batch = getBatch();
        auto processed = _callback(batch);
        std::lock_guard<std::mutex> mutex(batchesMtx);
        processedBatches.push_back(std::move(processed));
    }
}

std::vector<float> SampleBuffer::getProcessedBatch() {
    std::lock_guard<std::mutex> mutex(batchesMtx);
    if (!processedBatches.empty()) {
        auto batch = processedBatches.front();
        processedBatches.pop_front();
        return batch;
    }
    std::vector<float>();
}

void process(std::function<std::vector<float>(std::vector<float>)> _callback) {
    if (sampleBuffer)
    {
        sampleBuffer->processAndStoreBatch(_callback);
    }
}

std::vector<float> getBatch()
{
    if (sampleBuffer)
    {
        return sampleBuffer->getProcessedBatch();
    }
    return std::vector<float>();
}

// Callback function to handle received samples
int rx_callback(hackrf_transfer* transfer) {
    // Here, we simply print the number of bytes received to demonstrate data handling
    // In a real application, you would process the IQ samples contained in transfer->buffer

    if (sampleBuffer)
    {
        int numberOfSamples = transfer->valid_length;
        float arr [numberOfSamples];
        for (size_t i = 0; i < numberOfSamples; i++) {
            arr[i] = transfer->buffer[i] - 128;
        }

        sampleBuffer->addChunk(arr, numberOfSamples);
        std::cout << "Received " << transfer->valid_length << " bytes" << std::endl;
    }
    return HACKRF_SUCCESS;
}


int startHackRF(float _freq, int _sampleRate)
{
    int result = HACKRF_SUCCESS;

    // Initialize the HackRF library
    result = hackrf_init();
    if (result != HACKRF_SUCCESS) {
        std::cerr << "Failed to initialize HackRF: " << hackrf_error_name((hackrf_error)result) << std::endl;
        return EXIT_FAILURE;
    }

    // Open the first HackRF device found

    result = hackrf_open(&device);
    if (result != HACKRF_SUCCESS) {
        std::cerr << "Failed to open HackRF device: " << hackrf_error_name((hackrf_error)result) << std::endl;
        hackrf_exit(); // Clean up HackRF library
        return EXIT_FAILURE;
    }

    // Set frequency
    result = hackrf_set_freq(device, _freq); // 915 MHz
    if (result != HACKRF_SUCCESS) {
        std::cerr << "Failed to set frequency: " << hackrf_error_name((hackrf_error)result) << std::endl;
    }

    // Set sample rate
    result = hackrf_set_sample_rate(device, _sampleRate); // 10 MSPS
    if (result != HACKRF_SUCCESS) {
        std::cerr << "Failed to set sample rate: " << hackrf_error_name((hackrf_error)result) << std::endl;
    }

    // Set LNA gain
    result = hackrf_set_lna_gain(device, 30);
    if (result != HACKRF_SUCCESS) {
        std::cerr << "Failed to set LNA gain: " << hackrf_error_name((hackrf_error)result) << std::endl;
    }

    // Set VGA gain
    result = hackrf_set_vga_gain(device, 50);
    if (result != HACKRF_SUCCESS) {
        std::cerr << "Failed to set VGA gain: " << hackrf_error_name((hackrf_error)result) << std::endl;
    }

    // Enable or disable the AMP
    result = hackrf_set_amp_enable(device, 0); // 0 to disable
    if (result != HACKRF_SUCCESS) {
        std::cerr << "Failed to set AMP enable: " << hackrf_error_name((hackrf_error)result) << std::endl;
    }

    // Start receiving with the callback function
    result = hackrf_start_rx(device, rx_callback, nullptr);
    if (result != HACKRF_SUCCESS) {
        std::cerr << "Failed to start RX mode: " << hackrf_error_name((hackrf_error)result) << std::endl;
        hackrf_close(device);
        hackrf_exit();
        return EXIT_FAILURE;
    }
    //sampleBuffer.reset(new SampleBuffer(_sampleRate));
    sampleBuffer.reset(new SampleBuffer(44100));

}

void stopHackRF()
{
    // Stop the RX mode
    hackrf_stop_rx(device);

    // Close the device and clean up the library
    hackrf_close(device);
    hackrf_exit();

    std::cout << "stopped." << std::endl;

    sampleBuffer.reset();

}