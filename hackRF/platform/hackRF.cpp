#include <iostream>

#include <chrono>
#include <thread>

#include "audio.h"
#include "hackRF.h"

SampleBuffer::SampleBuffer(size_t batchSz) : batchSize(batchSz) {}

void SampleBuffer::addChunk(float* chunk, size_t chunkSize) {
    for (size_t i = 0; i < chunkSize; ++i) {
        buffer.push_back(chunk[i]);
    }
    delete[] chunk; // Properly delete the C-style array after copying the data
}

std::vector<float> SampleBuffer::getBatch() {
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
    return buffer.size() >= batchSize;
}

void SampleBuffer::processAndStoreBatch() {
    while (hasBatch()) {
        std::vector<float> batch = getBatch();
        processBatch(batch); // Process the batch

        // Store the processed batch
        processedBatches.push_back(batch);
    }
}

void SampleBuffer::forwardOneProcessedBatch() {
    if (!processedBatches.empty()) {
        auto& batch = processedBatches.front();
        forwardBatch(batch); // Forward the batch
        processedBatches.pop_front(); // Remove the batch from the list after forwarding
    }
}

void processBatch(const std::vector<float>& batch) {
    // Placeholder for actual processing logic
    for (float sample : batch) {
        std::cout << sample << " ";
    }
    std::cout << std::endl;
}

void forwardBatch(const std::vector<float>& batch) {
    // Placeholder for forwarding logic
    std::cout << "Forwarding batch: ";
    for (float sample : batch) {
        std::cout << sample << " ";
    }
    std::cout << std::endl;
}




// Callback function to handle received samples
int rx_callback(hackrf_transfer* transfer) {
    // Here, we simply print the number of bytes received to demonstrate data handling
    // In a real application, you would process the IQ samples contained in transfer->buffer

    for(int i = 100000; i < 100052; i += 2) { // Step by 2 to handle I and Q components
        float I = (float)(transfer->buffer[i] - 128);
        float Q = (float)(transfer->buffer[i+1] - 128);

        // Now I and Q are centered around 0 and can be used for further processing
        std::cout << "I: " << I << ", Q: " << Q << std::endl;
    }
    std::cout << "Received " << transfer->valid_length << " bytes" << std::endl;
    return HACKRF_SUCCESS;
}

int startHackRF()
{
    int result = HACKRF_SUCCESS;

    // Initialize the HackRF library
    result = hackrf_init();
    if (result != HACKRF_SUCCESS) {
        std::cerr << "Failed to initialize HackRF: " << hackrf_error_name((hackrf_error)result) << std::endl;
        return EXIT_FAILURE;
    }

    // Open the first HackRF device found
    hackrf_device* device = nullptr;
    result = hackrf_open(&device);
    if (result != HACKRF_SUCCESS) {
        std::cerr << "Failed to open HackRF device: " << hackrf_error_name((hackrf_error)result) << std::endl;
        hackrf_exit(); // Clean up HackRF library
        return EXIT_FAILURE;
    }

    // Set frequency
    result = hackrf_set_freq(device, 40000000); // 915 MHz
    if (result != HACKRF_SUCCESS) {
        std::cerr << "Failed to set frequency: " << hackrf_error_name((hackrf_error)result) << std::endl;
    }

    // Set sample rate
    result = hackrf_set_sample_rate(device, 20000000); // 10 MSPS
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

    // Receive for a short period (e.g., 5 seconds) then exit for this example
    std::cout << "Receiving for 5 seconds..." << std::endl;
    std::this_thread::sleep_for(std::chrono::seconds(1));

    // Stop the RX mode
    hackrf_stop_rx(device);

    // Close the device and clean up the library
    hackrf_close(device);
    hackrf_exit();

    std::cout << "Done." << std::endl;

}

