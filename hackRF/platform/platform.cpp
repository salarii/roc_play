#include <iostream>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <queue>
#include <string>
#include <optional>
#include <cctype>
#include "hackRF.h"

std::mutex mtx;
std::condition_variable cv;
std::queue<std::string> dataQueue; // Queue to hold data between input and processing thread

std::mutex processedMtx;
std::condition_variable processedCv;
std::queue<std::string> processedQueue; // Queue to hold data between processing and work thread


std::optional<float> stringToFloat(const std::string& str) {
    bool hasDecimal = false;
    bool hasExponent = false;
    bool hasNumber = false;

    for (size_t i = 0; i < str.size(); ++i) {
        if (str[i] == '.' && !hasDecimal && !hasExponent) {
            hasDecimal = true; // Mark that we found a decimal point
        } else if ((str[i] == 'e' || str[i] == 'E') && !hasExponent && hasNumber) {
            hasExponent = true; // Mark that we found an exponent
            hasDecimal = false; // Reset decimal flag in case of exponent notation
            if (i + 1 < str.size() && (str[i + 1] == '+' || str[i + 1] == '-')) {
                ++i; // Skip the sign of the exponent
            }
        } else if (std::isdigit(str[i])) {
            hasNumber = true; // Mark that we found a digit
        } else if (!std::isspace(str[i]) && (i != 0 || (str[i] != '+' && str[i] != '-'))) {
            // If the character is not a space, it's not at the start (allowing '+' or '-'), or it's not a digit, fail
            return std::nullopt;
        }
    }

    if (!hasNumber) { // If no digits were found, it's not a valid number
        return std::nullopt;
    }

    try {
        size_t processed = 0;
        float result = std::stof(str, &processed);
        if (processed == str.size()) { // Ensure the entire string was processed
            return result;
        }
    } catch (...) {
        // Catch block is only here as a fallback since exceptions were to be avoided.
    }

    return std::nullopt;
}

// Thread function to handle user keyboard input
void inputThread() {
    std::string input;
    while (true) {
        std::cout << "type exit, or frequency you want to operate format as  eg. 10000, 213.44344  or  1.3e3" << std::endl;
        std::getline(std::cin, input);
        if (input == "exit") break;

        {
            std::lock_guard<std::mutex> lk(mtx);
            dataQueue.push(input);
        }
        cv.notify_one();
    }
}

// Thread function to process data
void processingThread() {
    std::string data;
    while (true) {
        std::unique_lock<std::mutex> lk(mtx);
        cv.wait(lk, []{ return !dataQueue.empty(); });

        data = dataQueue.front();
        dataQueue.pop();
        lk.unlock();

        {
            std::lock_guard<std::mutex> processedLk(processedMtx);
            processedQueue.push(data);
        }
        processedCv.notify_one();
        if (data == "exit")
        {
             break;
        }
        else
        {
            auto freq = stringToFloat(data);
            if (freq )
            {
                if ( startHackRF() == EXIT_FAILURE )
                {
                    std::cout << "hack rf failed to start" << std::endl;
                    continue;
                }

            }
            else
            {
                std::cout << "input is not a number" << std::endl;
            }
        }
    }
}

// Thread function to work on processed data
void workThread() {
    std::string processedData;
    while (true) {
        std::unique_lock<std::mutex> processedLk(processedMtx);
        processedCv.wait(processedLk, []{ return !processedQueue.empty(); });

        processedData = processedQueue.front();
        processedQueue.pop();

        if (processedData == "Processed: exit") break;

        std::cout << processedData << std::endl;
    }
}

int main() {
    std::thread input(inputThread);
    std::thread processing(processingThread);
    std::thread work(workThread);

    // Wait for the threads to finish
    input.join();
    processing.join();
    work.join();

    return 0;
}
