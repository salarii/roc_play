#include <vector>
#include <iostream>
#include <cmath>
#include <fstream>
#include <functional>

std::vector<float> filterSignal(const std::vector<float>& u,
                                  const std::vector<float>& yPar,
                                  const std::vector<float>& uPar) {
    size_t inputLength = u.size();
    std::vector<float> y(inputLength, 0.0); // Initialize output buffer

    // Loop through each sample in the input
    for (size_t n = 0; n < inputLength; ++n) {
        float yn = 0.0;

        // Feedforward part
        for (size_t j = 0; j < uPar.size(); ++j) {
            yn += (n >= j) ? uPar[j] * u[n - j] : 0.0;
        }

        // Feedback part (excluding yPar[0] as it is assumed to be 1)
        for (size_t i = 1; i < yPar.size(); ++i) {
            yn -= (n >= i) ? yPar[i] * y[n - i] : 0.0;
        }

        y[n] = yn;
    }

    return y;
}

std::vector<float> decimate(const std::vector<float>& inputSignal, unsigned int M) {
    std::vector<float> decimatedSignal;

    // Check if M is valid
    if (M <= 0) {
        return decimatedSignal; // Return empty vector if M is not valid
    }

    // Reserve space to avoid reallocations
    decimatedSignal.reserve(inputSignal.size() / M);

    // Keep every Mth sample
    for (size_t i = 0; i < inputSignal.size(); i += M) {
        decimatedSignal.push_back(inputSignal[i]);
    }

    return decimatedSignal;
}

std::vector<float> fmDemodulate(const std::vector<float>& I, const std::vector<float>& Q) {
    if (I.size() != Q.size() || I.empty()) {
        // Return an empty vector if inputs are not of the same size or are empty
        return std::vector<float>();
    }

    std::vector<float> demodulatedSignal;
    demodulatedSignal.reserve(I.size());

    // Placeholder for previous phase to calculate phase difference
    float previousPhase = atan2(Q[0], I[0]);

    for (size_t i = 1; i < I.size(); ++i) {
        float currentPhase = atan2(Q[i], I[i]);

        // Calculate phase difference
        float phaseDifference = currentPhase - previousPhase;

        // Correct for phase wrap around
        while (phaseDifference > M_PI) phaseDifference -= 2 * M_PI;
        while (phaseDifference < -M_PI) phaseDifference += 2 * M_PI;

        demodulatedSignal.push_back(phaseDifference);

        // Update previous phase
        previousPhase = currentPhase;
    }

    // The demodulated signal here is the phase difference, which correlates to the frequency deviation
    // In a real application, you might need to further process this to extract the message signal
    return demodulatedSignal;
}

std::vector<float> processSignal(const std::vector<float>& originalSignal,
                                 const std::vector<float>& preFilterYPar,
                                 const std::vector<float>& preFilterUPar,
                                 unsigned int preDecimationFactor,
                                 const std::vector<float>& filterYPar,
                                 const std::vector<float>& filterUPar,
                                 unsigned int decimationFactor) {
    // Step 1: Pre-filtering
    std::vector<float> preFilteredSignal = filterSignal(originalSignal, preFilterYPar, preFilterUPar);

    // Step 2: Pre-decimation
    std::vector<float> preDecimatedSignal = decimate(preFilteredSignal, preDecimationFactor);

    // Step 3: Filtering
    std::vector<float> filteredSignal = filterSignal(preDecimatedSignal, filterYPar, filterUPar);

    // Step 4: Decimation
    std::vector<float> decimatedSignal = decimate(filteredSignal, decimationFactor);

    return decimatedSignal;
}


void processIQ(const std::vector<uint8_t>& interleavedIQ, std::vector<float>& I, std::vector<float>& Q) {
    // Assuming the input is 8-bit values in the range [0, 255]
    const uint8_t centeringValue = 128; // Value to subtract to center around 0

    size_t numSamples = interleavedIQ.size() / 2;
    I.resize(numSamples);
    Q.resize(numSamples);

    for (size_t i = 0; i < numSamples; ++i) {
        // Subtract centeringValue and convert to float
        I[i] = static_cast<float>(interleavedIQ[2 * i] - centeringValue);
        Q[i] = static_cast<float>(interleavedIQ[2 * i + 1] - centeringValue);
    }
}

std::vector<int> fmModulateIQInterleavedScaled(int numSamples, std::function<float(int)> modFunction, float maxPhaseShiftFrequency) {
    std::vector<int> iqSignal(numSamples * 2); // Initialize I/Q signal vector with float the number of samples
    float phase = 0.0; // Initial phase

    for (int i = 0; i < numSamples; ++i) {
        // Calculate phase shift for current sample
        float phaseShift = maxPhaseShiftFrequency * modFunction(i);
        // Update total phase
        phase += phaseShift;

        // Generate interleaved I/Q signals and scale from -1 to 1, to 0 to 255
        iqSignal[2*i] = static_cast<int>((std::cos(phase) + 1) * 0.5 * 255); // In-phase component (I)
        iqSignal[2*i + 1] = static_cast<int>((std::sin(phase) + 1) * 0.5 * 255); // Quadrature-phase component (Q)
    }

    return iqSignal;
}

auto generateModFunction(int numSamples, float frequency1, float frequency2) {
    // Capture the total number of samples and both frequencies
    return [numSamples, frequency1, frequency2](int sampleIndex) -> float {
        // Calculate the time for the current sample
        float time = static_cast<float>(sampleIndex) / numSamples;
        // Return the sine of the current sample based on the two frequencies
        return std::sin(2 * M_PI * frequency1 * time) + std::sin(2 * M_PI * frequency2 * time);
    };
}


void writeVectorToOctaveFileWithAngle(const std::vector<float>& vec, const std::string& filePath) {
    // Open a file stream for writing
    std::ofstream file(filePath);

    // Check if the file stream is open
    if (!file.is_open()) {
        std::cerr << "Error opening file: " << filePath << std::endl;
        return;
    }

    // Calculate the step size based on the vector size, assuming the span is 2pi
    float stepSize = 2 * M_PI / vec.size();

    // Write each element of the vector to the file, preceded by its corresponding angle
    for (size_t i = 0; i < vec.size(); ++i) {
        float angle = i * stepSize; // Calculate the angle for the current element
        file << angle << " " << vec[i] << std::endl;
    }

    // Close the file stream
    file.close();

    std::cout << "Data successfully written to " << filePath << std::endl;
}

std::vector<float> processAudioSignal(const std::vector<uint8_t>& inputSignal) {

    std::vector<float> I, Q;
    processIQ(inputSignal, I, Q);

    // Define filter parameters (example given, replace with actual function calls or processes)
    std::vector<float> preFilterYPar = {1.00000f, -3.89787f, 5.71840f, -3.74190f, 0.92144f};
    std::vector<float> preFilterUPar = {0.0011148f, -0.0038066f, 0.0054380f, -0.0038066f, 0.0011148f};
    unsigned preDecimationFactor = 10;

    std::vector<float> filterYPar = {1.00000f, -2.43144f, 3.56108f, -3.13820f, 1.71804f, -0.48897f};
    std::vector<float> filterUPar = {0.016428f, 0.036650f, 0.057172f, 0.057172f, 0.036650f, 0.016428f};
    unsigned decimationFactor = 2;

    // Process signals
    std::vector<float> Iprocessed = processSignal(I, preFilterYPar, preFilterUPar, preDecimationFactor, filterYPar, filterUPar, decimationFactor);
    std::vector<float> Qprocessed = processSignal(Q, preFilterYPar, preFilterUPar, preDecimationFactor, filterYPar, filterUPar, decimationFactor);

    // FM demodulate (assuming fmDemodulate is defined)
    std::vector<float> demodulated = fmDemodulate(Iprocessed, Qprocessed);

    // Final filter parameters
    std::vector<float> yPar = {1.00000f, -5.22957f, 11.43904f, -13.39218f, 8.84839f, -3.12759f, 0.46195f};
    std::vector<float> uPar = {0.00000068233f, 0.00000409396f, 0.00001023489f, 0.00001364653f, 0.00001023489f, 0.00000409396f, 0.00000068233f};

    // Apply final filter
    std::vector<float> readyForAudioSignal = filterSignal(demodulated, yPar, uPar);

    // Decimate (assuming decimate is defined)
    unsigned audioDec = 10;
    std::vector<float> audio = decimate(readyForAudioSignal, audioDec);

    // Return processed audio signal
    auto size =  audio.size();
    for (float& value : audio) {
        value *= 20; // Alter each element, e.g., multiply by 2
    }
    return audio;
}

/*
int main() {

    // Number of samples, modulation function, and max phase shift frequency
    int numSamples = 8820000;
    //int numSamples = 220e3;
    float maxPhaseShift =2*M_PI*75000/(float)numSamples;
    float  desiredFrequency1 = 1000000;
    float  desiredFrequency2 = 60000;
    auto modFunction = generateModFunction(numSamples, desiredFrequency1,desiredFrequency2);

    // Perform FM modulation and scaling
    std::vector<int> interleavedIQ = fmModulateIQInterleavedScaled(numSamples, modFunction, maxPhaseShift);

    std::vector<float> I, Q;

    processIQ(interleavedIQ, I, Q);

    std::vector<float> preFilterYPar = {1.00000f, -3.89787f, 5.71840f, -3.74190f, 0.92144f};
    std::vector<float> preFilterUPar = {0.0011148f, -0.0038066f, 0.0054380f, -0.0038066f, 0.0011148f};

    unsigned preDecimationFactor = 10;

    std::vector<float> filterYPar = {1.00000f, -2.43144f, 3.56108f, -3.13820f, 1.71804f, -0.48897f};
    std::vector<float> filterUPar = {0.016428f, 0.036650f, 0.057172f, 0.057172f, 0.036650f, 0.016428f};
    unsigned decimationFactor = 2;
    std::vector<float> Iprocessed = processSignal(
        I,
        preFilterYPar,
        preFilterUPar,
        preDecimationFactor,
        filterYPar,
        filterUPar,
        decimationFactor);

    std::vector<float> Qprocessed = processSignal(
        Q,
        preFilterYPar,
        preFilterUPar,
        preDecimationFactor,
        filterYPar,
        filterUPar,
        decimationFactor);

    std::vector<float> demodulated = fmDemodulate(Iprocessed, Qprocessed);

    std::vector<float> yPar = {1.00000f, -5.22957f, 11.43904f, -13.39218f, 8.84839f, -3.12759f, 0.46195f};
    std::vector<float> uPar = {0.00000068233f, 0.00000409396f, 0.00001023489f, 0.00001364653f, 0.00001023489f, 0.00000409396f, 0.00000068233f};


    std::vector<float> readyForAudioSignal = filterSignal(demodulated, yPar, uPar);

    unsigned audioDec = 10;
    // Step 2: Pre-decimation
    std::vector<float> audio = decimate(readyForAudioSignal, audioDec);
    writeVectorToOctaveFileWithAngle(audio, "data.txt");
    std::cout << audio.size() <<"\n";
    // Example input signal (populate with you
    return 0;
}
*/