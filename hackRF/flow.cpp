#include <vector>
#include <iostream>
#include <cmath>
#include <fstream>
#include <functional>

std::vector<float> filterSignal(const std::vector<float>& u,
                                const std::vector<float>& yPar,
                                const std::vector<float>& uPar) {
    size_t inputLength = u.size();
    std::vector<float> y(inputLength, 0.0f); // Initialize output buffer

    // Assuming yPar[0] is 1.0f and is not used in the computation
    for (size_t n = 0; n < inputLength; ++n) {
        float yn = 0.0f;
        for (size_t j = 0; j < uPar.size(); ++j) {
            yn += (n >= j) ? uPar[j] * u[n - j] : 0.0f;
        }
        for (size_t i = 1; i < yPar.size(); ++i) {
            yn -= (n >= i) ? yPar[i] * y[n - i] : 0.0f;
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


void processIQ(const std::vector<int>& interleavedIQ, std::vector<float>& I, std::vector<float>& Q) {
    // Assuming the input is 8-bit values in the range [0, 255]
    const int centeringValue = 128; // Value to subtract to center around 0

    size_t numSamples = interleavedIQ.size() / 2;
    I.resize(numSamples);
    Q.resize(numSamples);

    for (size_t i = 0; i < numSamples; ++i) {
        // Subtract centeringValue and convert to float
        I[i] = static_cast<float>(interleavedIQ[2 * i] - centeringValue);
        Q[i] = static_cast<float>(interleavedIQ[2 * i + 1] - centeringValue);
    }
}

std::vector<int> fmModulateIQInterleavedScaled(int numSamples, std::function<double(int)> modFunction, double maxPhaseShiftFrequency) {
    std::vector<int> iqSignal(numSamples * 2); // Initialize I/Q signal vector with double the number of samples
    double phase = 0.0; // Initial phase

    for (int i = 0; i < numSamples; ++i) {
        // Calculate phase shift for current sample
        double phaseShift = maxPhaseShiftFrequency * modFunction(i);
        // Update total phase
        phase += phaseShift;

        // Generate interleaved I/Q signals and scale from -1 to 1, to 0 to 255
        iqSignal[2*i] = static_cast<int>((std::cos(phase) + 1) * 0.5 * 255); // In-phase component (I)
        iqSignal[2*i + 1] = static_cast<int>((std::sin(phase) + 1) * 0.5 * 255); // Quadrature-phase component (Q)
    }

    return iqSignal;
}

auto generateModFunction(int numSamples, double frequency) {
    // Capture the total number of samples and desired frequency
    return [numSamples, frequency](int sampleIndex) -> double {
        // Calculate the time for the current sample
        double time = static_cast<double>(sampleIndex) / numSamples;
        // Return the sine of the current sample based on the desired frequency
        return std::sin(2 * M_PI * frequency * time);
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
int main() {

    // Number of samples, modulation function, and max phase shift frequency
    int numSamples = 8820000;

    double maxPhaseShift =2*M_PI*75000/(double)numSamples;
    float  desiredFrequency = 5000;
    auto modFunction = generateModFunction(numSamples, desiredFrequency);

    // Perform FM modulation and scaling
    std::vector<int> interleavedIQ = fmModulateIQInterleavedScaled(numSamples, modFunction, maxPhaseShift);


    //initial signal sampling rate 8820000
    // Example usage

    std::vector<float> I, Q;

    processIQ(interleavedIQ, I, Q);
    std::vector<float> preFilterYPar = {1.0f, -7.99230f, 28.91219f, -62.31535f, 88.58766f, -86.76707f, 59.28095f, -27.89030f, 8.64568f, -1.59424f, 0.13277f};
    std::vector<float> preFilterUPar = {0.0000000036197f, 0.0000000361970f, 0.0000001628863f, 0.0000004343634f, 0.0000007601360f, 0.0000009121632f, 0.0000007601360f, 0.0000004343634f, 0.0000001628863f, 0.0000000361970f, 0.0000000036197f};

    unsigned preDecimationFactor = 10;

    std::vector<float> filterYPar = {1.0f, -4.576198780309f, 12.378921753412f, -23.444731893213f,
                               34.268542340966f, -40.241502658082f, 39.033367564208f,
                               -31.746715867789f, 21.869263978165f, -12.822863985243f,
                               6.411647305961f, -2.730066552846f, 0.985693433036f,
                               -0.299456599484f, 0.075661517526f, -0.015628008993f,
                               0.002573311272f, -0.000325120899f, 0.000029616833f,
                               -0.000001732641f, 0.000000048912f};

    std::vector<float> filterUPar = {0.00000014134f, 0.00000282688f, 0.00002685531f, 0.00016113188f,
                               0.00068481050f, 0.00219139360f, 0.00547848400f, 0.01095696800f,
                               0.01780507300f, 0.02374009734f, 0.02611410707f, 0.02374009734f,
                               0.01780507300f, 0.01095696800f, 0.00547848400f, 0.00219139360f,
                               0.00068481050f, 0.00016113188f, 0.00002685531f, 0.00000282688f,
                               0.00000014134f};



    unsigned decimationFactor = 4;

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

    std::vector<float> yPar = {1.0f, -13.8268062f, 91.5231183f, -385.4454328f, 1157.8504604f,
                               -2636.1226478f, 4718.3436558f, -6796.6638327f, 8000.1939902f,
                               -7768.8810671f, 6256.6168696f, -4185.1725290f, 2320.7844565f,
                               -1060.8570535f, 395.7714269f, -118.6303476f, 27.8963253f,
                               -4.9591693f, 0.6269049f, -0.0502414f, 0.0019196f};

    std::vector<float> uPar = {3.1450e-14f, 6.2900e-13f, 5.9755e-12f, 3.5853e-11f, 1.5238e-10f,
                               4.8760e-10f, 1.2190e-09f, 2.4380e-09f, 3.9618e-09f, 5.2824e-09f,
                               5.8106e-09f, 5.2824e-09f, 3.9618e-09f, 2.4380e-09f, 1.2190e-09f,
                               4.8760e-10f, 1.5238e-10f, 3.5853e-11f, 5.9755e-12f, 6.2900e-13f,
                               3.1450e-14f};

    std::vector<float> readyForAudioSignal = filterSignal(demodulated, yPar, uPar);

    unsigned audioDec = 5;
    // Step 2: Pre-decimation
    std::vector<float> audio = decimate(readyForAudioSignal, audioDec);

    std::cout << audio.size() <<"\n";
    // Example input signal (populate with you
    return 0;
}
