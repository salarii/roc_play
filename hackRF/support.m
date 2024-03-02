data = load( "data.txt" );

plot(data(:,1), data(:,2));
xlabel('Frequency (Hz)');
ylabel('Phase (degrees)');
title('Phase Response of the Designed Filter');
grid on;
set(gca, 'FontSize', 30); % Increase font size of the axis ticks

filtered_signal= data(:,2)
Fs = size(filtered_signal, 1);
t = 0:1/Fs:1-1/Fs;
T = 1;

Y = fft(filtered_signal);

% Number of points in FFT/2 + 1
N = length(filtered_signal)/2 + 1; 

% Compute the single-sided spectrum
P1 = abs(Y/Fs);
P1 = P1(1:N);
P1(2:end-1) = 2*P1(2:end-1);

% Define the frequency domain f, correctly this time
f = Fs*(0:(N-1))/length(filtered_signal);

% Plot the single-sided amplitude spectrum
figure; % Open a new figure window
plot(f, P1)
title('Single-Sided Amplitude Spectrum of Composite Signal')
xlabel('Frequency (Hz)')
ylabel('|P1(f)|')
set(gca, 'FontSize', 30);