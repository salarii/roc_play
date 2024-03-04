data = load( "data.txt" );
%{
plot(data(:,1), data(:,2));
xlabel('Frequency (Hz)');
ylabel('Phase (degrees)');
title('Phase Response of the Designed Filter');
grid on;
set(gca, 'FontSize', 30); % Increase font size of the axis ticks
%}
filtered_signal= data(:,2);
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


%  fm  design 



pkg load signal

fSamp = 8.82e6

% Filter specifications
n = 4 ; % Filter order
Rp = 1; % Passband ripple in dB
Rs = 60; % Stopband attenuation in dB
Wp = 0.045; % Normalized cutoff frequency (half the sampling rate)

% Design the filter
[b, a] = ellip(n, Rp, Rs, Wp);
a
b

a =  []

b = [ ]

decim  = 10

fSamp = 8.82e5

% Filter specifications
n =  5; % Filter order
Rp = 3; % Passband ripple in dB
Rs = 60; % Stopband attenuation in dB
Wp = 0.4; % Normalized cutoff frequency (half the sampling rate)

[b, a] = ellip(n, Rp, Rs, Wp);
a
b

a =[  1.00000  -4.26975   7.67058  -7.20659   3.53448  -0.72425]


b =[ 0.0025653  -0.0026841   0.0023486   0.0023486  -0.0026841   0.0025653 ]


decim  = 4

fSamp = 441e3

Fc = 14e3;  % Cutoff frequency in Hz
N = 6;     % Filter order

% Normalize the cutoff frequency
Wn = Fc / (fSamp / 2);

% Generate filter coefficients
[b, a] = butter(N, Wn, 'low');

a
b

a =[ ]


b =[ ]

decim  = 10





% Display the filter coefficients
disp('Filter coefficients (b):');
disp(b);
disp('Filter coefficients (a):');
disp(a);

% Frequency response (magnitude and phase)
[h, w] = freqz(b, a, 1024);

% Plot magnitude response
figure;
subplot(2,1,1); % Plot in the upper half of the figure
plot((w/pi)*fSamp/2, 20*log10(abs(h)))
xlabel('Normalized Frequency (\times\pi rad/sample)')
ylabel('Magnitude (dB)')
title('Frequency Response of the Designed Elliptic Filter')
grid on
set(gca, 'FontSize', 30); % Increase font size of the axis ticks
% Plot phase response
subplot(2,1,2); % Plot in the lower half of the figure
plot((w/pi)*fSamp/2, unwrap(angle(h)) * (180/pi))
xlabel('Normalized Frequency (\times\pi rad/sample)')
ylabel('Phase (degrees)')
title('Phase Response of the Designed Elliptic Filter')
grid on

set(gca, 'FontSize', 30); % Increase font size of the axis ticks

% Plot zeros and poles
 zplane(b, a);
% Find objects in the current figure
objs = findobj(gcf);

% Identify zeros and poles by their marker style
zeros_objs = findobj(objs, 'Marker', 'o'); % Zeros are usually circles
poles_objs = findobj(objs, 'Marker', 'x'); % Poles are usually x's

% Increase marker size
set(zeros_objs, 'MarkerSize', 20); % Adjust as needed
set(poles_objs, 'MarkerSize', 20); % Adjust as needed




