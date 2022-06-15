from scipy.io import wavfile as wav
from scipy.fftpack import fft
import soundfile as sf
import numpy as np

import wave

rate, data = wav.read('ariana.wav')
fft_out = fft(data)

print(data[20000:25000])
