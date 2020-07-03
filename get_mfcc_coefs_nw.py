#!/usr/bin/env python
# -*- coding: utf-8 -*- #  

'''
# calculate spectral-envelope variability
# à la Gerosa et al. (2006)
# walk directory of wav files and corresponding textgrids, option to specify single wav file 
# Authors: Meg Cychosz & Keith Johnson 2018, 
# also includes pieces hobbled together from various scripts 
# of Ronald Sprouse 
# UC Berkeley
'''

import os, sys, fnmatch
import subprocess
import audiolabel
import librosa
import numpy as np
import re
import pandas as pd
from collections import OrderedDict
import matplotlib.pyplot as plt
from sys import argv



# Regex to identify segments
segments = re.compile("r|ae|s|e|u|tS|I|w|a|t|aI|k|v|o|i",re.IGNORECASE)
words = re.compile("kaemIg|tSImIg|kasep|kVfim|kIpon|raepoIn|rebiT|rifras|ralaid|rvglok|saemel|Sevas|saiprot|siplok|sudras|svbit|taiblor|tozel|tugraif|wemag|wakraed|waprot|wimel",re.IGNORECASE)

speakerlist = []
phonelist = []
framelist = []
followinglist = []
prevlist = []
wordlist = []
target1score = []
target2score = []
prosodyscore = []
notelist = []
triallist = []
t1list = []
t2list = []
durlist = []
worddurlist = []
normframelist = []



def processwav(wav, tg): # wav = path to the wav file in dir

	f, sr = librosa.load(wav, sr=12000) # load in file and the sampling rate you want
	pm = audiolabel.LabelManager(from_file=os.path.join(dirpath,tg),from_type="praat") # open text grid 

	for word in pm.tier('Word').search(words): 

		t1_idx = np.floor((word.t1)*sr) # Convert time to integer index
		t2_idx = np.floor((word.t2)*sr)
		snippet = f[t1_idx:t2_idx]
		snippet_pm = pm.tier('Phone').tslice(word.t1, word.t2, lincl=False, rincl=False)
	
	    #option to apply pre-emphasis 
	    #emp_f = np.append(f[0], f[1:] - 0.97 * f[:-1])
		
		# get the spectrogram
		FFT = librosa.stft(snippet, n_fft=n_fft, hop_length=hop, win_length=window)

		# convolve the filterbank over the spectrogram
		S = np.log(mel_f.dot(np.abs(FFT)))
		 # note that log is moved up here; log freq is only calculated with mean in function below for other scripts

		def get_vowel_spectrum(S,t1,t2,step_size=step_size, plot=False,label='c'):

		    start_frame = np.int(t1/step_size)
		    end_frame = np.int(t2/step_size)

		    frame = S[:,start_frame:end_frame] # taking log of mel filterbank energies

		    return frame

		#loop through all of the (specified) labels on the "phone" tier of the current word
		for v in snippet_pm: 
			if re.match(segments, v.text):
				#if v.t1==word.t1 or pm.tier('Phone').prev(v).t1==word.t1: # only measure the first two segments of the word
					#print(v.text, word.text)

				t1=v.t1-word.t1
				t2=v.t2-word.t1

				spectrum = get_vowel_spectrum(S, t1, t2,step_size=step_size)

				# define some general time range of the vowel's associated word

				for meas in spectrum:
					#print(meas)

					speakerlist.append(wav.split("_", 1)[1]) 
					phonelist.append(pm.tier('Phone').label_at(v.center).text)
					framelist.append(meas) # get vectors for each frame of phone
					#followinglist.append((pm.tier('Phone').next(v)).text)
					prevlist.append((pm.tier('Phone').prev(v)).text)
					wordlist.append((pm.tier('Word').label_at(v.center)).text)
					target1score.append(pm.tier('Target1Seg').label_at(v.center).text)
					target2score.append(pm.tier('Target2Seg').label_at(v.center).text)
					prosodyscore.append(pm.tier('ProsodyScore').label_at(v.center).text)
					for note in pm.tier('TransNotes'): # not a great method; will break for more labels (albeit there are few examples of this)
						if word.t1 <= note.t1 <= word.t2:
							notelist.append(note.text)
						else: 
							notelist.append('')	
					triallist.append(pm.tier('Trial').label_at(v.center).text)
					t1list.append(v.t1)
					t2list.append(v.t2)
					durlist.append(v.t2-v.t1) 

					worddurlist.append(word.t2-word.t1)
					normframelist = [x / (word.t2-word.t1) for x in framelist] # normalize each MFCC vector (varies by length of phone 11-14) by word duration

	df = pd.DataFrame( OrderedDict( (('Speaker', pd.Series(speakerlist)),
	('Phone', pd.Series(phonelist)), ('Frame', pd.Series(framelist)), 
	('Previous', pd.Series(prevlist)),
	#('Following', pd.Series(followinglist)), 
	('Word', pd.Series(wordlist)), ('Target1Seg', pd.Series(target1score)), 
	('Target2Seg', pd.Series(target2score)), ('ProsodyScore', pd.Series(prosodyscore)),
	('Note', pd.Series(notelist)), ('Trial', pd.Series(triallist)),
	('phone_t1', pd.Series(t1list)), ('phone_t2',  pd.Series(t2list)),
	('Phone_duration', pd.Series(durlist)), ('Word_duration', pd.Series(worddurlist)),
	('Normalized_Frames', pd.Series(normframelist)))))

	df.to_csv('nwr_mfcc_complete.csv', encoding='utf-8') 





# Input wavfile 
filelist = [] # a tuple of wav & tg
if sys.argv[1] == 'walk': # if walk is specified in command line, walk over directory
  for dirpath, dirs, files in os.walk('.'): # walk over current directory
      for soundfile in fnmatch.filter(files, '*.WAV'):
          #soundpath = os.path.join(dirpath, soundfile)
          filename = os.path.splitext(soundfile)[0]
          print(filename)
          tg = filename+'_MCtrans.TextGrid'  # get the accompanying textgrid
          thing_to_add = (soundfile, tg)
          filelist.append(thing_to_add)
else: # option to run single wav file
  soundfile = sys.argv[1]
  tg = os.path.splitext(soundfile)[0]+'_MCtrans.TextGrid'  # get the accompanying textgrid
  thing_to_add = (soundfile, tg)
  filelist.append(thing_to_add)
  dirpath = '.'

# define some parameters
sr = 12000 # option to specify desired sampling rate
step_size = 0.01   # 10 ms between spectra
frame_size = 0.0256  # 25.6 ms frame chunk
hop = np.int(step_size * sr)  
window = np.int(frame_size * sr) 
fmax = np.int(sr/2) # nyquist frequency
fmin = 100
n_fft = 2048 # # of FFT coefficients to compute
n_mels = 29  # # of Mel filter bands

# compute the mel frequency filter bank
mel_f = librosa.filters.mel(sr, n_fft=2048, n_mels=29, fmin=100.0, fmax=6000, htk=True, norm=1)

for wav, tg in filelist: 
      print(wav) # sanity check
      processwav(wav, tg)