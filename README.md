# coartic_experience

Repo contains scripts and data necessary to replicate analyses in:

Cychosz, M., Munson, B., & Edwards, J. (submitted). Practice and experience predict coarticulation in child speech. (https://psyarxiv.com/vwhtk/)

## Acoustic measures

`get_mfcc_coefs_nw.py` - example script to calculate Mel spectral vectors for each consonant and vowel, CV sequence duration, and word duration for nonwords; generates `nwr_mfcc_complete.csv` for further processing 

## Data preparation/analysis

**1a-get_repetition_scores.R** - combines dataframes from two sets of acoustic measures (`rwr_mfcc_complete.csv` and `nwr_mfcc_complete.csv`); generates repetition accuracy scores on the basis of consonant manner, prosody, etc.

**1b-get_repetition_scores_partII.R** - finishes **1a-get_repetition_scores.R** and generates dataframe with acoustic measures and repetition scores; generates `repetition_scores_mfcc.csv`

**2-match_words.R** - select repetitions that were 100% correct from `repetition_scores_mfcc.csv` and generates `final_mfcc_scores.csv`

**3-map_vocab_scores.R** - reads in `final_mfcc_scores.csv` and `vocab_scores.csv` and merges the dataframes; generates `final_mfccs_w_vocab.csv`

**4-model_coartic.R** - reads in `final_mfccs_w_vocab.csv` to process spectral vectors data and calculate Euclidean distance; modeling and figures for paper

## Output

**coartic_experience.pdf** - compiled paper

**coartic_experience.tex** - tex source of coartic_experience.pdf

## Data

`phono_prbability.csv` - phonotactic transition probabilities between CV sequence and first C of second syllable 

`rwr_mfcc_complete.csv` - acoustic measures (spectral vectors, word and phone duration) for each C and V in the real words

`nwr_mfcc_complete.csv` - acoustic measures (spectral vectors, word and phone duration) for each C and V in the nonwords

`repetition_scores_mfcc.csv` - dataframe with acoustic measures and repetition scores from word repetition task

`final_mfcc_scores.csv` - dataframe with 100% correct repetitions and acoustic measures

`vocab_scores.csv` - dataframe with vocabulary scores, LENA measures, and (limited) participant demographic information

`final_mfccs_w_vocab.csv` - dataframe of 100% correct repetitons with acoustic measures, vocabulary scores, LENA measures, and demographic information

