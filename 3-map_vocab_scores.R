# script to map speakers to their language assessment scores

rm(list=ls())

data <- read.csv ('Google_Drive/non-word_rep/acoustic_analyses/data/coartic/final_mfcc_scores.csv') 
vocab_data <- read.csv ('Google_Drive/non-word_rep/acoustic_analyses/data/vocab_scores.csv') 


# remove everything after the L to merge dfs
data$Participant_ID <- gsub("(L).*", '', data$Speaker)
vocab_data$Participant_ID <- gsub("(L).*", '', vocab_data$Participant_ID)


final <- merge(data, vocab_data, by = c("Participant_ID")) 

# only select relevant variables 
 vars <- c(
   "Participant_ID",
   "WordID",
    "Phone",
    "Word",
    "Phone_duration",
    "Word_duration",
    "word_type",
    "total_score",
   "Spectrum",
   "Normalized_Frames",
    "EVT_Standard",
    "PPVT_Standard",
    "PPVT_GSV",
    "EVT_GSV",
    "Female",
    "LateTalker",
   "RealWordRep_Age",
    "Maternal_Education",
    "Maternal_Education_Level",
    "LENA_Prop_Meaningful",
    "LENA_Prop_TV",
   "LENA_Hours",
   "LENA_Prop_Noise",
   "LENA_Prop_Silence",
   "LENA_Prop_Distant",
    "LENA_AWC_Hourly",
    "LENA_CTC_Hourly",
    "LENA_CVC_Hourly")
 newdata <- final[vars]


write.csv(final, file = "Google_Drive/non-word_rep/acoustic_analyses/data/coartic/final_mfccs_w_vocab.csv")

