# get repetition scores for dur part II
# have to use second script as weird thing happening with directory in part I

# combine separated prosody dfs (run separately bc the script was taking forever)
sep_scores <- read.csv('Google_Drive/non-word_rep/acoustic_analyses/data/coartic/sep_prosody_scores_mfcc.csv')
#rw_sep_scores <- read.csv('Google_Drive/non-word_rep/acoustic_analyses/data/sep_prosody_scores_mfcc_rw.csv')

#sep_scores <- rbind(nw_sep_scores, rw_sep_scores)

# separated elements of prosody scores
colnames(sep_scores) <- c("number", "seg1_prosody", "seg2_prosody", "syllable?")

# convert remaining NAs ("omitted") to 0
sep_scores[is.na(sep_scores)] <- 0

# create new variables w scores
# newdf dataset should be saved as it was run in pt 1, otherwise rerun part 1
newdf$Target1Seg <- as.numeric(newdf$Target1Seg)
newdf$Target1Seg[is.na(newdf$Target1Seg)] <- 0  # couple 0s that are registering as NAs
newdf$Target2Seg <- as.numeric(newdf$Target2Seg)
newdf$target_score <-
  newdf$Target1Seg + newdf$Target2Seg

final_data <- cbind(newdf, sep_scores)
final_data$prosody_score <-
  final_data$seg1_prosody + final_data$seg2_prosody +
  final_data$'syllable?'

# remove the number variable bc it's meaningless
myvar <- names(final_data) %in% c("number")
output <- final_data[!myvar]

write.csv(output, file = 'Google_Drive/non-word_rep/acoustic_analyses/data/coartic/repetition_scores_mfcc.csv')

# check the new file over
rep_scores <- read.csv('Google_Drive/non-word_rep/acoustic_analyses/data/coartic/repetition_scores_mfcc.csv')



