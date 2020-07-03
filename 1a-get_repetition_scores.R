# get participants' repetition scores for duration dataset

rm(list=ls())

rwr <- read.csv ('Google_Drive/non-word_rep/acoustic_analyses/data/coartic/rwr_mfcc_complete.csv') # rwr
nwr <- read.csv ('Google_Drive/non-word_rep/acoustic_analyses/data/coartic/nwr_mfcc_complete.csv') # nwr

# clean up some messes
nwr$Phone <- plyr::mapvalues(nwr$Phone, from = c("kVfim", "al"), to = c("k", "aI"))
rwr$Word <- plyr::mapvalues(rwr$Word, from = c("rVnIn"), to = c("ruvIN"))
rwr$Phone <- plyr::mapvalues(rwr$Phone, from = c("v"), to = c("V"))
nwr$Speaker <- plyr::mapvalues(nwr$Speaker, from = c("026L39MS3.wav", "620L41FS3.wav", "625L43FS3.wav", "628L49FS4.wav"), to = c("026L39MS3.WAV", "620L41FS3.WAV", "625L43FS3.WAV", "628L49FS4.WAV"))


# ---- combine nwr and rwr dataframes -----
# first create variable to delimit 
rwr$word_type <- NA
rwr$word_type[is.na(rwr$word_type)] <- "realword"
nwr$word_type <- NA
nwr$word_type[is.na(nwr$word_type)] <- "nonword"
# remove extra column in rwr
remove <- "Note"
rwr <- rwr[ , !(names(rwr) %in% remove)]
nwr <- nwr[ , !(names(nwr) %in% remove)]

# merge together
combined <- rbind(rwr, nwr)



# ---- subset data out if commented not to use -----
#sub.data <- subset(combined, Note !='DONT USE') 
sub.data <- subset(combined, Trial != 'DONT USE')
#sub.data <- subset(combined, Note !='DURATION ONLY') 
sub.data2 <- subset(sub.data, Trial != 'DURATION ONLY')
#sub.data2$Note <- droplevels(sub.data2$Note)
sub.data2$Trial <- droplevels(sub.data2$Trial)



# ---- subset productions that are 100% correct -----
#  clean up transcriptions (the yellow parts will change depending on what the labels are)
subtrans  <- subset(sub.data2, ProsodyScore != "NonResponse; ;0")
subtrans <- subset(subtrans, ProsodyScore != 'No Response;0')
subtrans <- subset(subtrans, ProsodyScore != '')

subtrans2 <- subset(subtrans, Target1Seg!="") 
subtrans2 <- subset(subtrans2, Target1Seg!="No Response;0") 
subtrans2 <- subset(subtrans2, Target1Seg!="Noise;NA,NA,NA;NA") 
subtrans2 <- subset(subtrans2, Target1Seg!="No Response;0") 
#subtrans2 <- subset(subtrans2, Target1Seg!="Omitted; Omitted, Omitted, Omitted;Omitted") 
#subtrans2 <- subset(subtrans2, Target1Seg!='Omitted;Omitted,Omitted,Omitted;0')
#subtrans2 <- subset(subtrans2, Target1Seg!='Omitted;Omitted,Omitted,Omitted;Omitted')
subtrans2 <- subset(subtrans2, Target1Seg!='Unclassifiable;Unclassifiable,Unclassifiable,Unclassifiable;0')
subtrans2 <- subset(subtrans2, Target1Seg!='NonResponse; ;0')
#subtrans2 <- subset(subtrans2, Target1Seg!='Omitted; Omitted, Omitted, Omitted;0')


#subtrans2 <- subset(subtrans2, Target2Seg!='Omitted;Omitted,Omitted,Omitted;0')
subtrans3 <- subset(subtrans2, Target2Seg!='Noise;NA,NA,NA;NA')
subtrans4 <- subset(subtrans3, Target2Seg!=' ')

subtrans5 <- subtrans4[!grepl("Unclassifiable,", subtrans4$Target2Seg),]

subtrans5$ProsodyScore <- droplevels(subtrans5$ProsodyScore)
subtrans5$Target1Seg <- droplevels(subtrans5$Target1Seg)
subtrans5$Target2Seg <- droplevels(subtrans5$Target2Seg)


# cut score off from transcription
subtrans2 <- subtrans5
subtrans2$Target1Seg <- sub(".*;", "", subtrans2$Target1Seg)
subtrans2$Target2Seg <- sub(".*;", "", subtrans2$Target2Seg)


# remove transcription from prosody score (first part of trans)
subtrans2$ProsodyScore <- gsub("[^0-9\\;]", "", subtrans2$ProsodyScore) 
# 6 and 3 are remaining from transcription so remove them
subtrans2$ProsodyScore <- gsub('6|3', "", subtrans2$ProsodyScore) 
# remove hanging semicolons
subtrans2$ProsodyScore <- gsub("^;+|;+$", "", subtrans2$ProsodyScore)

# there are weirdly some words without scores so remove those
newdf <- subtrans2[!grepl("0,", subtrans2$ProsodyScore),]



nw <- subset(newdf, word_type=='nonword')
rw <- subset(newdf, word_type=='realword')

# separate prosody scores out
# this might throw you an error about not changing
# working directory, if so close script and rerun
get_prosody <- function(data_column) {
  out <- data.frame()
  setwd(
    'Google_Drive/non-word_rep/acoustic_analyses/data/coartic/'
  )
  for (score in data_column) {
    print(score)
    mini_df <- unlist(strsplit(score, ";"))
    out <- rbind(out, mini_df)
  }
  write.csv(out, file = "sep_prosody_scores_mfcc.csv") 
}
get_prosody(newdf$ProsodyScore)



