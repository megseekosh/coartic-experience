# match nonwords and real words across participants
# Meg Cychosz, 2018


scores <- read.csv('Google_Drive/non-word_rep/acoustic_analyses/data/coartic/repetition_scores_mfcc.csv')


# create total score 
scores$total_score <- scores$target_score + scores$prosody_score

unique(scores$Word) # ensure there are 46 unique
scores$Word <- factor(scores$Word)
summary(scores$Word)

# assign ID to real words
# first check to make sure you only have real words
rwr <- subset(scores, scores$word_type=='realword')

rwr$WordID <- as.character(rwr$Word) # copy word list
rwr$WordID[rwr$WordID == 'raebIt']  <- "001"
rwr$WordID[rwr$WordID == 'kaend6l']  <- "002"
rwr$WordID[rwr$WordID == 'tSIkIn']  <- "003"
rwr$WordID[rwr$WordID == 'rezInz']  <- "004"
rwr$WordID[rwr$WordID == 'ridIN']  <- "005"
rwr$WordID[rwr$WordID == 'rvnIN']  <- "006"
rwr$WordID[rwr$WordID == 'saIdwak']  <- "007"
rwr$WordID[rwr$WordID == 'SErIN']  <- "008"
rwr$WordID[rwr$WordID == 'sutkes']  <- "009"
rwr$WordID[rwr$WordID == 'kVtIN']  <- "010"
rwr$WordID[rwr$WordID == 'kItSIn']  <- "011"
rwr$WordID[rwr$WordID == 'rakIN']  <- "012"
rwr$WordID[rwr$WordID == 'saendwItS']  <- "013"
rwr$WordID[rwr$WordID == 'sIst6r']  <- "014"
rwr$WordID[rwr$WordID == 'sVni']  <- "015"
rwr$WordID[rwr$WordID == 'taIg6r']  <- "016"
rwr$WordID[rwr$WordID == 'tost6r']  <- "017"
rwr$WordID[rwr$WordID == 'tuTbrVS']  <- "018"
rwr$WordID[rwr$WordID == 'wetIN']  <- "019"
rwr$WordID[rwr$WordID == 'waS6r']  <- "020"
rwr$WordID[rwr$WordID == 'wat6r']  <- "021"
rwr$WordID[rwr$WordID == 'wIndo']  <- "022"
rwr$WordID[rwr$WordID == 'kafi']  <- "023"



# assign ID to nonwords
nwr <- subset(scores, scores$word_type=='nonword')

nwr$WordID <- as.character(nwr$Word) # copy word list
nwr$WordID[nwr$WordID == 'raepoIn']  <- "001"
nwr$WordID[nwr$WordID == 'kaemIg']  <- "002"
nwr$WordID[nwr$WordID == 'tSImIg']  <- "003"
nwr$WordID[nwr$WordID == 'rebIT']  <- "004"
nwr$WordID[nwr$WordID == 'rifras']  <- "005"
nwr$WordID[nwr$WordID == 'rVglok']  <- "006"
nwr$WordID[nwr$WordID == 'saIprot']  <- "007"
nwr$WordID[nwr$WordID == 'Sevas']  <- "008"
nwr$WordID[nwr$WordID == 'sudras']  <- "009"
nwr$WordID[nwr$WordID == 'kVfim']  <- "010"
nwr$WordID[nwr$WordID == 'kIpon']  <- "011"
nwr$WordID[nwr$WordID == 'ralaId']  <- "012"
nwr$WordID[nwr$WordID == 'saemEl']  <- "013"
nwr$WordID[nwr$WordID == 'sIplok']  <- "014"
nwr$WordID[nwr$WordID == 'sVbIT']  <- "015"
nwr$WordID[nwr$WordID == 'taIblor']  <- "016"
nwr$WordID[nwr$WordID == 'tozEl']  <- "017"
nwr$WordID[nwr$WordID == 'tugraIf']  <- "018"
nwr$WordID[nwr$WordID == 'wemag']  <- "019"
nwr$WordID[nwr$WordID == 'wakraed']  <- "020"
nwr$WordID[nwr$WordID == 'waprot']  <- "021"
nwr$WordID[nwr$WordID == 'wImEl']  <- "022"
nwr$WordID[nwr$WordID == 'kasEp']  <- "023"


# now drop those NWs that don't have corresponding rw 
#NWR_pairs <-
##  NWR_scores[grep(
#    "raenol|rebIT|refim|regip|rifras|rVmaId|saIprot|Sevas|sudras|tSImIg",
#    NWR_scores$Word
#  ), ]

# create participant ID variable - probably not necessary
#install.packages('qdapRegex')
#library('qdapRegex')
#rwr$participant<- rm_between(rwr$Speaker, "_", "_", extract=TRUE)
#View(rwr)
#nwr$participant<- rm_between(nwr$Speaker, "_", "_", extract=TRUE)
#View(NWR_pairs)

# merge dfs
# note this will drop productions where the child only produced a nw or only a rw!
# (which is what we want for now but may not be in the future)

# concatenate horizontally to calculate difference between real and nonwords
#all_scores <-
#  merge(rwr, nwr, by = c("WordID", "Speaker"))

# get difference between rwr score and nwr score for corresponding words
#all_scores$real_non_difference <-
#  all_scores$total_score.x - all_scores$total_score.y
#summary(all_scores)

# concatenate vertically for acoustic analyses and to weed out only 100% correct productions
# still drops those productions where there aren't pairs
all_scores <-
  rbind(rwr, nwr)


# option to delimit only those words that were produced 100% accurately
# first create subsets for diphtongs and monophthongs
dip <- all_scores[ which(all_scores$Word=='taIg6r' | all_scores$Word=='taIblor' | 
                           all_scores$Word=='saIdwak' | all_scores$Word=='saIprot'), ]

mono <- all_scores[ which(all_scores$Word!='taIg6r'), ]
mono <- mono[ which(mono$Word!='taIblor'), ]
mono <- mono[ which(mono$Word!='saIdwak'), ]
mono2 <- mono[ which(mono$Word!='saIprot'), ]


# only those with score of 9 for mono2
mono_final <- mono2[ which(mono2$total_score==9), ]

# only those with score of 10 for dip
dip_final <- dip[ which(dip$total_score==10), ]

correct_scores <- rbind(mono_final, dip_final)

# results in massive dataset, remove unnecessary variables
vars <- c(
  "X.2",
  "X.1",
  "X",
  "Target1Seg",
  "Target2Seg",
  "Note",
  "Trial",
  "phone_t1",
  "phone_t2",
  "seg1_prosody",
  "seg2_prosody",
  "syllable",
  "ProsodyScore",
  "prosody_score",
  'target_score',
  'Frame'
)
final <- correct_scores[ , !(names(correct_scores) %in% vars)]


write.csv(final, file = "Google_Drive/non-word_rep/acoustic_analyses/data/coartic/final_mfcc_scores.csv")




