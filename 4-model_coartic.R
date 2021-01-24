library('dplyr')
library('tidyverse')
library('ggplot2')
library('stargazer')
library('lme4')
library('lmerTest')

# load data here
coartic_df1 <- read.csv('/Users/megcychosz/Google Drive/non-word_rep/acoustic_analyses/data/coartic/final_mfccs_w_vocab.csv')


# clean up a mess
coartic_df1$Word <- plyr::mapvalues(coartic_df1$Word, from = c("rVnIn"), to = c("rVnIN"))
coartic_df1$Word <- plyr::mapvalues(coartic_df1$Word, from = c("rvnIN"), to = c("rVnIN"))


# make sure that participant_ID is a factor!
coartic_df1$Participant_ID <- as.factor(coartic_df1$Participant_ID)

# ------- calculate range of target consonant and vowel durations ------
  
con_dur <- coartic_df1 %>%
  filter(Phone=='k' | Phone=='t' | Phone=='s' | Phone=='w' | Phone=='s' | Phone=='S' | Phone=='tS' | Phone=='r') %>%
  summarize(min=min(Phone_duration*1000),
            max=max(Phone_duration*1000))

vow_dur <- coartic_df1 %>%
  filter(Phone=='a' | Phone=='ae' | Phone=='aI' | Phone=='e' | Phone=='E' | Phone=='i' | Phone=='I' | Phone=='o' | Phone=='u' | Phone=='V') %>%
  summarize(min=min(Phone_duration*1000),
            max=max(Phone_duration*1000))
  
# ----- load information on CV-C phonotactic transition probability -----
probs <- read.csv('/Users/megcychosz/Google Drive/non-word_rep/acoustic_analyses/scripts/coartic/phono_prbability.csv')

probs$word <- plyr::mapvalues(probs$word, from = c("rvnIN"), to = c("rVnIN"))
probs$word <- plyr::mapvalues(probs$word, from = c("rebiT"), to = c("rebIT"))

probs$Word <- probs$word
probs$CV_C_probability <- probs$probability # make this more intuitive 
coartic_df <- probs %>%
  merge(., coartic_df1, by = ('Word')) %>%
  select(-type, -word, -probability)

# ------ create info on second syllable frequency ------
coartic_df$sec_frequency <- coartic_df$Word
coartic_df$sec_frequency <- plyr::mapvalues(coartic_df$sec_frequency, 
                                       from = c("kaemIg", "kaend6l", "kafi", "kasEp",
                                                "kIpon", "kItSIn", "kVfim", "kVtIN",
                                                "raebIt", "raepoIn", "rakIN", "ralaId",
                                                "rebIT", "rezInz", "ridIN", "rifras",
                                                "rVglok", "rVnIN", "saemEl", "saendwItS",
                                                "saIdwak", "saIprot", "SErIN", "Sevas",
                                                "sIplok", "sIst6r", "sudras", "sutkes",
                                                "sVbIT", "sVni", "taIblor", "taIg6r",
                                                "tost6r", "tozEl", "tSIkIn", "tSImIg",
                                                "tugraIf", "tuTbrVS", "wakraed", "waprot",
                                                "waS6r", "wat6r", "wemag", "wetIN", 
                                                "wImEl", "wIndo"), 
                                       to = c("2", "19", "94", "46", 
                                              "7", "3", "5", "48",
                                              "12", "0", "13", "9", 
                                              "0", "1", "35", "0",
                                              "0", "19", "17", "1", 
                                              "1", "14", "43", "0",
                                              "0", "108", "1", "8", 
                                              "0", "295", "0", "65",
                                              "108", "6", "23", "2", 
                                              "0", "2", "0", "14", 
                                              "12", "615", 
                                              "2", "48", "17", "13"))
coartic_df$sec_frequency <- as.numeric(coartic_df$sec_frequency)
#coartic_df$sec_frequency <- log(coartic_df$sec_frequency)


# -------- create CV sequence duration variable --------
coartic_df <- coartic_df %>%
  group_by(Participant_ID, Word) %>%
  mutate(CV_dur = sum(Phone_duration)) 


# ----- calculate spectral differences ------

# convert structure of spectral measurements at edges to something computable
# remove brackets
coartic_df$Spectrum <- gsub( ']', '', coartic_df$Spectrum)
coartic_df$Spectrum <- gsub( '[ ', '', coartic_df$Spectrum, fixed = TRUE) # open bracket denotes regex so fix it

# convert measurements to string
coartic_df$variable_sep <- str_extract_all(coartic_df$Spectrum, "[-0-9\\.]+")

# for euclidean distance and raw distance, convert to numeric: 
coartic_df$spec_vector <- lapply(coartic_df$variable_sep , FUN = as.numeric)
coartic_df <- as.data.frame(coartic_df)


# --------- option to find raw difference/euclidean between vectors --------
if(any(grepl("package:plyr", search()))) detach("package:plyr") else message("plyr not loaded")
library('dplyr')


# calculate raw difference and euc distance between vectors
diff_df <- coartic_df %>% 
  group_by(Word, Participant_ID) %>% 
  #mutate(raw_diff = map2(spec_vector, lead(spec_vector), `-`)) %>% # sanity check (note to take absolute value because the direction of the calculation will differ e.g. aI - K versus t - u)
  mutate(euc_dist = map2(spec_vector, lead(spec_vector), function(x, y) 
  sqrt(sum((x-y) ^ 2)))) %>% 
  as.data.frame() 

# remove NA rows where measurement was made upon but not stored
df.final <- subset(diff_df, euc_dist != '0')
df.final$euc_dist <- as.numeric(df.final$euc_dist)

# stats
non <- subset(df.final, word_type=='nonword')
real <- subset(df.final, word_type=='realword')

# check these values to ensure that dplyr was loaded in the right order above
mean(non$euc_dist) #8.645882
mean(real$euc_dist) #8.919246

mean(non$Word_duration) #1.101104
mean(real$Word_duration) #0.8746114
sd(non$Word_duration) #0.2697686
sd(real$Word_duration) #0.2794957

# create manner variable
df.final$manner <- plyr::mapvalues(df.final$Word, from = c("kaemIg", "kaend6l", "kafi",     "kasEp",     "kIpon",    "kItSIn",     
                                                           "kVfim",     "kVtIN",   
                                                           "raebIt",   "raepoIn",     "rakIN", 
                                                           "ralaId",     "rebIT",    "rezInz",     "ridIN",    "rifras",    
                                                           "rVglok",     "rVnIN",   
                                                           "saemEl", "saendwItS",   "saIdwak", 
                                                           "saIprot",     "SErIN",     "Sevas",    "sIplok",    "sIst6r",    
                                                           "sudras",    "sutkes",     "sVbIT",      "sVni",   "taIblor",    "taIg6r", 
                                                           "tost6r",     "tozEl",    "tSIkIn",    "tSImIg",   "tugraIf",   
                                                           "tuTbrVS",   "wakraed",    "waprot",     "waS6r",     "wat6r",     "wemag", 
                                                           "wetIN",     "wImEl",     "wIndo" ), 
                                   to = c("stop", "stop", "stop", "stop", "stop", "stop", 
                                          "stop", "stop", 
                                          "approximant", "approximant", "approximant",
                                    "approximant", "approximant", "approximant", "approximant", "approximant", 
                                    "approximant", "approximant", 
                                    "fricative", "fricative", "fricative",
                                    "fricative", "fricative", "fricative", "fricative", "fricative", 
                                    "fricative", "fricative", "fricative", "fricative", 
                                   "stop", "stop",  "stop",  "stop",  "affricate",  "affricate", "stop", "stop", "approximant", "approximant", "approximant", "approximant",
                                    "approximant", "approximant", "approximant", "approximant"))

# write out the data here to use in phonotactic frequency
# in nonword analysis
df.final %>%
  select(euc_dist, LENA_AWC_Hourly, LENA_CVC_Hourly, PPVT_GSV, word_type, Word, 
         Participant_ID, sec_frequency, manner) %>%
  write.csv(., '/Users/megcychosz/Google Drive/non-word_rep/acoustic_analyses/data/coartic/for_phon_prob.csv')







# ------ get ready to fit some models -------
center_scale <- function(x) {
  scale(x, scale = FALSE)
}
df.final$EVT_GSV_centered <- center_scale(df.final$EVT_GSV)
df.final$PPVT_GSV_centered <- center_scale(df.final$PPVT_GSV)
df.final$LENA_Prop_TV <- center_scale(df.final$LENA_Prop_TV)
df.final$LENA_Prop_Meaningful <- center_scale(df.final$LENA_Prop_Meaningful)
df.final$LENA_Prop_Distant <- center_scale(df.final$LENA_Prop_Distant)
df.final$LENA_Prop_Silence <- center_scale(df.final$LENA_Prop_Silence)
df.final$sec_frequency <- center_scale(df.final$sec_frequency)
df.final$LENA_AWC_Hourly_scaled <- center_scale(df.final$LENA_AWC_Hourly)
df.final$LENA_CVC_Hourly_scaled <- center_scale(df.final$LENA_CVC_Hourly)
df.final$Word <- as.factor(df.final$Word)
df.final$Word_duration <- center_scale(df.final$Word_duration)*1000






# fit baseline model
summary(baseline <- lmer(euc_dist ~ (1|Participant_ID) + (1|Word), data=df.final)) 

# control for speaking rate
summary(m1 <- lmer(euc_dist ~ Word_duration + (1|Participant_ID) + (1|Word), data=df.final))
anova(baseline, m1) # improves

# word type
summary(m2 <- lmer(euc_dist ~ word_type + Word_duration + (1|Participant_ID) + (1|Word), data=df.final)) 
anova(m1, m2) # doesn't improve

# interaction?
summary(m2a <- lmer(euc_dist ~ word_type*Word_duration + (1|Participant_ID) + (1|Word), data=df.final)) 
anova(m1, m2a) # doesn't improve
# no independent effect of word type on coarticulation



# make sure word type isn't interacting with transitional probability
summary(m2a <- lmer(euc_dist ~ CV_C_probability + Word_duration + (1|Participant_ID) + (1|Word), data=df.final)) 
anova(m1, m2a) # doesn't improve
summary(m2b <- lmer(euc_dist ~ CV_C_probability + word_type + Word_duration + (1|Participant_ID) + (1|Word), data=df.final)) 
summary(m2b) # word type not sig, even when controlling for CV_C_probability

# make sure role of word type is not due to second syllable probability
summary(m2c <- lmer(euc_dist ~ sec_frequency + Word_duration + (1|Participant_ID) + (1|Word), data=df.final)) 
anova(m1, m2c) # doesn't improve
summary(m2d <- lmer(euc_dist ~ sec_frequency + word_type + Word_duration + (1|Participant_ID) + (1|Word), data=df.final)) 
summary(m2d) # word type not sig, even when controlling for second syllable probability




# vocabulary
summary(m3 <- lmer(euc_dist ~ PPVT_GSV_centered + Word_duration + (1|Participant_ID) + (1|Word), data=df.final, control=lmerControl(optimizer="bobyqa",
                    optCtrl=list(maxfun=2e5))))
anova(m1,m3) # improves

# interacting effect of word type?
summary(m4 <- lmer(euc_dist ~ word_type*PPVT_GSV_centered + Word_duration + (1|Participant_ID) + (1|Word), data=df.final)) 
anova(m3,m4) # doesn't improve 

summary(m5 <- lmer(euc_dist ~ EVT_GSV_centered + Word_duration + (1|Participant_ID) + (1|Word), data=df.final)) 
anova(m1, m5) # improves

# does interaction result in better fit?
summary(m6 <- lmer(euc_dist ~ word_type*EVT_GSV_centered + Word_duration + (1|Participant_ID) + (1|Word), data=df.final)) 
anova(m5, m6) # doesn't improve


# does ppvt or evt result in better fit?
# word_type + PPVT AIC = 18316, BIC = 18352
# word_type + EVT AIC = 18316, BIC = 18353
# ppvt *marginally* stronger fit than EVT

# check maternal education 
df.final$Maternal_Education_Level <- as.factor(df.final$Maternal_Education_Level)
summary(mat <- lmer(euc_dist ~ Maternal_Education_Level + PPVT_GSV_centered + Word_duration + (1|Participant_ID) + (1|Word), data=df.final))
anova(m3, mat) # doesn't improve

# check gender
df.final$Female <- as.factor(df.final$Female)
summary(fem <- lmer(euc_dist ~ Female + PPVT_GSV_centered + Word_duration + (1|Participant_ID) + (1|Word), data=df.final))
anova(m3, fem) # doesn't improves


# ------ FINAL WORD TYPE*VOCAB MODEL -----
df.final$word_type <- relevel(df.final$word_type, ref = "nonword")

summary(mword <- lme4::lmer(euc_dist ~ PPVT_GSV_centered + # call lme4 so that stargazer works
                              Word_duration +
                              (1|Participant_ID) + 
                              (1|Word), 
                              data=df.final, 
                              control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e5)))) 



# print out the model summary
stargazer(mword, 
          header=FALSE, 
          dep.var.caption = "", 
          dep.var.labels.include = FALSE,  
          type = "latex", 
          star.cutoffs=c(0.05,0.01,0.001), 
          star.char = c("*", "**", "***"),
          title="The effects of vocabulary and word duration upon coarticulation",
          digits = 2, 
          ci = TRUE, 
          style = "all",
          order=c(3,1,2), 
          covariate.labels = c("Intercept", 
                               "Receptive vocabulary score (PPVT-4)", 
                               "Word duration (ms)"),
          out = "/Users/megcychosz/Google Drive/non-word_rep/acoustic_analyses/visuals/word*vocab_model_summary.tex")



# ----------- environmental factors --------
# how does the SES distribution differ between the kids who 
# did and did not complete daylong recordings?
LENA_SES <- df.final %>%
  distinct(Participant_ID, .keep_all = T) %>%
  mutate(LENA_complete = ifelse(LENA_Hours == 'NA', 'NO', 'YES')) %>%
  mutate(LENA_complete = replace_na(LENA_complete, 'NO')) 
yes <- LENA_SES %>% filter(LENA_complete=='YES') # n=6 (7.41%) have HS degree or less or 92.59% have more than hs degree
no <- LENA_SES %>% filter(LENA_complete =='NO') # n=3 (13.64%) have HS degree or less or 86.36% have more than hs degree



# 22 kids don't have LENA data so drop those kids 
LENA_df <- na.omit(df.final, cols='LENA_AWC_Hourly')
LENA_df <- subset(df.final, LENA_Hours>3) # drop the one kid with the 2 hour recording (this also drops kids w/o a recording)

# ----- first test word type -----
summary(mbase <- lmer(euc_dist ~ (1|Participant_ID) + (1|Word), data=LENA_df)) 

# speaking rate
summary(mrate <- lmer(euc_dist ~ Word_duration + (1|Participant_ID) + (1|Word), data=LENA_df))
anova(mbase, mrate) # improves

# role of word type
summary(mword <- lmer(euc_dist ~  word_type + Word_duration + (1|Participant_ID) + (1|Word), data=LENA_df)) 
anova(mrate, mword) # doesn't improve, just like the full dataset

# check interaction of word type with word duration
summary(mint <- lmer(euc_dist ~  word_type*Word_duration + (1|Participant_ID) + (1|Word), data=LENA_df)) 
anova(mrate, mint) # no effect of word type on coartic

#summary(mprobs <- lmer(euc_dist ~  CV_C_probability + Word_duration + (1|Participant_ID) + (1|Word), data=LENA_df)) 
#anova(mrate, mprobs) # doesn't improve
# word type is still significant, even after controlling for transitional probability
#summary(mprobs2 <- lmer(euc_dist ~ word_type + CV_C_probability + (1|Participant_ID) + (1|Word), data=LENA_df)) 

# ------ now make sure word type is independent of second syllable frequency
#summary(msyll <- lmer(euc_dist ~  sec_frequency + (1|Participant_ID) + (1|Word), data=LENA_df)) 
#anova(mbase, msyll) # doesn't improve
# word type is still significant, even after controlling for second syllable frequency
#summary(msyll2 <- lmer(euc_dist ~ word_type + sec_frequency + (1|Participant_ID) + (1|Word), data=LENA_df)) 





# ----- now replicate the vocabulary findings from above -----
summary(m6a <- lmer(euc_dist ~ EVT_GSV_centered + Word_duration +(1|Participant_ID) + (1|Word), data=LENA_df)) 
anova(mrate, m6a) # improves
summary(m7a <- lmer(euc_dist ~ PPVT_GSV_centered + Word_duration + (1|Participant_ID) + (1|Word), data=LENA_df)) 
anova(mrate, m7a) # almost improves (alpha < .1)
# the vocab results replicate in this subset of data

summary(m6 <- lmer(euc_dist ~ word_type*EVT_GSV_centered + Word_duration + (1|Participant_ID) + (1|Word), data=LENA_df)) 
anova(m6a, m6) # doesn't improve
summary(m7 <- lmer(euc_dist ~ word_type*PPVT_GSV_centered + Word_duration + (1|Participant_ID) + (1|Word), data=LENA_df)) 
anova(m7a, m7) # doesn't improve

# is PPVT or EVT a better fit for this subset of data?
# PPVT AIC = 14894, BIC = 14930
# EVT AIC = 14896, BIC = 14931
# EVT marginally better predictor
# overall, no real differences between EVT and PPVT 

# check maternal education for LENA_df
summary(mat <- lmer(euc_dist ~ Maternal_Education_Level + Word_duration + PPVT_GSV_centered + (1|Participant_ID) + (1|Word), data=LENA_df))
anova(m7a, mat) # doesn't improve

# check gender for LENA_df
summary(fem <- lmer(euc_dist ~ Female + Word_duration +PPVT_GSV_centered + (1|Participant_ID) + (1|Word), data=LENA_df))
anova(m7a, fem) # doesn't improve



# start checking LENA measures
summary(mCVC <- lmer(euc_dist ~  LENA_CVC_Hourly_scaled + Word_duration + EVT_GSV_centered+(1|Participant_ID) + (1|Word), data=LENA_df)) 
summary(mAWC <- lmer(euc_dist ~  LENA_AWC_Hourly_scaled + Word_duration + EVT_GSV_centered+(1|Participant_ID) + (1|Word), data=LENA_df)) 

anova(m7a, mCVC) #  improves (AIC = 14892)
anova(m7a, mAWC) # doesn't improve

# what about a model that includes both LENA measures?
summary(mAWCCVC <- lmer(euc_dist ~ LENA_CVC_Hourly_scaled + LENA_AWC_Hourly_scaled + Word_duration + PPVT_GSV_centered+(1|Participant_ID) + (1|Word), data=LENA_df)) 
anova(mCVC, mAWCCVC) # doesn't improve

# interaction not relevant
summary(m12 <- lmer(euc_dist ~ PPVT_GSV_centered*LENA_CVC_Hourly_scaled + Word_duration + (1|Participant_ID) + (1|Word), data=LENA_df)) 
anova(mCVC, m12) # doesn't improve 

summary(m12b <- lmer(euc_dist ~ EVT_GSV_centered*LENA_CVC_Hourly_scaled + Word_duration + (1|Participant_ID) + (1|Word), data=LENA_df)) 
anova(mCVC, m12b) # doesn't improve 

# are child vocs and vocabulary size correlated?
LENA_df_unq <- LENA_df %>% distinct(Participant_ID, .keep_all = T)
cor.test(LENA_df_unq$LENA_CVC_Hourly_scaled, LENA_df_unq$PPVT_GSV_centered, method=c("pearson"))
cor.test(LENA_df_unq$LENA_CVC_Hourly_scaled, LENA_df_unq$EVT_GSV_centered, method=c("pearson"))
ggplot(LENA_df_unq, aes(y = LENA_CVC_Hourly, x = EVT_GSV)) +
  geom_jitter() + 
  geom_smooth(method = "lm")
# not related

# fit mediation model
# effect of AWC on CVC
awc.cvc <- LENA_df %>%
  distinct(Speaker, .keep_all = T) %>% 
  lm(LENA_CVC_Hourly ~ LENA_AWC_Hourly_scaled + EVT_GSV_centered, data=.) 
summary(awc.cvc)

cvc.coartic <- lmer(euc_dist ~ LENA_AWC_Hourly_scaled + LENA_CVC_Hourly_scaled + EVT_GSV_centered + Word_duration + (1|Participant_ID) + (1|Word), data=LENA_df)
summary(cvc.coartic)

# ----------- FINAL CVC model RESULTS ---------
# word_type + CVC
# relevel
LENA_df$word_type <- relevel(LENA_df$word_type, ref = "nonword")

summary(mCVCfinal <- lme4::lmer(euc_dist ~ # call lme4 so that stargazer works
                                  LENA_CVC_Hourly_scaled + 
                                   Word_duration +
                                   EVT_GSV_centered + 
                                   (1|Participant_ID) + 
                                   (1|Word), 
                                   data=LENA_df)) 



# print out the model summary
stargazer(mCVCfinal, 
          header=FALSE, 
          dep.var.caption = "", 
          dep.var.labels.include = FALSE,  
          type = "latex", 
          star.cutoffs=c(0.05,0.01,0.001), 
          star.char = c("*", "**", "***"),
          title="The effects of speech practice and vocabulary upon coarticulation",
          digits = 2, 
          ci = TRUE, 
          style = "all",
          order=c(4,1,2,3), 
          covariate.labels = c("Intercept", 
                               "Hourly child vocalization count", 
                               "Word duration (ms)",
                               "Expressive vocabulary score (EVT-2)"),
          out = "/Users/megcychosz/Google Drive/non-word_rep/acoustic_analyses/visuals/CVC_model_summary.tex")






# LATE TALKER ANALYSIS
# for all kids
summary(latea <- lmer(euc_dist ~ Word_duration + PPVT_GSV_centered + (1|Participant_ID) + (1|Word), data=df.final)) 
summary(lateb <- lmer(euc_dist ~ Word_duration + PPVT_GSV_centered + LateTalker + (1|Participant_ID) + (1|Word), data=df.final)) 
anova(latea, lateb) # doesn't improve 
summary(latec <- lmer(euc_dist ~ Word_duration + PPVT_GSV_centered*LateTalker + (1|Participant_ID) + (1|Word), data=df.final)) 
anova(latea, latec) # doesn't improve 

# for LENA analyses
summary(latea <- lmer(euc_dist ~ Word_duration + LENA_CVC_Hourly_scaled + PPVT_GSV_centered + (1|Participant_ID) + (1|Word), data=LENA_df)) 
summary(lateb <- lmer(euc_dist ~ Word_duration + LENA_CVC_Hourly_scaled + PPVT_GSV_centered + LateTalker + (1|Participant_ID) + (1|Word), data=LENA_df)) 
anova(latea, lateb) # doesn't improve 
summary(latec <- lmer(euc_dist ~ Word_duration + LENA_CVC_Hourly_scaled*LateTalker + PPVT_GSV_centered + (1|Participant_ID) + (1|Word), data=LENA_df)) 
anova(latea, latec) # doesn't improve 


# let's make sure our results aren't contingent upon 
# late talkers
# models w/o late talkers 
nolate <- LENA_df[which(LENA_df$LateTalker=='0'), ]
summary(late2 <- lmer(euc_dist ~ Word_duration + LENA_CVC_Hourly_scaled + PPVT_GSV_centered +(1|Participant_ID) + (1|Word), data=nolate)) 
# effects are present


# visualize -------------
# relevel levels of word_type variable so that they align w/ plots
LENA_df$word_type <- factor(LENA_df$word_type, levels = c("realword", "nonword"))
df.final$word_type <- factor(df.final$word_type, levels = c("realword", "nonword"))

# ----- median split ------
# make some subgroups
# by vocab
# median evt GSV score for LENA dataset = 134
LENA_df$vocab_group <- ifelse(LENA_df$EVT_GSV < 134, "Smaller vocabulary group", 
                                                       "Larger vocabulary group")
LENA_df$vocab_group <- as.factor(LENA_df$vocab_group)

# order correctly for plotting
df.final$word_type <- factor(df.final$word_type, levels = c("realword", "nonword"))
LENA_df$word_type <- factor(LENA_df$word_type, levels = c("realword", "nonword"))
LENA_df$vocab_group <- factor(LENA_df$vocab_group, levels = c("Smaller vocabulary group", "Larger vocabulary group"))


# for when we want to plot relationships between single LENA measurements (like AWC & CVC) - not for Euc_dist
LENA_df_unique <- subset(LENA_df, !duplicated(Participant_ID))





# by manner
ggplot(df.final, aes(y = euc_dist, x = PPVT_GSV, color=word_type, shape=word_type)) + 
  xlab("PPVT") + ylab("Mel spectral distance between phones") +
  geom_smooth(method = "lm", se=TRUE) + 
  geom_point(size=2, alpha=.5) +
  coord_cartesian(ylim=c(0,40)) + # to zoom in, not remove pts
  scale_color_manual(values=c("darkorchid3", "darkorange2")) +
  facet_wrap(~manner) +
  scale_x_continuous(breaks=seq(100,500,100)) +
#stat_cor(method = "pearson", label.x = 450, label.y = 110)
  labs(title= "C-V coarticulation by number of child \n vocalizations and phoneme manner") + 
  theme(axis.title=element_text(size=16)) + 
  theme(plot.title = element_text(size = 25, face = "bold")) +
  theme(axis.text.x = element_text(face="bold", size=12),
        axis.text.y = element_text(face ='bold', size=15)) +
  theme(strip.text.x = element_text(size = 12))





jpeg("/Users/megcychosz/Google Drive/non-word_rep/acoustic_analyses/visuals/PPVT*word_type.jpeg", height = 500, width = 500)
df.final %>%
ggplot(., aes(y = euc_dist, x = PPVT_GSV, color = word_type, shape=word_type)) + 
  xlab("Receptive vocabulary score (PPVT-4)") + ylab("Mel spectral distance between phones") +
  geom_smooth(method = "lm", se=TRUE) + 
  geom_jitter(size=1, alpha=.5) + 
  coord_cartesian(ylim=c(3,20)) + # to zoom in, not remove pts
  guides(fill=guide_legend(title="Word Type")) +
  #scale_color_manual(values=c("forestgreen", "dodgerblue3"))
  scale_color_manual(values=c("darkorchid3", "darkorange2")) +
  #labs(title= "C-V coarticulation by \n receptive vocabulary size") + 
  theme(axis.title=element_text(size=16)) + 
  theme(plot.title = element_text(size = 25, face = "bold")) +
  theme(axis.text.x = element_text(face="bold", size=15),
        axis.text.y = element_text(face ='bold', size=15))
dev.off()

# try to plot each child's mean, instead of individual trials
jpeg("/Users/megcychosz/Google Drive/non-word_rep/acoustic_analyses/visuals/PPVT_word_type_means.jpeg", height = 500, width = 450)
tocheck <- df.final %>%
  group_by(word_type, Participant_ID) %>%
  mutate(coartic_med = median(euc_dist)) %>%
  distinct_at(., vars(word_type,Participant_ID), .keep_all = T) #%>%
ggplot(., aes(y = coartic_med, x = PPVT_GSV, color = word_type, shape=word_type)) + 
  xlab("Receptive vocabulary score (PPVT-4)") + ylab("Median Mel spectral \n distance between phones") +
  geom_smooth(method = "lm", se=TRUE) + 
  geom_jitter(size=4, alpha=.75) + 
  #coord_cartesian(ylim=c(4,13)) + # to zoom in, not remove pts
  guides(fill=guide_legend(title="Word Type")) +
  #scale_color_manual(values=c("forestgreen", "dodgerblue3"))
  scale_color_manual(values=c("darkorchid3", "darkorange2")) +
  #labs(title= "C-V coarticulation by \n receptive vocabulary size") + 
  theme(axis.title=element_text(size=16)) + 
  theme(plot.title = element_text(size = 25, face = "bold")) +
  theme(axis.text.x = element_text(face="bold", size=15),
        axis.text.y = element_text(face ='bold', size=15),
        legend.text=element_text(size=15),
        legend.title = element_text(size=15))
dev.off()


jpeg("/Users/megcychosz/Google Drive/non-word_rep/acoustic_analyses/visuals/word_type*CVC", height = 500, width = 500)
ggplot(LENA_df, aes(y = euc_dist, x = LENA_CVC_Hourly, color=word_type, shape=word_type)) + 
  xlab("Number of child vocalizations \n per hour (LENA)") + ylab("Mel spectral distance between phones") +
  geom_smooth(method = "lm", se=TRUE) + 
  geom_jitter(size=1, alpha=.5) + 
  coord_cartesian(ylim=c(3,20)) + # to zoom in, not remove pts
  guides(fill=guide_legend(title="Word Type")) +
  scale_color_manual(values=c("darkorange2", "darkorchid3")) +
  labs(title= "C-V coarticulation by \n child vocalization count") + 
  theme(axis.title=element_text(size=16)) + 
  theme(plot.title = element_text(size = 25, face = "bold")) +
  theme(axis.text.x = element_text(face="bold", size=15),
        axis.text.y = element_text(face ='bold', size=15))
dev.off()

# three way interaction
jpeg("/Users/megcychosz/Google Drive/non-word_rep/acoustic_analyses/visuals/word_type-CVC-EVT.jpeg", height = 500, width = 600)
ggplot(LENA_df, aes(y = euc_dist, x = LENA_CVC_Hourly, color=word_type, shape=word_type)) + 
  xlab("Number of child vocalizations \n per hour (LENA)") + ylab("Mel spectral distance between phones") +
  geom_smooth(method = "lm", se=TRUE)+ 
  geom_jitter(size=1, alpha=.5)+ 
  coord_cartesian(ylim=c(3,20)) + # to zoom in, not remove pts
  guides(fill=guide_legend(title="Word Type")) +
  facet_wrap(~vocab_group) +
  scale_color_manual(values=c("darkorchid3", "darkorange2")) + 
 # labs(title= "C-V coarticulation by child vocalizations \n and expressive vocabulary") + 
  theme(axis.title=element_text(size=16)) + 
  theme(plot.title = element_text(size = 22, face = "bold")) +
  theme(axis.text.x = element_text(face="bold", size=15),
        axis.text.y = element_text(face ='bold', size=15)) +
  theme(strip.text.x = element_text(size = 15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))
dev.off()

jpeg("/Users/megcychosz/Google Drive/non-word_rep/acoustic_analyses/visuals/coartic/AWC*coartic*wordtype", height = 500, width = 500)
ggplot(LENA_df, aes(y = euc_dist, x = LENA_AWC_Hourly, color=word_type, shape=word_type)) + 
  xlab("Number of adult words heard \n per hour (LENA)") + ylab("Mel spectral distance between phones") +
  geom_smooth(method = "lm", se=TRUE) + 
 geom_jitter(size=1, alpha=.5) + 
  coord_cartesian(ylim=c(3,20)) + # to zoom in, not remove pts
  guides(fill=guide_legend(title="Word Type")) +
  scale_x_continuous(breaks=seq(0,3000,500)) + 
  scale_color_manual(values=c("darkorange2", "darkorchid3")) +
  #scale_color_manual(values=c("dodgerblue", "tomato2"))
  labs(title= "C-V coarticulation by \n adult word count") + 
  theme(axis.title=element_text(size=16)) + 
  theme(plot.title = element_text(size = 25, face = "bold")) +
  theme(axis.text.x = element_text(face="bold", size=15),
        axis.text.y = element_text(face ='bold', size=15))
dev.off()



cor.test(LENA_df_unique$LENA_AWC_Hourly, LENA_df_unique$LENA_CVC_Hourly, method="pearson") 
jpeg("/Users/megcychosz/Google Drive/non-word_rep/acoustic_analyses/visuals/CVC-AWC_plain.jpeg", height = 500, width = 350)
ggplot(LENA_df_unique, aes(y = LENA_CVC_Hourly, x = LENA_AWC_Hourly)) + 
  xlab("Number of adult words \n per hour (LENA)") + ylab("Number of child vocalizations \n per hour (LENA)") +
  geom_smooth(method = "lm", se=TRUE) + 
  geom_jitter(size=2, alpha=.5) +
  scale_x_continuous(breaks=seq(0,3000,500)) + 
  scale_y_continuous(breaks=seq(0,500,100)) +
 # labs(title= "Child vocalization count by \n adult word count") + 
  theme(axis.title=element_text(size=16)) + 
  theme(plot.title = element_text(size = 25, face = "bold")) +
  theme(axis.text.x = element_text(face="bold", size=15),
        axis.text.y = element_text(face ='bold', size=15))
dev.off()





# split by phoneme identity
# split by manner : fricatives, stops, glides

# first group by manner
df.final$Manner <- plyr::mapvalues(df.final$Word, from = c("kaemIg", "kaend6l", "kafi", "kasEp", "kIpon", "kItSIn",
                                                         "kVfim", "kVtIN", "raebIt", "raepoIn", "rakIN", "ralaId",
                                                         "rebIT", "rezInz", "ridIN", "rifras", "rVglok", "rVnIN",
                                                         "saemEl", "saendwItS", "saIdwak", "saIprot", "SErIN", 
                                                         "Sevas", "sIplok", "sIst6r", "sudras", "sutkes", "sVbIT", 
                                                         "sVni", "taIblor", "taIg6r", "tost6r", "tozEl", "tSIkIn",
                                                         "tSImIg", "tugraIf", "tuTbrVS", "wakraed", "waprot", "waS6r",
                                                         "wat6r", "wemag", "wetIN", "wImEl", "wIndo"), 
                                  to = c("Stop", "Stop", "Stop","Stop","Stop","Stop","Stop","Stop","Approximant",
                                         "Approximant","Approximant","Approximant","Approximant","Approximant","Approximant","Approximant","Approximant","Approximant",
                                         "Fricative","Fricative","Fricative","Fricative","Fricative","Fricative",
                                         "Fricative","Fricative","Fricative","Fricative","Fricative","Fricative",
                                         "Stop","Stop","Stop","Stop","Affricate","Affricate","Stop","Stop","Approximant","Approximant",
                                         "Approximant","Approximant","Approximant","Approximant","Approximant","Approximant"))
# put in the right order for plotting
df.final$Manner <- factor(df.final$Manner, levels = c("Approximant", "Fricative", "Affricate", "Stop"))

# make LENA df again
LENA_df <- na.omit(df.final)
LENA_df <- subset(df.final, LENA_Hours>3) # drop the one kid with the 2 hour recording (this also drops kids w/o a recording)

# remove affricates
LENA_df_noaff <- LENA_df %>% filter(Manner!='Affricate')

jpeg("/Users/megcychosz/Google Drive/non-word_rep/acoustic_analyses/visuals/CVC_manner_noaff.jpeg", height = 500, width = 750)
ggplot(LENA_df_noaff, aes(y = euc_dist, x = LENA_CVC_Hourly, color=word_type, shape=word_type)) + 
  xlab("Number of child vocalizations \n per hour (LENA)") + ylab("Mel spectral distance between phones") +
  geom_smooth(method = "lm", se=TRUE) + 
  geom_point(size=2, alpha=.5) +
  coord_cartesian(ylim=c(3,20)) + # to zoom in, not remove pts
  scale_color_manual(values=c("darkorchid3", "darkorange2")) +
  facet_wrap(~Manner) +
#stat_cor(method = "pearson", label.x = 450, label.y = 110)
 # labs(title= "C-V coarticulation by number of child \n vocalizations and phoneme manner") + 
  theme(axis.title=element_text(size=16)) + 
  theme(plot.title = element_text(size = 25, face = "bold")) +
  theme(axis.text.x = element_text(face="bold", size=15),
        axis.text.y = element_text(face ='bold', size=15)) +
  theme(strip.text.x = element_text(size = 15),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15))
dev.off()

# remove affricates
df.final_noaff <- df.final %>% filter(manner!='affricate')

# use whole dataset for this one, not just LENA kids
jpeg("/Users/megcychosz/Google Drive/non-word_rep/acoustic_analyses/visuals/PPVT_manner_noaff.jpeg", height = 500, width = 750)
ggplot(df.final_noaff, aes(y = euc_dist, x = PPVT_GSV, color=word_type, shape=word_type)) + 
  xlab("Receptive vocabulary score (PPVT-4)") + ylab("Mel spectral distance between phones") +
  geom_smooth(method = "lm", se=TRUE) + 
  geom_point(size=2, alpha=.5) +
  coord_cartesian(ylim=c(0,20)) + # to zoom in, not remove pts
  scale_color_manual(values=c("darkorchid3", "darkorange2")) +
  facet_wrap(~Manner) +
 # scale_x_continuous(breaks=seq(100,500,100)) 
#stat_cor(method = "pearson", label.x = 450, label.y = 110)
  #labs(title= "C-V coarticulation by receptive \n vocabulary size and phoneme manner") + 
  theme(axis.title=element_text(size=16)) + 
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.text.x = element_text(face="bold", size=15),
        axis.text.y = element_text(face ='bold', size=15)) +
  theme(strip.text.x = element_text(size = 15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))
dev.off()






