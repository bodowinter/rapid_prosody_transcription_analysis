## Bodo Winter
## May 29, 2014; June 19, 2015
## First analysis of rapid prosody transcription (RPT) experiment

########################################################################
######################## Preliminaries:
########################################################################

## Load in data:

mainDir <- "/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/data"
setwd(mainDir)
RPT <- read.csv("RPT_summary_processed.csv")

## Load in packages:

library(DMwR)
library(party)



########################################################################
######################## Imputation:
########################################################################

## The random forests analysis with "conditional = TRUE" ...
## ... (permutation test variable importance) require no missing values.
## We will create an additional dataset with imputed Mean and Max Pitch
## It's pointless to impute "RangeST" & "SlopeST" since there is not enough data.
## For this, we will do a subset analysis on only the accented words.

## We will create five data frames:
## (1) - central imputed
## (2) - knn imputed
## (3) - reduced data set (missing values excluded)
## (4) - subset of only those for which SlopeST and RangeST is not NA

## Only (4) can be used to test the relative ranking of SlopeST, RangeST

## How many missing values do we have anyway?

sum(is.na(RPT$MeanPitch))				# only 5
sum(is.na(RPT$MeanPitch))/nrow(RPT)		# less than 1% of the data
RPT[is.na(RPT$MeanPitch),]				# the data for which MeanPitch = NA is also MaxPitch = NA

## Impute the missing pitch values:

RPT_central <- RPT
RPT_central[,c("MeanPitch","MaxPitch")] <- centralImputation(RPT_central[,c("MeanPitch","MaxPitch")])

## K-nearest neighbor imputation on all the continuous variables:

continuous_variables <- c("NSyll","LogFreq","MeanPitch","MaxPitch",
	"RMS_amplitude","SyllableDur","VowelDur")
RPT_KNN <- RPT
RPT_KNN[,continuous_variables] <- knnImputation(RPT_KNN[,continuous_variables])

## Reduced dataset (without the 5 missing data points):

RPT_red <- RPT[complete.cases(RPT$MeanPitch),]

## Make a subset with those for which "RangeST" and "SlopeST" are not NA:

RPT_accented <- RPT[complete.cases(RPT$PitchRangeST),]

## What counts and percentage are those for which we have "RangeST" and "SlopeST"?

nrow(RPT_accented)					# 206 rows
nrow(RPT_accented)/nrow(RPT)			# 37% of the entire data set



########################################################################
######################## Random forests without "RangeST" and "SlopeST"
########################################################################

## Define "uber formula" that I'll use for those models, containing all variables:

the_uber_formula <- as.formula("p_score ~ VowelDur + SyllableDur + RMS_amplitude +
	LogFreq + MaxPitch + MeanPitch + POS + NSyll + 
	AccentPosition + AccentType + POS_class + Vowel + LastArgument + Focused")
accented_formula <- as.formula("p_score ~ VowelDur + SyllableDur + RMS_amplitude +
	LogFreq + MaxPitch + MeanPitch + POS + NSyll + PitchRangeST + PitchSlopeST + 
	AccentPosition + AccentType + POS_class + Vowel + LastArgument + Focused")

## Define controls:

data.controls <- cforest_unbiased(ntree=2000,
	mtry=round(sqrt(14)))	## 2,000 trees with 4 variables each (k = 14 parameters)
	## this is the same for the accented analysis with k = 16 parameters

## Run the forests, first, the raw one:

set.seed(42)
forest_raw <- cforest(the_uber_formula,RPT,controls=data.controls)
raw_predictions <- predict(forest_raw)
raw_varimp <- varimp(forest_raw,conditional=FALSE)		# conditional = F coz of missing values
save(forest_raw,raw_predictions,raw_varimp,file="raw_forests.RData")

## Now remove the old baggage for memory space:

rm(forest_raw,raw_predictions,raw_varimp)

## Run the forest on central imputed data:

set.seed(42)
forest_central <- cforest(the_uber_formula,RPT_central,controls=data.controls)
central_predictions <- predict(forest_central)
central_varimp_conditional <- varimp(forest_central,conditional=FALSE)
save(forest_central,central_predictions,central_varimp_conditional,file="central_forests.RData")

## Remove old baggage to gain memory:

rm(forest_central,central_predictions,central_varimp_conditional)

## Run the forest on KNN imputed data:

set.seed(42)
forest_KNN <- cforest(the_uber_formula,RPT_KNN,controls=data.controls)
KNN_predictions <- predict(forest_KNN)
KNN_varimp_conditional <- varimp(forest_KNN,conditional=FALSE)
save(forest_KNN,KNN_predictions,KNN_varimp_conditional,file="KNN_forests.RData")

## Remove old baggage to gain memory:

rm(forest_KNN,KNN_predictions,KNN_varimp_conditional)

## The forest on the reduced dataset (missing values excluded):

set.seed(42)
forest_red <- cforest(the_uber_formula,RPT_red,controls=data.controls)
red_predictions <- predict(forest_red)
red_varimp_conditional <- varimp(forest_red,conditional=FALSE)
save(forest_red,red_predictions,red_varimp_conditional,file="reduced_forests.RData")

## Remove old baggage to gain memory:

rm(forest_red,red_predictions,red_varimp_conditional)

## Finally, the random forests on the subset for which we have slope and range values:

set.seed(42)
forest_accented <- cforest(accented_formula,RPT_accented,controls=data.controls)
accented_predictions <- predict(forest_accented)
accented_varimp_conditional <- varimp(forest_accented,conditional=FALSE)
save(forest_accented,accented_predictions,accented_varimp_conditional,file="accented_forests.RData")










## Bodo Winter
## April 25, 2015; Added new analyses: June 19, 2015
## Visualizations of Rapid Prosody Transcription Data


########################################################################
######################## Load in data:
########################################################################

## Path for main analysis:

mainDir <- "/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/data"
setwd(mainDir)

## Load:

RPT <- read.csv("RPT_individual_processed.csv")

## Load in libraries needed:

library(lme4)



########################################################################
######################## Coding predictor variables:
########################################################################

## Take absolute variables for slope, treating strongly falling...
## ... and strongly rising the same:

RPT$PitchRangeST_abs <- abs(RPT$PitchRangeST)
RPT$PitchSlopeST_abs <- abs(RPT$PitchSlopeST)

## Z-transform variables:

RPT$MeanPitch_z <- scale(RPT$MeanPitch)
RPT$MaxPitch_z <- scale(RPT$MaxPitch)
RPT$RMS_amplitude_z <- scale(RPT$RMS_amplitude)
RPT$NSyll_z <- scale(RPT$NSyll)
RPT$SyllableDur_z <- scale(RPT$SyllableDur)
RPT$VowelDur_z <- scale(RPT$VowelDur)
RPT$LogFreq_z <- scale(RPT$LogFreq)
RPT$PitchRangeST_z <- scale(RPT$PitchRangeST)
RPT$PitchSlopeST_z <- scale(RPT$PitchSlopeST)
RPT$PitchRangeST_abs_z <- scale(RPT$PitchRangeST_abs)
RPT$PitchSlopeST_abs_z <- scale(RPT$PitchSlopeST_abs)

## Create contrast coded predictors:

RPT$Vowel_c <- RPT$Vowel
contrasts(RPT$Vowel_c) <- contr.sum(2)

RPT$POS_c <- RPT$POS
contrasts(RPT$POS_c) <- contr.sum(length(levels(RPT$POS)))

RPT$POS_class_c <- RPT$POS_class
contrasts(RPT$POS_class_c) <- contr.sum(2)

RPT$AccentPosition_c <- RPT$AccentPosition
contrasts(RPT$AccentPosition_c) <- contr.sum(length(levels(RPT$AccentPosition)))

RPT$AccentType_c <- RPT$AccentType
contrasts(RPT$AccentType_c) <- contr.sum(length(levels(RPT$AccentType)))

RPT$ListenerGender_c <- RPT$ListenerGender
contrasts(RPT$ListenerGender_c) <- contr.sum(2)

RPT$SpeakerGender_c <- RPT$SpeakerGender
contrasts(RPT$SpeakerGender_c) <- contr.sum(2)

## For Accent Type recode so that -1 = the middle prominence ("falling"):

contrasts(RPT$AccentType_c)[1,] <- c(-1,-1,-1,-1)
contrasts(RPT$AccentType_c)[5,] <- c(1,0,0,0)
# (this is not necessary for AccentPosition because -1 happened to be the most middle catgory anyway)

## Create a "word nested within sentence" variable:

RPT$WordWithinSentence <- 1:562
# RPT$WordWithinSentence <- paste(RPT$Word,RPT$Sentence,sep=":")



########################################################################
######################## Mixed model analysis:
########################################################################

## First, let's test the gender effects:

xmdl.Gender <- glmer(Prominence ~ ListenerGender*SpeakerGender +
                          (1|Listener) + (0+SpeakerGender|Listener) +
                          (1|Speaker) + (0+ListenerGender|Speaker) + 
                          (1|WordWithinSentence) + (1|Sentence),
                     RPT,family="binomial")

## MaxPitch model:

xmdl.MaxPitch <- glmer(Prominence ~ MaxPitch_z + 
                            (1|Listener) + (0+MaxPitch_z|Listener) + 
                            (1|Speaker) + (0+MaxPitch_z|Speaker) + 
                            (1|WordWithinSentence) + (1|Sentence),
                       RPT,family="binomial")
xmdl.MaxPitch.Null <- glmer(Prominence ~ 1 + 
                                 (1|Listener) + (0+MaxPitch_z|Listener) + 
                                 (1|Speaker) + (0+MaxPitch_z|Speaker) + 
                                 (1|WordWithinSentence) + (1|Sentence),
                            RPT,family="binomial")
save.image("mixed_models_word_integer_sequence_not_nested.RData")

## MeanPitch model:

xmdl.MeanPitch = glmer(Prominence ~ MeanPitch_z +
                            (1|Listener) + (0+MeanPitch_z|Listener) + 
                            (1|Speaker) + (0+MeanPitch_z|Speaker) + 
                            (1|WordWithinSentence) + (1|Sentence),
                       RPT,family="binomial")
xmdl.MeanPitch.Null = glmer(Prominence ~ 1 +
                                 (1|Listener) + (0+MeanPitch_z|Listener) + 
                                 (1|Speaker) + (0+MeanPitch_z|Speaker) + 
                                 (1|WordWithinSentence) + (1|Sentence),
                            RPT,family="binomial")
save.image("mixed_models_word_integer_sequence_not_nested.RData")

## RMS_amplitude:

xmdl.RMS_amplitude = glmer(Prominence ~ RMS_amplitude_z +
                                (1|Listener) + (0+RMS_amplitude_z|Listener) + 
                                (1|Speaker) + (0+RMS_amplitude_z|Speaker) + 
                                (1|WordWithinSentence) + (1|Sentence),
                           RPT,family="binomial")
xmdl.RMS_amplitude.Null = glmer(Prominence ~ 1 +
                                     (1|Listener) + (0+RMS_amplitude_z|Listener) + 
                                     (1|Speaker) + (0+RMS_amplitude_z|Speaker) + 
                                     (1|WordWithinSentence) + (1|Sentence),
                                RPT,family="binomial")
save.image("mixed_models_word_integer_sequence_not_nested.RData")

## VowelDur:

xmdl.VowelDur = glmer(Prominence ~ VowelDur_z +
                           (1|Listener) + (0+VowelDur_z|Listener) + 
                           (1|Speaker) + (0+VowelDur_z|Speaker) + 
                           (1|WordWithinSentence) + (1|Sentence),
                      RPT,family="binomial")
xmdl.VowelDur.Null = glmer(Prominence ~ VowelDur_z +
                                (1|Listener) + (0+VowelDur_z|Listener) + 
                                (1|Speaker) + (0+VowelDur_z|Speaker) + 
                                (1|WordWithinSentence) + (1|Sentence),
                           RPT,family="binomial")
save.image("mixed_models_word_integer_sequence_not_nested.RData")

## SyllableDur:

xmdl.SyllableDur = glmer(Prominence ~ SyllableDur_z +
                              (1|Listener) + (0+SyllableDur_z|Listener) + 
                              (1|Speaker) + (0+SyllableDur_z|Speaker) + 
                              (1|WordWithinSentence) + (1|Sentence),
                         RPT,family="binomial")
xmdl.SyllableDur.Null = glmer(Prominence ~ 1 +
                                   (1|Listener) + (0+SyllableDur_z|Listener) + 
                                   (1|Speaker) + (0+SyllableDur_z|Speaker) + 
                                   (1|WordWithinSentence) + (1|Sentence),
                              RPT,family="binomial")
save.image("mixed_models_word_integer_sequence_not_nested.RData")

## SyllableDur:

xmdl.NSyll = glmer(Prominence ~ NSyll_z +
                        (1|Listener) + (0+NSyll_z|Listener) + 
                        (1|Speaker) + (0+NSyll_z|Speaker) + 
                        (1|WordWithinSentence) + (1|Sentence),
                   RPT,family="binomial")
xmdl.NSyll.Null = glmer(Prominence ~ 1 +
                             (1|Listener) + (0+NSyll_z|Listener) + 
                             (1|Speaker) + (0+NSyll_z|Speaker) + 
                             (1|WordWithinSentence) + (1|Sentence),
                        RPT,family="binomial")
save.image("mixed_models_word_integer_sequence_not_nested.RData")

## RangeST:

xmdl.RangeST_abs = RPT[complete.cases(RPT$PitchRangeST_abs),]
xmdl.RangeST = glmer(Prominence ~ PitchRangeST_abs_z +
                          (1|Listener) + (0+PitchRangeST_abs_z|Listener) + 
                          (1|Speaker) + (0+PitchRangeST_abs_z|Speaker) + 
                          (1|WordWithinSentence) + (1|Sentence),
                     RPT,family="binomial")
xmdl.RangeST.Null = glmer(Prominence ~ 1 +
                               (1|Listener) + (0+PitchRangeST_abs_z|Listener) + 
                               (1|Speaker) + (0+PitchRangeST_abs_z|Speaker) + 
                               (1|WordWithinSentence) + (1|Sentence),
                          RPT,family="binomial")
save.image("mixed_models_word_integer_sequence_not_nested.RData")

## SlopeST:

RPT.SlopeST = RPT[complete.cases(RPT$PitchSlopeST_abs),]
xmdl.SlopeST = glmer(Prominence ~ PitchSlopeST_abs_z +
                          (1|Listener) + (0+PitchSlopeST_abs_z|Listener) + 
                          (1|Speaker) + (0+PitchSlopeST_abs_z|Speaker) + 
                          (1|WordWithinSentence) + (1|Sentence),
                     RPT,family="binomial")
xmdl.SlopeST.Null = glmer(Prominence ~ 1 +
                               (1|Listener) + (0+PitchSlopeST_abs_z|Listener) + 
                               (1|Speaker) + (0+PitchSlopeST_abs_z|Speaker) + 
                               (1|WordWithinSentence) + (1|Sentence),
                          RPT,family="binomial")
save.image("mixed_models_word_integer_sequence_not_nested.RData")

## Log frequency:

xmdl.Freq = glmer(Prominence ~ LogFreq_z +
                       (1|Listener) + (0+LogFreq_z|Listener) + 
                       (1|Speaker) + (0+LogFreq_z|Speaker) + 
                       (1|WordWithinSentence) + (1|Sentence),
                  RPT,family="binomial")
xmdl.Freq.Null = glmer(Prominence ~ 1 +
                            (1|Listener) + (0+LogFreq_z|Listener) + 
                            (1|Speaker) + (0+LogFreq_z|Speaker) + 
                            (1|WordWithinSentence) + (1|Sentence),
                       RPT,family="binomial")
save.image("mixed_models_word_integer_sequence_not_nested.RData")

## Phonological Vowel Duration:

xmdl.Vowel = glmer(Prominence ~ Vowel_c +
                        (1|Listener) + (0+Vowel_c|Listener) + 
                        (1|Speaker) + (0+Vowel_c|Speaker) + 
                        (1|WordWithinSentence) + (1|Sentence),
                   RPT,family="binomial")
xmdl.Vowel.Null = glmer(Prominence ~ 1 +
                             (1|Listener) + (0+Vowel_c|Listener) + 
                             (1|Speaker) + (0+Vowel_c|Speaker) + 
                             (1|WordWithinSentence) + (1|Sentence),
                        RPT,family="binomial")
save.image("mixed_models_word_integer_sequence_not_nested.RData")

## Part of Speech class:

xmdl.POS_class = glmer(Prominence ~ POS_class_c +
                            (1|Listener) + (0+POS_class_c|Listener) + 
                            (1|Speaker) + (0+POS_class_c|Speaker) + 
                            (1|WordWithinSentence) + (1|Sentence),
                       RPT,family="binomial")
xmdl.POS_class.Null = glmer(Prominence ~ 1 +
                                 (1|Listener) + (0+POS_class_c|Listener) + 
                                 (1|Speaker) + (0+POS_class_c|Speaker) + 
                                 (1|WordWithinSentence) + (1|Sentence),
                            RPT,family="binomial")
save.image("mixed_models_word_integer_sequence_not_nested.RData")

# ## Part of Speech (this will never converge):

# xmdl.POS = glmer(Prominence ~ POS_c +
# (1|Listener) + (0+POS_c|Listener) + 
# (1|Speaker) + (0+POS_c|Speaker) + 
# (1|WordWithinSentence) + (1|Sentence),
# RPT,family="binomial")
# xmdl.POS.Null = glmer(Prominence ~ 1 +
# (1|Listener) + (0+POS_c|Listener) + 
# (1|Speaker) + (0+POS_c|Speaker) + 
# (1|WordWithinSentence) + (1|Sentence),
# RPT,family="binomial")
# save.image("mixed_models_word_integer_sequence_not_nested.RData")

## AccentType:

xmdl.AccentType = glmer(Prominence ~ AccentType_c +
                             (1|Listener) + (0+AccentType_c|Listener) + 
                             (1|Speaker) + (0+AccentType_c|Speaker) + 
                             (1|WordWithinSentence) + (1|Sentence),
                        RPT,family="binomial")
xmdl.AccentType.Null = glmer(Prominence ~ 1 +
                                  (1|Listener) + (0+AccentType_c|Listener) + 
                                  (1|Speaker) + (0+AccentType_c|Speaker) + 
                                  (1|WordWithinSentence) + (1|Sentence),
                             RPT,family="binomial")
save.image("mixed_models_word_integer_sequence_not_nested.RData")

## AccentPosition:

xmdl.AccentPosition = glmer(Prominence ~ AccentPosition_c +
                                 (1|Listener) + (0+AccentPosition_c|Listener) + 
                                 (1|Speaker) + (0+AccentPosition_c|Speaker) + 
                                 (1|WordWithinSentence) + (1|Sentence),
                            RPT,family="binomial")
xmdl.AccentPosition.Null = glmer(Prominence ~ 1 +
                                      (1|Listener) + (0+AccentPosition_c|Listener) + 
                                      (1|Speaker) + (0+AccentPosition_c|Speaker) + 
                                      (1|WordWithinSentence) + (1|Sentence),
                                 RPT,family="binomial")
save.image("mixed_models_word_integer_sequence_not_nested.RData")


