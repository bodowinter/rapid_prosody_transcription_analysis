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
save.image("mixed_models.RData")

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
save.image("mixed_models.RData")

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
save.image("mixed_models.RData")

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
save.image("mixed_models.RData")

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
save.image("mixed_models.RData")

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
save.image("mixed_models.RData")

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
save.image("mixed_models.RData")

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
save.image("mixed_models.RData")

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
save.image("mixed_models.RData")

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
save.image("mixed_models.RData")

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
save.image("mixed_models.RData")

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
# save.image("mixed_models.RData")

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
save.image("mixed_models.RData")

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
save.image("mixed_models.RData")



