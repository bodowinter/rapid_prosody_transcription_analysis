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



########################################################################
######################## Mixed model analysis:
########################################################################

## First, let's test the gender effects:

xmdl.Gender <- glmer(Prominence ~ ListenerGender_c*SpeakerGender_c +
	(1|Listener) + 
	(1|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")

## MaxPitch model:

xmdl.MaxPitch.bobyqa <- glmer(Prominence ~ MaxPitch_z + SpeakerGender_c + 
	(1|Listener) + (0+MaxPitch_z|Listener) + 
	(1|Speaker) + (0+MaxPitch_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=glmerControl(optimizer="bobyqa"))
xmdl.MaxPitch.Null.bobyqa <- glmer(Prominence ~ 1 + SpeakerGender_c + 
	(1|Listener) + (0+MaxPitch_z|Listener) + 
	(1|Speaker) + (0+MaxPitch_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=glmerControl(optimizer="bobyqa"))

## MeanPitch model:

xmdl.MeanPitch.bobyqa = glmer(Prominence ~ MeanPitch_z + SpeakerGender_c + 
	(1|Listener) + (0+MeanPitch_z|Listener) + 
	(1|Speaker) + (0+MeanPitch_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=glmerControl(optimizer="bobyqa"))
xmdl.MeanPitch.Null.bobyqa = glmer(Prominence ~ 1 + SpeakerGender_c + 
	(1|Listener) + (0+MeanPitch_z|Listener) + 
	(1|Speaker) + (0+MeanPitch_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=glmerControl(optimizer="bobyqa"))

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

## VowelDur:

xmdl.VowelDur.bobyqa  = glmer(Prominence ~ VowelDur_z +
	(1|Listener) + (0+VowelDur_z|Listener) + 
	(1|Speaker) + (0+VowelDur_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=glmerControl(optimizer="bobyqa"))
xmdl.VowelDur.Null.bobyqa  = glmer(Prominence ~ 1 +
	(1|Listener) + (0+VowelDur_z|Listener) + 
	(1|Speaker) + (0+VowelDur_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=glmerControl(optimizer="bobyqa"))

## SyllableDur:

mdl.SyllableDur.bobyqa = glmer(Prominence ~ SyllableDur_z +
	(1|Listener) + (0+SyllableDur_z|Listener) + 
	(1|Speaker) + (0+SyllableDur_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=glmerControl(optimizer="bobyqa"))
xmdl.SyllableDur.Null.bobyqa = glmer(Prominence ~ 1 +
	(1|Listener) + (0+SyllableDur_z|Listener) + 
	(1|Speaker) + (0+SyllableDur_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=glmerControl(optimizer="bobyqa"))

## No. of Syllables:

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

## Phonological Vowel Duration:

xmdl.Vowel.bobyqa = glmer(Prominence ~ Vowel_c +
	(1|Listener) + (0+Vowel_c|Listener) + 
	(1|Speaker) + (0+Vowel_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=glmerControl(optimizer="bobyqa"))
xmdl.Vowel.Null.bobyqa = glmer(Prominence ~ 1 +
	(1|Listener) + (0+Vowel_c|Listener) + 
	(1|Speaker) + (0+Vowel_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=glmerControl(optimizer="bobyqa"))

## Part of Speech class:

xmdl.POS_class.bobyqa = glmer(Prominence ~ POS_class_c +
	(1|Listener) + (0+POS_class_c|Listener) + 
	(1|Speaker) + (0+POS_class_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=glmerControl(optimizer="bobyqa"))
xmdl.POS_class.Null.bobyqa = glmer(Prominence ~ 1 +
	(1|Listener) + (0+POS_class_c|Listener) + 
	(1|Speaker) + (0+POS_class_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=glmerControl(optimizer="bobyqa"))

## Focus particle:

xmdl.Focused.bobyqa = glmer(Prominence ~ Focused +
	(1|Listener) + (0+Focused|Listener) + 
	(1|Speaker) + (0+Focused|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=glmerControl(optimizer="bobyqa"))
xmdl.Focused.Null.bobyqa = glmer(Prominence ~ 1 +
	(1|Listener) + (0+Focused|Listener) + 
	(1|Speaker) + (0+Focused|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=glmerControl(optimizer="bobyqa"))

## Last argument:

xmdl.argument.bobyqa = glmer(Prominence ~ LastArgument +
	(1|Listener) + (0+LastArgument|Listener) + 
	(1|Speaker) + (0+LastArgument|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=glmerControl(optimizer="bobyqa"))
xmdl.argument.Null.bobyqa = glmer(Prominence ~ 1 +
	(1|Listener) + (0+LastArgument|Listener) + 
	(1|Speaker) + (0+LastArgument|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=glmerControl(optimizer="bobyqa"))

## To make AccentType and AccentPosition converge, find offending conditions:

RPT_red <- filter(RPT,AccentPosition!="postnuclear")
RPT_red <- filter(RPT_red,AccentPosition!="no_accent")
RPT_red$AccentPosition <- factor(RPT_red$AccentPosition)

RPT_red$AccentPosition_c <- RPT_red$AccentPosition
contrasts(RPT_red$AccentPosition_c) <- contr.sum(3)

xmdl.AccentPosition.bobyqa = glmer(Prominence ~ AccentPosition_c +
	(1|Listener) + (0+AccentPosition_c|Listener) + 
	(1|Speaker) + (0+AccentPosition_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT_red,family="binomial",control=glmerControl(optimizer="bobyqa"))
xmdl.AccentPosition.Null.bobyqa = glmer(Prominence ~ 1 +
	(1|Listener) + (0+AccentPosition_c|Listener) + 
	(1|Speaker) + (0+AccentPosition_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT_red,family="binomial",control=glmerControl(optimizer="bobyqa"))


## To make AccentType and AccentPosition converge, find offending conditions:

RPT_red <- filter(RPT,AccentType!="no_accent")
RPT_red$AccentType <- factor(RPT_red$AccentType)

RPT_red$AccentType_c <- RPT_red$AccentType
contrasts(RPT_red$AccentType_c) <- contr.sum(4)

xmdl.AccentType.bobyqa = glmer(Prominence ~ AccentType_c +
	(1|Listener) + (0+AccentType_c|Listener) + 
	(1|Speaker) + (0+AccentType_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=glmerControl(optimizer="bobyqa"))
xmdl.AccentType.Null.bobyqa = glmer(Prominence ~ 1 +
	(1|Listener) + (0+AccentType_c|Listener) + 
	(1|Speaker) + (0+AccentType_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT_red,family="binomial",control=glmerControl(optimizer="bobyqa"))

## Test accented versus non-accented:

RPT$Accented <- factor(ifelse(RPT$AccentType=="no_accent","no","yes"))
RPT$Accented_c <- RPT$Accented
contrasts(RPT$Accented_c) = contr.sum(2)

xmdl.Accented.bobyqa = glmer(Prominence ~ Accented_c +
	(1|Listener) + (0+Accented_c|Listener) + 
	(1|Speaker) + (0+Accented_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=glmerControl(optimizer="bobyqa"))
xmdl.Accented.Null.bobyqa = glmer(Prominence ~ 1 +
	(1|Listener) + (0+Accented_c|Listener) + 
	(1|Speaker) + (0+Accented_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=glmerControl(optimizer="bobyqa"))

save.image("mixed_models.RData")


