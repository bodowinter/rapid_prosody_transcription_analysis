## Bodo Winter
## June 21, 2015
## Mixed model analyses convergence fixes


mainDir <- "/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/data"
setwd(mainDir)

## Load:

RPT <- read.csv("RPT_individual_processed.csv")

## Load in libraries needed:

library(lme4)
library(optimx)


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



########################################################################
######################## Run those models that did not work
########################################################################

## Define optimizers:

## First, let's test the gender effects:

xlmer.bobyqa = lmer(Lgrt ~ number*hand + (1|subject) + (0+number*hand|subject), xdata, )
xlmer.Nelder = lmer(Lgrt ~ number*hand + (1|subject) + (0+number*hand|subject), xdata, )
xlmer.nlminb = lmer(Lgrt ~ number*hand + (1|subject) + (0+number*hand|subject), xdata, control=)
xlmer.LBFGSB = lmer(Lgrt ~ number*hand + (1|subject) + (0+number*hand|subject), xdata, )
 
## Gender model:

xmdl.Gender.bobyqa <- glmer(Prominence ~ ListenerGender*SpeakerGender +
	(1|Listener) + (0+SpeakerGender|Listener) +
	(1|Speaker) + (0+ListenerGender|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="bobyqa"))

xmdl.Gender.Nelder <- glmer(Prominence ~ ListenerGender*SpeakerGender +
	(1|Listener) + (0+SpeakerGender|Listener) +
	(1|Speaker) + (0+ListenerGender|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="Nelder_Mead"))

xmdl.Gender.nlminb <- glmer(Prominence ~ ListenerGender*SpeakerGender +
	(1|Listener) + (0+SpeakerGender|Listener) +
	(1|Speaker) + (0+ListenerGender|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx",optCtrl=list(method="nlminb")))

xmdl.Gender.LBFGSB <- glmer(Prominence ~ ListenerGender*SpeakerGender +
	(1|Listener) + (0+SpeakerGender|Listener) +
	(1|Speaker) + (0+ListenerGender|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))
save.image("mixed_models_convergence.RData")

## VowelDur:

xmdl.VowelDur.bobyqa = glmer(Prominence ~ VowelDur_z +
	(1|Listener) + (0+VowelDur_z|Listener) + 
	(1|Speaker) + (0+VowelDur_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="bobyqa"))
xmdl.VowelDur.Null.bobyqa = glmer(Prominence ~ VowelDur_z +
	(1|Listener) + (0+VowelDur_z|Listener) + 
	(1|Speaker) + (0+VowelDur_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="bobyqa"))

xmdl.VowelDur.nelder = glmer(Prominence ~ VowelDur_z +
	(1|Listener) + (0+VowelDur_z|Listener) + 
	(1|Speaker) + (0+VowelDur_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="Nelder_Mead"))
xmdl.VowelDur.Null.nelder = glmer(Prominence ~ VowelDur_z +
	(1|Listener) + (0+VowelDur_z|Listener) + 
	(1|Speaker) + (0+VowelDur_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="Nelder_Mead"))

xmdl.VowelDur.nlminb = glmer(Prominence ~ VowelDur_z +
	(1|Listener) + (0+VowelDur_z|Listener) + 
	(1|Speaker) + (0+VowelDur_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx",optCtrl=list(method="nlminb")))
xmdl.VowelDur.Null.nlminb = glmer(Prominence ~ VowelDur_z +
	(1|Listener) + (0+VowelDur_z|Listener) + 
	(1|Speaker) + (0+VowelDur_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx",optCtrl=list(method="nlminb")))

xmdl.VowelDur.LBFGSB = glmer(Prominence ~ VowelDur_z +
	(1|Listener) + (0+VowelDur_z|Listener) + 
	(1|Speaker) + (0+VowelDur_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))
xmdl.VowelDur.Null.LBFGSB = glmer(Prominence ~ VowelDur_z +
	(1|Listener) + (0+VowelDur_z|Listener) + 
	(1|Speaker) + (0+VowelDur_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))
save.image("mixed_models_convergence.RData")

## Phonological Vowel Duration:

xmdl.Vowel.bobyqa = glmer(Prominence ~ Vowel_c +
	(1|Listener) + (0+Vowel_c|Listener) + 
	(1|Speaker) + (0+Vowel_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="bobyqa"))
xmdl.Vowel.Null.bobyqa = glmer(Prominence ~ 1 +
	(1|Listener) + (0+Vowel_c|Listener) + 
	(1|Speaker) + (0+Vowel_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="bobyqa"))

xmdl.Vowel.nelder = glmer(Prominence ~ Vowel_c +
	(1|Listener) + (0+Vowel_c|Listener) + 
	(1|Speaker) + (0+Vowel_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="Nelder_Mead"))
xmdl.Vowel.Null.nelder = glmer(Prominence ~ 1 +
	(1|Listener) + (0+Vowel_c|Listener) + 
	(1|Speaker) + (0+Vowel_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="Nelder_Mead"))

xmdl.Vowel.nlminb = glmer(Prominence ~ Vowel_c +
	(1|Listener) + (0+Vowel_c|Listener) + 
	(1|Speaker) + (0+Vowel_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx",optCtrl=list(method="nlminb")))
xmdl.Vowel.Null.nlminb = glmer(Prominence ~ 1 +
	(1|Listener) + (0+Vowel_c|Listener) + 
	(1|Speaker) + (0+Vowel_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx",optCtrl=list(method="nlminb")))

xmdl.Vowel.LBFGSB = glmer(Prominence ~ Vowel_c +
	(1|Listener) + (0+Vowel_c|Listener) + 
	(1|Speaker) + (0+Vowel_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))
xmdl.Vowel.Null.LBFGSB = glmer(Prominence ~ 1 +
	(1|Listener) + (0+Vowel_c|Listener) + 
	(1|Speaker) + (0+Vowel_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))
save.image("mixed_models_convergence.RData")

## Part of Speech class:

xmdl.POS_class.bobyqa = glmer(Prominence ~ POS_class_c +
	(1|Listener) + (0+POS_class_c|Listener) + 
	(1|Speaker) + (0+POS_class_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="bobyqa"))
xmdl.POS_class.Null.bobyqa = glmer(Prominence ~ 1 +
	(1|Listener) + (0+POS_class_c|Listener) + 
	(1|Speaker) + (0+POS_class_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="bobyqa"))

xmdl.POS_class.nelder = glmer(Prominence ~ POS_class_c +
	(1|Listener) + (0+POS_class_c|Listener) + 
	(1|Speaker) + (0+POS_class_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="Nelder_Mead"))
xmdl.POS_class.Null.nelder = glmer(Prominence ~ 1 +
	(1|Listener) + (0+POS_class_c|Listener) + 
	(1|Speaker) + (0+POS_class_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="Nelder_Mead"))

xmdl.POS_class.nlminb = glmer(Prominence ~ POS_class_c +
	(1|Listener) + (0+POS_class_c|Listener) + 
	(1|Speaker) + (0+POS_class_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx",optCtrl=list(method="nlminb")))
xmdl.POS_class.Null.nlminb = glmer(Prominence ~ 1 +
	(1|Listener) + (0+POS_class_c|Listener) + 
	(1|Speaker) + (0+POS_class_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx",optCtrl=list(method="nlminb")))
	
xmdl.POS_class.LBFGSB = glmer(Prominence ~ POS_class_c +
	(1|Listener) + (0+POS_class_c|Listener) + 
	(1|Speaker) + (0+POS_class_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))
xmdl.POS_class.Null.LBFGSB = glmer(Prominence ~ 1 +
	(1|Listener) + (0+POS_class_c|Listener) + 
	(1|Speaker) + (0+POS_class_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))

save.image("mixed_models_convergence.RData")


## AccentType:

xmdl.AccentType.bobyqa = glmer(Prominence ~ AccentType_c +
	(1|Listener) + (0+AccentType_c|Listener) + 
	(1|Speaker) + (0+AccentType_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="bobyqa"))
xmdl.AccentType.Null.bobyqa = glmer(Prominence ~ 1 +
	(1|Listener) + (0+AccentType_c|Listener) + 
	(1|Speaker) + (0+AccentType_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="bobyqa"))

xmdl.AccentType.nelder = glmer(Prominence ~ AccentType_c +
	(1|Listener) + (0+AccentType_c|Listener) + 
	(1|Speaker) + (0+AccentType_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="Nelder_Mead"))
xmdl.AccentType.Null.nelder = glmer(Prominence ~ 1 +
	(1|Listener) + (0+AccentType_c|Listener) + 
	(1|Speaker) + (0+AccentType_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="Nelder_Mead"))

xmdl.AccentType.nlminb = glmer(Prominence ~ AccentType_c +
	(1|Listener) + (0+AccentType_c|Listener) + 
	(1|Speaker) + (0+AccentType_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx",optCtrl=list(method="nlminb")))
xmdl.AccentType.Null.nlminb = glmer(Prominence ~ 1 +
	(1|Listener) + (0+AccentType_c|Listener) + 
	(1|Speaker) + (0+AccentType_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx",optCtrl=list(method="nlminb")))

xmdl.AccentType.LBFGSB = glmer(Prominence ~ AccentType_c +
	(1|Listener) + (0+AccentType_c|Listener) + 
	(1|Speaker) + (0+AccentType_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))
xmdl.AccentType.Null.LBFGSB = glmer(Prominence ~ 1 +
	(1|Listener) + (0+AccentType_c|Listener) + 
	(1|Speaker) + (0+AccentType_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))


save.image("mixed_models_convergence.RData")

## AccentPosition:


xmdl.AccentPosition.bobyqa = glmer(Prominence ~ AccentPosition_c +
	(1|Listener) + (0+AccentPosition_c|Listener) + 
	(1|Speaker) + (0+AccentPosition_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="bobyqa"))
xmdl.AccentPosition.Null.bobyqa = glmer(Prominence ~ 1 +
	(1|Listener) + (0+AccentPosition_c|Listener) + 
	(1|Speaker) + (0+AccentPosition_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="bobyqa"))

xmdl.AccentPosition.nelder = glmer(Prominence ~ AccentPosition_c +
	(1|Listener) + (0+AccentPosition_c|Listener) + 
	(1|Speaker) + (0+AccentPosition_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="Nelder_Mead"))
xmdl.AccentPosition.Null.nelder = glmer(Prominence ~ 1 +
	(1|Listener) + (0+AccentPosition_c|Listener) + 
	(1|Speaker) + (0+AccentPosition_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",control=lmerControl(optimizer="Nelder_Mead"))

xmdl.AccentPosition.nlminb = glmer(Prominence ~ AccentPosition_c +
	(1|Listener) + (0+AccentPosition_c|Listener) + 
	(1|Speaker) + (0+AccentPosition_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx",optCtrl=list(method="nlminb")))
xmdl.AccentPosition.Null.nlminb = glmer(Prominence ~ 1 +
	(1|Listener) + (0+AccentPosition_c|Listener) + 
	(1|Speaker) + (0+AccentPosition_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx",optCtrl=list(method="nlminb")))

xmdl.AccentPosition.LBFGSB = glmer(Prominence ~ AccentPosition_c +
	(1|Listener) + (0+AccentPosition_c|Listener) + 
	(1|Speaker) + (0+AccentPosition_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))
xmdl.AccentPosition.Null.LBFGSB = glmer(Prominence ~ 1 +
	(1|Listener) + (0+AccentPosition_c|Listener) + 
	(1|Speaker) + (0+AccentPosition_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial",lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))

save.image("mixed_models_convergence.RData")





########################################################################
######################## Different random effects structure, with correlation:
########################################################################


## Part of Speech class:

xmdl.POS_class.corr = glmer(Prominence ~ POS_class_c +
	(1+POS_class_c|Listener) +
	(1+POS_class_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
xmdl.POS_class.Null.corr = glmer(Prominence ~ 1 +
	(1+POS_class_c|Listener) +
	(1+POS_class_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
save.image("mixed_models_convergence.RData")


## AccentType:

xmdl.AccentType.corr = glmer(Prominence ~ AccentType_c +
	(1+AccentType_c|Listener) +
	(1+AccentType_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
xmdl.AccentType.Null.corr = glmer(Prominence ~ 1 +
	(1+AccentType_c|Listener) +
	(1+AccentType_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
save.image("mixed_models_convergence.RData")

## AccentPosition:

xmdl.AccentPosition.corr = glmer(Prominence ~ AccentPosition_c +
	(1+AccentPosition_c|Listener) +
	(1+AccentPosition_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
xmdl.AccentPosition.Null.corr = glmer(Prominence ~ 1 +
	(1+AccentPosition_c|Listener) +
	(1+AccentPosition_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
save.image("mixed_models_convergence.RData")


