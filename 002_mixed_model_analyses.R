## Bodo Winter
## April 25, 2015; Added new analyses: June 19, 2015
## July 26, 2016: Finishing brushes and incorporation of spectral tilt
## Visualizations of Rapid Prosody Transcription Data


##------------------------------------------------------------------
## Load in data and packages:
##------------------------------------------------------------------

## Path for main analysis:

mainDir <- '/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/data'
setwd(mainDir)

## Load:

RPT <- read.csv('RPT_individual_processed.csv')

## Load in libraries needed:

library(lme4)
library(dplyr)


##------------------------------------------------------------------
## Coding predictor variables:
##------------------------------------------------------------------

## Take absolute variables for slope, treating strongly falling...
## ... and strongly rising the same:

RPT <- mutate(RPT,
	PitchRangeST_abs = abs(PitchRangeST),
	PitchSlopeST_abs = abs(PitchSlopeST))

## Z-transform acoustic variables:

all_z <- select(RPT, MeanPitch:VowelDur)
all_z <- cbind(all_z, select(RPT, one_of('NSyll', 'LogFreq', 'PitchRangeST_abs', 'PitchSlopeST_abs')))
all_z <- apply(all_z, 2, scale)
colnames(all_z) <- paste(colnames(all_z), 'z', sep = '_')
RPT <- cbind(RPT, all_z)

## Create a "word nested within sentence" variable:

RPT$WordWithinSentence <- 1:562

## Contrast code the gender variable:

RPT <- mutate(RPT,
	SpeakerGender_c = ifelse(SpeakerGender == 'M', 0.5, -0.5))

## Write to table:

write.table(RPT, 'RPT_individual_processed_with_z.csv', sep = ',', row.names = F)



##------------------------------------------------------------------
## Mixed model analysis:
##------------------------------------------------------------------

## MaxPitch model and null model (continuous effect):

xmdl.MaxPitch <- glmer(Prominence ~ MaxPitch_z + SpeakerGender_c + 
	(1|Listener) + (0 + MaxPitch_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial')
xmdl.MaxPitch.Null <- glmer(Prominence ~ 1 + SpeakerGender_c + 
	(1|Listener) + (0 + MaxPitch_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial')

## MeanPitch model and null model (continuous effect):

xmdl.MeanPitch <- glmer(Prominence ~ MeanPitch_z + SpeakerGender_c + 
	(1|Listener) + (0 + MeanPitch_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial', control = glmerControl(optimizer = 'bobyqa'))
xmdl.MeanPitch.Null <- glmer(Prominence ~ 1 + SpeakerGender_c + 
	(1|Listener) + (0 + MeanPitch_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial', control = glmerControl(optimizer = 'bobyqa'))

## RMS_amplitude model and null model (continuous effect):

xmdl.RMS_amplitude <- glmer(Prominence ~ RMS_amplitude_z +
	(1|Listener) + (0 + RMS_amplitude_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial')
xmdl.RMS_amplitude.Null <- glmer(Prominence ~ 1 +
	(1|Listener) + (0 + RMS_amplitude_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial')

## Spectral emphasis model and null model (continuous effect):

xmdl.SpectralEmphasis <- glmer(Prominence ~ SpectralEmphasis_z +
	(1|Listener) + (0 + SpectralEmphasis_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial', control = glmerControl(optimizer = 'bobyqa'))
xmdl.SpectralEmphasis.Null <- glmer(Prominence ~ 1 +
	(1|Listener) + (0 + SpectralEmphasis_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial', control = glmerControl(optimizer = 'bobyqa'))

## H1-A2 model and null model (continuous effect):

xmdl.H1A2 <- glmer(Prominence ~ H1A2_z +
	(1|Listener) + (0 + H1A2_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial', control = glmerControl(optimizer = 'bobyqa'))
xmdl.H1A2.Null <- glmer(Prominence ~ 1 +
	(1|Listener) + (0 + H1A2_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial', control = glmerControl(optimizer = 'bobyqa'))

## H1-A3 model and null model (continuous effect):

xmdl.H1A3 <- glmer(Prominence ~ H1A3_z +
	(1|Listener) + (0 + H1A3_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial')
xmdl.H1A3.Null <- glmer(Prominence ~ 1 +
	(1|Listener) + (0 + H1A3_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial')

## VowelDur model and null model (continuous effect):

xmdl.VowelDur <- glmer(Prominence ~ VowelDur_z +
	(1|Listener) + (0 + VowelDur_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial')
xmdl.VowelDur.Null  <- glmer(Prominence ~ 1 +
	(1|Listener) + (0 + VowelDur_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial')

## SyllableDur model and null model (continuous effect):

xmdl.SyllableDur <- glmer(Prominence ~ SyllableDur_z +
	(1|Listener) + (0 + SyllableDur_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial')
xmdl.SyllableDur.Null <- glmer(Prominence ~ 1 +
	(1|Listener) + (0 + SyllableDur_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial')

## No. of Syllables model and null model (treated as continuous effect):

xmdl.NSyll <- glmer(Prominence ~ NSyll_z +
	(1|Listener) + (0 + NSyll_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial', control = glmerControl(optimizer = 'bobyqa'))
xmdl.NSyll.Null <- glmer(Prominence ~ 1 +
	(1|Listener) + (0 + NSyll_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial', control = glmerControl(optimizer = 'bobyqa'))

## RangeST model and null model (continuous effect):
## (Computed only based on subset for which there are pitch accents)

RPT.RangeST <- RPT[complete.cases(RPT$PitchRangeST_abs), ]
xmdl.RangeST <- glmer(Prominence ~ PitchRangeST_abs_z +
	(1|Listener) + (0 + PitchRangeST_abs_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT.RangeST, family = 'binomial')
xmdl.RangeST.Null = glmer(Prominence ~ 1 +
	(1|Listener) + (0 + PitchRangeST_abs_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT.RangeST, family = 'binomial')

## SlopeST model and null model (continuous effect):
## (Computed only based on subset for which there are pitch accents)

RPT.SlopeST <- RPT[complete.cases(RPT$PitchSlopeST_abs), ]
xmdl.SlopeST <- glmer(Prominence ~ PitchSlopeST_abs_z +
	(1|Listener) + (0 + PitchSlopeST_abs_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT.SlopeST, family = 'binomial')
xmdl.SlopeST.Null <- glmer(Prominence ~ 1 +
	(1|Listener) + (0 + PitchSlopeST_abs_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT.SlopeST, family = 'binomial')
rm(RPT.SlopeST)

## Log frequency model and null model (continuous effect):

xmdl.Freq <- glmer(Prominence ~ LogFreq_z +
	(1|Listener) + (0 + LogFreq_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial')
xmdl.Freq.Null <- glmer(Prominence ~ 1 +
	(1|Listener) + (0 + LogFreq_z|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial')

## Phonological Vowel Duration model and null model (categorical effect, 2 levels):

RPT$Vowel_c <- as.numeric(RPT$Vowel) - 1.5
xmdl.Vowel <- glmer(Prominence ~ Vowel_c +
	(1|Listener) + (0 + Vowel_c|Listener) +
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial')
xmdl.Vowel.Null <- glmer(Prominence ~ 1 +
	(1|Listener) + (0 + Vowel_c|Listener) +
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial')

## Part of Speech class model and null model (categorical effect, 2 levels):

RPT$POS_class_c <- as.numeric(RPT$POS_class) - 1.5
xmdl.POS_class <- glmer(Prominence ~ POS_class_c +
	(1|Listener) + (0 + POS_class_c|Listener) +
	(1|Speaker) + (1|Sentence),	# no word because the variance was conflated with POS(?)
	RPT, family = 'binomial', control = glmerControl(optimizer = 'bobyqa'))
xmdl.POS_class.Null <- glmer(Prominence ~ 1 +
	(1 + POS_class|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial', control = glmerControl(optimizer = 'bobyqa'))

## Focus particle model and null model (categorical effect, 2 levels):

RPT$Focused_c <- as.numeric(RPT$Focused) - 1.5
xmdl.Focused <- glmer(Prominence ~ Focused_c +
	(1|Listener) + (0 + Focused_c|Listener) +
	(1|WordWithinSentence),		
	RPT, family = 'binomial', control = glmerControl(optimizer = 'bobyqa'))
# by-listener random slopes could only be estimated with "WordWithinSentence" instead of sentence/speaker
xmdl.Focused.Null <- glmer(Prominence ~ 1 +
	(1|Listener) + (0 + Focused_c|Listener) +
	(1|WordWithinSentence),
	RPT, family = 'binomial')

## Last argument model and null model (categorical effect, 2 levels):

RPT$LastArgument_c <- as.numeric(RPT$LastArgument) - 1.5
xmdl.argument <- glmer(Prominence ~ LastArgument_c +
	(1|Listener) + (0 + LastArgument_c|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial')
# when looking at Prominence ~ Word ~ LastArgument, there's complete separation
xmdl.argument.Null <- glmer(Prominence ~ 1 +
	(1|Listener) + (0 + LastArgument_c|Listener) + 
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial')

## To make AccentType and AccentPosition converge, find offending conditions:

RPT_red <- filter(RPT, AccentPosition != 'postnuclear')
RPT_red <- filter(RPT_red, AccentPosition != 'no_accent')
RPT_red$AccentPosition <- factor(RPT_red$AccentPosition)

RPT_red$AccentPosition_c <- RPT_red$AccentPosition
contrasts(RPT_red$AccentPosition_c) <- contr.sum(3)

xmdl.AccentPosition.bobyqa = glmer(Prominence ~ AccentPosition_c +
	(1|Listener) + (0 + AccentPosition_c|Listener),
	RPT_red, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa'))
# model with sentence and speaker converged but were deprecate / variances not estimated
xmdl.AccentPosition.Null.bobyqa = glmer(Prominence ~ 1 +
	(1|Listener) + (0 + AccentPosition_c|Listener),
	RPT_red, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa'))

## To make AccentType and AccentPosition converge, find offending conditions:

RPT_red <- filter(RPT, AccentType != 'no_accent')
RPT_red$AccentType <- factor(RPT_red$AccentType)

RPT_red$AccentType_c <- RPT_red$AccentType
contrasts(RPT_red$AccentType_c) <- contr.sum(4)

xmdl.AccentType.bobyqa = glmer(Prominence ~ AccentType_c +
	(1|Listener) + (0 + AccentType_c|Listener),
	RPT_red, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa'))
# model with sentence and speaker converged but were deprecate / variances not estimated
xmdl.AccentType.Null.bobyqa = glmer(Prominence ~ 1 +
	(1|Listener) + (0 + AccentType_c|Listener),
	RPT_red, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa'))

## Test accented versus non-accented:

RPT$Accented <- factor(ifelse(RPT$AccentType == 'no_accent', 'no', 'yes'))
RPT$Accented_c <- as.numeric(RPT$Accented) - 1.5

xmdl.Accented.bobyqa <- glmer(Prominence ~ Accented_c +
	(1|Listener) + (0 + Accented_c|Listener) +
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa'))
xmdl.Accented.Null.bobyqa = glmer(Prominence ~ 1 +
	(1|Listener) + (0 + Accented_c|Listener) +
	(1|Speaker) + (1|Sentence),
	RPT, family = 'binomial', control = glmerControl(optimizer = 'bobyqa'))

## Save output:

rm(all_z, mainDir, RPT_red, RPT)
save.image('mixed_models.RData')


