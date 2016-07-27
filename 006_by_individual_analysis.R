## Bodo Winter
## By-individual analyses
## June 22, 2015
## July 27, 2016: Finishing brushes and incorporation of spectral tilt
## July 27, 2016: Replaced ggplot2 with base graphs

## Load mixed models:

mainDir <- '/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/data'
setwd(mainDir)
load('mixed_models.RData')

## Generate a matrix with all the listeners' random effects:

allsubs <- data.frame(Subject = rownames(coef(xmdl.MeanPitch)$Listener),
	MeanPitch = coef(xmdl.MeanPitch)$Listener[, 2], 
	MaxPitch = coef(xmdl.MaxPitch)$Listener[, 2], 
	Amplitude = coef(xmdl.RMS_amplitude)$Listener[, 2], 
	VowelDur = coef(xmdl.VowelDur)$Listener[, 2],
	SyllableDur = coef(xmdl.SyllableDur)$Listener[, 2], 
	NSyll = coef(xmdl.NSyll)$Listener[, 2], 
	Range = coef(xmdl.RangeST)$Listener[, 2], 
	Slope = coef(xmdl.SlopeST)$Listener[, 2], 
	Freq = coef(xmdl.Freq)$Listener[, 2], 
	Vowel = coef(xmdl.Vowel)$Listener[, 2], 
	POS = coef(xmdl.POS_class)$Listener[, 2], 			# for content
	Focused = coef(xmdl.Focused)$Listener[, 2], 			# for focus particle
	Argument = coef(xmdl.argument)$Listener[, 2],			# for last argument
	SpectralEmphasis = coef(xmdl.SpectralEmphasis)$Listener[, 2],
	H1A2 = coef(xmdl.H1A2)$Listener[, 2],
	H1A3 = coef(xmdl.H1A3)$Listener[, 2]
	)

## Hand-code accenteds:

accenteds <- coef(xmdl.Accented.bobyqa)$Listener
allsubs$Accented <- accenteds$Accented_cyes - accenteds$Accented_cno

## Add mean absolute change for the multi-level factors:

allsubs$AccentType <- rowMeans(abs(coef(xmdl.AccentType.bobyqa)$Listener[, 1:4]))
allsubs$AccentPosition <- rowMeans(abs(coef(xmdl.AccentPosition.bobyqa)$Listener[, c(1, 3, 4)]))

## Generate correlation matrices:

round(cor(allsubs[, -1]), 2)
round(cor(allsubs[, c('Accented', 'Focused', 'Argument', 'POS', 'AccentType', 'AccentPosition')]), 2)
round(cor(allsubs[, c('POS', 'MaxPitch', 'Amplitude', 'VowelDur', 'AccentType', 'AccentPosition')]), 2)

## Group variables according to meaningful categories:

prosodic_variables <- c('Accented', 'AccentPosition', 'AccentType')
syntactic_variables <- c('Argument', 'Focused', 'POS')
phonetic_variables <- c('MeanPitch', 'MaxPitch', 'Amplitude', 'VowelDur', 'SyllableDur',
	'SpectralEmphasis', 'H1A2', 'H1A3')

## Create a data frame with means of these variables:

newsubs <- data.frame(Prosody = rowMeans(allsubs[, prosodic_variables]),
	Syntax = rowMeans(allsubs[, syntactic_variables]),
	Phonetics = rowMeans(allsubs[, phonetic_variables]),
	Freq = allsubs$Freq)

## Group variables according to different phonetic parameters:

pitch <- c('MeanPitch', 'MaxPitch')
spectrum <- c('SpectralEmphasis', 'H1A2', 'H1A3')
duration <- c('VowelDur', 'SyllableDur')

## Create a data frame with means of these variables:

subsphon <- data.frame(Pitch = rowMeans(allsubs[, pitch]),
	Spectrum = rowMeans(abs(allsubs[, spectrum])),
	Duration = rowMeans(allsubs[, duration]))

## Perform correlations:

cor(newsubs)
cor(subsphon)

## Perform significance test on correlations for the 'newsubs' data frame:

cor.test(newsubs$Prosody, newsubs$Syntax)
cor.test(newsubs$Prosody, newsubs$Phonetics)
cor.test(newsubs$Prosody, newsubs$Freq)
cor.test(newsubs$Syntax, newsubs$Freq)
cor.test(newsubs$Phonetics, newsubs$Freq)
cor.test(newsubs$Syntax, newsubs$Phonetics)

## Perform Dunn-Sidak correction:

dunnsidak <- function(P, N) 1 - ((1 - P) ^ N)
dunnsidak(cor.test(newsubs$Prosody, newsubs$Syntax)$p.val, 6)
dunnsidak(cor.test(newsubs$Prosody, newsubs$Phonetics)$p.val, 6)
dunnsidak(cor.test(newsubs$Prosody, newsubs$Freq)$p.val, 6)
dunnsidak(cor.test(newsubs$Syntax, newsubs$Freq)$p.val, 6)
dunnsidak(cor.test(newsubs$Phonetics, newsubs$Freq)$p.val, 6)
dunnsidak(cor.test(newsubs$Syntax, newsubs$Phonetics)$p.val, 6)

## Perform significance test on correlations for the 'subsphon' data frame:

cor.test(subsphon$Pitch, subsphon$Spectrum)
cor.test(subsphon$Pitch, subsphon$Duration)
cor.test(subsphon$Duration, subsphon$Spectrum)

## Perform Dunn-Sidak correction:

dunnsidak(cor.test(subsphon$Pitch, subsphon$Spectrum)$p.val, 3)
dunnsidak(cor.test(subsphon$Pitch, subsphon$Duration)$p.val, 3)
dunnsidak(cor.test(subsphon$Duration, subsphon$Spectrum)$p.val, 3)


