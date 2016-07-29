## June 19,  2015
## Bodo Winter
## Analysis of random forest output

##------------------------------------------------------------------
## Load in data and packages:
##------------------------------------------------------------------

## Load in summary data:

mainDir <- '/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/data'
setwd(mainDir)
RPT <- read.csv('RPT_summary_processed.csv')

## Load libraries:

library(party)

## Load random forest output:

# load('accented_forests.RData')
# load('central_forests.RData')
# load('KNN_forests.RData')
load('raw_forests.RData')
# load('reduced_forests.RData')


##------------------------------------------------------------------
## Check predictive accuracy:
##------------------------------------------------------------------

## Which one works best?

cor(raw_predictions, RPT$PScore)
mean(as.vector(raw_predictions - RPT$PScore) ^ 2)	# MSE
# cor(central_predictions, RPT$p_score)
# cor(KNN_predictions, RPT$p_score)
# cor(red_predictions, RPT_red$p_score)
# cor(accented_predictions, RPT_accented$p_score)


##------------------------------------------------------------------
## Plot variable importances:
##------------------------------------------------------------------

## Sort variable importances:

raw_varimp <- sort(raw_varimp)

## Change names of variable importances for display purposes:

names(raw_varimp)[names(raw_varimp) == 'Vowel'] <- 'Phon. Vowel'
names(raw_varimp)[names(raw_varimp) == 'Focused'] <- 'Focus Particle'
names(raw_varimp)[names(raw_varimp) == 'LastArgument'] <- 'Last Argument'
names(raw_varimp)[names(raw_varimp) == 'H1A2'] <- 'H1-A2'
names(raw_varimp)[names(raw_varimp) == 'H1A3'] <- 'H1-A3'
names(raw_varimp)[names(raw_varimp) == 'NSyll'] <- 'No. of Syll.'
names(raw_varimp)[names(raw_varimp) == 'SpectralEmphasis'] <- 'Spectral Emphasis'
names(raw_varimp)[names(raw_varimp) == 'SyllableDur'] <- 'Syllable Dur.'
names(raw_varimp)[names(raw_varimp) == 'VowelDur'] <- 'Vowel Dur.'
names(raw_varimp)[names(raw_varimp) == 'LogFreq'] <- 'Log Frequency'
names(raw_varimp)[names(raw_varimp) == 'RMS_amplitude'] <- 'RMS Amplitude'
names(raw_varimp)[names(raw_varimp) == 'MeanPitch'] <- 'Mean F0'
names(raw_varimp)[names(raw_varimp) == 'MaxPitch'] <- 'Max F0'
names(raw_varimp)[names(raw_varimp) == 'AccentType'] <- 'Accent Type'
names(raw_varimp)[names(raw_varimp) == 'AccentPosition'] <- 'Accent Position'
names(raw_varimp)[names(raw_varimp) == 'POS_class'] <- 'Part of Speech'

## Plot raw variable importance:

quartz('', 9, 6)
par(mai = c(1, 2, 0.15, 0.5))
plot(1, 1, type = 'n', bty = 'n',
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n',
	xlim = c(-10, 350), ylim = c(0.25, 19.25))
abline(h = 1:length(raw_varimp), col = 'darkgrey')
points(raw_varimp, 1:length(raw_varimp), pch = 19, cex = 1.5)
axis(side = 1, at = seq(0, 300, 50),s
	font = 2, cex.axis = 1.25, lwd = 2, lwd.ticks = 2)
axis(side = 2, at = 1:length(raw_varimp),
	las = 2, font = 2, tick = F, cex.axis = 1.15, labels = names(raw_varimp),
	line = -0.5)
segments(x0 = 0, y0 = 0, y1 = 18, lwd = 2, lty = 2)
mtext(side = 1, 'Variable Importance', line = 3.25, cex = 2, font = 2)



##------------------------------------------------------------------
## Plot conditional inference tree:
##------------------------------------------------------------------

## LogFrequency:

xtree <- ctree(PScore ~ AccentPosition + AccentType + Accented + MeanPitch + MaxPitch + POS_class +
	SyllableDur + VowelDur + Vowel + RMS_amplitude + NSyll +
	LogFreq + LastArgument + Focused +
	H1A2 + H1A3 + SpectralEmphasis,
	RPT)
quartz('', 11, 6)
plot(xtree)



