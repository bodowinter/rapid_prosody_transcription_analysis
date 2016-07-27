## Bodo Winter
## June 20, 2015
## July 26, 2016: Finishing brushes and incorporation of spectral tilt
## July 26, 2016: Replaced ggplot2 with base graphs
## Mixed model interpretation and analysis:

##------------------------------------------------------------------
## Load in data and packages:
##------------------------------------------------------------------

## Libraries:

library(dplyr)
library(lme4)

## Load in dataset (with transformed variables):

mainDir <- '/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/data'
setwd(mainDir)
RPT <- read.csv('RPT_individual_processed_with_z.csv')

## Load mixed models:

load('mixed_models.RData')


##------------------------------------------------------------------
## Analyses of mixed model results:
##------------------------------------------------------------------

## Get character vectors with all model names for full and null models:

all_models <- grep('xmdl', ls(), value = T)
all_nulls <- grep('Null', all_models, value = T)
all_models <- setdiff(all_models, all_nulls)

## Create an empty data frame to be filled with results from likelihood ratio tests:

LRTs <- data.frame(Chisq = numeric(length(all_models)),
	Df = numeric(length(all_models)),
	Pval = numeric(length(all_models)))

## Loop through model names and append do data frame:

for (i in 1:length(all_models)) {
	this_full <- get(all_models[i])
	this_null <- get(all_nulls[i])
	this_LRT <- anova(this_null, this_full, test = 'Chisq')

	LRTs[i, ]$Chisq <- this_LRT$Chisq[2]
	LRTs[i, ]$Df <- this_LRT$'Chi Df'[2]
	LRTs[i, ]$Pval <- this_LRT$'Pr(>Chisq)'[2]
	}
rownames(LRTs) <- gsub('xmdl.', '', all_models)

## Dunn-Sidak transform the p-values:

dunnsidak <- function(P, N) 1 - ((1 - P) ^ N)
LRTs$P_corr <- dunnsidak(LRTs$Pval, N = nrow(LRTs))	# correct for performing N tests

## Round p-values:

LRTs$Pval <- round(LRTs$Pval, 4)
LRTs$P_corr <- round(LRTs$P_corr, 4)


##------------------------------------------------------------------
## Get predictions for mixed models, continuous variables:
##------------------------------------------------------------------

## Get prediction function (need to be connected to the web):

source('http://bodowinter.com/predict.glmm.R')

## Create data frame for predictions of continuous variables:

z_vals <- seq(-3, 3, 0.01)
all_preds <- data.frame(SpeakerGender_c = 0,
	MeanPitch_z = z_vals,
	MaxPitch_z = z_vals,
	LogFreq_z = z_vals,
	VowelDur_z = z_vals,
	SyllableDur_z = z_vals,
	SpectralEmphasis_z = z_vals,
	H1A2_z = z_vals,
	H1A3_z = z_vals,
	PitchRangeST_abs_z = z_vals,
	PitchSlopeST_abs_z = z_vals)

## Define models to get predictions for:
	
these_models <- c('xmdl.MeanPitch', 'xmdl.MaxPitch', 'xmdl.Freq',
	'xmdl.VowelDur', 'xmdl.SyllableDur',
	'xmdl.SpectralEmphasis', 'xmdl.H1A2', 'xmdl.H1A3',
	'xmdl.RangeST', 'xmdl.SlopeST')

## Append predictions:

these_columns <- c('Prominence', 'UB', 'LB')

for (i in 1:length(these_models)) {
	## Select model:
	
	this_model <- get(these_models[i])
	
	## Get prdictions:
	
	xpred <- predict.glmm(this_model,
		newdata = all_preds, type = 'binomial')[, these_columns]
	
	## Rename columns:
	
	colnames(xpred) <- paste(gsub('xmdl.', '', these_models[i]), colnames(xpred), sep = '_')
	
	## Append:
	
	all_preds <- cbind(all_preds, xpred)
	
	}

##------------------------------------------------------------------
## Get predictions for mixed models, categorical variables:
##------------------------------------------------------------------

## Define data frames with factor levels to get predictions for:

vowel_pred <- data.frame(Vowel = c('short', 'long'))
POS_pred <- data.frame(POS_class = c('function', 'content'))
focus_pred <- data.frame(Focused = c('no_focus_particle', 'focus_particle'))
argument_pred <- data.frame(LastArgument = c('not_last', 'last'))
accented_pred <- data.frame(Accented_c = c('no', 'yes'))
position_pred <- data.frame(AccentPosition_c = c('prenuclear', 'nuclear_ip', 'nuclear_IP'))
type_pred <- data.frame(AccentType_c = c('low', 'falling', 'high', 'rising'))

## Get predictions for this:

vowel_pred <- predict.glmm(xmdl.Vowel, newdata = vowel_pred, type = 'binomial')
POS_pred <- predict.glmm(xmdl.POS_class, newdata = POS_pred, type = 'binomial')
focus_pred <- predict.glmm(xmdl.Focused, newdata = focus_pred, type = 'binomial')
argument_pred <- predict.glmm(xmdl.argument, newdata = argument_pred, type = 'binomial')
accented_pred <- predict.glmm(xmdl.Accented.bobyqa, newdata = accented_pred, type = 'binomial')
position_pred <- predict.glmm(xmdl.AccentPosition.bobyqa, newdata = position_pred, type = 'binomial')
type_pred <- predict.glmm(xmdl.AccentType.bobyqa, newdata = type_pred, type = 'binomial')



##------------------------------------------------------------------
## Plot predictions of mixed models:
##------------------------------------------------------------------

## Make a quadruple plot with MaxPitch, SyllableDur, SpectralEmphasis and Frequency:

set.seed(42)
quartz('', 10, 9)
par(mfrow = c(2, 2), omi = c(1.5, 1.5, 0.25, 0.25), mai = c(0, 0.25, 0.65, 0.25))
## First plot:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(-2, 2), ylim = c(-0.1, 1.15))
text(x = -1.9, y = 1.1, labels = '(A)', font = 2, cex = 1.35)
points(RPT$MaxPitch_z, jitter(RPT$Prominence, 0.1),
	bg = rgb(0, 0, 0, 0.05), pch = 21, cex = 0.85, col = NA)
polygon(x = c(all_preds$MaxPitch_z, rev(all_preds$MaxPitch_z)),
	y = c(all_preds$MaxPitch_UB, rev(all_preds$MaxPitch_LB)),
	border = NA, col = rgb(0, 0, 0, 0.4))
points(all_preds$MaxPitch_z, all_preds$MaxPitch_Prominence,
	type = 'l', lwd = 4)	
axis(side = 2, at = seq(0, 1, 0.5), font = 2, las = 2,
	lwd = 2, lwd.ticks = 2,
	cex.axis = 1.5, labels = c('0%', '50%', '100%'))
mtext(side = 2, '%Prominent', font = 2, line = 5, cex = 2)	
mtext(side = 3, 'Maximum F0', font = 2, line = 0.75, cex = 1.75)
box(lwd = 2)
## Second plot:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(-2, 2), ylim = c(-0.1, 1.15))
text(x = -1.9, y = 1.1, labels = '(B)', font = 2, cex = 1.35)
points(RPT$SyllableDur_z, jitter(RPT$Prominence, 0.1),
	bg = rgb(0, 0, 0, 0.05), pch = 21, cex = 0.85, col = NA)
polygon(x = c(all_preds$SyllableDur_z, rev(all_preds$SyllableDur_z)),
	y = c(all_preds$SyllableDur_UB, rev(all_preds$SyllableDur_LB)),
	border = NA, col = rgb(0, 0, 0, 0.4))
points(all_preds$SyllableDur_z, all_preds$SyllableDur_Prominence,
	type = 'l', lwd = 4)
mtext(side = 3, 'Syllable Duration', font = 2, line = 0.75, cex = 1.75)	
box(lwd = 2)
## Third plot:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(-2, 2), ylim = c(-0.1, 1.15))
text(x = -1.9, y = 1.1, labels = '(C)', font = 2, cex = 1.35)
points(RPT$SpectralEmphasis_z, jitter(RPT$Prominence, 0.1),
	bg = rgb(0, 0, 0, 0.05), pch = 21, cex = 0.85, col = NA)
polygon(x = c(all_preds$SpectralEmphasis_z, rev(all_preds$SpectralEmphasis_z)),
	y = c(all_preds$SpectralEmphasis_UB, rev(all_preds$SpectralEmphasis_LB)),
	border = NA, col = rgb(0, 0, 0, 0.4))
points(all_preds$SpectralEmphasis_z, all_preds$SpectralEmphasis_Prominence,
	type = 'l', lwd = 4)
axis(side = 2, at = seq(0, 1, 0.5), font = 2, las = 2,
	lwd = 2, lwd.ticks = 2,
	cex.axis = 1.5, labels = c('0%', '50%', '100%'))
mtext(side = 2, '%Prominent', font = 2, line = 5, cex = 2)	
mtext(side = 3, 'Spectral Emphasis', font = 2, line = 0.75, cex = 1.75)	
axis(side = 1, at = seq(-2, 2, 1), lwd = 2, lwd.ticks = 2, labels = F)
axis(side = 1, at = seq(-2, 2, 1), tick = F, font = 2, cex.axis = 1.25, line = -0.15)
mtext(side = 1, 'z-scores', font = 2, line = 2.75, cex = 1.75)
box(lwd = 2)
## Fourth plot:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(-2, 2), ylim = c(-0.1, 1.15))
text(x = -1.9, y = 1.1, labels = '(D)', font = 2, cex = 1.35)
points(RPT$LogFreq_z, jitter(RPT$Prominence, 0.1),
	bg = rgb(0, 0, 0, 0.05), pch = 21, cex = 0.85, col = NA)
polygon(x = c(all_preds$LogFreq_z, rev(all_preds$LogFreq_z)),
	y = c(all_preds$Freq_UB, rev(all_preds$Freq_LB)),
	border = NA, col = rgb(0, 0, 0, 0.4))
points(all_preds$LogFreq_z, all_preds$Freq_Prominence,
	type = 'l', lwd = 4)
mtext(side = 3, 'Log Word Frequency', font = 2, line = 0.75, cex = 1.75)	
axis(side = 1, at = seq(-2, 2, 1), lwd = 2, lwd.ticks = 2, labels = F)
axis(side = 1, at = seq(-2, 2, 1), tick = F, font = 2, cex.axis = 1.25, line = -0.15)
mtext(side = 1, 'z-scores', font = 2, line = 2.75, cex = 1.75)
box(lwd = 2)

## Make a double plot with slope and range for accented syllables only:

set.seed(666)
quartz('', 11, 5)
par(mfrow = c(1,2), omi = c(1, 1.5, 0.5, 0.25), mai = rep(0.25, 4))
## First plot:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(-2, 2), ylim = c(-0.1, 1.15))
text(x = -1.9, y = 1.1, labels = '(A)', font = 2, cex = 1.35)
points(RPT$PitchSlopeST_z, jitter(RPT$Prominence, 0.1),
	bg = rgb(0, 0, 0, 0.05), pch = 21, cex = 0.85, col = NA)
polygon(x = c(all_preds$PitchSlopeST_abs_z, rev(all_preds$PitchSlopeST_abs_z)),
	y = c(all_preds$SlopeST_UB, rev(all_preds$SlopeST_LB)),
	border = NA, col = rgb(0, 0, 0, 0.3))
points(all_preds$PitchSlopeST_abs_z, all_preds$SlopeST_Prominence,
	type = 'l', lwd = 4)
axis(side = 1, at = seq(-2, 2, 1), lwd = 2, lwd.ticks = 2, labels = F)
axis(side = 1, at = seq(-2, 2, 1), tick = F, font = 2, cex.axis = 1.25, line = -0.15)
mtext(side = 1, 'z-scores', font = 2, line = 2.75, cex = 1.75)
axis(side = 2, at = seq(0, 1, 0.5), font = 2, las = 2,
	lwd = 2, lwd.ticks = 2,
	cex.axis = 1.5, labels = c('0%', '50%', '100%'))
mtext(side = 2, '%Prominent', font = 2, line = 5, cex = 2)	
mtext(side = 3, 'Absolute Pitch Slope', font = 2, line = 0.75, cex = 1.75)
box(lwd = 2)
## Second plot:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(-2, 2), ylim = c(-0.1, 1.15))
text(x = -1.9, y = 1.1, labels = '(B)', font = 2, cex = 1.35)
points(RPT$PitchRangeST_z, jitter(RPT$Prominence, 0.1),
	bg = rgb(0, 0, 0, 0.05), pch = 21, cex = 0.85, col = NA)
polygon(x = c(all_preds$PitchRangeST_abs_z, rev(all_preds$PitchRangeST_abs_z)),
	y = c(all_preds$RangeST_UB, rev(all_preds$RangeST_LB)),
	border = NA, col = rgb(0, 0, 0, 0.4))
points(all_preds$PitchRangeST_abs_z, all_preds$RangeST_Prominence,
	type = 'l', lwd = 4)
axis(side = 1, at = seq(-2, 2, 1), lwd = 2, lwd.ticks = 2, labels = F)
axis(side = 1, at = seq(-2, 2, 1), tick = F, font = 2, cex.axis = 1.25, line = -0.15)
mtext(side = 1, 'z-scores', font = 2, line = 2.75, cex = 1.75)
mtext(side = 3, 'Absolute Pitch Range', font = 2, line = 0.75, cex = 1.75)
box(lwd = 2)

## Make a plot with all binary categorical variables:

xfac <- 0.25
quartz('', 12, 5)
par(mai = c(1, 1.5, 0.25, 0.25))
## First plot:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(0.75, 10.75), ylim = c(-0.1, 1.15))
# axis(side = 1, at = 1:10 + rep(c(xfac, -xfac), 5),
	# lwd = 2, lwd.ticks = 2, labels = F)
# axis(side = 1, at = 1:10 + rep(c(xfac, -xfac), 5),
	# lwd = 2, lwd.ticks = 2, labels = c('short', 'long', 'function', 'content',
	# 'not last', 'last', 'no focus', 'focus', 'no accent', 'accented'), las = 2, font = 2)
axis(side = 1, at = seq(1.5, 10.5, 2), lwd = 2, lwd.ticks = 2, labels = F)
axis(side = 1, at = seq(1.5, 10.5, 2), tick = F, line = 1.5, font = 2, cex.axis = 1.5,
	labels = c('Phonological\nVowel Length', 'Lexical\nCategory', 'Last\nArgument',
		'Focus\nParticle', 'Pitch\nAccent'))
text(x = 1:10 + rep(c(xfac, -xfac), 5),
	y = c(vowel_pred$UB, POS_pred$UB, argument_pred$UB,
		focus_pred$UB, accented_pred$UB) + 0.03,
	labels = c('short', 'long', 'function', 'content',
		'not last', 'last', 'no focus', 'focus', 'no accent', 'accented'),
		srt = 90, font = 2, adj = 0, cex = 1.15)
axis(side = 2, at = seq(0, 1, 0.5), font = 2, las = 2,
	lwd = 2, lwd.ticks = 2,
	cex.axis = 1.5, labels = c('0%', '50%', '100%'))
mtext(side = 2, '%Prominent', font = 2, line = 5, cex = 2)	
arrows(x0 = c(1 + xfac, 2 - xfac),
	y0 = vowel_pred$LB, y1 = vowel_pred$UB, angle = 90, code = 3, lwd = 2, length = 0.1)
points(x = c(1 + xfac, 2 - xfac), y = vowel_pred$Prominence,
	pch = 15, cex = 1.5)
arrows(x0 = c(3 + xfac, 4 - xfac),
	y0 = POS_pred$LB, y1 = POS_pred$UB, angle = 90, code = 3, lwd = 2, length = 0.1)
points(x = c(3 + xfac, 4 - xfac), y = POS_pred$Prominence,
	pch = 15, cex = 1.5)
arrows(x0 = c(5 + xfac, 6 - xfac),
	y0 = argument_pred$LB, y1 = argument_pred$UB, angle = 90, code = 3, lwd = 2, length = 0.1)
points(x = c(5 + xfac, 6 - xfac), y = argument_pred$Prominence,
	pch = 15, cex = 1.5)	
arrows(x0 = c(7 + xfac, 8 - xfac),
	y0 = focus_pred$LB, y1 = focus_pred$UB, angle = 90, code = 3, lwd = 2, length = 0.1)
points(x = c(7 + xfac, 8 - xfac), y = focus_pred$Prominence,
	pch = 15, cex = 1.5)
arrows(x0 = c(9 + xfac, 10 - xfac),
	y0 = accented_pred$LB, y1 = accented_pred$UB, angle = 90, code = 3, lwd = 2, length = 0.1)
points(x = c(9 + xfac, 10 - xfac), y = accented_pred$Prominence,
	pch = 15, cex = 1.5)
abline(h = c(0, 1), lty = 2)
box(lwd = 2)

## Make a plot for accent position and accent type:

quartz('', 11, 5.5)
par(mfrow = c(1,2), omi = c(1.5, 1.5, 0.5, 0.25), mai = rep(0.25, 4))
## First plot:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(0.5, 3.5), ylim = c(-0.1, 1.15))
text(x = -1.9, y = 1.1, labels = '(A)', font = 2, cex = 1.35)
axis(side = 1, at = 1:3, lwd = 2, lwd.ticks = 2, labels = F)
axis(side = 1, at = 1:3, tick = F, line = 0.2, font = 2, cex.axis = 1.5,
	labels = c('Prenuclear', 'Nuclear ip', 'Nuclear IP'), las = 2, adj = 0)
points(x = 1:3, y = position_pred$Prominence,
	pch = 15, cex = 1.5)
arrows(x0 = 1:3,
	y0 = position_pred$LB, y1 = position_pred$UB, angle = 90, code = 3, lwd = 2, length = 0.1)
axis(side = 2, at = seq(0, 1, 0.5), font = 2, las = 2,
	lwd = 2, lwd.ticks = 2,
	cex.axis = 1.5, labels = c('0%', '50%', '100%'))
mtext(side = 2, '%Prominent', font = 2, line = 5, cex = 2)	
mtext(side = 3, 'Accent Position', font = 2, line = 0.75, cex = 1.75)
box(lwd = 2)
## Second plot:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(0.5, 4.5), ylim = c(-0.1, 1.15))
axis(side = 1, at = 1:4, lwd = 2, lwd.ticks = 2, labels = F)
axis(side = 1, at = 1:4, tick = F, line = 0.2, font = 2, cex.axis = 1.5,
	labels = c('Low', 'Falling', 'High', 'Rising'), las = 2, adj = 0)
points(x = 1:4, y = type_pred$Prominence,
	pch = 15, cex = 1.5)
arrows(x0 = 1:4,
	y0 = type_pred$LB, y1 = type_pred$UB, angle = 90, code = 3, lwd = 2, length = 0.1)
text(x = -1.9, y = 1.1, labels = '(B)', font = 2, cex = 1.35)
mtext(side = 3, 'Accent Type', font = 2, line = 0.75, cex = 1.75)
box(lwd = 2)


