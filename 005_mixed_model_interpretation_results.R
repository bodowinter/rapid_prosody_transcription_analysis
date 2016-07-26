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

load('../mixed_models.RData')


##------------------------------------------------------------------
## Analyses of mixed model results:
##------------------------------------------------------------------

## Get character vectors with all model names for full and null models:

all_models <- grep('xmdl', ls(), value = T)
all_nulls <- grep('Null', all_models, value = T)
all_models <- setdiff(all_nulls, all_models)

## Create an empty data frame to be filled with results from likelihood ratio tests:

LRTs <- data.frame(ChiSq = numeric(length(all_models)),
	Df = numeric(length(all_models)),
	Pval = numeric(length(all_models)))

## Loop through model names and append do data frame:

for (i in 1:length(all_models)) {
	this_full <- all_models[i]
	this_null <- all_nulls[i]
	this_LRT <- anova(this_null, this_full, test = 'Chisq')
	LRTs[i, ]$Pval <- this_LRT$'Pr(>Chisq)'
	}

anova(xmdl.Accented, xmdl.Accented.Null, test = 'Chisq')
anova(xmdl.AccentPosition, xmdl.AccentPosition.Null, test = 'Chisq')
anova(xmdl.AccentType, xmdl.AccentType.Null, test = 'Chisq')
anova(xmdl.MeanPitch, xmdl.MeanPitch.Null, test = 'Chisq')
anova(xmdl.MaxPitch, xmdl.MaxPitch.Null, test = 'Chisq')
anova(xmdl.RMS_amplitude, xmdl.RMS_amplitude.Null, test = 'Chisq')
anova(xmdl.SpectralEmphasis, xmdl.SpectralEmphasis.Null, test = 'Chisq')
anova(xmdl.H1A2, xmdl.H1A2.Null, test = 'Chisq')
anova(xmdl.H1A3, xmdl.H1A3.Null, test = 'Chisq')
anova(xmdl.VowelDur, xmdl.VowelDur.Null, test = 'Chisq')
anova(xmdl.SyllableDur, xmdl.SyllableDur.Null, test = 'Chisq')
anova(xmdl.NSyll, xmdl.NSyll.Null, test = 'Chisq')
anova(xmdl.RangeST, xmdl.RangeST.Null, test = 'Chisq')
anova(xmdl.SlopeST, xmdl.SlopeST.Null, test = 'Chisq')
anova(xmdl.Freq, xmdl.Freq.Null, test = 'Chisq')
anova(xmdl.Vowel, xmdl.Vowel.Null, test = 'Chisq')			# n.s
anova(xmdl.POS_class, xmdl.POS_class.Null, test = 'Chisq')
anova(xmdl.argument, xmdl.argument.Null, test = 'Chisq')
anova(xmdl.Focused, xmdl.Focused.Null, test = 'Chisq')

## Plot output and inspect models:

summary(xmdl.Accented)
summary(xmdl.AccentPosition)
summary(xmdl.AccentType)
summary(xmdl.MeanPitch)
summary(xmdl.MaxPitch)
summary(xmdl.RMS_amplitude)
summary(xmdl.SpectralEmphasis)
summary(xmdl.H1A2)
summary(xmdl.H1A3)
summary(xmdl.VowelDur)
summary(xmdl.SyllableDur)
summary(xmdl.NSyll)
summary(xmdl.RangeST)
summary(xmdl.SlopeST)
summary(xmdl.Freq)
summary(xmdl.Vowel)
summary(xmdl.POS_class)
summary(xmdl.argument)
summary(xmdl.Focused)

## Dunnsidak pvals:

dunnsidak <- function(P, N){1 - ((1 - P) ^ N)}
dunnsidak(anova(xmdl.Accented, xmdl.Accented.Null, test = 'Chisq')$'Pr(>Chisq)', 15)
dunnsidak(anova(xmdl.AccentPosition, xmdl.AccentPosition.Null, test = 'Chisq')$'Pr(>Chisq)', 15)	# n.s
dunnsidak(anova(xmdl.AccentType, xmdl.AccentType.Null, test = 'Chisq')$'Pr(>Chisq)', 15)			# n.s
dunnsidak(anova(xmdl.MeanPitch, xmdl.MeanPitch.Null, test = 'Chisq')$'Pr(>Chisq)', 15)
dunnsidak(anova(xmdl.MaxPitch, xmdl.MaxPitch.Null, test = 'Chisq')$'Pr(>Chisq)', 15)
dunnsidak(anova(xmdl.RMS_amplitude, xmdl.RMS_amplitude.Null, test = 'Chisq')$'Pr(>Chisq)', 15)
dunnsidak(anova(xmdl.SpectralEmphasis, xmdl.SpectralEmphasis.Null, test = 'Chisq')$'Pr(>Chisq)', 15)
dunnsidak(anova(xmdl.H1A2, xmdl.H1A2.Null, test = 'Chisq')$'Pr(>Chisq)', 15)
dunnsidak(anova(xmdl.H1A3, xmdl.H1A3.Null, test = 'Chisq')$'Pr(>Chisq)', 15)
dunnsidak(anova(xmdl.VowelDur, xmdl.VowelDur.Null, test = 'Chisq')$'Pr(>Chisq)', 15)
dunnsidak(anova(xmdl.SyllableDur, xmdl.SyllableDur.Null, test = 'Chisq')$'Pr(>Chisq)', 15)
dunnsidak(anova(xmdl.NSyll, xmdl.NSyll.Null, test = 'Chisq')$'Pr(>Chisq)', 15)
dunnsidak(anova(xmdl.RangeST, xmdl.RangeST.Null, test = 'Chisq')$'Pr(>Chisq)', 15)
dunnsidak(anova(xmdl.SlopeST, xmdl.SlopeST.Null, test = 'Chisq')$'Pr(>Chisq)', 15)			# n.s.
dunnsidak(anova(xmdl.Freq, xmdl.Freq.Null, test = 'Chisq')$'Pr(>Chisq)', 15)
dunnsidak(anova(xmdl.Vowel, xmdl.Vowel.Null, test = 'Chisq')$'Pr(>Chisq)', 15)		# n.s.
dunnsidak(anova(xmdl.POS_class, xmdl.POS_class.Null, test = 'Chisq')$'Pr(>Chisq)', 15)
dunnsidak(anova(xmdl.argument, xmdl.argument.Null, test = 'Chisq')$'Pr(>Chisq)', 15)
dunnsidak(anova(xmdl.Focused, xmdl.Focused.Null, test = 'Chisq')$'Pr(>Chisq)', 15)



##------------------------------------------------------------------
## Get predictions for mixed models and perform plotting:
##------------------------------------------------------------------

## Get prediction function (need to be connected to the web):

source('http://bodowinter.com/predict.glmm.R')




## Make a scatterplot of Pitch:

newdata <- data.frame(MaxPitch_z = rep(seq(-2, 2, 0.01), 2))
newdata$SpeakerGender_c <- as.factor(c(rep('M', nrow(newdata)/2), 
	rep('F', nrow(newdata)/2)))
contrasts(newdata$SpeakerGender_c) <- contr.sum(2)
newdata$Prominence <- 0
mm  =  model.matrix(terms(xmdl.MaxPitch), newdata)
newdata$Prominence  =  predict(xmdl.MaxPitch, newdata, re.form = NA)
pvar1  =  diag(mm %*% tcrossprod(vcov(xmdl.MaxPitch), mm))
newdata$UB  =  plogis(newdata$Prominence + 1.96*sqrt(pvar1))
newdata$LB  =  plogis(newdata$Prominence - 1.96*sqrt(pvar1))
newdata$Prominence  =  plogis(newdata$Prominence)

p <- ggplot(RPT, aes(MaxPitch_z, jitter(Prominence, factor = 0.15)*100)) + 
	geom_point(shape = 16, color = 'grey50', size = 4, alpha = 1/50) +
	coord_cartesian(ylim = c(-10, 110), xlim = c(-2, 2)) + 
	labs(y = 'Prominence', x = '\nMaximum Pitch (z-scored)') + 
	theme_minimal() + theme(axis.text.y = element_text(face = 'bold'), 
		axis.title.y = element_text(face = 'bold', size = 20), 
		axis.title.x = element_text(face = 'bold', size = 20), 
		axis.text.x = element_text(face = 'bold'), 
		axis.title.x = element_text(face = 'bold'), 
		strip.text.x  =  element_text(size = 20, face = 'bold', vjust = 2))

quartz('', 9, 5)
p + 
geom_ribbon(data  =  newdata[newdata$SpeakerGender_c =  = 'M', ], 
	aes(x = MaxPitch_z,  ymin = LB*100,  ymax = UB*100 ),  alpha = 0.4 ,  fill = '#dc7331') +
geom_line(data = newdata[newdata$SpeakerGender_c =  = 'M', ], 
	aes(x = MaxPitch_z, y = Prominence*100), color = 'black', size = 1.5)
ggsave('maxpitch.png')


## Make a scatterplot of Frequency:

newdata <- predict.logisticLMM(xmdl.Freq, -2, 2)

p <- ggplot(RPT, aes(LogFreq_z, jitter(Prominence, factor = 0.15)*100)) + 
	geom_point(shape = 16, color = 'grey50', size = 4, alpha = 1/50) +
	coord_cartesian(ylim = c(-10, 110), xlim = c(-2, 2)) + 
	labs(y = 'Prominence', x = '\nLogFrequency (z-scored)') + 
	theme_minimal() + theme(axis.text.y = element_text(face = 'bold'), 
		axis.title.y = element_text(face = 'bold', size = 20), 
		axis.title.x = element_text(face = 'bold', size = 20), 
		axis.text.x = element_text(face = 'bold'), 
		axis.title.x = element_text(face = 'bold'), 
		strip.text.x  =  element_text(size = 20, face = 'bold', vjust = 2))

quartz('', 9, 5)
p + 
geom_ribbon(data  =  newdata,  aes(x = LogFreq_z ,  ymin = LB*100,  ymax = UB*100 ),  alpha = 0.4 ,  fill = '#dc7331') +
geom_line(data = newdata, aes(x = LogFreq_z, y = Prominence*100), color = 'black', size = 1.5)
ggsave('plot15.png')

## Make a scatterplot of Amplitude:

newdata <- predict.logisticLMM(xmdl.RMS_amplitude, -2, 2)

p <- ggplot(RPT, aes(jitter(RMS_amplitude_z, factor = 0.25), jitter(Prominence, factor = 0.15)*100)) + 
	geom_point(shape = 16, color = 'grey50', size = 4, alpha = 1/50) +
	coord_cartesian(ylim = c(-10, 110), xlim = c(-2, 2)) + 
	labs(y = 'Prominence', x = '\nRMS Amplitude (z-scored)') + 
	theme_minimal() + theme(axis.text.y = element_text(face = 'bold'), 
		axis.title.y = element_text(face = 'bold', size = 20), 
		axis.title.x = element_text(face = 'bold', size = 20), 
		axis.text.x = element_text(face = 'bold'), 
		axis.title.x = element_text(face = 'bold'), 
		strip.text.x  =  element_text(size = 20, face = 'bold', vjust = 2))

quartz('', 9, 5)
p + 
geom_ribbon(data  =  newdata,  aes(x = RMS_amplitude_z,  ymin = LB*100,  ymax = UB*100 ),  alpha = 0.4 ,  fill = '#dc7331') +
geom_line(data = newdata, aes(x = RMS_amplitude_z, y = Prominence*100), color = 'black', size = 1.5)
ggsave('plot16.png')

## Make a scatterplot of Vowel Duration:

newdata <- predict.logisticLMM(xmdl.VowelDur, -2, 2)

p <- ggplot(RPT, aes(jitter(VowelDur_z, factor = 0.25), jitter(Prominence, factor = 0.15)*100)) + 
	geom_point(shape = 16, color = 'grey50', size = 4, alpha = 1/50) +
	coord_cartesian(ylim = c(-10, 110), xlim = c(-2, 2)) + 
	labs(y = 'Prominence', x = '\nVowel Duration (z-scored)') + 
	theme_minimal() + theme(axis.text.y = element_text(face = 'bold'), 
		axis.title.y = element_text(face = 'bold', size = 20), 
		axis.title.x = element_text(face = 'bold', size = 20), 
		axis.text.x = element_text(face = 'bold'), 
		axis.title.x = element_text(face = 'bold'), 
		strip.text.x  =  element_text(size = 20, face = 'bold', vjust = 2))

quartz('', 9, 5)
p + 
geom_ribbon(data  =  newdata,  aes(x = VowelDur_z,  ymin = LB*100,  ymax = UB*100 ),  alpha = 0.4 ,  fill = '#dc7331') +
geom_line(data = newdata, aes(x = VowelDur_z, y = Prominence*100), color = 'black', size = 1.5)
ggsave('plot17.png')

## Make a scatterplot of Syllable Duration:

newdata <- predict.logisticLMM(xmdl.SyllableDur, -2, 2)

p <- ggplot(RPT, aes(jitter(SyllableDur_z, factor = 0.25), jitter(Prominence, factor = 0.15)*100)) + 
	geom_point(shape = 16, color = 'grey50', size = 4, alpha = 1/50) +
	coord_cartesian(ylim = c(-10, 110), xlim = c(-2, 2)) + 
	labs(y = 'Prominence', x = '\nSyllable Duration (z-scored)') + 
	theme_minimal() + theme(axis.text.y = element_text(face = 'bold'), 
		axis.title.y = element_text(face = 'bold', size = 20), 
		axis.title.x = element_text(face = 'bold', size = 20), 
		axis.text.x = element_text(face = 'bold'), 
		axis.title.x = element_text(face = 'bold'), 
		strip.text.x  =  element_text(size = 20, face = 'bold', vjust = 2))

quartz('', 9, 5)
p + 
geom_ribbon(data  =  newdata,  aes(x = SyllableDur_z,  ymin = LB*100,  ymax = UB*100 ),  alpha = 0.4 ,  fill = '#dc7331') +
geom_line(data = newdata, aes(x = SyllableDur_z, y = Prominence*100), color = 'black', size = 1.5)
ggsave('plot18.png')

## Make a scatterplot of Range:

newdata <- predict.logisticLMM(xmdl.RangeST, -2, 2)

p <- ggplot(RPT, aes(jitter(PitchRangeST_abs_z, factor = 0.25), jitter(Prominence, factor = 0.15)*100)) + 
	geom_point(shape = 16, color = 'grey50', size = 4, alpha = 1/50) +
	coord_cartesian(ylim = c(-10, 110), xlim = c(-2, 2)) + 
	labs(y = 'Prominence', x = '\nAbsolute Pitch Range (z-scored)') + 
	theme_minimal() + theme(axis.text.y = element_text(face = 'bold'), 
		axis.title.y = element_text(face = 'bold', size = 20), 
		axis.title.x = element_text(face = 'bold', size = 20), 
		axis.text.x = element_text(face = 'bold'), 
		axis.title.x = element_text(face = 'bold'), 
		strip.text.x  =  element_text(size = 20, face = 'bold', vjust = 2))

quartz('', 9, 5)
p + 
geom_ribbon(data  =  newdata,  aes(x = PitchRangeST_abs_z,  ymin = LB*100,  ymax = UB*100 ),  alpha = 0.4 ,  fill = '#dc7331') +
geom_line(data = newdata, aes(x = PitchRangeST_abs_z, y = Prominence*100), color = 'black', size = 1.5)
ggsave('plot19.png')

## Make a scatterplot of Slope:

newdata <- predict.logisticLMM(xmdl.SlopeST, -2, 2)

p <- ggplot(RPT, aes(jitter(PitchSlopeST_abs_z, factor = 0.25), jitter(Prominence, factor = 0.15)*100)) + 
	geom_point(shape = 16, color = 'grey50', size = 4, alpha = 1/50) +
	coord_cartesian(ylim = c(-10, 110), xlim = c(-2, 2)) + 
	labs(y = 'Prominence', x = '\nAbsolute Pitch Slope (z-scored)') + 
	theme_minimal() + theme(axis.text.y = element_text(face = 'bold'), 
		axis.title.y = element_text(face = 'bold', size = 20), 
		axis.title.x = element_text(face = 'bold', size = 20), 
		axis.text.x = element_text(face = 'bold'), 
		axis.title.x = element_text(face = 'bold'), 
		strip.text.x  =  element_text(size = 20, face = 'bold', vjust = 2))

quartz('', 9, 5)
p + 
geom_ribbon(data  =  newdata,  aes(x = PitchSlopeST_abs_z,  ymin = LB*100,  ymax = UB*100 ),  alpha = 0.4 ,  fill = '#dc7331') +
geom_line(data = newdata, aes(x = PitchSlopeST_abs_z, y = Prominence*100), color = 'black', size = 1.5)
ggsave('plot20.png')




