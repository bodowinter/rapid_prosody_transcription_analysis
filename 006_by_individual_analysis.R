## Bodo Winter
## By-individual analyses
## June 22, 2015
## July 27, 2016: Finishing brushes and incorporation of spectral tilt
## July 27, 2016: Replaced ggplot2 with base graphs

##------------------------------------------------------------------
## Load in data and packages + preprocessing:
##------------------------------------------------------------------

## Load in libraries:

library(cluster)
library(pvclust)

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
	POS = coef(xmdl.POS)$Listener[, 2], 			# for content
	Focused = coef(xmdl.Focused)$Listener[, 2], 			# for focus particle
	Argument = coef(xmdl.argument)$Listener[, 2],			# for last argument
	SpectralEmphasis = coef(xmdl.SpectralEmphasis)$Listener[, 2],
	H1A2 = coef(xmdl.H1A2)$Listener[, 2],
	H1A3 = coef(xmdl.H1A3)$Listener[, 2],
	Accented = coef(xmdl.Accented)$Listener[, 2]
	)

## Add mean absolute change for the multi-level factors:

allsubs$AccentType <- rowMeans(abs(coef(xmdl.AccentType)$Listener[, 1:4]))
allsubs$AccentPosition <- rowMeans(abs(coef(xmdl.AccentPosition)$Listener[, c(1:3)]))


##------------------------------------------------------------------
## Correlations between random effects slopes:
##------------------------------------------------------------------

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



##------------------------------------------------------------------
## Cluster analysis following Levshina (2016, Ch. 5):
##------------------------------------------------------------------

## Create a hierarchical agglomerative cluster model:

slope.dist <- allsubs[, -1]
rownames(slope.dist) <- allsubs$Subject
slope.dist <- dist(slope.dist, method = 'euclidian')
slope.hc <- hclust(slope.dist, method = 'ward.D2')

## Test Silhouette width of different cluster solutions:

asw <- sapply(2:10,
	function(x) summary(silhouette(cutree(slope.hc, k = x), slope.dist))$avg.width)
asw		# three cluster solution is best

## Plot this with subject names:

quartz('', 11, 6)
plot(slope.hc, hang = -1)
rect.hclust(slope.hc, k = 3)

## Get cluster affiliations:

allsubs_clust <- cbind(allsubs,
	Cluster = cutree(slope.hc, k = 3))[, -1]
allsubs_clust <- split(allsubs_clust, allsubs_clust$Cluster)
clust_sum <- lapply(allsubs_clust, FUN = colMeans)
clust_sum <- as.data.frame(clust_sum)
clust_sum <- clust_sum[-nrow(clust_sum), ]
names(clust_sum) <- paste0('Cluster', 1:3)

## How do cluster 1 and cluster 2 differ?

diffs <- clust_sum$Cluster2 - clust_sum$Cluster1
names(diffs) <- rownames(clust_sum)
# cluster 2 pays more attention to: frequency, POS, focus particle and last argument
# much less to Accented

## Validate cluster solution:

this_df <- allsubs[, -1]
rownames(this_df) <- allsubs$Subject

set.seed(42)
slope.pvc <- pvclust(t(this_df),
	method.hclust = 'ward.D2', method.dist = 'euclidian')

## Visualize this with clusters that surpass a = 0.05:

quartz('', 11, 6)
plot(slope.pvc, hang = -1)
pvrect(slope.pvc, alpha = 0.95)




