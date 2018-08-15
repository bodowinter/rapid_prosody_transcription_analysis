## Bodo Winter
## May 29,  2014; June 19,  2015
## July 26,  2016: Finishing brushes and incorporation of spectral tilt
## First analysis of rapid prosody transcription (RPT) experiment

##------------------------------------------------------------------
## Load in data and packages:
##------------------------------------------------------------------

## Load in data:

mainDir <- '/Users/winterb/Research/rapid_prosody_transcription/analysis/data/'
setwd(mainDir)
RPT <- read.csv('RPT_summary_processed.csv')

## Load in packages:

library(DMwR)
# library(party)
library(ranger)
library(dplyr)

## Create absolute value of pitch range and slope:

RPT <- mutate(RPT,
	PitchRangeST_abs = abs(PitchRangeST),
	PitchSlopeST_abs = abs(PitchSlopeST))

## Create accented versus not accented variable:

RPT$Accented <- factor(ifelse(RPT$AccentType == 'no_accent', 'no', 'yes'))





##------------------------------------------------------------------
## Impute missing data in columns MaxPitch, MeanPitch, SpectralEmphasis, H1A2, H1A3:
##------------------------------------------------------------------

## The random forests analysis with 'conditional  =  TRUE' ...
## ... (permutation test variable importance) require no missing values.
## We will create an additional dataset with imputed Mean and Max Pitch
## It's pointless to impute 'RangeST' & 'SlopeST' since there is not enough data.
## For this,  we will do a subset analysis on only the accented words.

## We will create three data frames, on top of the already existing data frame that has NAs:
## (1) - central imputed
## (2) - knn imputed
## (3) - reduced data set (missing values excluded)

## How many missing values do we have anyway?

sum(is.na(RPT$MeanPitch))				# only 1
sum(is.na(RPT$MeanPitch)) / nrow(RPT)	# less than 1% of the data
# the data for which MeanPitch  =  NA is also MaxPitch  =  NA

## Create an accented variable:

RPT$Accented <- factor(ifelse(RPT$AccentType == 'no_accent', 'no', 'yes'))

## Impute the missing pitch values:

RPT_central <- RPT	# create copy
these_columns <- c('MeanPitch', 'MaxPitch', 'SpectralEmphasis', 'H1A2', 'H1A3')
RPT_central[, these_columns] <- centralImputation(RPT_central[, these_columns])

## K-nearest neighbor imputation on all the continuous variables:

continuous_variables <- c('NSyll', 'LogFreq', 'MeanPitch', 'MaxPitch', 
	'RMS_amplitude', 'SyllableDur', 'VowelDur', 'SpectralEmphasis', 'H1A2', 'H1A3')
RPT_KNN <- RPT	# create copy
RPT_KNN[, continuous_variables] <- knnImputation(RPT_KNN[, continuous_variables])

## Reduced dataset (without the 5 missing data points):

RPT_red <- filter(RPT,
	complete.cases(MeanPitch, MaxPitch, H1A2, SpectralEmphasis))



##------------------------------------------------------------------
## Correlations among cues:
##------------------------------------------------------------------

cor(RPT[, continuous_variables], use = 'complete.obs')


##------------------------------------------------------------------
## Random forests with new data (full analysis):
##------------------------------------------------------------------

## Create formula:

myFormula <- as.formula('PScore ~ VowelDur + SyllableDur + RMS_amplitude +
	LogFreq + MaxPitch + MeanPitch + NSyll + 
	SpectralEmphasis + H1A2 + H1A3 +
	AccentPosition + AccentType + POS_class + 
	LastArgument + Focused + Accented + LogFreq')

## Set the seed value:

set.seed(42)

## Run the forest:

##------
## Addendum August 15, 2018:
## Using ranger means that variable importances are not
## conditional variable importances;
## however, similar results are obtained when using the party package
##------

myforest_KNN <- ranger(myFormula, data = RPT_KNN,
	importance = 'permutation', mtry = 4, num.trees = 2000)
myforest_red <- ranger(myFormula, data = RPT_red,
	importance = 'permutation', mtry = 4, num.trees = 2000)
myforest_central <- ranger(myFormula, data = RPT_central,
	importance = 'permutation', mtry = 4, num.trees = 2000)

## Check R-squared:

myforest_KNN$r.squared
myforest_red$r.squared
myforest_central$r.squared

## Check variable importances:

sort(myforest_KNN$variable.importance)
sort(myforest_red$variable.importance)
sort(myforest_central$variable.importance)


##------------------------------------------------------------------
## Random forests with proper 70% random set:
##------------------------------------------------------------------

## Create random subsets:

set.seed(42)
KNN_ids <- sample(1:nrow(RPT_KNN),
	round(0.7 * nrow(RPT_KNN)))
red_ids <- sample(1:nrow(RPT_red),
	round(0.7 * nrow(RPT_red)))
central_ids <- sample(1:nrow(RPT_central),
	round(0.7 * nrow(RPT_central)))

## Run the forests on the training sets:

myforest_KNN_train <- ranger(myFormula, data = RPT_KNN[KNN_ids, ],
	importance = 'permutation', mtry = 4, num.trees = 2000)
myforest_red_train <- ranger(myFormula, data = RPT_red[red_ids, ],
	importance = 'permutation', mtry = 4, num.trees = 2000)
myforest_central_train <- ranger(myFormula, data = RPT_central[central_ids, ],
	importance = 'permutation', mtry = 4, num.trees = 2000)

## Get predictions for unseen data:

myforest_KNN_preds <- predict(myforest_KNN_train,
	data = RPT_KNN[-KNN_ids, ])
myforest_red_preds <- predict(myforest_red_train,
	data = RPT_red[-red_ids, ])
myforest_central_preds <- predict(myforest_central_train,
	data = RPT_central[-central_ids, ])

## Get correlation values and R-squareds:

summary(lm(scale(myforest_KNN_preds$predictions) ~
	-1 + scale(RPT_KNN[-KNN_ids, ]$PScore)))
summary(lm(scale(myforest_red_preds$predictions) ~
	-1 + scale(RPT_red[-red_ids, ]$PScore)))
summary(lm(scale(myforest_central_preds$predictions) ~
	-1 + scale(RPT_central[-central_ids, ]$PScore)))




##------------------------------------------------------------------
## Plot variable importances:
##------------------------------------------------------------------

## Sort variable importances:

raw_varimp <- sort(myforest_central$variable.importance)

## Change names of variable importances for display purposes:

names(raw_varimp)[names(raw_varimp) == 'Focused'] <- 'Focus Particle'
names(raw_varimp)[names(raw_varimp) == 'LastArgument'] <- 'Last Argument'
names(raw_varimp)[names(raw_varimp) == 'H1A2'] <- 'Spectral Tilt (H1-A2)'
names(raw_varimp)[names(raw_varimp) == 'H1A3'] <- 'Spectral Tilt (H1-A3)'
names(raw_varimp)[names(raw_varimp) == 'NSyll'] <- 'Number of Syllables'
names(raw_varimp)[names(raw_varimp) == 'SpectralEmphasis'] <- 'Spectral Emphasis'
names(raw_varimp)[names(raw_varimp) == 'SyllableDur'] <- 'Syllable Duration'
names(raw_varimp)[names(raw_varimp) == 'VowelDur'] <- 'Vowel Duration'
names(raw_varimp)[names(raw_varimp) == 'LogFreq'] <- 'Log Frequency'
names(raw_varimp)[names(raw_varimp) == 'RMS_amplitude'] <- 'RMS Amplitude'
names(raw_varimp)[names(raw_varimp) == 'MeanPitch'] <- 'Mean F0'
names(raw_varimp)[names(raw_varimp) == 'MaxPitch'] <- 'Max F0'
names(raw_varimp)[names(raw_varimp) == 'AccentType'] <- 'Accent Type'
names(raw_varimp)[names(raw_varimp) == 'AccentPosition'] <- 'Accent Position'
names(raw_varimp)[names(raw_varimp) == 'POS_class'] <- 'Part-Of-Speech'

## Plot raw variable importance:

quartz('', 9, 6)
par(mai = c(1, 2.5, 0.05, 0.5))
plot(1, 1, type = 'n', bty = 'n',
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n',
	xlim = c(-10, 350), ylim = c(0.25, 18.25))
abline(h = 1:length(raw_varimp), col = 'darkgrey')
points(raw_varimp, 1:length(raw_varimp), pch = 19, cex = 1.5)
axis(side = 1, at = seq(0, 350, 50),
	font = 2, cex.axis = 1.25, lwd = 2, lwd.ticks = 2)
axis(side = 2, at = 1:length(raw_varimp),
	las = 2, font = 2, tick = F, cex.axis = 1.15, labels = names(raw_varimp),
	line = -0.5)
segments(x0 = 0, y0 = 0, y1 = 17.5, lwd = 2, lty = 2)
mtext(side = 1, 'Variable Importance', line = 3.25, cex = 2, font = 2)




## -----------------------------------------------------------------------------

## OLD RANDOM FOREST ANALYSIS:
## INCLUDES PITCH RANGE & SLOPE & VOWEL + A DIFFERENT PACKAGE (SLOWER):

# ##------------------------------------------------------------------
# ## Random forests without 'RangeST' and 'SlopeST':
# ##------------------------------------------------------------------

# ## Define 'uber formula' that I'll use for those models,  containing all variables:

# the_uber_formula <- as.formula('PScore ~ VowelDur + SyllableDur + RMS_amplitude +
	# LogFreq + MaxPitch + MeanPitch + NSyll + 
	# SpectralEmphasis + H1A2 + H1A3 +
	# AccentPosition + AccentType + POS_class + Vowel +
	# LastArgument + Focused + Accented + LogFreq')

# ## Define controls:

# data.controls <- cforest_unbiased(ntree = 2000, 
	# mtry = round(sqrt(18)))	## 2,000 trees with 4 variables each (k  =  18 parameters)

# ## Run the forests, first, the raw one:

# set.seed(42)
# forest_raw <- cforest(the_uber_formula, RPT, controls = data.controls)
# raw_predictions <- predict(forest_raw)
# raw_varimp <- varimp(forest_raw, conditional = FALSE)		# conditional  =  F coz of missing values
# save(forest_raw, raw_predictions, raw_varimp, file = 'raw_forests.RData')

# ## Now remove the old baggage for memory space:

# rm(forest_raw, raw_predictions, raw_varimp)

# print(Sys.time())

# data.controls <- cforest_unbiased(ntree = 1500, 	# for computational feasability
	# mtry = round(sqrt(18)))	## 2,000 trees with 4 variables each (k  =  18 parameters)

# ## Run the forest on central imputed data:

# set.seed(42)
# forest_central <- cforest(the_uber_formula, RPT_central, controls = data.controls)
# central_predictions <- predict(forest_central)
# central_varimp_conditional <- varimp(forest_central, conditional = T)
# save(forest_central, central_predictions, central_varimp_conditional, file = 'central_forests.RData')

# print(Sys.time())

# ## Remove old baggage to gain memory:

# rm(forest_central, central_predictions, central_varimp_conditional)

# ## Run the forest on KNN imputed data:

# set.seed(42)
# forest_KNN <- cforest(the_uber_formula, RPT_KNN, controls = data.controls)
# KNN_predictions <- predict(forest_KNN)
# KNN_varimp_conditional <- varimp(forest_KNN, conditional = T)
# save(forest_KNN, KNN_predictions, KNN_varimp_conditional, file = 'KNN_forests.RData')

# print(Sys.time())

# ## Remove old baggage to gain memory:

# rm(forest_KNN, KNN_predictions, KNN_varimp_conditional)

# ## The forest on the reduced dataset (missing values excluded):

# set.seed(42)
# forest_red <- cforest(the_uber_formula, RPT_red, controls = data.controls)
# red_predictions <- predict(forest_red)
# red_varimp_conditional <- varimp(forest_red, conditional = T)
# save(forest_red, red_predictions, red_varimp_conditional, file = 'reduced_forests.RData')

# print(Sys.time())

# ## Remove old baggage to gain memory:

# rm(forest_red, red_predictions, red_varimp_conditional)


