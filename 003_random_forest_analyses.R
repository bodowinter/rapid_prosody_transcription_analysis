## Bodo Winter
## May 29,  2014; June 19,  2015
## July 26,  2016: Finishing brushes and incorporation of spectral tilt
## First analysis of rapid prosody transcription (RPT) experiment

##------------------------------------------------------------------
## Load in data and packages:
##------------------------------------------------------------------

## Load in data:

mainDir <- '/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/data'
setwd(mainDir)
RPT <- read.csv('RPT_summary_processed.csv')

## Load in packages:

library(DMwR)
library(party)
library(dplyr)



##------------------------------------------------------------------
## Handling missing data:
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

sum(is.na(RPT$MeanPitch))				# only 5
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

RPT_red <- filter(RPT, complete.cases(MeanPitch))
RPT_red <- filter(RPT_red, complete.cases(H1A2))



##------------------------------------------------------------------
## Random forests without 'RangeST' and 'SlopeST':
##------------------------------------------------------------------

## Define 'uber formula' that I'll use for those models,  containing all variables:

the_uber_formula <- as.formula('PScore ~ VowelDur + SyllableDur + RMS_amplitude +
	LogFreq + MaxPitch + MeanPitch + NSyll + 
	SpectralEmphasis + H1A2 + H1A3 +
	AccentPosition + AccentType + POS_class + Vowel +
	LastArgument + Focused + Accented + LogFreq')

## Define controls:

data.controls <- cforest_unbiased(ntree = 2000, 
	mtry = round(sqrt(18)))	## 2,000 trees with 4 variables each (k  =  18 parameters)

## Run the forests, first, the raw one:

set.seed(42)
forest_raw <- cforest(the_uber_formula, RPT, controls = data.controls)
raw_predictions <- predict(forest_raw)
raw_varimp <- varimp(forest_raw, conditional = FALSE)		# conditional  =  F coz of missing values
save(forest_raw, raw_predictions, raw_varimp, file = 'raw_forests.RData')

## Now remove the old baggage for memory space:

rm(forest_raw, raw_predictions, raw_varimp)

print(Sys.time())

data.controls <- cforest_unbiased(ntree = 1500, 	# for computational feasability
	mtry = round(sqrt(18)))	## 2,000 trees with 4 variables each (k  =  18 parameters)

## Run the forest on central imputed data:

set.seed(42)
forest_central <- cforest(the_uber_formula, RPT_central, controls = data.controls)
central_predictions <- predict(forest_central)
central_varimp_conditional <- varimp(forest_central, conditional = T)
save(forest_central, central_predictions, central_varimp_conditional, file = 'central_forests.RData')

print(Sys.time())

## Remove old baggage to gain memory:

rm(forest_central, central_predictions, central_varimp_conditional)

## Run the forest on KNN imputed data:

set.seed(42)
forest_KNN <- cforest(the_uber_formula, RPT_KNN, controls = data.controls)
KNN_predictions <- predict(forest_KNN)
KNN_varimp_conditional <- varimp(forest_KNN, conditional = T)
save(forest_KNN, KNN_predictions, KNN_varimp_conditional, file = 'KNN_forests.RData')

print(Sys.time())

## Remove old baggage to gain memory:

rm(forest_KNN, KNN_predictions, KNN_varimp_conditional)

## The forest on the reduced dataset (missing values excluded):

set.seed(42)
forest_red <- cforest(the_uber_formula, RPT_red, controls = data.controls)
red_predictions <- predict(forest_red)
red_varimp_conditional <- varimp(forest_red, conditional = T)
save(forest_red, red_predictions, red_varimp_conditional, file = 'reduced_forests.RData')

print(Sys.time())

## Remove old baggage to gain memory:

rm(forest_red, red_predictions, red_varimp_conditional)


