## Bodo Winter
## May 29, 2014; June 19, 2015
## First analysis of rapid prosody transcription (RPT) experiment

########################################################################
######################## Preliminaries:
########################################################################

## Load in data:

mainDir <- "/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/data"
setwd(mainDir)
RPT <- read.csv("RPT_summary_processed.csv")

## Load in packages:

library(DMwR)
library(party)



########################################################################
######################## Imputation:
########################################################################

## The random forests analysis with "conditional = TRUE" ...
## ... (permutation test variable importance) require no missing values.
## We will create an additional dataset with imputed Mean and Max Pitch
## It's pointless to impute "RangeST" & "SlopeST" since there is not enough data.
## For this, we will do a subset analysis on only the accented words.

## We will create five data frames:
## (1) - central imputed
## (2) - knn imputed
## (3) - reduced data set (missing values excluded)
## (4) - subset of only those for which SlopeST and RangeST is not NA

## Only (4) can be used to test the relative ranking of SlopeST, RangeST

## How many missing values do we have anyway?

sum(is.na(RPT$MeanPitch))				# only 5
sum(is.na(RPT$MeanPitch))/nrow(RPT)		# less than 1% of the data
RPT[is.na(RPT$MeanPitch),]				# the data for which MeanPitch = NA is also MaxPitch = NA

## Impute the missing pitch values:

RPT_central <- RPT
RPT_central[,c("MeanPitch","MaxPitch")] <- centralImputation(RPT_central[,c("MeanPitch","MaxPitch")])

## K-nearest neighbor imputation on all the continuous variables:

continuous_variables <- c("NSyll","LogFreq","MeanPitch","MaxPitch",
	"RMS_amplitude","SyllableDur","VowelDur")
RPT_KNN <- RPT
RPT_KNN[,continuous_variables] <- knnImputation(RPT_KNN[,continuous_variables])

## Reduced dataset (without the 5 missing data points):

RPT_red <- RPT[complete.cases(RPT$MeanPitch),]

## Make a subset with those for which "RangeST" and "SlopeST" are not NA:

RPT_accented <- RPT[complete.cases(RPT$PitchRangeST),]

## What counts and percentage are those for which we have "RangeST" and "SlopeST"?

nrow(RPT_accented)					# 206 rows
nrow(RPT_accented)/nrow(RPT)		# 37% of the entire data set



########################################################################
######################## Random forests without "RangeST" and "SlopeST"
########################################################################

## Define "uber formula" that I'll use for those models, containing all variables:

the_uber_formula <- as.formula("p_score ~ VowelDur + SyllableDur + RMS_amplitude +
	LogFreq + MaxPitch + MeanPitch + POS + NSyll + 
	AccentPosition + AccentType + POS_class + Vowel + LastArgument + Focused")
accented_formula <- as.formula("p_score ~ VowelDur + SyllableDur + RMS_amplitude +
	LogFreq + MaxPitch + MeanPitch + POS + NSyll + PitchRangeST + PitchSlopeST + 
	AccentPosition + AccentType + POS_class + Vowel + LastArgument + Focused")

## Define controls:

data.controls <- cforest_unbiased(ntree=3000,
	mtry=round(sqrt(14)))	## 3,000 trees with 4 variables each (k = 14 parameters)
	## this is the same for the accented analysis with k = 16 parameters

## Run the forests:

set.seed(42)
forest_raw <- cforest(the_uber_formula,RPT,controls=data.controls)
set.seed(42)
forest_central <- cforest(the_uber_formula,RPT_central,controls=data.controls)
set.seed(42)
forest_KNN <- cforest(the_uber_formula,RPT_KNN,controls=data.controls)
set.seed(42)
forest_red <- cforest(the_uber_formula,RPT_red,controls=data.controls)
set.seed(42)
forest_accented <- cforest(accented_formula,RPT_accented,controls=data.controls)

## Create predictions:

raw_predictions <- predict(forest_raw)
central_predictions <- predict(forest_central)
KNN_predictions <- predict(forest_KNN)
red_predictions <- predict(forest_red)
accented_predictions <- predict(forest_accented)

## Create variable importances, has to be conditional = FALSE for the raw data:

set.seed(42)
raw_varimp <- varimp(forest_raw,conditional=FALSE)
set.seed(42)
central_varimp_conditional <- varimp(forest_central,conditional=TRUE)
set.seed(42)
KNN_varimp_conditional <- varimp(forest_KNN,conditional=TRUE)
set.seed(42)
red_varimp_conditional <- varimp(forest_red,conditional=TRUE)
set.seed(42)
accnted_varimp_conditional <- varimp(forest_accented,conditional=TRUE)

## Save all output:

save.image("RPT_random_forests_output.RData")


