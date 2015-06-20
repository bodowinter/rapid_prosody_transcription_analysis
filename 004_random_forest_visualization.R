## June 19, 2015
## Bodo Winter
## Analysis of random forest output

## Load random forest output:

mainDir <- "/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/data"
setwd(mainDir)
load("RPT_random_forests_output.RData")

## Compute prediction accuracy:

## ...

## Which one works best?

cor(raw_predictions,RPT$p_score)
cor(central_predictions,RPT$p_score)
cor(KNN_predictions,RPT$p_score)
cor(red_predictions,RPT_red$p_score)
cor(accented_predictions,RPT_accented$p_score)

## Write a function for plotting variable importances:

## ...

## Plot variable importances ...







