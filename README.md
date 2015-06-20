Statistical analysis of rapid prosody description
=============

*Study design:* Stefan Baumann
*Data collection & preparation:* Stefan Baumann & Janina Kalbertodt
*Statistical Analysis:* Bodo Winter

## Libraries required for this analysis:

- lme4
- dplyr
- DMwR
- reshape2
- party
- ggplot2

## Script files contained in this analysis:

-	001_RPT_individual_analysis_preprocessing.R
	The main preprocessing script, works on Excel files and outputs tidy csv files.
-	002_mixed_model_analyses.R
	Computes mixed models (but does not interpret and visualize them). Warning: Takes a lot of time to run.
-	003_random_forest_analyses.R
	Computes random forests and variable importances (but does not interpret and visualize them). Warning: Takes a lot of tim to run.
-	004_random_forest_visualization.R
	Interprets and visualizes random forests.
-	005_visualizations.R
	Interprets and visualizes mixed models and other analyses.



