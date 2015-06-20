Statistical analysis of rapid prosody description
=============

-	**Study design:** Stefan Baumann
-	**Data collection & preparation:** Stefan Baumann & Janina Kalbertodt
-	**Statistical Analysis:** Bodo Winter

## Libraries required for this analysis:

-	lme4
-	dplyr
-	DMwR
-	reshape2
-	party
-	ggplot2

## Script files contained in this analysis:

-	**001_RPT_individual_analysis_preprocessing.R**<br>
	The main preprocessing script, works on Excel files and outputs tidy csv files.
-	**002_mixed_model_analyses.R**<br>
	Computes mixed models (but does not interpret and visualize them). Warning: Takes a lot of time to run.
-	**003_random_forest_analyses.R**<br>
	Computes random forests and variable importances (but does not interpret and visualize them). Warning: Takes a lot of tim to run.
-	**004_random_forest_visualization.R**<br>
	Interprets and visualizes random forests.
-	**005_visualizations.R**<br>
	Interprets and visualizes mixed models and other analyses.

## Data files contained in this analysis:

-	**rpt-Daten-15juli2015.xls**
	Contains all summary data, that is, prominence score averages (overa all listeners) for each word.
-	**rpt_Einzelwerte-25juli2014-1.xls**
	Contains individual level data, that is, all prominence ratings from each listener (wide format)
-	**RPT_summary_processed.csv**
	The summary data, cleaned and in English.
-	**RPT_individual_processed.csv**
	The individual level data, cleaned (long format) and in English.
-	**CODEBOOK.md** description of all columns.
-	**listener_gender_info.csv** is needed to map genders onto listeners.
-	**speaker_gender_info.csv** is needed to map genders onto speakers.
-	**block_order_information.csv** is needed to map block orders (there were
	two block orders) for each participant.