# PRC Data Challenge Submission

This repository contains the code used by **Team Amazing Forest** in their submission.

## What We Built

**Team Amazing Forest** used a Random Forest model to predict `tow` in the submission set, using a feature set consisting of:

- trajectory data summarized at a `flight_id` level, with fields quantifying information like: median altitude, time spent in various rate of climb stages, head and tailwinds, etc.
- summary statistics per `flight_id` from the flight list: information like `month_of_year`, `flown_distance`, `velocity`, etc.
- exogenous data pulled in to enrich the airports and airplane models: information such as `elevation_ft` for airports, `oew` and `range` for each plane model, etc.

Our modeling pipeline performed a series of transformations on the data prior to training the Random Forest, focusing specifically on:

- Creating log and squared transformations of continuous variables where non-linear effects might take place
- Bucketing key continuous variables (like `flown_distance`) into N group categorical features
- Generating derived features sourced from the base features: `flown_distance_over_great_circle_distance` is a good example, as is `relative_flown_velocity_aircraft_type`
- Creating interaction variables between all feature combinations of interest: `interaction_aircaft_type_flown_distance_bucket` being a great example

We trained our model a number of times - every time, the pipeline would dump the feature importances for us to analyze later, and serialize the random forest into an artifact that we could load in to make our final submission set predictions.

## Description of Files

Code to Pre-Process Trajectories:

- `batch_process_trajectories.R`: read in a single .parquet trajectory file and output a .csv at the `flight_id` level with summarized details
- `check_and_process.sh`: Bash script which checks to see all the .parquet files which have yet to be processed (ie. have a .csv output in the directory) and triggers the runs on outstanding files
- `combined_trajectories.Rmd`: knits together the directory of daily .csvs into a single dataframe

Modeling Code:

- `atow_predictions.Rmd`: loads all data, trains random forest model using `ranger` and `R`, outputs feature importances, and saves the model to a file
- `submissions.Rmd`: loads the model from a file, processes the data, and makes `tow` predictions for the submission set

Additional Data for Model:

- `aircraft_oew_mtow.csv`: exogenous dataset with `oew`, `mtow`, and `range` per `aircraft_type` to use as features
- `airports.csv`": exogenous dataset to get `lat`, `lng`, `elevation` per airport from `adep` and `ades`
- `aircraft_type_clusters_kmeans.csv`: materialized mapping of `aircraft_type` to a `cluster`, to reduce the dimensionality associated with plane types while capturing major differences
