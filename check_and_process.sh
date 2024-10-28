#!/bin/bash

# Define directories
TRAJECTORIES_DIR="/home/conor/atow_predictions/trajectories"
PROCESSED_DIR="/home/conor/atow_predictions/processed_trajectories_v5"
R_SCRIPT_PATH="/home/conor/atow_predictions/batch_process_trajectories.R"

# Function to calculate the day of the year from YYYY-MM-DD
get_day_of_year() {
  date -d "$1" +%j
}

# Loop through all .parquet files in the trajectories directory
for parquet_file in "$TRAJECTORIES_DIR"/*.parquet
do
  # Extract the base filename (without extension)
  filename=$(basename -- "$parquet_file" .parquet)
  
  # Full path to the expected CSV file
  expected_csv="$PROCESSED_DIR/$filename.csv"
  
  # Print the filenames being checked for debugging
  echo "Checking if $expected_csv exists..."
  
  # Check if the corresponding .csv file exists in the processed directory
  if [ ! -f "$expected_csv" ]; then
    echo "$filename.csv is missing, processing $filename.parquet"
    
    # Calculate the day of the year from the filename (assumes format YYYY-MM-DD)
    day_of_year=$(get_day_of_year "$filename")
    
    # Run the R script with the day of the year as the parameter
    Rscript "$R_SCRIPT_PATH" "$day_of_year"
  else
    echo "$filename.csv already exists, skipping"
  fi
done
