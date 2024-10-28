library(arrow)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(e1071)


# Haversine formula function
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Haversine formula
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  r <- 6371  # Radius of Earth in kilometers
  distance <- r * c
  
  return(distance)  # Return distance in KM
}

# https://sgichuki.github.io/Atmo/
windDir <-function(u,v){
  (270-atan2(u,v)*180/pi)%%360 
}

windSpd <-function(u,v){
  sqrt(u^2+v^2)
}

# Function to process each file
process_file <- function(file_path) {
  # Open the dataset
  trajectory_df <- open_dataset(file_path)
  
  # Select only necessary columns to reduce memory load
  selected_data <- trajectory_df %>%
    select(flight_id, timestamp, groundspeed, altitude, latitude, longitude, vertical_rate, u_component_of_wind, v_component_of_wind, track, specific_humidity) %>%
    collect()  # Pull the data into R memory
  
  # Define speed and altitude bins after collecting
  vertical_rate_bins <- c(-Inf, -2000, -1000, -300, 300, 1000, 2000, Inf)  # Define bins for descent, level, and climb stages
  
  # Process and summarize the data in R
  time_spent_data <- selected_data %>%
    arrange(timestamp) %>%
    group_by(flight_id) %>%
    mutate(
      climb_stage = cut(vertical_rate, breaks = vertical_rate_bins, labels = FALSE),
      time_diff = as.numeric(difftime(lead(timestamp), timestamp, units = "secs"))
    ) %>%
    filter(!is.na(time_diff)) %>%
    group_by(flight_id, climb_stage) %>%
    summarise(total_time = sum(time_diff, na.rm = TRUE)) %>%
    ungroup()
  
  # Calculate total time for each flight
  total_time_per_flight <- time_spent_data %>%
    group_by(flight_id) %>%
    summarise(total_time_flight = sum(total_time, na.rm = TRUE))
  
  # Merge the total time back into the original dataset
  climb_time_spent <- time_spent_data %>%
    left_join(total_time_per_flight, by = "flight_id") %>%
    mutate(percentage_time = (total_time / total_time_flight) * 100)
  
  # Pivot to get time spent in each speed-altitude-climb category as columns
  time_spent_wide <- climb_time_spent %>%
    mutate(climb_category = paste0("climb_", climb_stage)) %>%
    select(flight_id, climb_category, total_time, percentage_time) %>%
    pivot_wider(
      names_from = climb_category, 
      values_from = c(total_time, percentage_time), 
      values_fill = 0
    )
  
  # Create a numeric timestamp variable
  selected_data$timestamp_numeric <- as.numeric(as.POSIXct(selected_data$timestamp))
  
  # Calculate wind speed and direction
  selected_data$wind_speed <- sqrt(selected_data$u_component_of_wind^2 + selected_data$v_component_of_wind^2)
  selected_data$wind_direction <- atan2(selected_data$v_component_of_wind, selected_data$u_component_of_wind) * (180 / pi)
  selected_data$wind_direction <- ifelse(selected_data$wind_direction < 0, selected_data$wind_direction + 360, selected_data$wind_direction)
  # Calculate relative wind angle (track vs wind)
  selected_data$relative_wind_angle <- (selected_data$track - selected_data$wind_direction + 180) %% 360 - 180
  # Calculate headwind/tailwind
  selected_data$headwind_tailwind <- selected_data$wind_speed * cos(selected_data$relative_wind_angle * (pi / 180))
  
  # Additional summary metrics per flight_id
  additional_metrics <- selected_data %>%
    group_by(flight_id) %>%
    summarise(
      count_trajectory_observations = n(),
      # Distance
      great_circle_distance = haversine_distance(first(latitude), first(longitude), last(latitude), last(longitude)),
      median_track = median(track, na.rm = TRUE),
      # Altitude    
      max_altitude = quantile(altitude, probs = 0.99, na.rm = TRUE),
      median_altitude = median(altitude, na.rm = TRUE),
      p25_altitude = quantile(altitude, probs = 0.25, na.rm = TRUE),
      p75_altitude = quantile(altitude, probs = 0.75, na.rm = TRUE),
      iqr_altitude = IQR(altitude, na.rm = TRUE),
      sd_altitude = sd(altitude, na.rm = TRUE),
      # Groundspeed
      max_groundspeed = quantile(groundspeed, probs = 0.99, na.rm = TRUE),
      median_groundspeed = median(groundspeed, na.rm = TRUE),
      p25_groundspeed = quantile(groundspeed, probs = 0.25, na.rm = TRUE),
      p75_groundspeed = quantile(groundspeed, probs = 0.75, na.rm = TRUE),
      iqr_groundspeed = IQR(groundspeed, na.rm = TRUE),
      sd_groundspeed = sd(groundspeed, na.rm = TRUE),
      # Wind
      avg_headwind_tailwind = mean(headwind_tailwind, na.rm = TRUE),
      # Ascent Features
      max_vertical_rate = max(vertical_rate[vertical_rate > 0], na.rm = TRUE),   # Maximum climb rate
      avg_positive_vertical_rate = mean(vertical_rate[vertical_rate > 0], na.rm = TRUE),  # Average climb rate
      total_time_ascent = sum(diff(timestamp[vertical_rate > 300]), na.rm = TRUE), # Total time in ascent
      cumulative_ascent = sum(vertical_rate[vertical_rate > 300] * diff(timestamp_numeric[vertical_rate > 300]), na.rm = TRUE),
      # Descent Features
      min_vertical_rate = max(vertical_rate[vertical_rate < 0], na.rm = TRUE),   # Maximum descent rate
      avg_negative_vertical_rate = mean(vertical_rate[vertical_rate < 0], na.rm = TRUE),  # Average descent rate
      total_time_descent = sum(diff(timestamp[vertical_rate < 300]), na.rm = TRUE), # Total time in ascent
      cumulative_descent = sum(vertical_rate[vertical_rate < 300] * diff(timestamp_numeric[vertical_rate < 300]), na.rm = TRUE),
      # other
      avg_humidity = mean(specific_humidity, na.rm = TRUE),
      # Skew
      altitude_skewness = skewness(altitude, na.rm = TRUE),
      altitude_kurtosis = kurtosis(altitude, na.rm = TRUE),
      groundspeed_skewness = skewness(groundspeed, na.rm = TRUE),
      groundspeed_kurtosis = kurtosis(groundspeed, na.rm = TRUE)
    )
  
  # Define flight phases by altitude change and vertical rate
  selected_data <- selected_data %>%
    mutate(
      phase = case_when(
        altitude < 10000 & vertical_rate > 300 ~ "climb",
        altitude > 10000 & vertical_rate < 300 & vertical_rate > -300 ~ "cruise",
        altitude < 10000 & vertical_rate < -300 ~ "descent",
        TRUE ~ "other"
      )
    )
  
  # Summarise statistics for each phase
  phase_summary <- selected_data %>%
    group_by(flight_id, phase) %>%
    summarise(
      avg_altitude = mean(altitude, na.rm = TRUE),
      avg_groundspeed = mean(groundspeed, na.rm = TRUE),
      total_time_phase = sum(as.numeric(difftime(lead(timestamp), timestamp, units = "secs")), na.rm = TRUE)
    ) %>%
    pivot_wider(
      names_from = phase,
      values_from = c(avg_altitude, avg_groundspeed, total_time_phase),
      values_fill = 0
    )
  
  # Calculate rate of change of groundspeed
  selected_data <- selected_data %>%
    arrange(timestamp) %>%
    group_by(flight_id) %>%
    mutate(
      speed_diff = lead(groundspeed) - groundspeed,
      time_diff = as.numeric(difftime(lead(timestamp), timestamp, units = "secs")),
      acceleration = speed_diff / time_diff
    )
  
  # Summarize max/min acceleration and deceleration
  acceleration_summary <- selected_data %>%
    group_by(flight_id) %>%
    summarise(
      max_acceleration = max(acceleration, na.rm = TRUE),
      min_acceleration = min(acceleration, na.rm = TRUE)
    )
  
  # Calculate altitude IQR for each phase
  altitude_iqr_per_phase <- selected_data %>%
    group_by(flight_id, phase) %>%
    summarise(altitude_iqr = IQR(altitude, na.rm = TRUE)) %>%
    pivot_wider(
      names_from = phase,
      values_from = altitude_iqr,
      values_fill = 0,
      names_prefix = "altitude_iqr_"
    )
  
  # Calculate average headwind/tailwind and crosswind during climb and descent
  wind_phase_summary <- selected_data %>%
    filter(phase %in% c("climb", "descent")) %>%
    group_by(flight_id, phase) %>%
    summarise(
      avg_headwind_tailwind = mean(headwind_tailwind, na.rm = TRUE),
      avg_crosswind = mean(wind_speed * sin(relative_wind_angle * (pi / 180)), na.rm = TRUE)
    ) %>%
    pivot_wider(
      names_from = phase,
      values_from = c(avg_headwind_tailwind, avg_crosswind),
      values_fill = 0
    )
  
  # Fit a spline for altitude vs time for each flight
  # Extract features from the spline (e.g., curvature, inflection points)
  # spline_features <- selected_data %>%
  #   group_by(flight_id) %>%
  #   do(spline_model = smooth.spline(.$timestamp_numeric, .$altitude, spar = 0.5)) %>%
  #   reframe(
  #     flight_id,
  #     curvature = sum(abs(diff(spline_model$y, differences = 2))),  # Approx curvature
  #     num_inflection_points = sum(diff(sign(diff(spline_model$y))) != 0)  # Inflection points
  #   )
  
  # # Apply Fourier Transform to altitude vs time
  # # Extract the dominant frequency component
  # fourier_features <- selected_data %>%
  #   group_by(flight_id) %>%
  #   summarise(
  #     fft_altitude = fft(altitude),
  #     fft_groundspeed = fft(groundspeed),
  #   ) %>% 
  #   summarise(
  #     dominant_freq_altitude = which.max(Mod(fft_altitude)),
  #     dominant_freq_groundspeed = which.max(Mod(fft_groundspeed))
  #   ) %>%
  #   mutate(
  #     dominant_freq_altitude = coalesce(dominant_freq_altitude, 0),  # Replace NA with 0
  #     dominant_freq_groundspeed = coalesce(dominant_freq_groundspeed, 0)  # Replace NA with 0
  #   )
  
  # Define altitude and groundspeed bins
  altitude_bins <- c(0, 5000, 10000, 15000, 20000, 25000, 30000, 35000, 40000, Inf)
  groundspeed_bins <- c(0, 100, 200, 300, 400, 500, 600, 700, Inf)
  
  # Create histogram features for cruise phase
  histogram_features <- selected_data %>%
    filter(phase == "cruise") %>%
    group_by(flight_id) %>%
    summarise(
      altitude_hist = list(hist(altitude, breaks = altitude_bins, plot = FALSE)$counts),
      groundspeed_hist = list(hist(groundspeed, breaks = groundspeed_bins, plot = FALSE)$counts)
    ) %>%
    unnest_wider(altitude_hist, names_sep = "_alt_bin_") %>%
    unnest_wider(groundspeed_hist, names_sep = "_speed_bin_")
  
  # Calculate the rate of phase transitions
  phase_transitions <- selected_data %>%
    group_by(flight_id) %>%
    mutate(phase_shift = lag(phase) != phase) %>%
    summarise(total_phase_transitions = sum(phase_shift, na.rm = TRUE))
  
  
  # Early and late segment summaries
  segment_summary <- selected_data %>%
    group_by(flight_id) %>%
    filter(timestamp_numeric <= min(timestamp_numeric) + 600 | timestamp_numeric >= max(timestamp_numeric) - 600) %>%
    mutate(segment = if_else(timestamp_numeric <= min(timestamp_numeric) + 600, "departure", "arrival")) %>%
    group_by(flight_id, segment) %>%
    summarise(
      avg_vertical_rate_segment = mean(vertical_rate, na.rm = TRUE),
      avg_groundspeed_segment = mean(groundspeed, na.rm = TRUE),
      avg_altitude_segment = mean(altitude, na.rm = TRUE)
    ) %>%
    pivot_wider(names_from = segment, values_from = c(avg_vertical_rate_segment, avg_groundspeed_segment, avg_altitude_segment), values_fill = 0)
  
  # Headwind and crosswind for each flight phase
  wind_phase_summary <- selected_data %>%
    group_by(flight_id, phase) %>%
    summarise(
      avg_headwind = mean(headwind_tailwind, na.rm = TRUE),
      avg_crosswind = mean(wind_speed * sin(relative_wind_angle * (pi / 180)), na.rm = TRUE)
    ) %>%
    pivot_wider(names_from = phase, values_from = c(avg_headwind, avg_crosswind), values_fill = 0)
  
  # Power indicators during climb and descent
  power_indicators <- selected_data %>%
    filter(phase %in% c("climb", "descent")) %>%
    group_by(flight_id, phase) %>%
    summarise(
      avg_power_ratio = mean(vertical_rate / groundspeed, na.rm = TRUE)  # Vertical rate-to-groundspeed ratio
    ) %>%
    pivot_wider(names_from = phase, values_from = avg_power_ratio, values_fill = 0, names_prefix = "power_ratio_")
  
  # Effective climb length in terms of time and distance
  climb_length <- selected_data %>%
    filter(phase == "climb") %>%
    group_by(flight_id) %>%
    summarise(
      climb_duration = sum(as.numeric(difftime(lead(timestamp), timestamp, units = "secs")), na.rm = TRUE),
      climb_distance = sum(haversine_distance(latitude, longitude, lead(latitude), lead(longitude)), na.rm = TRUE)
    )
  
  # Merge all feature sets together into the final summary
  final_summary <- time_spent_wide %>%
    left_join(additional_metrics, by = "flight_id") %>%
    left_join(phase_summary, by = "flight_id") %>%
    left_join(acceleration_summary, by = "flight_id") %>%
    left_join(altitude_iqr_per_phase, by = "flight_id") %>%
    left_join(wind_phase_summary, by = "flight_id") %>% 
    left_join(histogram_features, by = "flight_id") %>%
    #left_join(spline_features, by = "flight_id") %>%
    #left_join(fourier_features, by = "flight_id") %>%
    left_join(phase_transitions, by = "flight_id") %>% 
    left_join(wind_phase_summary, by = "flight_id") %>%
    left_join(segment_summary, by = "flight_id") %>%
    left_join(power_indicators, by = "flight_id") %>%
    left_join(climb_length, by = "flight_id")
  
  # Create CSV file name based on parquet file name
  csv_file_name <- gsub("\\.parquet$", ".csv", basename(file_path))
  
  # Write final_summary to CSV
  write_csv(final_summary, file.path("processed_trajectories_v5", csv_file_name))
  
  # Cleanup
  rm(list = ls())
  gc()
  
}

# Capture command-line arguments
args <- commandArgs(trailingOnly = TRUE)

# Extract the first argument
start_index <- as.numeric(args[1])
end_index <- start_index# + 1

# Print the parameter (or use it in your script)
cat("start_index:", start_index, "\t", "end_index:", end_index)

# Get a list of files to process
file_list <- list.files(path = "trajectories/", pattern = "*.parquet", full.names = TRUE)

# Iterate over files
walk(file_list[start_index:end_index], process_file)

