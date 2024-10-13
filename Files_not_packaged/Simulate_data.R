##### Simulate data #####

simulate_behavior <- function(
    behavior_type = 0,
    duration = 0,
    start_time = 0,
    shallow_depth = 0,
    shallow_depth_sd = 0,
    bottom_depth = 0,
    bottom_depth_sd = 0,
    dive_duration_mean = 0,
    dive_duration_sd = 0,
    surface_interval_mean = 0,
    surface_interval_sd = 0,
    combine_with_bounce = "none"
) {
  # Create a sequence of times
  time <- seq(from = start_time, to = start_time + duration - 1, by = 1)

  depth <- numeric(length(time))

  # Initial depth
  current_depth <- bottom_depth

  if (behavior_type == "diel vertical migration") {
    i <- 1
    while (i <= length(time)) {
      hour_of_day <- (time[i] %% 1440) %/% 60
      if (hour_of_day < 6 || hour_of_day >= 18) { # Nighttime
        if (combine_with_bounce == "night" || combine_with_bounce == "both") {
          # Simulate occasional bounce dives at night
          if (runif(1) < 0.02) { # Random chance to start a deeper bounce dive
            dive_time <- max(round(rnorm(1, mean = dive_duration_mean / 1.5, sd = dive_duration_sd / 2)), 1)
            for (j in i:(i + dive_time - 1)) {
              if (j <= length(time)) {
                depth[j] <- rnorm(1, mean = bottom_depth, sd = bottom_depth_sd / 2)
              }
            }
            i <- i + dive_time + max(round(rnorm(1, mean = surface_interval_mean, sd = surface_interval_sd)), 1)
          } else {
            depth[i] <- rnorm(1, mean = shallow_depth, sd = shallow_depth_sd / 2)
            i <- i + 1
          }
        } else {
          depth[i] <- rnorm(1, mean = shallow_depth, sd = shallow_depth_sd)
          i <- i + 1
        }
      } else { # Daytime

        depth[i] <- rnorm(1, mean = bottom_depth, sd = bottom_depth_sd)

        if (combine_with_bounce == "day" || combine_with_bounce == "both") {
          # Simulate longer bounce dives and less frequent surface intervals during the day
          dive_time <- max(round(rnorm(1, mean = dive_duration_mean * 2, sd = dive_duration_sd)), 1)
          surface_time <- max(round(rnorm(1, mean = surface_interval_mean * 2, sd = surface_interval_sd)), 1)
          for (j in i:(i + dive_time - 1)) {
            if (j <= length(time)) {
              depth[j] <- rnorm(1, mean = bottom_depth, sd = bottom_depth_sd)
            }
          }
          i <- i + dive_time + surface_time
        } else {
          i <- i + 1
        }
      }
    }
  } else if (behavior_type == "reverse diel vertical migration") {
    for (i in 1:length(time)) {
      hour_of_day <- (time[i] %% 1440) %/% 60
      if (hour_of_day < 6 || hour_of_day >= 18) {
        # Night-time: deeper depths
        depth[i] <- rnorm(1, mean = bottom_depth, sd = bottom_depth_sd)
      } else {
        # Daytime: shallower depths
        depth[i] <- rnorm(1, mean = shallow_depth, sd = shallow_depth_sd)
      }
    }
  } else if (behavior_type == "bounce diving") {
    # Simulate a series of dives and surface intervals
    i <- 1
    while (i <= length(time)) {
      dive_time <- max(round(rnorm(1, mean = dive_duration_mean, sd = dive_duration_sd)), 1)
      surface_time <- max(round(rnorm(1, mean = surface_interval_mean, sd = surface_interval_sd)), 1)

      if (i + dive_time - 1 > length(time)) {
        dive_time <- length(time) - i + 1
      }

      # Simulate the diving depths
      for (j in i:(i + dive_time - 1)) {
        if (j <= length(time)) {
          depth[j] <- rnorm(1, mean = bottom_depth, sd = bottom_depth_sd)
        }
      }

      # Simulate the surface depths
      for (j in (i + dive_time):(i + dive_time + surface_time - 1)) {
        if (j <= length(time)) {
          depth[j] <- rnorm(1, mean = shallow_depth, sd = shallow_depth_sd)  # Simulating variable surface depth
        }
      }

      i <- i + dive_time + surface_time

      if (i > length(time)) {
        break
      }
    }
  } else if (behavior_type == "surface foraging") {
    # Stay near the surface with occasional shallow dives
    depth <- rnorm(duration, mean = shallow_depth, sd = shallow_depth_sd)
  }

  # Ensure depth is never less than 0
  depth <- pmax(depth, 0)

  return(data.frame(time = time, depth = depth))
}


# Helper function to convert minutes to hours since start
convert_to_hours <- function(minutes) {
  return(minutes / 60)
}

# Helper function for the probability of changing behaviour. 25% to change behaviour at the end of each day.
get_behavior_change_probability <- function(behavior) {
  switch(behavior,
         "diel vertical migration" = 0.25,
         "reverse diel vertical migration" = 0.25,
         "surface foraging" = 0.25,
         "bounce diving" = 0.25
  )
}

# Helper function to calculate which behaviour is next
get_next_behavior <- function(current_behavior) {
  # List of possible behaviours
  behaviors <- c("diel vertical migration", "reverse diel vertical migration", "surface foraging", "bounce diving")

  # Return one of the behaviours randomly, excluding the current behaviour from the list
  return(sample(behaviors[behaviors != current_behavior], 1))
}

# Simulate a sequence with a 1 hour random walk
simulate_behavior_sequence <- function(total_days) {
  # Initialise
  current_behavior <- sample(c("diel vertical migration", "reverse diel vertical migration", "bounce diving", "surface foraging"), 1)
  DVM_bottom_depth <- runif(1, 185, 250)
  RDVM_bottom_depth <- runif(1, 60, 135)
  bounce_bottom_depth <- runif(1, 135, 165)
  depth_change_direction <- sample(c("increase", "decrease"), 1)
  consecutive_days <- 0 # Counter for consecutive days with the same behavior

  # Initialize data frame
  all_data <- data.frame()

  for (day in 1:total_days) {
    for (hour_block in 0:23) {
      # Calculate start time for the one-hour block
      start_time <- (day - 1) * 24 * 60 + hour_block * 60

      # Adjust bottom depth based on random walk, every hour to a maximum 2.5%
      if (depth_change_direction == "increase") {
        DVM_bottom_depth <- min(DVM_bottom_depth * (1 + runif(1, 0, 0.025)), 250)
        RDVM_bottom_depth <- min(RDVM_bottom_depth * (1 + runif(1, 0, 0.025)), 135)
        bounce_bottom_depth <- min(bounce_bottom_depth * (1 + runif(1, 0, 0.025)), 165)
      } else {
        DVM_bottom_depth <- max(DVM_bottom_depth * (1 - runif(1, 0, 0.025)), 185)
        RDVM_bottom_depth <- max(RDVM_bottom_depth * (1 - runif(1, 0, 0.025)), 60)
        bounce_bottom_depth <- max(bounce_bottom_depth * (1 - runif(1, 0, 0.025)), 135)
      }

      # Create data for each one-hour block
      block_duration <- min(60, (day * 24 * 60) - start_time) # Ensure not to exceed total duration

      if (current_behavior == "diel vertical migration") {
        block_data <- simulate_behavior(
          behavior_type = "diel vertical migration",
          duration = block_duration,
          start_time = start_time,
          shallow_depth = 12.2,
          shallow_depth_sd = 4.2,
          bottom_depth = DVM_bottom_depth,
          bottom_depth_sd = 5.2,
          dive_duration_mean = 40,
          dive_duration_sd = 10,
          surface_interval_mean = 10,
          surface_interval_sd = 10
        )
      } else if (current_behavior == "reverse diel vertical migration") {
        block_data <- simulate_behavior(
          behavior_type = "reverse diel vertical migration",
          duration = block_duration,
          start_time = start_time,
          shallow_depth = 30,
          shallow_depth_sd = 15,
          bottom_depth = RDVM_bottom_depth,
          bottom_depth_sd = 20
        )
      } else if (current_behavior == "bounce diving") {
        block_data <- simulate_behavior(
          behavior_type = "bounce diving",
          duration = block_duration,
          start_time = start_time,
          bottom_depth = bounce_bottom_depth,
          bottom_depth_sd = 2.5,
          shallow_depth = 15,
          shallow_depth_sd = 3.5,
          dive_duration_mean = 35,
          dive_duration_sd = 8,
          surface_interval_mean = 10,
          surface_interval_sd = 3
        )
      } else if (current_behavior == "surface foraging") {
        block_data <- simulate_behavior(
          behavior_type = "surface foraging",
          duration = block_duration,
          start_time = start_time,
          shallow_depth = 1.3,
          shallow_depth_sd = 1.0
        )
      }

      # Convert the time to hours and add behavior column
      block_data$time_hours <- convert_to_hours(block_data$time)
      block_data$behavior <- current_behavior

      # Collate all data
      all_data <- rbind(all_data, block_data)

      # Randomly change direction of depth, every hour
      if (runif(1) < 0.25) {  # 25% chance to change direction each hour
        depth_change_direction <- sample(c("increase", "decrease"), 1)
      }
    }

    # Check if it's time to change behaviour
    if (consecutive_days < 0) {
      consecutive_days <- consecutive_days + 1
    } else {
      if (runif(1) < get_behavior_change_probability(current_behavior)) {
        current_behavior <- get_next_behavior(current_behavior)
        consecutive_days <- 0 # Reset counter
        depth_change_direction <- sample(c("increase", "decrease"), 1)
      }
    }
  }

  # Add a constant group for plotting
  all_data$group <- "group"

  return(all_data)
}

Days <- 11
behavior_sequence_data <- simulate_behavior_sequence(Days)
behavior_sequence_data$days <- behavior_sequence_data$time_hours / 24

# Convert time into POSIXct format
behavior_sequence_data$date <- as.POSIXct("2000-01-01", tz = "UTC", format = "%Y-%m-%d") + as.difftime(behavior_sequence_data$time, units = "mins")

# Create date_only
behavior_sequence_data$date_only <- as.Date(behavior_sequence_data$date)

# Load necessary library
library(dplyr)

# Calculate vertical velocity
behavior_sequence_data <- behavior_sequence_data %>%
  mutate(
    VV_ms = c(0, diff(depth) / diff(time*60))  # Calculate vertical velocity as depth change over time change (converted to seconds)
  )

##### Modifying depth to limit VV_ms to an absolute value of 2.75 #####
# Function to adjust depths
adjust_depths <- function(data, max_vv) {
  # Calculate depth differences
  data <- data %>%
    mutate(diff_depth = depth - lag(depth, default = first(depth)),
           VV_ms = diff_depth / 60)  # Where time interval is 1 minute

  # Identify rows where abs(VV_ms) exceeds the limit
  exceed_indices <- which(abs(data$VV_ms) > max_vv)

  # Function to adjust depth values
  adjust_surrounding_depths <- function(index, data, max_vv) {
    # Define range of rows to adjust
    start <- max(1, index - 1)
    end <- min(nrow(data), index + 1)

    # Adjust the depths
    for (i in start:end) {
      if (i > 1) {
        max_diff <- max_vv * 60
        new_diff <- sign(data$VV_ms[i]) * min(abs(data$VV_ms[i]), max_vv) * 60
        data$depth[i] <- data$depth[i - 1] + new_diff
      }
    }
    return(data)
  }

  # Apply adjustments
  for (index in exceed_indices) {
    data <- adjust_surrounding_depths(index, data, max_vv)
  }

  # Recalculate VV_ms after adjustments
  data <- data %>%
    mutate(diff_depth = depth - lag(depth, default = first(depth)),
           VV_ms = diff_depth / 60)

  return(data)
}

# Set the maximum vertical velocity
max_vv <- 2.75  # in m/s

# Adjust the depths in the dataset
behavior_sequence_data <- adjust_depths(behavior_sequence_data, max_vv)


# Format the depth and date to the desired format
behavior_sequence_data$depth <- round(behavior_sequence_data$depth, 1)
behavior_sequence_data$time <- format(behavior_sequence_data$date, format = "%H:%M:%S %d-%b-%Y")

# Clean up
rm(adjust_depths, max_vv)
#

# CHECK SAVE FOLDER
folder <- ("E:/Ch 3 data/data")
if (!dir.exists(folder)) {
  dir.create(folder, recursive = TRUE)
}

# Save the sequence data as rds
saveRDS(behavior_sequence_data, file = file.path(folder, "behavior_sequence_data.rds"))

rm(simulate_behavior, convert_to_hours, get_behavior_change_probability, get_next_behavior, simulate_behavior_sequence, Days)

#
##### Plotting the behaviour sequence data #####
library(ggplot2)

# Calculate sampling interval
sampling_interval <- behavior_sequence_data$time_hours[2] * 60 - behavior_sequence_data$time_hours[1]  * 60

if (sampling_interval < 5) {
  # Filter to every nth record
  every_nth <- seq(5, nrow(behavior_sequence_data), by = 5)
  quick_plot_data <- behavior_sequence_data[every_nth, ]
} else {
  quick_plot_data <- behavior_sequence_data
}

plot <- ggplot(data = quick_plot_data, aes(x = days, y = depth, group = group, color = behavior)) +
  geom_line() +
  scale_x_continuous(
    name = "Time series (days)",
    breaks = seq(0, max(quick_plot_data$days), by = 1),
    expand = c(0, 0, 0, 0), position = "top"
    ) +
  scale_y_reverse(limits = c(max(quick_plot_data$depth), 0), expand = c(0, 0, 0, 0)) +
  labs(
    #title = paste0("Simulated Diving Behaviours Over ", Days, " Days"),
    y = "Depth (meters)",
    color = "Behavior"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  )
plot

# Use ggsave to save the plot
ggsave(file.path(folder, "SIM_days_behaviours.png"), plot = plot, width = 10, height = 3)

# Count the number of unique days for each behaviour
days_per_behavior <- aggregate(date_only ~ behavior, data = behavior_sequence_data, FUN = function(x) length(unique(x)))

# Rename columns for clarity
colnames(days_per_behavior) <- c("Behavior", "Number_of_Days")
print(days_per_behavior)

# Save the cluster output as an excel sheet
openxlsx::write.xlsx(days_per_behavior, file = file.path(folder, "SIM_clusters.xlsx"), rowNames = FALSE)

rm(sampling_interval, every_nth, quick_plot_data, plot, days_per_behavior)

# Create archive_days from simulation data
archive_days <- behavior_sequence_data

# Reduce to columns used in wavelet analysis 'date', 'depth', and 'date_only'
archive_days <- archive_days[,c("date", "depth")]
attr(archive_days, "time_zone") <- "UTC"

saveRDS(archive_days, file = file.path(folder, "archive_days.rds"))

archive <- behavior_sequence_data[,c("time", "depth")]
write.csv(archive, file = file.path(folder, "data-Archive.csv"), row.names = FALSE, quote = FALSE)

rm(folder, behavior_sequence_data, archive)
#
##### In case of need to re-load the behaviour sequence #####
# behavior_sequence_data <- readRDS(file = "E:/Ch 3 data/test180/behavior_sequence_data.rds")
# archive_days <- readRDS(paste0("E:/Ch 3 data/test3???/archive_days.rds"))

# END
