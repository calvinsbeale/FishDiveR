## alternative attempts to fix VV



#
##### Expand the data to limit maximum vertical velocities #####
library(dplyr)
library(tidyr)

behavior_sequence_data <- readRDS(file = paste0("E:/Ch 3 data/test3/behavior_sequence_data.rds"))

# Calculate vertical velocity
behavior_sequence_data <- behavior_sequence_data %>%
  mutate(
    vertical_velocity = c(0, diff(depth) / diff(time*60))  # Calculate vertical velocity as depth change over time change
  )


head(behavior_sequence_data)
str(behavior_sequence_data)

# Assuming behavior_sequence_data is your dataset
max_ascent_rate_m_per_s <- 1.1  # meters per second
max_descent_rate_m_per_s <- 1.5  # meters per second

# Calculate depth changes and time differences
stretched_sequence_data <- behavior_sequence_data %>%
  mutate(
    next_depth = lead(depth),
    depth_change = next_depth - depth,
    time_diff = lead(time) - time,
    is_ascent = depth_change < 0,
    max_rate = if_else(is_ascent, max_ascent_rate_m_per_s, max_descent_rate_m_per_s),
    max_change_allowed = max_rate * time_diff,
    interpolations_needed = ceiling(abs(depth_change / max_change_allowed)) - 1,
    interpolations_needed = if_else(is.na(interpolations_needed), 0, interpolations_needed)  # Replace NA with 0
  )

# Function to generate interpolated rows for a single transition
generate_interpolated_rows <- function(row) {
  if (row$interpolations_needed > 0) {
    seq_depth <- seq(from = row$depth, to = row$next_depth, length.out = row$interpolations_needed + 2)
    seq_time <- seq(from = row$time, to = row$time + row$time_diff, length.out = row$interpolations_needed + 2)

    data.frame(
      time = seq_time[-length(seq_time)],  # Exclude the last time as it will be included in the next row
      depth = seq_depth[-length(seq_depth)],  # Exclude the last depth for the same reason
      time_hours = seq_time[-length(seq_time)] / 60,
      days = seq_time[-length(seq_time)] / (60 * 24),
      behavior = row$behavior,
      group = row$group
    )
  } else {
    return(data.frame(time = row$time, depth = row$depth, time_hours = row$time_hours, days = row$days, behavior = row$behavior, group = row$group))
  }
}

Sys.time() # Takes 7 minutes for 360 days of data
# Apply the function to each row and bind the results
interpolated_data <- do.call(rbind, lapply(1:nrow(stretched_sequence_data), function(i) generate_interpolated_rows(stretched_sequence_data[i, ])))

# Ensure data is ordered correctly
interpolated_data <- interpolated_data %>% arrange(time)

# Set new time sequence
interpolated_data$time <- seq(from = 0, to = nrow(interpolated_data)-1, by = 1)
interpolated_data$time_hours <- interpolated_data$time / 60
interpolated_data$days <- interpolated_data$time_hours / 24

# Check the first few rows to verify the results
head(interpolated_data, 25)


## Plot the stretched data
plot_data <- interpolated_data

sampling_interval <- as.numeric(plot_data$time[2] - plot_data$time[1], units = "mins")

if (sampling_interval < 5) {
  # Filter to every nth record
  every_nth <- seq(5, nrow(plot_data), by = 5)
  quick_plot_data <- plot_data[every_nth, ]
} else {
  quick_plot_data <- plot_data
}

p1 <- ggplot(data = quick_plot_data, aes(x = days, y = depth, group = group, color = behavior)) +
  geom_line() +
  scale_x_continuous(
    name = "Time series (days)",
    breaks = seq(0, max(quick_plot_data$days), by = 30),
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
p1

##### Attempt 2 #####

library(dplyr)
library(lubridate)

# Define the maximum ascent and descent rates
max_ascent_rate_m_per_s <- 1.1  # meters per second
max_descent_rate_m_per_s <- 1.5  # meters per second

# Function to apply the interpolation within a segment
interpolate_segment <- function(segment) {
  segment <- segment %>%
    mutate(
      next_depth = lead(depth),
      depth_change = next_depth - depth,
      time_diff = lead(time) - time,
      is_ascent = depth_change < 0,
      max_rate = if_else(is_ascent, max_ascent_rate_m_per_s, max_descent_rate_m_per_s),
      max_change_allowed = max_rate * time_diff,
      interpolations_needed = ceiling(abs(depth_change / max_change_allowed)) - 1,
      interpolations_needed = if_else(is.na(interpolations_needed), 0, interpolations_needed)  # Replace NA with 0
    )

  # Function to generate interpolated rows for a single transition
  generate_interpolated_rows <- function(row) {
    if (row$interpolations_needed > 0) {
      seq_depth <- seq(from = row$depth, to = row$next_depth, length.out = row$interpolations_needed + 2)
      seq_time <- seq(from = row$time, to = row$time + row$time_diff, length.out = row$interpolations_needed + 2)

      data.frame(
        time = seq_time[-length(seq_time)],  # Exclude the last time as it will be included in the next row
        depth = seq_depth[-length(seq_depth)],  # Exclude the last depth for the same reason
        time_hours = seq_time[-length(seq_time)] / 3600,
        days = seq_time[-length(seq_time)] / (3600 * 24),
        behavior = row$behavior,
        group = row$group
      )
    } else {
      return(data.frame(time = row$time, depth = row$depth, time_hours = row$time_hours, days = row$days, behavior = row$behavior, group = row$group))
    }
  }

  # Apply the function to each row and bind the results
  interpolated_segment <- do.call(rbind, lapply(1:nrow(segment), function(i) generate_interpolated_rows(segment[i, ])))

  return(interpolated_segment)
}

# Split the data into segments based on behavior periods
behavior_segments <- behavior_sequence_data %>%
  group_by(date_only, behavior) %>%
  group_split()

# Apply the interpolation within each segment
interpolated_segments <- lapply(behavior_segments, interpolate_segment)

# Combine the interpolated segments
interpolated_data <- do.call(rbind, interpolated_segments) %>%
  arrange(time)

# Set new time sequence
interpolated_data$time <- seq(from = 0, to = nrow(interpolated_data) - 1, by = 1)
interpolated_data$time_hours <- interpolated_data$time / 3600
interpolated_data$days <- interpolated_data$time_hours / 24

# Check the first few rows to verify the results
head(interpolated_data, 25)

# Calculate vertical velocity
interpolated_data <- interpolated_data %>%
  mutate(
    vertical_velocity = c(0, diff(depth) / diff(time))  # Calculate vertical velocity as depth change over time change
  )



## Plot the stretched data
plot_data <- interpolated_data

sampling_interval <- as.numeric(plot_data$time[2] - plot_data$time[1], units = "mins")

if (sampling_interval < 5) {
  # Filter to every nth record
  every_nth <- seq(5, nrow(plot_data), by = 5)
  quick_plot_data <- plot_data[every_nth, ]
} else {
  quick_plot_data <- plot_data
}

ggplot(data = quick_plot_data, aes(x = days, y = depth, group = group, color = behavior)) +
  geom_line() +
  scale_x_continuous(
    name = "Time series (days)",
    breaks = seq(0, max(quick_plot_data$days), by = 30),
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






