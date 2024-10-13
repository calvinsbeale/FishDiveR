##### Example plots #####
# Diel vertical migration
dvm <- simulate_behavior(
  behavior_type = "diel vertical migration",
  duration = (2 * 24 * 60),
  start_time = (0 * 60),
  shallow_depth = 0,
  bottom_depth = 100,
  depth_sd = 15,
  dive_duration_mean = 60,
  dive_duration_sd = 10,
  surface_interval_mean = 10,
  surface_interval_sd = 30,
  # combine_with_bounce = "none"
  # combine_with_bounce = "day"
  combine_with_bounce = "night"
  # combine_with_bounce = "both"
)

# Convert time to hours since start
dvm$time_hours <- convert_to_hours(dvm$time)

# Plotting the data using ggplot
ggplot(data = dvm, aes(x = time_hours, y = depth)) +
  geom_line() +
  scale_x_continuous(name = "Time (hours since start)", breaks = seq(0, max(dvm$time), by = 6), expand = c(0, 1)) +
  scale_y_reverse(limits = c(max(dvm$depth), 0)) +
  labs(
    title = "Diel Vertical Migration Behavior Simulation",
    y = "Depth (meters)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Reverse diel vertical migration
rdvm <- simulate_behavior(
  behavior_type = "reverse diel vertical migration",
  duration = (2 * 24 * 60),
  start_time = (0 * 60),
  shallow_depth = 0,
  bottom_depth = 100,
  depth_sd = 15
)

# Convert time to hours since start
rdvm$time_hours <- convert_to_hours(rdvm$time)

# Plotting the data using ggplot
ggplot(data = rdvm, aes(x = time_hours, y = depth)) +
  geom_line() +
  scale_x_continuous(name = "Time (hours since start)", breaks = seq(0, max(rdvm$time), by = 6), expand = c(0, 1)) +
  scale_y_reverse(limits = c(max(rdvm$depth), 0)) +
  labs(
    title = "Reverse Diel Vertical Migration Behavior Simulation",
    y = "Depth (meters)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bounce diving
bouncing <- simulate_behavior(
  behavior_type = "bounce diving",
  duration = (1 * 24 * 60),
  start_time = 1 * 24 * 60,
  bottom_depth = 100,
  depth_sd = 10,
  dive_duration_mean = 40,
  dive_duration_sd = 10,
  surface_interval_mean = 20,
  surface_interval_sd = 10
)

# Convert time to hours since start
bouncing$time_hours <- convert_to_hours(bouncing$time)

# Plotting the data using ggplot
ggplot(data = bouncing, aes(x = time_hours, y = depth)) +
  geom_line() +
  scale_x_continuous(name = "Time (hours since start)", breaks = seq(0, max(bouncing$time), by = 6), expand = c(0, 1)) +
  scale_y_reverse(limits = c(max(bouncing$depth), 0)) +
  labs(
    title = "Bounce Diving Behavior Simulation",
    y = "Depth (meters)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Surface foraging
surface_foraging <- simulate_behavior(
  behavior_type = "surface foraging",
  duration = (2 * 24 * 60),
  start_time = 0,
  depth_mean = 0,
  depth_sd = 5
)

# Convert time to hours since start
surface_foraging$time_hours <- convert_to_hours(surface_foraging$time)

# Plotting the data using ggplot
ggplot(data = surface_foraging, aes(x = time_hours, y = depth)) +
  geom_line() +
  scale_x_continuous(name = "Time (hours since start)", breaks = seq(0, max(surface_foraging$time), by = 6), expand = c(0, 1)) +
  scale_y_reverse(limits = c(max(surface_foraging$depth), 0)) +
  labs(
    title = "Surface foraging Behavior Simulation",
    y = "Depth (meters)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#

##### Simulating with smoothing #####
library(ggplot2)
library(lubridate)


simulate_behavior <- function(
    behavior_type,
    duration,
    start_time,
    depth_mean,
    depth_sd,
    dive_duration_mean,
    dive_duration_sd,
    surface_interval_mean,
    surface_interval_sd
) {
  # Create a sequence of times
  time <- seq(from = start_time, to = start_time + duration - 1, by = 1)
  depth <- numeric(length(time))

  # Initial depth
  current_depth <- depth_mean

  for (i in 1:length(time)) {
    hour_of_day <- (time[i] %% 1440) %/% 60

    # Adjust depth mean based on behavior type and time of day
    if (behavior_type == "diel vertical migration") {
      depth_mean_current <- ifelse(hour_of_day < 6 || hour_of_day >= 18, depth_mean / 2, depth_mean)
    } else if (behavior_type == "reverse diel vertical migration") {
      depth_mean_current <- ifelse(hour_of_day < 6 || hour_of_day >= 18, depth_mean, depth_mean / 2)
    } else {
      depth_mean_current <- depth_mean
    }

    # Simulate depth change as a random walk
    depth_change <- rnorm(1, mean = 0, sd = depth_sd)
    current_depth <- current_depth + depth_change

    # Keep the depth within logical limits
    current_depth <- pmax(pmin(current_depth, depth_mean_current * 2), 0)
    depth[i] <- current_depth
  }

  # Apply a smoothing function to the depth
  depth <- stats::filter(depth, rep(1/6, 6), sides = 2)

  # Ensure depth is never less than 0
  depth <- pmax(depth, 0)

  return(data.frame(time = time, depth = depth))
}

# Function to convert minutes to hours since start
convert_to_hours <- function(minutes) {
  hours_since_start <- minutes / 60
  return(hours_since_start)
}

simulate_behavior_sequence <- function(total_days) {
  # Define the duration in minutes (total days * 24 hours * 60 minutes)
  duration <- total_days * 24 * 60

  # Simulate the behavior for the entire duration
  data <- simulate_behavior(
    behavior_type = "diel vertical migration",  # Or any other default behavior
    duration = duration,
    start_time = 0,
    depth_mean = 100,   # Adjust as needed
    depth_sd = 15,      # Adjust as needed
    dive_duration_mean = 40,  # Adjust as needed
    dive_duration_sd = 10,    # Adjust as needed
    surface_interval_mean = 10,   # Adjust as needed
    surface_interval_sd = 30      # Adjust as needed
  )

  # Convert the time to hours
  data$time_hours <- convert_to_hours(data$time)

  return(data)
}

# Generate the simulated behavior data for 30 days
behavior_sequence_data <- simulate_behavior_sequence(30)

# Plotting the data using ggplot
ggplot(data = behavior_sequence_data, aes(x = time_hours, y = depth)) +
  geom_line() +
  scale_x_continuous(name = "Time (hours since start)", breaks = seq(0, max(behavior_sequence_data$time_hours), by = 12), expand = c(0, 1)) +
  scale_y_reverse(limits = c(max(behavior_sequence_data$depth), 0)) +
  labs(title = "Simulated Diving Behaviors Over 30 Days", y = "Depth (meters)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Warning: There are NA's introduced by the smoothing.

#
##### Simulating with 24 hour walk #####
simulate_behavior_sequence <- function(total_days) {
  # Randomise starting behaviour
  current_behavior <- sample(c("diel vertical migration", "reverse diel vertical migration", "bounce diving", "surface foraging"), 1)

  # Randomise starting bottom depth
  bottom_depth <- runif(1, 80, 150)

  # Randomise walk direction
  depth_change_direction <- sample(c("increase", "decrease"), 1)

  # Initialise data frame
  all_data <- data.frame()

  # Start count for days since walk direction changed
  #days_since_direction_change <- 0

  for (day in 1:total_days) {
    # Set start time to 6 AM of the current day
    start_time <- (day - 1) * 24 * 60 + (6 * 60)

    # Adjust bottom depth based on random walk
    if (depth_change_direction == "increase") {
      # Set maximum depth to 150 m
      bottom_depth <- min(bottom_depth * (1 + runif(1, 0, 0.10)), 150)
    } else {
      # Set minimum depth to 80 m
      bottom_depth <- max(bottom_depth * (1 - runif(1, 0, 0.10)), 80)
    }

    # Create daily data
    if (current_behavior == "diel vertical migration") {
      daily_data <- simulate_behavior(
        behavior_type = "diel vertical migration",
        duration = (1 * 24 * 60),
        start_time = (day - 1) * 24 * 60,
        shallow_depth = 10,
        shallow_depth_sd = 5,
        bottom_depth = bottom_depth,
        bottom_depth_sd = 5,
        dive_duration_mean = 40,
        dive_duration_sd = 10,
        surface_interval_mean = 10,
        surface_interval_sd = 10
      )
    } else if (current_behavior == "reverse diel vertical migration") {
      daily_data <- simulate_behavior(
        behavior_type = "reverse diel vertical migration",
        duration = (1 * 24 * 60),
        start_time = (day - 1) * 24 * 60,
        shallow_depth = 10,
        shallow_depth_sd = 5,
        bottom_depth = bottom_depth,
        bottom_depth_sd = 5
      )
    } else if (current_behavior == "bounce diving") {
      daily_data <- simulate_behavior(
        behavior_type = "bounce diving",
        duration = (1 * 24 * 60),
        start_time = (day - 1) * 24 * 60,
        bottom_depth = bottom_depth,
        bottom_depth_sd = 5,
        dive_duration_mean = 40,
        dive_duration_sd = 10,
        surface_interval_mean = 20,
        surface_interval_sd = 10
      )
    } else if (current_behavior == "surface foraging") {
      daily_data <- simulate_behavior(
        behavior_type = "surface foraging",
        duration = (1 * 24 * 60),
        start_time = (day - 1) * 24 * 60,
        shallow_depth = 5,
        shallow_depth_sd = 5,
      )
    }

    # Convert the time to hours
    daily_data$time_hours <- convert_to_hours(daily_data$time)

    # Collate all data
    all_data <- rbind(all_data, daily_data)

    # Check if it's time to change behavior (next day at 6 AM)
    if ((day * 24 * 60 + 6 * 60) > length(all_data$time)) {
      if (runif(1) < get_behavior_change_probability(current_behavior)) {
        current_behavior <- get_next_behavior(current_behavior)
        bottom_depth <- 100  # Reset bottom depth when behavior changes
        depth_change_direction <- sample(c("increase", "decrease"), 1)
        days_since_direction_change <- 0
      } else {
        days_since_direction_change <- days_since_direction_change + 1

        # Change direction every 2 days with a 50% chance
        if (days_since_direction_change >= 2 && runif(1) < 0.5) {
          depth_change_direction <- ifelse(depth_change_direction == "increase", "decrease", "increase")
          days_since_direction_change <- 0
        }
      }
    }
  }

  all_data$group <- "group"

  return(all_data)
}
#
##### smoothing with 4 behaviours and no NA's single behaviour #####


simulate_behavior <- function(
    behavior_type,
    duration,
    start_time,
    depth_mean,
    depth_sd,
    dive_duration_mean,
    dive_duration_sd,
    surface_interval_mean,
    surface_interval_sd
) {
  # Create a sequence of times
  time <- seq(from = start_time, to = start_time + duration - 1, by = 1)
  depth <- numeric(length(time))

  # Initial depth and standard deviation
  current_depth <- depth_mean
  depth_sd_current <- depth_sd  # Initialize the depth standard deviation

  for (i in 1:length(time)) {
    hour_of_day <- (time[i] %% 1440) %/% 60

    # Adjust depth mean based on behavior type and time of day
    if (behavior_type == "diel vertical migration") {
      depth_mean_current <- ifelse(hour_of_day < 6 || hour_of_day >= 18, depth_mean / 2, depth_mean)
    } else if (behavior_type == "reverse diel vertical migration") {
      depth_mean_current <- ifelse(hour_of_day < 6 || hour_of_day >= 18, depth_mean, depth_mean / 2)
    } else if (behavior_type == "bounce diving") {
      # For bounce diving, the depth changes more rapidly
      depth_mean_current <- depth_mean
      depth_sd_current <- depth_sd * 2
    } else if (behavior_type == "surface foraging") {
      # For surface foraging, depth is closer to the surface
      depth_mean_current <- depth_mean / 4
      depth_sd_current <- depth_sd / 2
    } else {
      depth_mean_current <- depth_mean
      depth_sd_current <- depth_sd
    }

    # Simulate depth change as a random walk
    depth_change <- rnorm(1, mean = 0, sd = depth_sd_current)
    current_depth <- current_depth + depth_change

    # Keep the depth within logical limits
    current_depth <- pmax(pmin(current_depth, depth_mean_current * 2), 0)
    depth[i] <- current_depth
  }

  # Apply a smoothing function to the depth
  smoothed_depth <- stats::filter(depth, rep(1/6, 6), sides = 2)

  # Handle NA values caused by smoothing
  smoothed_depth[1:2] <- depth[1]
  smoothed_depth[(length(depth)-2):length(depth)] <- depth[length(depth)]

  # Ensure depth is never less than 0
  smoothed_depth <- pmax(smoothed_depth, 0)

  return(data.frame(time = time, depth = smoothed_depth))
}


# Function to convert minutes to hours since start
convert_to_hours <- function(minutes) {
  hours_since_start <- minutes / 60
  return(hours_since_start)
}

simulate_behavior_sequence <- function(total_days) {
  # Define the duration in minutes (total days * 24 hours * 60 minutes)
  duration <- total_days * 24 * 60

  # Simulate the behavior for the entire duration
  data <- simulate_behavior(
    behavior_type = "diel vertical migration",  # Or any other default behavior
    duration = duration,
    start_time = 0,
    depth_mean = 100,   # Adjust as needed
    depth_sd = 15,      # Adjust as needed
    dive_duration_mean = 40,  # Adjust as needed
    dive_duration_sd = 10,    # Adjust as needed
    surface_interval_mean = 10,   # Adjust as needed
    surface_interval_sd = 30      # Adjust as needed
  )

  # Convert the time to hours
  data$time_hours <- convert_to_hours(data$time)

  return(data)
}

# Generate the simulated behavior data for 30 days
behavior_sequence_data <- simulate_behavior_sequence(30)

# Plotting the data using ggplot
ggplot(data = behavior_sequence_data, aes(x = time_hours, y = depth)) +
  geom_line() +
  scale_x_continuous(name = "Time (hours since start)", breaks = seq(0, max(behavior_sequence_data$time_hours), by = 12), expand = c(0, 1)) +
  scale_y_reverse(limits = c(max(behavior_sequence_data$depth), 0)) +
  labs(title = "Simulated Diving Behaviors Over 30 Days", y = "Depth (meters)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#

##### smoothing with 4 behaviours and no NA's multiple behaviours #####


simulate_behavior <- function(
    behavior_type,
    duration,
    start_time,
    depth_mean,
    depth_sd,
    dive_duration_mean,
    dive_duration_sd,
    surface_interval_mean,
    surface_interval_sd
) {
  # Create a sequence of times
  time <- seq(from = start_time, to = start_time + duration - 1, by = 1)
  depth <- numeric(length(time))

  # Initial depth and standard deviation
  current_depth <- depth_mean
  depth_sd_current <- depth_sd  # Initialize the depth standard deviation

  for (i in 1:length(time)) {
    hour_of_day <- (time[i] %% 1440) %/% 60

    # Adjust depth mean based on behavior type and time of day
    if (behavior_type == "diel vertical migration") {
      depth_mean_current <- ifelse(hour_of_day < 6 || hour_of_day >= 18, depth_mean / 2, depth_mean)
    } else if (behavior_type == "reverse diel vertical migration") {
      depth_mean_current <- ifelse(hour_of_day < 6 || hour_of_day >= 18, depth_mean, depth_mean / 2)
    } else if (behavior_type == "bounce diving") {
      # For bounce diving, the depth changes more rapidly
      depth_mean_current <- depth_mean
      depth_sd_current <- depth_sd * 2
    } else if (behavior_type == "surface foraging") {
      # For surface foraging, depth is closer to the surface
      depth_mean_current <- depth_mean / 4
      depth_sd_current <- depth_sd / 2
    } else {
      depth_mean_current <- depth_mean
      depth_sd_current <- depth_sd
    }

    # Simulate depth change as a random walk
    depth_change <- rnorm(1, mean = 0, sd = depth_sd_current)
    current_depth <- current_depth + depth_change

    # Keep the depth within logical limits
    current_depth <- pmax(pmin(current_depth, depth_mean_current * 2), 0)
    depth[i] <- current_depth
  }

  # Apply a smoothing function to the depth
  smoothed_depth <- stats::filter(depth, rep(1/6, 6), sides = 2)

  # Handle NA values caused by smoothing
  smoothed_depth[1:2] <- depth[1]
  smoothed_depth[(length(depth)-2):length(depth)] <- depth[length(depth)]

  # Ensure depth is never less than 0
  smoothed_depth <- pmax(smoothed_depth, 0)

  return(data.frame(time = time, depth = smoothed_depth))
}


# Function to convert minutes to hours since start
convert_to_hours <- function(minutes) {
  hours_since_start <- minutes / 60
  return(hours_since_start)
}

simulate_behavior_sequence <- function(total_days) {
  # Define the duration in minutes (total days * 24 hours * 60 minutes)
  duration <- total_days * 24 * 60

  # Simulate the behavior for the entire duration
  data <- simulate_behavior(
    behavior_type = "diel vertical migration",  # Or any other default behavior
    duration = duration,
    start_time = 0,
    depth_mean = 100,   # Adjust as needed
    depth_sd = 15,      # Adjust as needed
    dive_duration_mean = 40,  # Adjust as needed
    dive_duration_sd = 10,    # Adjust as needed
    surface_interval_mean = 10,   # Adjust as needed
    surface_interval_sd = 30      # Adjust as needed
  )

  # Convert the time to hours
  data$time_hours <- convert_to_hours(data$time)

  return(data)
}

# Generate the simulated behavior data for 30 days
behavior_sequence_data <- simulate_behavior_sequence(30)

# Plotting the data using ggplot
ggplot(data = behavior_sequence_data, aes(x = time_hours, y = depth)) +
  geom_line() +
  scale_x_continuous(name = "Time (hours since start)", breaks = seq(0, max(behavior_sequence_data$time_hours), by = 12), expand = c(0, 1)) +
  scale_y_reverse(limits = c(max(behavior_sequence_data$depth), 0)) +
  labs(title = "Simulated Diving Behaviors Over 30 Days", y = "Depth (meters)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#
