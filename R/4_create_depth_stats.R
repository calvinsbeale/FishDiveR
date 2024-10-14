#' Create depth statistics
#'
#' `create_depth_stats` creates the various daily and diel depth statistics
#' for each day
#'
#' @name create_depth_stats
#'
#' @importFrom lubridate with_tz
#' @importFrom lubridate hour
#' @importFrom lubridate parse_date_time
#' @importFrom moments skewness
#' @importFrom moments kurtosis
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom stats aggregate
#' @importFrom utils write.csv
#' @importFrom data.table setDT
#' @importFrom rlang :=
#' @importFrom suncalc getSunlightTimes
#'
#' @inheritParams import_tag_data
#' @param archive Data frame containing processed time series depth data
#' @param sunrise_time Sunrise time (local time zone) in 24-hour clock. E.g.
#'   "05:45:00"
#' @param sunset_time Sunset time (local time zone) in 24-hour clock. E.g.
#'   "18:30:00"
#' @param diel Include diel statistics when TRUE
#' @param GPS Either FALSE or the location of the GPS file containing columns
#'   'date', 'lat' (latitude) and 'lon' (longitude) if one exists. 'date'
#'   columns must be in a format readable by lubridate::dmy()
#' @param sunset_type Choose which type of sunset to include 'NULL', civil',
#'   'nautical', or 'astronomical'
#'
#' @returns A set of statistics calculated daily for the depth data. If diel
#'   is 'TRUE', additional diel statistics will be returned. An attribute
#'   'diel' with value 'TRUE' is given when diel statistics are included.
#'
#' @export
#'
#' @examples
#' # Set file path
#' filepath <- system.file("extdata", package = "FishDiveR")
#'
#' # Load archive_days
#' archive_days <- readRDS(file.path(filepath, "data/archive_days.rds"))
#'
#' # Run create_depth_stats function
#' depthStats <- create_depth_stats(
#'   archive = archive_days,
#'   tag_ID = "data",
#'   diel = TRUE,
#'   sunrise_time = "06:00:00",
#'   sunset_time = "18:00:00",
#'   GPS = file.path(filepath, "data/GPS.csv"),
#'   sunset_type = "civil",
#'   output_folder = tempdir()
#' )
#'
# Function to create the depth statistics on the daily time frame
create_depth_stats <- function(archive = archive_days,
                               tag_ID,
                               diel = FALSE,
                               sunrise_time = NULL,
                               sunset_time = NULL,
                               GPS = FALSE,
                               sunset_type = "civil",
                               output_folder = data_dir) {
  cat("\nRunning create_depth_stats() on tag ID", tag_ID, "\n")
  # Check format of inputs, on error stop
  if (!is.data.frame(archive)) {
    stop("archive must be a data frame.")
  }
  if (!is.character(tag_ID) || length(tag_ID) != 1) {
    stop("tag_ID must be a character string.")
  }
  if (!is.logical(diel) || length(diel) != 1) {
    stop("diel must be TRUE or FALSE.")
  }
  if (!is.null(sunrise_time) && !grepl("^\\d{2}:\\d{2}:\\d{2}$", sunrise_time)) {
    stop("sunrise_time must be a string in 'HH:MM:SS' format.")
  }
  if (!is.null(sunset_time) && !grepl("^\\d{2}:\\d{2}:\\d{2}$", sunset_time)) {
    stop("sunset_time must be a string in 'HH:MM:SS' format.")
  }
  if (!is.null(sunset_type) && !(sunset_type %in% c("civil", "nautical", "astronomical"))) {
    stop("sunset_type must be NULL or one of 'civil', 'nautical', or 'astronomical'")
  }
  # Test for GPS
  if (GPS != FALSE && !(is.character(GPS) && file.exists(GPS))) {
    stop("GPS must be either FALSE or a valid file path to a CSV file.")
  }
  # Additional check for diel == TRUE and GPS == FALSE
  if (diel == TRUE && GPS == FALSE && (is.null(sunrise_time) || is.null(sunset_time))) {
    stop("Please input either a GPS file or both sunrise_time and sunset_time.")
  }

  # Save time zone attribute
  time_zone <- attr(archive, "time_zone")

  # Keep only 'date', 'depth' , and 'date_only' columns
  archive <- as.data.frame(archive)[c("date", "depth", "date_only")]

  # Convert to data.table for efficiency
  data.table::setDT(archive)

  # Create date_tz_adjusted
  archive[, date_tz_adjusted := as.Date(format(lubridate::with_tz(date, tzone = time_zone), "%Y-%m-%d"))]

  # Create vertical velocity in ms
  archive$VV_ms <- c(0, diff(archive$depth) / as.numeric(difftime(archive$date[2], archive$date[1], units = "secs")))

  # Perform aggregations using data.table
  depthStats <- archive[, list(
    depth.mean = mean(depth),
    depth.sd = sd(depth),
    depth.min = min(depth),
    depth.max = max(depth),
    mean.abs_vv = mean(abs(VV_ms)),
    max.dsc_vv = max(VV_ms),
    max.asc_vv = min(VV_ms) * -1, # This makes ascent VV positive for ease of comparison of variables.
    skewness_depth = moments::skewness(depth),
    kurtosis_depth = moments::kurtosis(depth)
  ), by = list(date_tz_adjusted)]

  # Rename the date column
  names(depthStats)[names(depthStats) == "date_tz_adjusted"] <- "date_only"

  # Define surface depth threshold
  surface_depth_threshold <- 7.5

  # Calculate proportion of time spent near the surface for each day
  surface_proportion <- aggregate(depth ~ date_tz_adjusted, data = archive, function(x) mean(x < surface_depth_threshold))

  # Rename the column
  names(surface_proportion)[names(surface_proportion) == "depth"] <- "surface_proportion"

  # Merge daily surface proportion with depthStats (aggregated daily)
  depthStats$surface_proportion <- surface_proportion$surface_proportion

  # Function to convert time strings into POSIXct datetime objects for each date in archive
  convert_time <- function(time_str, date) {
    as.POSIXct(paste(date, time_str), format = "%Y-%m-%d %H:%M:%S")
  }

  # Function to check date format of 'gps' file
  check_date_format <- function(date_column) {
    # Ensure the date_column is a character vector
    if (!is.character(date_column)) {
      stop("GPS date must be a character string readable by ludridate::dmy()")
    }

    # Try to parse the dates with the expected format
    parsed_dates <- lubridate::dmy(date_column)

    # Check for any NA values in the parsed dates
    if (any(is.na(parsed_dates))) {
      stop("GPS date must be in the format 'dd-mmm-yyyy'")
    }

    return(TRUE)
  }

  # Function to calculate diel difference statistics
  calculate_metrics <- function(daily_subset, daily_max_depth, surface_depth_threshold) {
    # Extract daytime and nighttime data using the 'daytime' column
    daytime_data <- daily_subset[daily_subset$daytime == 1, ]
    nighttime_data <- daily_subset[daily_subset$daytime == 0, ]

    # Define a helper function to calculate statistics
    calculate_stats <- function(data) {
      list(
        mean_depth = mean(data$depth),
        sd_depth = sd(data$depth),
        range_depth = diff(range(data$depth)),
        abs_VV = mean(abs(data$VV_ms)),
        dsc_VV = max(data$VV_ms),
        asc_VV = -min(data$VV_ms), # This makes ascent VV positive for ease of comparison of variables.
        surf_prop = mean(data$depth < surface_depth_threshold)
      )
    }

    # Calculate statistics for daytime and nighttime
    day_stats <- calculate_stats(daytime_data)
    night_stats <- calculate_stats(nighttime_data)

    # Calculate diel differences
    diel_diff <- list(
      mean_diff = night_stats$mean_depth - day_stats$mean_depth,
      sd_diff = night_stats$sd_depth - day_stats$sd_depth,
      range_diff = night_stats$range_depth - day_stats$range_depth,
      absVV_diff = night_stats$abs_VV - day_stats$abs_VV,
      dscVV_diff = night_stats$dsc_VV - day_stats$dsc_VV,
      ascVV_diff = night_stats$asc_VV - day_stats$asc_VV,
      surf_prop_diff = night_stats$surf_prop - day_stats$surf_prop
    )

    # Calculate 'standardised' diel difference by dividing by maximum depth on that day
    st_diel_diff <- list(
      st_mean_diff = diel_diff$mean_diff / daily_max_depth,
      st_sd_diff = diel_diff$sd_diff / daily_max_depth,
      st_range_diff = diel_diff$range_diff / daily_max_depth,
      st_absVV_diff = diel_diff$absVV_diff / daily_max_depth,
      st_dscVV_diff = diel_diff$dscVV_diff / daily_max_depth,
      st_ascVV_diff = diel_diff$ascVV_diff / daily_max_depth,
      surf_prop_diff = diel_diff$surf_prop_diff
    )

    return(st_diel_diff)
  }

  # Calculate diel statistics if diel == TRUE
  if (diel) {
    # Check if using GPS co-ordinates for sunrise and sunset
    if (GPS != FALSE) {
      # Check if sunset_type is NULL
      if (is.null(sunset_type)) {
        stop("When importing a GPS file for daylight calculations, a sunset type must be chosen. Please select one of 'nautical', 'astronomical', or 'civil'")
      }

      cat("Reading in GPS locations. Using actual sunrise and sunset times to calculate diel statistics\n")

      # Read in GPS coordinates
      gps <- data.table::fread(file.path(GPS), select = c("date", "lat", "lon"))

      # Check that date, latitude and longitude exist in the 'gps' data frame
      required_columns <- c("date", "lat", "lon")
      if (!all(required_columns %in% names(gps))) {
        # If not all columns are present, throw an error
        stop("If GPS is enabled, 'date', 'lat', and 'lon' must exist in the data frame 'gps'.")
      }

      # Check for NA's in 'gps' file
      if (any(is.na(gps))) {
        stop("NA's in GPS data. Please fix.")
      }

      # Check format of gps$date
      check_date_format(gps$date)

      # Convert gps$date to a Date object
      gps$date <- lubridate::dmy(gps$date)

      # Add a day before the first date and after the last date
      first_date <- min(gps$date) - 1
      last_date <- max(gps$date) + 1
      extra_dates <- data.frame(date = c(first_date, last_date), lat = c(gps$lat[1], gps$lat[nrow(gps)]), lon = c(gps$lon[1], gps$lon[nrow(gps)]))
      gps <- rbind(extra_dates[1, ], gps, extra_dates[2, ])

      # Select relevant columns based on sunset_type
      if (sunset_type == "nautical") {
        sun <- suncalc::getSunlightTimes(data = gps, tz = time_zone, keep = c("dawn", "dusk"))
      } else if (sunset_type == "civil") {
        sun <- suncalc::getSunlightTimes(data = gps, tz = time_zone, keep = c("sunrise", "sunset"))
      } else if (sunset_type == "astronomical") {
        sun <- suncalc::getSunlightTimes(data = gps, tz = time_zone, keep = c("nightEnd", "nauticalDusk"))
      }

      # Rename date column for merging
      sun$lat <- NULL
      sun$lon <- NULL
      names(sun)[2:3] <- c("sunrise", "sunset")

      # Create
      sun$sunrise_date <- as.Date(sun$sunrise, tz = time_zone)
      sun$sunset_date <- as.Date(sun$sunset, tz = time_zone)

      # Merge sunrise and sunset times into the archive data frame
      archive <- merge(archive, sun[, c("sunrise_date", "sunrise")], by.x = "date_tz_adjusted", by.y = "sunrise_date", all.x = TRUE)
      archive <- merge(archive, sun[, c("sunset_date", "sunset")], by.x = "date_tz_adjusted", by.y = "sunset_date", all.x = TRUE)

      if (any(is.na(archive$sunrise))) {
        stop("NA's in diel data. Please check GPS dates match tag dates.")
      }

      # Reorder columns to match [, c("date", "depth", "date_only", "VV_ms", "sunrise", "sunset", "date_tz_adjusted")]
      archive <- as.data.frame(archive)
      archive <- archive[, c(2:ncol(archive), 1)]

      # Convert to data.table for efficiency
      data.table::setDT(archive)

      # Assign daytime column and re-assign time_zome
      archive[, daytime := ifelse(date >= sunrise & date < sunset, 1, 0)]
      attr(archive, "time_zone") <- time_zone

      # Save the 'tag_archive' object to output_folder
      archive_path <- file.path(output_folder, tag_ID, "archive_days.rds")
      if (file.exists(archive_path)) {
        cat("Archive updated with diel periods based on GPS calculated times\n")
      }
      create_directory(file.path(output_folder, tag_ID))
      saveRDS(object = archive, file = archive_path)
    } else if (!is.null(sunrise_time) & !is.null(sunset_time)) {
      cat("\nUsing fixed sunrise and sunset times to calculate diel statistics\n")

      # Convert sunrise and sunset times for each date
      archive[, sunrise := convert_time(sunrise_time, date_tz_adjusted)]
      archive[, sunset := convert_time(sunset_time, date_tz_adjusted)]

      # Add 'daytime' column based on sunrise and sunset times
      archive[, daytime := as.integer(date >= sunrise & date < sunset)]

      # Reorder columns
      archive <- as.data.frame(archive)
      archive <- archive[, c("date", "depth", "date_only", "VV_ms", "sunrise", "sunset", "date_tz_adjusted", "daytime")]

      # Convert to data.table for efficiency
      data.table::setDT(archive)

      # Reassign time zone
      attr(archive, "time_zone") <- time_zone

      # Save the 'tag_archive' object to output_folder
      archive_path <- file.path(output_folder, tag_ID, "archive_days.rds")
      if (file.exists(archive_path)) {
        cat("Archive updated with diel periods\n")
      }

      create_directory(file.path(output_folder, tag_ID))
      saveRDS(object = archive, file = archive_path)
    }

    # Initialise data frame
    diel_metrics <- data.frame(
      mean_Depth = numeric(),
      sd_Depth = numeric(),
      range_Depth = numeric(),
      mean_abs_VV = numeric(),
      max_dsc_VV = numeric(),
      max_asc_VV = numeric()
    )

    # Create the lists of metrics
    dates <- unique(archive$date_tz_adjusted)[1]
    for (dates in unique(archive$date_tz_adjusted)) {
      # Subset one day of depth data
      daily_subset <- archive[archive$date_tz_adjusted == dates, ]

      # Calculate maximum depth on that day
      daily_max_depth <- max(daily_subset$depth)

      # Skip days with max depth zero to avoid division by zero
      if (daily_max_depth == 0) next

      # Calculate the daily metrics using 'calculate_metrics' function
      metrics_result <- calculate_metrics(
        daily_subset = daily_subset,
        daily_max_depth = daily_max_depth,
        surface_depth_threshold = surface_depth_threshold
      )

      # Bind all the diel metrics into a data frame
      diel_metrics <- rbind(diel_metrics, metrics_result)
    }

    # Bind the diel metrics to the depthStats data frame
    depthStats <- cbind(depthStats, diel_metrics)

    # Assign value TRUE to 'diel' attribute
    attr(depthStats, "diel") <- TRUE
  }

  # Create the directory if it doesn't exist
  create_directory(file.path(output_folder, tag_ID, "3_Stats"))

  utils::write.csv(depthStats, file = file.path(output_folder, tag_ID, "3_Stats", paste0(tag_ID, "_depthStats.csv")), row.names = FALSE)
  cat(paste0("\nOutput folder: ", output_folder, "/", tag_ID, "/3_Stats/", tag_ID, "_depthStats.csv \n"))

  return(depthStats)
}
