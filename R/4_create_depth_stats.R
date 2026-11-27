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
#' @param sunset_type Choose which type of sunset to include 'NULL', 'civil',
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
#'   output = TRUE,
#'   output_folder = tempdir(),
#'   verbose = TRUE
#' )
#'
# Function to create the depth statistics on the daily time frame
create_depth_stats <- function(archive,
                               tag_ID,
                               diel = FALSE,
                               sunrise_time = NULL,
                               sunset_time = NULL,
                               GPS = FALSE,
                               sunset_type = "civil",
                               output = FALSE,
                               output_folder = NULL,
                               verbose = FALSE) {
  if (verbose) message(paste0("Running create_depth_stats() on tag ID ", tag_ID))
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
    stop("sunset_type must be 'NULL' or one of 'civil', 'nautical', or 'astronomical'")
  }
  # Test for GPS
  if (GPS != FALSE && !(is.character(GPS) && file.exists(GPS))) {
    stop("GPS must be either FALSE or a valid file path to a CSV file.")
  }
  # Additional check for diel == TRUE and GPS == FALSE
  if (diel == TRUE && GPS == FALSE && (is.null(sunrise_time) || is.null(sunset_time))) {
    stop("Please input either a GPS file or both sunrise_time and sunset_time.")
  }
  if (isTRUE(output) && is.null(output_folder)) {
    stop("When output = TRUE, output_folder must be provided.")
  }

  # Save time zone attribute
  time_zone <- attr(archive, "time_zone")

  if (is.null(time_zone) || !is.character(time_zone) || length(time_zone) != 1 || is.na(time_zone)) {
    stop("archive must have a valid 'time_zone' attribute (single character string).")
  }

  # Keep the archive's attributes from regularise_archive(), to save them again
  original <- archive

  # Keep only 'date', 'depth' , and 'date_only' columns, and the gap flags
  gap_cols <- intersect(c("filled", "long_gap"), names(archive))
  archive <- as.data.frame(archive)[c("date", "depth", "date_only", gap_cols)]

  # Convert to data.table for efficiency
  data.table::setDT(archive)

  # Create date_tz_adjusted
  archive[, date_tz_adjusted := as.Date(format(lubridate::with_tz(date, tzone = time_zone), "%Y-%m-%d"))]

  # Create vertical velocity in ms, between consecutive records only: not
  # across a gap, or the filled values of a long gap
  secs <- as.numeric(archive$date)
  sampling_interval <- attr(original, "interval_s")
  if (is.null(sampling_interval)) sampling_interval <- secs[2] - secs[1]
  archive$VV_ms <- c(0, diff(archive$depth) / diff(secs))
  consecutive <- c(TRUE, abs(diff(secs) - sampling_interval) < 1e-6)
  if ("long_gap" %in% gap_cols) {
    consecutive <- consecutive & !archive$long_gap & !c(FALSE, archive$long_gap[-nrow(archive)])
  }
  archive$VV_ms[!consecutive] <- NA

  # Records used for the statistics: not those filled in long gaps, and not
  # on days with too few real records (see regularise_archive())
  analysed <- function(a) {
    use <- rep(TRUE, nrow(a))
    if ("long_gap" %in% names(a)) use <- !a$long_gap
    days <- kept_days(original)
    if (!is.null(days)) use <- use & a$date_tz_adjusted %in% days
    use
  }
  stats_archive <- archive[analysed(archive)]
  if (nrow(stats_archive) == 0) stop("No days have enough records to analyse.")

  # Perform aggregations using data.table
  depthStats <- stats_archive[, list(
    depth.mean = mean(depth),
    depth.sd = sd(depth),
    depth.min = min(depth),
    depth.max = max(depth),
    mean.abs_vv = mean(abs(VV_ms), na.rm = TRUE),
    max.dsc_vv = max(VV_ms, na.rm = TRUE),
    max.asc_vv = min(VV_ms, na.rm = TRUE) * -1, # This makes ascent VV positive for ease of comparison of variables.
    skewness_depth = moments::skewness(depth),
    kurtosis_depth = moments::kurtosis(depth)
  ), by = list(date_tz_adjusted)]

  # Rename the date column
  names(depthStats)[names(depthStats) == "date_tz_adjusted"] <- "date_only"

  # Define surface depth threshold
  surface_depth_threshold <- 7.5

  # Calculate proportion of time spent near the surface for each day
  surface_proportion <- aggregate(depth ~ date_tz_adjusted, data = stats_archive, function(x) mean(x < surface_depth_threshold))

  # Rename the column
  names(surface_proportion)[names(surface_proportion) == "depth"] <- "surface_proportion"

  # Merge daily surface proportion with depthStats (aggregated daily)
  depthStats$surface_proportion <- surface_proportion$surface_proportion

  # Function to convert time strings into POSIXct datetime objects for each
  # date in archive, as local times in the tag's time zone
  convert_time <- function(time_str, date) {
    as.POSIXct(paste(date, time_str), format = "%Y-%m-%d %H:%M:%S", tz = time_zone)
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
        abs_VV = mean(abs(data$VV_ms), na.rm = TRUE),
        dsc_VV = max(data$VV_ms, na.rm = TRUE),
        asc_VV = -min(data$VV_ms, na.rm = TRUE), # This makes ascent VV positive for ease of comparison of variables.
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

      if (verbose) message("Reading in GPS locations. Using actual sunrise and sunset times to calculate diel statistics")

      # Define possible names for date, latitude, and longitude
      possible_columns <- list(
        date = c("date", "Date", "DATE"),
        lat = c("lat", "Lat", "LAT", "latitude", "Latitude", "LATITUDE"),
        lon = c("lon", "Lon", "LON", "longitude", "Longitude", "LONGITUDE")
      )

      # Read in the GPS file (without specifying columns yet, we'll handle this dynamically)
      gps <- data.table::fread(file.path(GPS))

      # Function to find the first matching column from possible names
      find_column <- function(possible_names, data) {
        match <- which(tolower(names(data)) %in% tolower(possible_names))
        if (length(match) > 0) {
          return(names(data)[match[1]]) # Return the first match found
        } else {
          return(NULL)
        }
      }

      # Find actual columns for date, lat, and lon
      date_col <- find_column(possible_columns$date, gps)
      lat_col <- find_column(possible_columns$lat, gps)
      lon_col <- find_column(possible_columns$lon, gps)

      # Check if all required columns were found
      if (is.null(date_col) || is.null(lat_col) || is.null(lon_col)) {
        stop("If GPS is enabled, 'date', 'lat', and 'lon' (or their variations) must exist in the data frame 'gps'.")
      }

      # Select only the relevant columns
      gps <- gps[, list(get(date_col), get(lat_col), get(lon_col))]

      # Rename the columns to standard names for further use
      data.table::setnames(gps, c("V1", "V2", "V3"), c("date", "lat", "lon"))

      # Convert to data frame
      gps <- as.data.frame(gps)

      # Check for NA's in 'gps' file
      if (any(is.na(gps))) {
        stop("NA's in GPS data. Please fix.")
      }

      # Read the GPS dates as Dates, detecting their format (see
      # detect_date_format()), unless fread() already read them as dates
      if (inherits(gps$date, c("Date", "IDate", "POSIXct"))) {
        gps$date <- as.Date(gps$date)
      } else {
        detected <- detect_date_format(gps$date, type = "date", what = "date column of the GPS file")
        if (verbose) message("GPS dates read as ", detected$label, ", e.g. '", detected$example, "'")
        dates <- as.Date(parse_date_times(gps$date, detected$format))
        if (anyNA(dates)) {
          stop("Could not read every date in the GPS file, e.g. '", gps$date[which(is.na(dates))[1]], "'.")
        }
        gps$date <- dates
      }

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
      archive <- copy_regular_attrs(archive, original)

      # Save the 'tag_archive' object to output_folder
      if (output) {
        archive_path <- file.path(output_folder, tag_ID, "archive_days.rds")
        if (verbose && file.exists(archive_path)) message("Archive updated with diel periods based on GPS calculated times")
        dir.create(file.path(output_folder, tag_ID), recursive = TRUE, showWarnings = FALSE)
        saveRDS(object = archive, file = archive_path)
      }

    } else if (!is.null(sunrise_time) & !is.null(sunset_time)) {
      if (verbose) message("Using fixed sunrise and sunset times to calculate diel statistics")

      # Convert sunrise and sunset times for each date
      archive[, sunrise := convert_time(sunrise_time, date_tz_adjusted)]
      archive[, sunset := convert_time(sunset_time, date_tz_adjusted)]

      # Add 'daytime' column based on sunrise and sunset times
      archive[, daytime := as.integer(date >= sunrise & date < sunset)]

      # Reorder columns
      archive <- as.data.frame(archive)
      archive <- archive[, c("date", "depth", "date_only", gap_cols, "VV_ms", "sunrise", "sunset", "date_tz_adjusted", "daytime")]

      # Convert to data.table for efficiency
      data.table::setDT(archive)

      # Reassign time zone
      attr(archive, "time_zone") <- time_zone
      archive <- copy_regular_attrs(archive, original)

      # Save the 'tag_archive' object to output_folder
      if (output) {
        archive_path <- file.path(output_folder, tag_ID, "archive_days.rds")
        if (verbose && file.exists(archive_path)) message("Archive updated with diel periods")
        dir.create(file.path(output_folder, tag_ID), recursive = TRUE, showWarnings = FALSE)
        saveRDS(object = archive, file = archive_path)
      }

    }

    # Create the lists of metrics, from the records analysed, one row per day
    stats_archive <- archive[analysed(archive)]
    days <- unique(stats_archive$date_tz_adjusted)
    diel_metrics <- lapply(seq_along(days), function(d) {
      # Subset one day of depth data
      daily_subset <- stats_archive[stats_archive$date_tz_adjusted == days[d], ]

      # Calculate maximum depth on that day
      daily_max_depth <- max(daily_subset$depth)

      # Calculate the daily metrics using 'calculate_metrics' function
      metrics_result <- as.data.frame(calculate_metrics(
        daily_subset = daily_subset,
        daily_max_depth = daily_max_depth,
        surface_depth_threshold = surface_depth_threshold
      ))

      # A day spent at the surface has no difference between day and night.
      # (Its standardised differences would divide by a maximum depth of 0.)
      if (daily_max_depth == 0) metrics_result[] <- 0

      cbind(date_only = days[d], metrics_result)
    })
    diel_metrics <- do.call(rbind, diel_metrics)

    # Add the diel metrics to the depthStats data frame, matching days by date
    diel_metrics <- diel_metrics[match(depthStats$date_only, diel_metrics$date_only), , drop = FALSE]
    depthStats <- cbind(depthStats, diel_metrics[, setdiff(names(diel_metrics), "date_only"), drop = FALSE])

    # Assign value TRUE to 'diel' attribute
    attr(depthStats, "diel") <- TRUE
  }

  if (output) {
    # Create the directory if it doesn't exist
    dir.create(file.path(output_folder, tag_ID, "3_Stats"), recursive = TRUE, showWarnings = FALSE)

    utils::write.csv(depthStats, file = file.path(output_folder, tag_ID, "3_Stats", paste0(tag_ID, "_depthStats.csv")), row.names = FALSE)
    if (verbose) message(paste0("Output folder: ", output_folder, "/", tag_ID, "/3_Stats/", tag_ID, "_depthStats.csv"))
  }

  return(depthStats)
}
