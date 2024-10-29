#' Load time-depth series data from csv file
#'
#' `import_tag_data` processes the time-series depth data of marine animal tags.
#' Data to import should be a csv file with a 'date_time' column and a depth
#' column. Data is cropped by deployment and release times.
#'
#' @details Creates and additionally outputs a data frame `archive_days.rds`
#'   containing the cropped data. Data is cropped to full days from midnight to
#'   midnight in local time based on the time zone supplied.
#'
#' @name import_tag_data
#'
#' @importFrom data.table fread
#' @importFrom data.table setDT
#' @importFrom utils read.csv
#' @importFrom stats sd
#'
#' @param tag_ID Unique tag identification number in a vector of characters.
#'   E.g. "123456"
#' @param tag_deploy_UTC UTC deployment time in the allowed `POSIXct` format:
#'   E.g. "2013-10-25 02:46:00"
#' @param tag_release_UTC UTC release time in the allowed `POSIXct` format: E.g.
#'   "2014-04-23 23:17:35"
#' @param archive File path of the time-series depth archive. E.g. ("C:/Tag
#'   data/123456/123456-Archive.csv")
#' @param date_time_col Column number of the date time series
#' @param depth_col Column number of the depth series
#' @param temp_col (Optional) Column number of temperature series
#' @param time_zone Time zone of the data. E.g. "Asia/Tokyo"
#' @param output_folder Output folder path. E.g. "C:/Tag data". Defaults to
#'   'data_dir'
#'
#' @returns A data frame of processed tag data. Columns kept are:
#'   * 'date' a POSIXct date_time object in format "yyyy-mm-dd hh:mm:ss"
#'   * 'depth' numerical depth data
#'   * 'temp' numerical temperature data
#'   * 'date_only' an as.Date version of the 'date' column
#'   * An attribute 'time_zone' is added to the date frame containing the time zone of the 'date'
#'
#' @export
#'
#' @examples
#' # Set file path
#' filepath <- system.file("extdata", package = "FishDiveR")
#'
#' # Run import_tag_data function on tag archive csv file
#' archive_days <- import_tag_data(
#'   tag_ID = "data",
#'   tag_deploy_UTC = "2000-01-01 00:00:00",
#'   tag_release_UTC = "2000-01-11 23:59:00",
#'   archive = file.path(filepath, "data/data-Archive.csv"),
#'   date_time_col = 1,
#'   depth_col = 2,
#'   temp_col = NA,
#'   time_zone = "Asia/Tokyo",
#'   output_folder = tempdir()
#' )
#'
# Function to import tag data from csv file. Crop to deployment length then crop by desired time period (e.g. 24 hours)
import_tag_data <- function(tag_ID,
                            tag_deploy_UTC,
                            tag_release_UTC,
                            archive,
                            date_time_col = 1,
                            depth_col = 2,
                            temp_col = NA,
                            time_zone,
                            output_folder = data_dir) {
  # Check format of inputs, on error stop
  if (!is.character(tag_ID)) {
    stop("tag_ID must be a character string.")
  }
  if (tryCatch(as.POSIXct(tag_deploy_UTC, format = "%Y-%m-%d %H:%M:%S"), error = function(e) TRUE) == TRUE) {
    stop("tag_deploy_UTC must be in 'yyyy-mm-dd hh:mm:ss' format or compatible.")
  }
  if (tryCatch(as.POSIXct(tag_release_UTC, format = "%Y-%m-%d %H:%M:%S"), error = function(e) TRUE) == TRUE) {
    stop("tag_release_UTC must be in 'yyyy-mm-dd hh:mm:ss' format or compatible.")
  }
  # Check if the user-specified file exists
  if (!file.exists(archive)) {
    stop(paste0("\nArchive: '", archive, "' cannot be found. Check folder and file names."))
  }
  if (!is.numeric(date_time_col) || date_time_col <= 0 || floor(date_time_col) != date_time_col) {
    stop("date_time_col must be a positive integer.")
  }
  if (!is.numeric(depth_col) || depth_col <= 0 || floor(depth_col) != depth_col) {
    stop("depth_col must be a positive integer.")
  }
  if (!is.na(temp_col) && (!is.numeric(temp_col) || temp_col <= 0 || floor(temp_col) != temp_col)) {
    stop("temp_col must be NA or a positive integer.")
  }
  if (!time_zone %in% OlsonNames()) {
    stop("time_zone must be an accepted R time zone.")
  }

  # Read the data using fread
  if (is.na(temp_col)) {
    tag_archive <- data.table::fread(archive, select = c(date_time_col, depth_col), col.names = c("date", "depth"))
  } else {
    tag_archive <- data.table::fread(archive, select = c(date_time_col, depth_col, temp_col), col.names = c("date", "depth", "temp"))
  }

  # Check that the number of rows is at least 1
  if (nrow(tag_archive) < 1) {
    stop("\nError: The tag archive has no rows.")
  }

  # Print tag ID
  cat(paste0("\nTag ID = ", tag_ID, "\n"))

  # Remove rows where Depth column is NA
  tag_archive <- tag_archive[!is.na(tag_archive$depth), ]

  # Check that the number of rows is at least 1
  if (nrow(tag_archive) < 1) {
    stop("\nError: The tag archive has no rows after removing NA values in the Depth column.\n")
  }

  # Set date as a POSIXct object
  tag_archive$date <- as.POSIXct(tag_archive$date, format = "%H:%M:%S %d-%b-%Y", tz = "UTC")

  # Find the range of dates in tag_archive
  min_date <- min(tag_archive$date, na.rm = TRUE)
  max_date <- max(tag_archive$date, na.rm = TRUE)

  # Convert deployment and retrieval dates to POSIXct
  deploy_date <- as.POSIXct(tag_deploy_UTC, tz = "UTC")
  release_date <- as.POSIXct(tag_release_UTC, tz = "UTC")

  # Check if deploy_date and release_date are within the range
  if (deploy_date < min_date || release_date > max_date) {
    if (deploy_date < min_date && release_date > max_date) {
      stop("\nBoth deploy and release dates are outside the archive range.")
    } else if (deploy_date < min_date) {
      stop("\nDeploy date is outside the archive range: ", min_date)
    } else {
      stop("\nRelease date is outside the archive range: ", max_date)
    }
  }

  # Crop the data to the deployment and release dates (tag_archive must be a data.table object)
  tag_archive <- tag_archive[date >= as.POSIXct(tag_deploy_UTC, tz = "UTC") &
    date <= as.POSIXct(tag_release_UTC, tz = "UTC"), ]

  # Identifying duplicated dates (including the first occurrence)
  duplicated_dates <- tag_archive[duplicated(date) | duplicated(date, fromLast = TRUE)]

  if (nrow(duplicated_dates) > 0) {
    # Printing rows with duplicated dates
    cat("\nWARNING - DUPLICATE DATES DETECTED after removing blank depth data rows in archive date_time: \n")
    print(duplicated_dates)

    # Count the number of occurrences of each date
    date_counts <- table(tag_archive$date)

    # Check if any date has more than 2 occurrences
    if (any(date_counts > 2)) {
      stop("Multiple duplicates of one date_time found. Check tag archive")
    } else {
      # Identify indices of the second occurrence of duplicated dates
      duplicated_dates_indices <- which(duplicated(tag_archive$date))

      # Safely remove the second occurrence of each duplicate
      tag_archive <- tag_archive[-duplicated_dates_indices, ]

      cat("The second date_time has been removed. If this is a wildlife computers tag, this may be the corrosion release interval. Please check before continuing \n")
    }
  }

  # Convert deployment and retrieval date_times to local time zone for cropping to full local days
  deploy_local <- lubridate::with_tz(as.POSIXct(tag_deploy_UTC, tz = "UTC"), tzone = time_zone)
  release_local <- lubridate::with_tz(as.POSIXct(tag_release_UTC, tz = "UTC"), tzone = time_zone)

  # Find the start of the first full day after deployment and end of the last full day before retrieval
  start_full_day <- lubridate::ceiling_date(deploy_local, unit = "day")

  # Find the end of the last full day before retrieval
  end_full_day <- lubridate::floor_date(release_local, unit = "day")

  # Filter the data directly using logical indexing
  tag_archive <- tag_archive[tag_archive$date >= start_full_day & tag_archive$date < end_full_day, ]

  # Set the local time zone
  tag_archive$date <- lubridate::with_tz(tag_archive$date, tzone = time_zone)

  # Metadata from tag deployment
  sampling_interval <- as.numeric(difftime(tag_archive$date[2], tag_archive$date[1], units = "secs"))
  cat(paste0("\nDepth sampling interval is ", sampling_interval, " seconds \n"))

  # Count values less than 0 in the depth column before correction
  values_above_zero <- sum(tag_archive$depth < 0)

  # Fix depth above the surface (<0)
  tag_archive$depth <- ifelse(tag_archive$depth < 0, 0, tag_archive$depth)

  # Report the number of values changed
  cat("\nNumber of depth values corrected (above 0):", values_above_zero, "\n")

  # Print mean, SD and maximum depths
  cat(paste0(
    "Mean depth = ", round(mean(tag_archive$depth), 1),
    " SD = ", round(sd(tag_archive$depth), 1), "\n"
  ))

  cat(paste0("Maximum depth = ", max(tag_archive$depth), "\n"))

  # Calculate the number of days between the first and last data points
  first_date <- as.Date(format(tag_archive$date[1], format = "%Y-%m-%d", tz = time_zone))
  last_date <- as.Date(format(tag_archive$date[length(tag_archive$date)], format = "%Y-%m-%d", tz = time_zone))
  num_days <- as.numeric(difftime(last_date, first_date, units = "days"))

  # Print the number of days in the full days data set
  cat(paste0("Number of full days in dataset: ", num_days + 1, "\n"))

  # Create date_only column
  tag_archive$date_only <- as.Date(format(lubridate::with_tz(tag_archive$date, tzone = time_zone), "%Y-%m-%d"))

  # Set time zone attribute
  attr(tag_archive, "time_zone") <- time_zone

  # Create the directory if it doesn't exist
  create_directory(file.path(output_folder, tag_ID))

  # Save the 'tag_archive' object to output_folder
  saveRDS(tag_archive, file = file.path(output_folder, tag_ID, "archive_days.rds"))
  cat(paste0("\nOutput file: ", output_folder, "/", tag_ID, "/archive_days.rds\n"))

  # Return the cropped dataset
  return(tag_archive)
}

#' Plot the time-series depth dataset
#'
#' This function plots the time-series depth data from the imported tag.
#'
#' @name plot_TDR
#'
#' @import ggplot2
#'
#' @inheritParams import_tag_data
#' @inheritParams pca_data
#' @param every_nth Numerical. Optional down-sampling of data points to plot.
#'   Defaults to 10, plotting every 10th record.
#' @param every_s  Numerical. Alternative to every_nth. Optional down-sampling
#'   of data points to plot by number of seconds, as opposed to records. E.g.
#'   plots every 60th second, rather than 10th row of data. Must be a multiple of the
#'   sampling frequency. Overrides every_nth if != 0.
#' @param plot_size ggSave height and width for saving the output plot. Must be
#'   numeric, positive and 2 elements long. Default to 'c(12,6)'
#' @param X_lim Optional. Vector with two dates delimiting the time-depth record
#'   to plot. E.g. c("2000-01-01", "2000-11-23")
#' @param Y_lim Character vector with minimum depth, maximum depth, and sequence
#'   for ticks on Y-axis. Must be numeric, positive and 3 elements long. E.g.
#'   c(0,1500,100).
#' @param date_breaks X-axis ggplot2 date breaks. E.g, "24 hour, "3 day",
#'   "2 week".
#' @param dpi Numerical. DPI to use for 'ggsave()' output. E.g, 600
#'
#' @returns A data frame of plot data
#'
#' @export
#'
#' @examples
#' # Set file path
#' filepath <- system.file("extdata", package = "FishDiveR")
#'
#' # Run depth_data function
#' TDR_plot <- plot_TDR(
#'   tag_ID = "data",
#'   data_folder = filepath,
#'   every_nth = 10,
#'   every_s = 0,
#'   plot_size = c(12, 6),
#'   X_lim = NULL,
#'   Y_lim = c(0, 300, 50),
#'   date_breaks = "24 hour",
#'   dpi = 100,
#'   output_folder = tempdir()
#' )
#'
# Utility function to combine the depth statistics and the pc scores for input to k-means clustering
plot_TDR <- function(tag_ID,
                     data_folder = data_dir,
                     every_nth = 20,
                     every_s = 0,
                     plot_size = c(12, 6),
                     X_lim = NULL,
                     Y_lim = c(0, 1500, 100),
                     date_breaks = "14 day",
                     dpi = 300,
                     output_folder = data_dir) {
  # Check format of inputs, on error stop
  if (!is.character(tag_ID)) {
    stop("tag_ID must be a character string.")
  }
  if (!is.numeric(every_nth) || every_nth <= 0) {
    stop("every_nth must be a positive integer.")
  }
  if ((!is.numeric(every_s) || every_s < 0)) {
    stop("every_s must be a positive integer.")
  }
  # Check that plot_size is numeric, positive and exactly 2 elements long
  if (!is.numeric(plot_size) || length(plot_size) != 2 || any(plot_size <= 0)) {
    stop("plot_size must be a numeric vector of length 2 with positive values.")
  }
  # Check that Y_lim is numeric, positive and exactly 3 elements long
  if (!is.numeric(Y_lim) || length(Y_lim) != 3 || any(Y_lim < 0)) {
    stop("Y_lim must be a numeric vector of length 3 with non-negative values.")
  }
  if ((!is.numeric(dpi) || dpi <= 0)) {
    stop("dpi must be a positive integer.")
  }
  # Check X_lim if provided
  if (!is.null(X_lim)) {
    if (!is.character(X_lim) || length(X_lim) != 2) {
      stop("X_lim must be a character vector of two dates in 'YYYY-MM-DD' format.")
    }
    # Convert to Date format for subsetting
    X_lim <- as.Date(X_lim)
    if (any(is.na(X_lim))) {
      stop("Invalid dates in X_lim. Ensure they are in 'YYYY-MM-DD' format.")
    }
  }

  # Read in tag archive
  archive_days <- readRDS(file = file.path(data_folder, tag_ID, "/archive_days.rds"))

  # Calculate the time differences between consecutive records
  time_diffs <- diff(as.numeric(archive_days$date, units = "secs"))

  # Check if the original sampling frequency is consistent
  sampling_interval <- unique(time_diffs)

  if (length(sampling_interval) != 1) {
    stop("Error: The original data does not have a consistent sampling frequency.")
  }

  # Check X_lim if provided
  if (!is.null(X_lim)) {
    # Crop archive_days to the subset set by the limits
    archive_days <- subset(archive_days, date_only >= as.Date(X_lim[1]) & date_only <= as.Date(X_lim[2]))
    if (nrow(archive_days) == 0) {
      stop("No data available within the specified X_lim date range.")
    }
    cat("Data has been filtered between X-axis limits \n")
  }

  # Print sampling interval
  cat(paste0("\nData sampling interval is ", sampling_interval, " seconds\n"))

  if (every_s != 0) { # Using time, rather than number of rows to plot data.
    # Check if every_s is a multiple of the original sampling frequency
    if (every_s %% sampling_interval != 0) {
      stop(paste("Error: The specified sampling interval (", every_s, " seconds) is not a multiple of the original sampling interval (", sampling_interval, " seconds).", sep = ""))
    }

    # Get the first timestamp as a numeric value
    start_time <- as.numeric(archive_days$date[1])

    # Filter the dataset by checking if the difference between the current timestamp
    # and the start time is divisible by every_s (the desired interval)
    archive_days <- archive_days[as.numeric(archive_days$date - start_time) %% every_s == 0, ]

    # Print sampling interval
    cat("Plotting every", every_s, "seconds \n")
  } else {
    if (every_nth != 1) {
      # Subset to every nth record
      crop_sq <- seq(every_nth, nrow(archive_days), by = every_nth)
      archive_days <- archive_days[crop_sq, ]

      # Print sampling interval
      cat("Plotting every", every_nth, "records \n")
    } else {
      # Print sampling interval
      cat("Plotting every record \n")
    }
  }

  # Messages
  cat("\nMaximum depth is", max(archive_days$depth), "meters\n")

  # Select plot data
  plot_data <- archive_days[, c("date", "depth")]

  TDR_plot <- ggplot(plot_data, aes(x = date, y = depth)) +
    geom_path() +
    scale_y_reverse(limits = c(Y_lim[2], Y_lim[1]), breaks = seq(Y_lim[1], Y_lim[2], Y_lim[3]), expand = c(0, 0)) +
    scale_x_datetime(date_breaks = date_breaks, date_labels = "%Y-%m-%d", expand = c(0, 0, 0, 0), position = "top") +
    labs(
      x = "Date",
      y = "Depth (m)"
    ) +
    theme_classic() +
    theme(
      axis.text = element_text(size = 14, colour = "black"),
      axis.title = element_text(size = 16), # Set font size and style for x-axis label
      # plot.margin = unit(c(0, 3, 0.1, 0.1), "lines") # top, right, bottom, left
    )
  print(TDR_plot)

  ggsave(file.path(output_folder, tag_ID, paste0(tag_ID, "_archive.png")), plot = TDR_plot, width = plot_size[1], height = plot_size[2], dpi = dpi, create.dir = TRUE)
  cat("\nOutput file:", file.path(output_folder, tag_ID, paste0(tag_ID, "_archive.png")))

  return(plot_data)
}
