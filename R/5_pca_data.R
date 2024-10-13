#' Prepare all data for Principal Component Analysis
#'
#' `pca_data` loads the wavelet statistics for each of the tags listed in
#' 'tag_vector'. Performs various checks to ensure compatibility of wavelets,
#' and combines them into a data frame containing only the chosen statistics.
#'
#' @name pca_data
#'
#' @param tag_vector A character vector of tag IDs. E.g. 'c("123456", "456283",
#'   "AB98XJ").
#' @param data_folder Parent folder path with separate folders for each tag
#'   data. E.g. "C:/Tag data". Defaults to 'data_dir'
#' @param phase_mean TRUE or FALSE to include this wavelet statistic. Default
#'   FALSE
#' @param phase_variance TRUE or FALSE to include this wavelet statistic.
#'   Default FALSE
#' @param power_mean TRUE or FALSE to include this wavelet statistic. Default
#'   TRUE
#' @param power_variance TRUE or FALSE to include this wavelet statistic.
#'   Default TRUE
#' @param mean_sq_power TRUE or FALSE to include this wavelet statistic. Default
#'   FALSE
#' @param amplitude_mean TRUE or FALSE to include this wavelet statistic.
#'   Default TRUE
#' @param amplitude_variance TRUE or FALSE to include this wavelet statistic.
#'   Default FALSE
#' @param output_folder Parent folder path with separate folders for each tag
#'   data. E.g. "C:/Tag data". Defaults to 'data_dir'
#'
#' @returns A data frame with the combined data for all tag ID's listed,
#'   containing the wavelet statistics to be used in Principal Component
#'   Analysis.
#'
#' @export
#'
#' @examples
#' # Set file path
#' filepath <- system.file("extdata", package = "FishDiveR")
#'
#' # Run pca_data function
#' pc_data <- pca_data(
#'   tag_vector = c("data"),
#'   data_folder = filepath,
#'   phase_mean = FALSE,
#'   phase_variance = FALSE,
#'   power_mean = TRUE,
#'   power_variance = TRUE,
#'   mean_sq_power = FALSE,
#'   amplitude_mean = TRUE,
#'   amplitude_variance = FALSE,
#'   output_folder = tempdir()
#' )
#'
# New function not including depth statistics in pc_data, for all tags listed
pca_data <- function(tag_vector,
                     data_folder = data_dir,
                     phase_mean = FALSE,
                     phase_variance = FALSE,
                     power_mean = TRUE,
                     power_variance = TRUE,
                     mean_sq_power = FALSE,
                     amplitude_mean = TRUE,
                     amplitude_variance = FALSE,
                     output_folder = data_dir) {
  # Check if tag_vector is a character vector
  if (!is.character(tag_vector)) {
    stop(" tag_vector should be a vector of characters. Check input.")
  }

  # Combine all logical parameters into a list for streamlined checking
  logical_params <- list(
    phase_mean = phase_mean,
    phase_variance = phase_variance,
    power_mean = power_mean,
    power_variance = power_variance,
    mean_sq_power = mean_sq_power,
    amplitude_mean = amplitude_mean,
    amplitude_variance = amplitude_variance
  )

  # Use lapply to check each parameter, and stop if any are not logical
  lapply(names(logical_params), function(param_name) {
    param_value <- logical_params[[param_name]]
    if (!is.logical(param_value)) {
      stop(sprintf("%s must be TRUE or FALSE.", param_name))
    }
  })

  # Initialize an empty list to store the data frames
  pc_data <- list()
  wave_length <- list() # List to store the length of each wave data frame

  # Initialize a variable to track the diel inclusion
  includes_diel <- FALSE

  # Loop through each tag_ID
  for (tag_ID in tag_vector) {
    # Locate wavelet statistics file
    wave_stats_path <- file.path(data_folder, tag_ID, "3_Stats", paste0(tag_ID, "_waveStats.csv"))

    if (file.exists(wave_stats_path)) {
      # Read in wavelet statistics
      wave_stats <- read.csv(file = wave_stats_path, header = TRUE)

      # List the lengths of wave statistics for each tag_ID
      wave_length[[tag_ID]] <- ncol(wave_stats)

      pc_data[[tag_ID]] <- wave_stats

      # Read in the wavelet meta and carry it forward
      wavelet_meta <- readRDS(file = file.path(data_folder, tag_ID, "1_Wavelets/wavelet_meta.rds"))
    } else {
      message("\n Files do not exist for tag ", tag_ID)
    }
  }

  # Check if there is more than 1 tag
  if (length(pc_data) > 1) {
    # More than one tag.
    # Check if wave_length is uneven (same number of wavelet periods amongst tags)
    if (length(unique(unlist(wave_length))) != 1) {
      stop("\n Differing numbers of wavelet periods amongst tags. Cannot combine wavelet statistics of different lengths. Check number of periods. \n")
    }

    # Check if all data frames in pc_data have the same column names
    all_names <- lapply(pc_data, names)
    if (!all(sapply(all_names, identical, all_names[[1]]))) {
      stop("\n Data frames have inconsistent column names, cannot combine.\n")
    }
  } else if (length(pc_data) == 0) {
    # No Tags - Error
    stop("\n No data available to combine.\n")
  }

  # Call list into data frame
  pc_data <- do.call(rbind, pc_data)

  # Set date_only as a date
  pc_data$date_only <- as.Date(pc_data$date_only)

  ## This section allows the function call to remove any wavelet statistics not in use (FALSE)

  # Phase: If the phase is consistently related to the time of day, it may reflect diel patterns in diving behaviour.
  # Remove Phase as it scored low on all the PC's but increases the overall number returned a lot
  if (phase_mean == FALSE) {
    pc_data <- pc_data[, !grepl("Phase_mean", names(pc_data), ignore.case = TRUE)]
  }
  if (phase_variance == FALSE) {
    pc_data <- pc_data[, !grepl("Phase_variance", names(pc_data), ignore.case = TRUE)]
  }

  # Power: The power of a wavelet can be interpreted as the "energy" or "strength" of a pattern at a certain scale and position.
  # High power could indicate a strong, consistent diving pattern, while low power could indicate more random or inconsistent behaviour.
  if (power_mean == FALSE) {
    pc_data <- pc_data[, !grepl("Power_mean", names(pc_data), ignore.case = TRUE)]
  }
  if (power_variance == FALSE) {
    pc_data <- pc_data[, !grepl("Power_variance", names(pc_data), ignore.case = TRUE)]
  }
  if (mean_sq_power == FALSE) {
    pc_data <- pc_data[, !grepl("_Mean_sq_power", names(pc_data), ignore.case = TRUE)]
  } # We remove mean sq power as it increases returned PCs without improving k-means

  # Amplitude: The maximum (peak) depth during the dive or the average depth throughout the dive could be important.
  # This could help differentiate between shallow and deep diving behaviours.
  if (amplitude_mean == FALSE) {
    pc_data <- pc_data[, !grepl("_Amplitude_mean", names(pc_data), ignore.case = TRUE)]
  }

  # Variance of amplitude: This can indicate how much the depth varies during the dive.
  # High variance might indicate a dive with a lot of changes in depth,
  # while low variance could indicate a more consistent dive.
  if (amplitude_variance == FALSE) {
    pc_data <- pc_data[, !grepl("_Amplitude_variance", names(pc_data), ignore.case = TRUE)]
  }

  # Set the attribute for the data frame
  attr(pc_data, "Includes diel") <- includes_diel

  # Check the number of tags being processed
  if (length(tag_vector) > 1) {
    save_folder <- file.path(output_folder, "Combined_tags/4_PCA")
  } else {
    save_folder <- file.path(output_folder, tag_vector, "4_PCA")

    # Set the attribute for the data frame
    attr(pc_data, "unique_tag_ID") <- tag_vector
  }

  # Set the attribute for the data frame
  attr(pc_data, "LP") <- wavelet_meta$LP
  attr(pc_data, "UP") <- wavelet_meta$UP
  attr(pc_data, "SO") <- wavelet_meta$SO

  # Create save folder
  create_directory(save_folder)

  # Save the 'pc_data' object as pc_data.rds to the output_folder
  saveRDS(pc_data, file = file.path(save_folder, "pc_data.rds"))
  cat(paste0("\nOutput file: ", save_folder, "/pc_data.rds\n"))

  return(pc_data)
}
