#' create_wavelet_stats
#'
#' `create_wavelet_stats` aggregates the wavelet variables over the specified
#' time periods
#'
#' @name create_wavelet_stats
#'
#' @importFrom lubridate days
#' @importFrom stats var
#' @importFrom utils write.csv
#'
#' @inheritParams import_tag_data
#' @param wavelet An object of class "analyze.wavelet" from package
#'   'WaveletComp'
#'
#' @returns A data frame containing the seven wavelet statistics for each
#'   period. One observation is available per period per day:
#' \itemize{
#'   \item Amplitude_mean
#'   \item Amplitude_variance
#'   \item Mean_sq_power
#'   \item Power_mean
#'   \item Power_variance
#'   \item Phase_mean
#'   \item Phase_variance
#' }
#'
#' @export
#'
#' @examples
#' # Set file path
#' filepath <- system.file("extdata", package = "FishDiveR")
#'
#' # Load my.w wavelet object
#' my.w <- readRDS(file.path(filepath, "data/1_Wavelets/data_wavelet.rds"))
#'
#' # Run create_wavelet_stats function on wavelet object
#' waveStats <- create_wavelet_stats(
#'   wavelet = my.w,
#'   tag_ID = "data",
#'   output_folder = tempdir()
#' )
#'
# Function to aggregate the nine wavelet statistics on a daily time frame
create_wavelet_stats <- function(wavelet = my.w,
                                 tag_ID,
                                 output_folder = data_dir) {
  cat("\nRunning create_wavelet_stats() on tag ID", tag_ID, "\n")
  # Check if 'wavelet' is a list
  if (!is.list(wavelet)) {
    stop("Wavelet must be a list")
  }

  # Check the length of the list
  if (length(wavelet) != 22) {
    stop("Wavelet length is not equal to 22. Check wavelet has not been modified.")
  }

  # Check if 'wavelet' inherits from the class "analyze.wavelet"
  if (!inherits(wavelet, "analyze.wavelet")) {
    stop("Wavelet is not of class 'analyze.wavelet'")
  }

  # Validate 'tag_ID' (Must be a single character string)
  if (!is.character(tag_ID) || length(tag_ID) != 1) {
    stop("tag_ID must be a single character string.")
  }

  # Creating daily blocks
  aggregate_period_hours <- 24

  # Convert the aggregation period to seconds
  aggregate_period_secs <- aggregate_period_hours * 3600

  # Define wavelet periods and reverse the order
  wavelet_periods <- seq(wavelet$nr, 1, -1) # All wavelet periods
  start_date <- wavelet$series$date[1]

  # Define the daily block size to calculate values
  sampling_interval <- as.numeric(difftime(wavelet$series[2, 1], wavelet$series[1, 1], units = "secs"))
  block_size <- (aggregate_period_secs / sampling_interval)

  # Calculate the number of blocks in each row. Should equal the number of days if aggregate_period_hours = 24
  num_blocks <- wavelet$nc %/% block_size

  # Initialize matrices to store aggregated data
  aggAmpl_mean <- matrix(0, nrow = wavelet$nr, ncol = num_blocks)
  aggPhase_mean <- matrix(0, nrow = wavelet$nr, ncol = num_blocks)
  aggPhase_var <- matrix(0, nrow = wavelet$nr, ncol = num_blocks)
  aggPower_mean <- matrix(0, nrow = wavelet$nr, ncol = num_blocks)
  aggVar_ampl <- matrix(0, nrow = wavelet$nr, ncol = num_blocks)
  aggVar_power <- matrix(0, nrow = wavelet$nr, ncol = num_blocks)
  aggmean_sq_power <- matrix(0, nrow = wavelet$nr, ncol = num_blocks)

  # Calculate the aggregated data
  for (i in seq(1, wavelet$nc, block_size)) {
    tryCatch(
      {
        start_of_block <- i
        end_of_block <- min(i + block_size - 1, wavelet$nc)

        # Extract blocks for each variable
        blockAmpl <- wavelet$Ampl[, start_of_block:end_of_block]
        blockPhase <- wavelet$Phase[, start_of_block:end_of_block]
        blockPower <- wavelet$Power[, start_of_block:end_of_block]
        blockWaveCoeffs <- wavelet$Wave[, start_of_block:end_of_block]

        # Calculate all metrics within a single loop iteration
        aggAmpl_mean[, (i - 1) / block_size + 1] <- rowMeans(blockAmpl)
        aggPhase_mean[, (i - 1) / block_size + 1] <- rowMeans(blockPhase)
        aggPhase_var[, (i - 1) / block_size + 1] <- apply(blockPhase, 1, var)
        aggPower_mean[, (i - 1) / block_size + 1] <- rowMeans(blockPower)
        aggVar_ampl[, (i - 1) / block_size + 1] <- apply(blockAmpl, 1, var)
        aggVar_power[, (i - 1) / block_size + 1] <- apply(blockPower, 1, var)
        aggmean_sq_power[, (i - 1) / block_size + 1] <- rowMeans(blockPower^2)
      },
      error = function(e) {
        stop("Error during wave statistic calculation. Please check tag deployment and release are within the input dataset.\n")
      }
    )
  }

  # Nested function to extract variables for each period
  get_waves_df <- function(period) {
    index <- which(wavelet_periods == period)

    df <- data.frame(
      Amplitude_mean = aggAmpl_mean[period, ],
      Phase_mean = aggPhase_mean[period, ],
      Phase_variance = aggPhase_var[period, ],
      Power_mean = aggPower_mean[period, ],
      Amplitude_variance = aggVar_ampl[period, ],
      Power_variance = aggVar_power[period, ],
      Mean_sq_power = aggmean_sq_power[period, ]
    )

    # Rename all columns in df
    colnames(df) <- paste0("p", period, "_", colnames(df))

    # Add tag_ID and date_only to df
    if (index == length(wavelet_periods)) {
      df$tag_ID <- tag_ID
      df$date_only <- seq(start_date, (start_date + lubridate::days(nrow(as.data.frame(aggAmpl_mean[period, ])) - 1)), by = "day")
    }
    return(df)
  }

  # Create data frames for each time frame
  waveStats <- lapply(wavelet_periods, get_waves_df)
  waveStats <- do.call(cbind, waveStats)

  # Set tag_ID as factor and date_only as date:
  waveStats$tag_ID <- factor(waveStats$tag_ID)
  waveStats$date_only <- as.Date(waveStats$date_only)

  # Create the directory if it doesn't exist
  create_directory(file.path(output_folder, tag_ID, "3_Stats"))

  utils::write.csv(waveStats, file = file.path(output_folder, tag_ID, "3_Stats", paste0(tag_ID, "_waveStats.csv")), row.names = FALSE)
  cat(paste0("\nOutput file: ", output_folder, "/", tag_ID, "/3_Stats/", tag_ID, "_waveStats.csv \n"))

  # Combine the attributes into a list
  # wavelet_meta <- list(LP = attr(wavelet, "LP"), UP = attr(wavelet, "UP"), SO = attr(wavelet, "SO"))

  # Save the list as an RDS file
  # saveRDS(wavelet_meta, file = file.path(output_folder, tag_ID, "3_Stats", "wavelet_meta.rds"))

  return(waveStats)
}
