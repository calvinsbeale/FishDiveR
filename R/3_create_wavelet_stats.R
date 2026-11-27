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
#' @param mask_ends Logical. If TRUE, also leave out the wavelet power near the
#'   start and end of the record, as near a long gap. Defaults to FALSE.
#' @param min_coverage The percentage of a day's records, and of its wavelet
#'   power at every period, that must be valid for the day to be analysed.
#'   NULL (the default) uses the archive's setting from [regularise_archive()],
#'   or 70.
#'
#' @details Each day is the wavelet's records on that local date. Days with
#'   less than \code{min_coverage} percent real records (see
#'   [regularise_archive()]) are left out.
#'
#'   Wavelet power near a gap too long to fill, where the wavelet ran over
#'   filled values, is not valid. For each period, the power within the
#'   e-folding time of its scale (the square root of 2 times the scale, about
#'   1.35 times the period) either side of the gap is left out of the day's
#'   statistics (Torrence & Compo 1998). With \code{mask_ends = TRUE}, the
#'   power this close to the start and end of the record is left out too. A
#'   day is left out if less than \code{min_coverage} percent of its power is
#'   valid at any period, e.g. within about 1.4 days of a gap for a 24 hour
#'   upper period.
#'
#'   The days left out, and why, are in the attribute 'dropped_days'.
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
#'   output = TRUE,
#'   output_folder = tempdir(),
#'   verbose = TRUE
#' )
#'
# Function to aggregate the wavelet statistics on a daily time frame
create_wavelet_stats <- function(wavelet,
                                 tag_ID,
                                 output = FALSE,
                                 output_folder = NULL,
                                 verbose = FALSE,
                                 mask_ends = FALSE,
                                 min_coverage = NULL) {
  if (verbose) message(paste0("Running create_wavelet_stats() on tag ID ", tag_ID))
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
  if (isTRUE(output) && is.null(output_folder)) {
    stop("When output = TRUE, output_folder must be provided.")
  }

  if (!is.logical(mask_ends) || length(mask_ends) != 1 || is.na(mask_ends)) {
    stop("mask_ends must be TRUE or FALSE.")
  }
  # Days need this percentage of real records, and of valid wavelet power
  if (is.null(min_coverage)) min_coverage <- attr(wavelet, "min_coverage")
  if (is.null(min_coverage)) min_coverage <- 70
  if (!is.numeric(min_coverage) || length(min_coverage) != 1 || min_coverage < 0 || min_coverage > 100) {
    stop("min_coverage must be a percentage between 0 and 100.")
  }

  # Define wavelet periods and reverse the order
  wavelet_periods <- seq(wavelet$nr, 1, -1) # All wavelet periods
  nc <- wavelet$nc

  # Each column of the wavelet belongs to the local day of its time
  dates <- wavelet$series$date
  time_zone <- attr(wavelet, "time_zone")
  if (is.null(time_zone)) time_zone <- attr(dates, "tzone")
  if (is.null(time_zone) || !nzchar(time_zone)) time_zone <- "UTC"
  sampling_interval <- as.numeric(difftime(dates[2], dates[1], units = "secs"))
  first_midnight <- lubridate::floor_date(lubridate::with_tz(dates[1], time_zone), "day")
  last_midnight <- lubridate::ceiling_date(lubridate::with_tz(dates[nc], time_zone), "day", change_on_boundary = TRUE)
  # "DSTday" steps calendar days, which are 23 or 25 hours at clock changes
  midnights <- seq(first_midnight, last_midnight, by = "DSTday")
  day <- findInterval(as.numeric(dates), as.numeric(midnights))
  n_days <- length(midnights) - 1
  day_dates <- as.Date(format(midnights[seq_len(n_days)], "%Y-%m-%d", tz = time_zone))
  day_first <- match(seq_len(n_days), day)
  day_last <- nc + 1 - match(seq_len(n_days), rev(day))
  expected <- diff(as.numeric(midnights)) / sampling_interval

  # Days with too few real records: as found by regularise_archive(), or for
  # other records, days with too few records in the wavelet
  day_coverage <- attr(wavelet, "day_coverage")
  if (!is.null(day_coverage)) {
    coverage <- day_coverage$coverage[match(day_dates, day_coverage$date)]
  } else {
    coverage <- 100 * ifelse(is.na(day_first), 0, day_last - day_first + 1) / expected
  }
  coverage[is.na(coverage)] <- 0
  dropped <- data.frame(date = day_dates, reason = ifelse(coverage < min_coverage, paste0("records ", round(coverage, 1), "%"), NA))

  # Wavelet power near a long gap (or the record's start and end) is affected
  # by the missing data. Leave out the power within the e-folding time of
  # each scale, sqrt(2) * scale (Torrence & Compo 1998): about 1.35 times the
  # period on each side.
  reach <- ceiling(sqrt(2) * wavelet$Scale / wavelet$dt)
  masks <- attr(wavelet, "long_gaps")
  if (is.null(masks)) masks <- data.frame(first = integer(), last = integer())
  if (mask_ends) masks <- rbind(masks, data.frame(first = c(-Inf, nc + 1), last = c(0, Inf)))

  # Initialize matrices to store aggregated data
  new_matrix <- function() matrix(NA_real_, nrow = wavelet$nr, ncol = n_days)
  aggAmpl_mean <- new_matrix()
  aggPhase_mean <- new_matrix()
  aggPhase_var <- new_matrix()
  aggPower_mean <- new_matrix()
  aggVar_ampl <- new_matrix()
  aggVar_power <- new_matrix()
  aggmean_sq_power <- new_matrix()

  row_stats <- function(ampl, phase, power, d, rows) {
    aggAmpl_mean[rows, d] <<- rowMeans(ampl)
    aggPhase_mean[rows, d] <<- rowMeans(phase)
    aggPhase_var[rows, d] <<- apply(phase, 1, var)
    aggPower_mean[rows, d] <<- rowMeans(power)
    aggVar_ampl[rows, d] <<- apply(ampl, 1, var)
    aggVar_power[rows, d] <<- apply(power, 1, var)
    aggmean_sq_power[rows, d] <<- rowMeans(power^2)
  }

  # Calculate the aggregated data for each day
  for (d in seq_len(n_days)) {
    if (!is.na(dropped$reason[d])) next
    cols <- day_first[d]:day_last[d]
    blockAmpl <- wavelet$Ampl[, cols, drop = FALSE]
    blockPhase <- wavelet$Phase[, cols, drop = FALSE]
    blockPower <- wavelet$Power[, cols, drop = FALSE]
    row_stats(blockAmpl, blockPhase, blockPower, d, seq_len(wavelet$nr))

    # Periods whose power is affected by a gap or edge on this day, longest
    # (furthest reaching) first
    if (nrow(masks) == 0) next
    for (r in rev(seq_len(wavelet$nr))) {
      lo <- masks$first - reach[r]
      hi <- masks$last + reach[r]
      near <- which(lo <= day_last[d] & hi >= day_first[d])
      if (length(near) == 0) next
      valid <- rep(TRUE, length(cols))
      for (m in near) {
        valid[(max(lo[m], day_first[d]) - day_first[d] + 1):(min(hi[m], day_last[d]) - day_first[d] + 1)] <- FALSE
      }
      share <- 100 * sum(valid) / expected[d]
      if (share < min_coverage) {
        dropped$reason[d] <- paste0("wavelet power ", round(share, 1), "% (", signif(wavelet$Period[r] * 24, 3), " h period)")
        break
      }
      row_stats(
        blockAmpl[r, valid, drop = FALSE], blockPhase[r, valid, drop = FALSE], blockPower[r, valid, drop = FALSE],
        d, r
      )
    }
  }
  kept <- is.na(dropped$reason)
  dropped <- dropped[!kept, , drop = FALSE]
  if (!any(kept)) stop("No days have enough records and wavelet power to analyse.")

  # Rescale power to the variance of the real records, if long gaps were
  # filled (see add_gap_attrs()). Amplitude scales with its square root.
  power_scale <- attr(wavelet, "power_scale")
  if (!is.null(power_scale)) {
    aggPower_mean <- aggPower_mean * power_scale
    aggVar_power <- aggVar_power * power_scale^2
    aggmean_sq_power <- aggmean_sq_power * power_scale^2
    aggAmpl_mean <- aggAmpl_mean * sqrt(power_scale)
    aggVar_ampl <- aggVar_ampl * power_scale
  }

  # Nested function to extract variables for each period
  get_waves_df <- function(period) {
    index <- which(wavelet_periods == period)

    df <- data.frame(
      Amplitude_mean = aggAmpl_mean[period, kept],
      Phase_mean = aggPhase_mean[period, kept],
      Phase_variance = aggPhase_var[period, kept],
      Power_mean = aggPower_mean[period, kept],
      Amplitude_variance = aggVar_ampl[period, kept],
      Power_variance = aggVar_power[period, kept],
      Mean_sq_power = aggmean_sq_power[period, kept]
    )

    # Rename all columns in df
    colnames(df) <- paste0("p", period, "_", colnames(df))

    # Add tag_ID and date_only to df
    if (index == length(wavelet_periods)) {
      df$tag_ID <- tag_ID
      df$date_only <- day_dates[kept]
    }
    return(df)
  }

  # Create data frames for each time frame
  waveStats <- lapply(wavelet_periods, get_waves_df)
  waveStats <- do.call(cbind, waveStats)

  # Set tag_ID as factor and date_only as date:
  waveStats$tag_ID <- factor(waveStats$tag_ID)
  waveStats$date_only <- as.Date(waveStats$date_only)

  # Report the days left out
  attr(waveStats, "dropped_days") <- dropped
  if (nrow(dropped) > 0 && verbose) {
    message(
      "Left out ", nrow(dropped), " day", if (nrow(dropped) > 1) "s", " with less than ", min_coverage,
      "% of ", if (nrow(dropped) > 1) "their" else "its", " records or valid wavelet power: ",
      paste0(dropped$date, " (", dropped$reason, ")", collapse = ", ")
    )
  }

  if (output) {
    # Create the directory if it doesn't exist
    dir.create(file.path(output_folder, tag_ID, "3_Stats"), recursive = TRUE, showWarnings = FALSE)

    utils::write.csv(waveStats, file = file.path(output_folder, tag_ID, "3_Stats", paste0(tag_ID, "_waveStats.csv")), row.names = FALSE)
    if (verbose) message(paste0("\nOutput file: ", output_folder, "/", tag_ID, "/3_Stats/", tag_ID, "_waveStats.csv \n"))
  }

  # Combine the attributes into a list
  # wavelet_meta <- list(LP = attr(wavelet, "LP"), UP = attr(wavelet, "UP"), SO = attr(wavelet, "SO"))

  # Save the list as an RDS file
  # saveRDS(wavelet_meta, file = file.path(output_folder, tag_ID, "3_Stats", "wavelet_meta.rds"))

  return(waveStats)
}
