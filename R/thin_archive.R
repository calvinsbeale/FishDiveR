#' Thin a depth record to a longer sampling interval
#'
#' `thin_archive` averages a processed depth record into blocks of
#' \code{interval_s} seconds, e.g. from 1 second to 15 second sampling. Use it
#' on the archive passed to [create_wavelet()] to reduce the memory and time
#' the wavelet analysis needs for long, high-frequency records.
#'
#' @details
#' Each block of \code{interval_s} seconds becomes one record, timed at the
#' start of the block, with the mean depth (and mean temperature, if present)
#' of the block. Averaging, rather than keeping every nth record, stops
#' movements faster than the new interval appearing as false longer cycles
#' (aliasing).
#'
#' Thinning only removes cycles shorter than about twice the new interval. As
#' a rule of thumb, keep at least 20 records per cycle of the wavelet's lower
#' period, i.e. \code{interval_s <= lower_period_mins * 60 / 20}: up to 15
#' seconds for a 5 minute lower period.
#'
#' Keep using the full record for [create_depth_stats()], as averaging slightly
#' reduces maximum depths. Tags clustered together should all be thinned to the
#' same interval.
#'
#' @name thin_archive
#'
#' @param archive Data frame of processed time series depth data from
#'   [import_tag_data()].
#' @param interval_s Numerical. New sampling interval in seconds. Must be a
#'   multiple of the current sampling interval and divide a day evenly, e.g. 5,
#'   10, 15, 30 or 60.
#' @param verbose Logical. If TRUE, progress messages are shown. Defaults to FALSE.
#'
#' @returns The thinned archive, with the same columns and 'time_zone'
#'   attribute. Returned unchanged if \code{interval_s} equals the current
#'   sampling interval.
#'
#' @export
#'
#' @examples
#' # Set file path
#' filepath <- system.file("extdata", package = "FishDiveR")
#'
#' # Load archive_days, sampled every 60 seconds
#' archive_days <- readRDS(file.path(filepath, "data/archive_days.rds"))
#'
#' # Thin to 5 minute sampling
#' archive_5min <- thin_archive(archive_days, interval_s = 300, verbose = TRUE)
#'
thin_archive <- function(archive, interval_s, verbose = FALSE) {
  # Check format of inputs, on error stop
  if (!is.data.frame(archive) || !all(c("date", "depth", "date_only") %in% names(archive))) {
    stop("archive must be a data frame with 'date', 'depth' and 'date_only' columns.")
  }
  if (nrow(archive) < 2) {
    stop("archive must have at least two records.")
  }
  if (!is.numeric(interval_s) || length(interval_s) != 1 || interval_s <= 0 || interval_s != round(interval_s)) {
    stop("interval_s must be a positive whole number of seconds.")
  }

  sampling_interval <- as.numeric(difftime(archive$date[2], archive$date[1], units = "secs"))
  if (interval_s %% sampling_interval != 0) {
    stop("interval_s (", interval_s, " s) must be a multiple of the sampling interval (", sampling_interval, " s).")
  }
  if (86400 %% interval_s != 0) {
    stop("interval_s must divide a day evenly, e.g. 5, 10, 15, 30 or 60 seconds.")
  }
  if (interval_s == sampling_interval) {
    return(archive)
  }

  # Block number of each record, counted from the first record. Records start
  # at local midnight, so blocks never span two days.
  block <- floor(as.numeric(difftime(archive$date, archive$date[1], units = "secs")) / interval_s)
  block_mean <- function(x) {
    sums <- rowsum(x, block, reorder = FALSE, na.rm = TRUE)
    counts <- rowsum(as.numeric(!is.na(x)), block, reorder = FALSE)
    as.vector(ifelse(counts > 0, sums / counts, NA))
  }

  # Keep the first record of each block for its date and date_only
  thinned <- archive[!duplicated(block), ]
  thinned$depth <- block_mean(archive$depth)
  if ("temp" %in% names(archive)) {
    thinned$temp <- block_mean(archive$temp)
  }
  attr(thinned, "time_zone") <- attr(archive, "time_zone")

  if (verbose) {
    message(
      "Thinned from ", sampling_interval, " to ", interval_s, " second sampling: ",
      format(nrow(archive), big.mark = ","), " to ", format(nrow(thinned), big.mark = ","), " records"
    )
  }

  thinned
}
