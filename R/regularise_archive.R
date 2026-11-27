#' Put a depth record on a regular time grid, filling short gaps
#'
#' `regularise_archive()` puts a depth record on a regular time grid, so that
#' each record is the same time apart, as the wavelet analysis assumes. It is
#' run by [import_tag_data()], so imported records are already regular.
#'
#' @details The grid runs from local midnight on the first day to midnight
#'   after the last record, every `interval_s` seconds, in step with the first
#'   record so that real records keep their times. Each grid time takes
#'   the nearest record within half an interval. Duplicate times, times out of
#'   order and records sampled at other rates are resolved this way.
#'
#'   Grid times with no record are gaps, filled by linear interpolation
#'   between the records either side:
#'   \itemize{
#'     \item Short gaps, up to `max_fill_mins` long (from one record to the
#'       next), are filled and used in the analysis, with a warning.
#'     \item Longer gaps, and any time before the first or after the last
#'       record, are filled only so that the wavelet can run, and marked in
#'       the `long_gap` column. [create_depth_stats()] leaves them out, and
#'       [create_wavelet_stats()] leaves out the wavelet power near them.
#'   }
#'
#'   Each day's coverage is the percentage of its grid times that have a real
#'   record. Days with less than `min_coverage` are left out of the wavelet
#'   and depth statistics. A day with a filled gap of 6 hours has 75%
#'   coverage.
#'
#' @param archive A depth record: a data frame with a POSIXct 'date' column, a
#'   numeric 'depth' column and, optionally, a 'temp' column, such as from
#'   [import_tag_data()]. Its 'time_zone' attribute sets the days.
#' @param interval_s The sampling interval of the grid in seconds. NULL (the
#'   default) uses the record's most common interval.
#' @param max_fill_mins The longest gap, in minutes, to fill by interpolation
#'   for analysis (default 5). At most 360 (6 hours). 0 fills no gaps.
#' @param min_coverage The percentage of a day's records that must be real (not
#'   filled) for the day to be analysed (default 70).
#' @param verbose Print a summary.
#'
#' @returns The record on the regular grid: a data frame with 'date', 'depth',
#'   'temp' (if present), 'date_only', and the logical columns 'filled'
#'   (interpolated) and 'long_gap' (filled only for the wavelet). Its
#'   attributes are:
#'   \itemize{
#'     \item time_zone, interval_s, max_fill_mins and min_coverage
#'     \item day_coverage: for each day, its real records, expected records,
#'       percentage coverage, and whether it is kept for analysis
#'     \item gaps: each gap's start and end (the records either side), length
#'       in seconds, missing records, and whether it was filled for analysis
#'     \item sampling_report: the [sampling_report()] of the records before
#'       they were put on the grid
#'   }
#'
#' @seealso [sampling_report()] to check a record's sampling.
#'
#' @export
#'
#' @examples
#' archive_days <- readRDS(system.file("extdata", "data", "archive_days.rds", package = "FishDiveR"))
#'
#' # Remove a few records to make two gaps
#' gappy <- archive_days[-c(100:102, 5000:5300), ]
#' regular <- suppressWarnings(regularise_archive(gappy, max_fill_mins = 5))
#'
#' attr(regular, "gaps")
#' head(attr(regular, "day_coverage"))
regularise_archive <- function(archive,
                               interval_s = NULL,
                               max_fill_mins = 5,
                               min_coverage = 70,
                               verbose = FALSE) {
  # Check inputs
  if (!is.data.frame(archive) || !all(c("date", "depth") %in% names(archive))) {
    stop("archive must be a data frame with 'date' and 'depth' columns.")
  }
  if (!inherits(archive$date, "POSIXct")) {
    stop("The 'date' column of archive must be POSIXct.")
  }
  if (!is.numeric(max_fill_mins) || length(max_fill_mins) != 1 || is.na(max_fill_mins) ||
    max_fill_mins < 0 || max_fill_mins > 360) {
    stop("max_fill_mins must be between 0 and 360 minutes (6 hours).")
  }
  if (!is.numeric(min_coverage) || length(min_coverage) != 1 || is.na(min_coverage) ||
    min_coverage < 0 || min_coverage > 100) {
    stop("min_coverage must be a percentage between 0 and 100.")
  }
  if (!is.null(interval_s) && (!is.numeric(interval_s) || length(interval_s) != 1 || interval_s <= 0)) {
    stop("interval_s must be NULL or a positive number of seconds.")
  }

  time_zone <- attr(archive, "time_zone")
  if (is.null(time_zone)) time_zone <- attr(archive$date, "tzone")
  if (is.null(time_zone) || !nzchar(time_zone)) time_zone <- "UTC"

  # The real records, in time order
  raw <- as.data.frame(archive)
  raw <- raw[!is.na(raw$date) & !is.na(raw$depth), , drop = FALSE]
  raw <- raw[order(raw$date), , drop = FALSE]
  if (nrow(raw) < 2) stop("archive must have at least two records with a date and depth.")
  attr(raw, "time_zone") <- time_zone
  report <- sampling_report(raw, interval_s = interval_s)
  if (is.null(interval_s)) interval_s <- report$interval_s

  # Grid from local midnight on the first day to midnight after the last
  # record, in step with the first record, so real records keep their times
  first_midnight <- lubridate::floor_date(lubridate::with_tz(raw$date[1], time_zone), "day")
  phase <- as.numeric(difftime(raw$date[1], first_midnight, units = "secs")) %% interval_s
  start <- first_midnight + phase
  end <- lubridate::ceiling_date(lubridate::with_tz(raw$date[nrow(raw)], time_zone), "day", change_on_boundary = TRUE)
  # "DSTday" steps calendar days, which are 23 or 25 hours at clock changes
  midnights <- seq(first_midnight, end, by = "DSTday")
  n_grid <- ceiling(as.numeric(difftime(end, start, units = "secs")) / interval_s - 1e-9)
  grid <- seq_len(n_grid)

  # Each grid time takes the nearest record within half an interval
  secs <- as.numeric(difftime(raw$date, start, units = "secs"))
  index <- pmin(pmax(round(secs / interval_s) + 1, 1), n_grid)
  offset <- abs(secs - (index - 1) * interval_s)
  nearest <- order(index, offset)
  nearest <- nearest[!duplicated(index[nearest])]
  real_index <- index[nearest]
  real <- logical(n_grid)
  real[real_index] <- TRUE

  # Fill the gaps by linear interpolation, flat before the first and after the
  # last record
  depth <- stats::approx(real_index, raw$depth[nearest], xout = grid, rule = 2)$y

  # Length of each gap, from the record before it to the record after it
  previous_real <- cummax(ifelse(real, grid, 0L))
  next_real <- rev(cummin(rev(ifelse(real, grid, n_grid + 1L))))
  at_ends <- previous_real == 0L | next_real > n_grid
  gap_s <- (next_real - previous_real) * interval_s
  long_gap <- !real & (at_ends | gap_s > max_fill_mins * 60)

  dates <- start + (grid - 1) * interval_s
  day <- findInterval(as.numeric(dates), as.numeric(midnights))
  day_dates <- as.Date(format(midnights, "%Y-%m-%d", tz = time_zone))

  out <- data.frame(date = dates, depth = depth)
  if ("temp" %in% names(raw)) {
    temp <- raw$temp[nearest]
    has_temp <- !is.na(temp)
    out$temp <- if (sum(has_temp) >= 2) {
      stats::approx(real_index[has_temp], temp[has_temp], xout = grid, rule = 2)$y
    } else {
      NA_real_
    }
  }
  out$date_only <- day_dates[day]
  out$filled <- !real
  out$long_gap <- long_gap

  # Coverage of each day by real records
  n_days <- length(midnights) - 1
  expected <- diff(as.numeric(midnights)) / interval_s
  records <- tabulate(day[real], nbins = n_days)
  coverage <- 100 * records / expected
  day_coverage <- data.frame(
    date = day_dates[seq_len(n_days)],
    records = records,
    expected = expected,
    coverage = round(coverage, 2),
    kept = coverage >= min_coverage
  )

  # Each run of grid times without a record is one gap
  runs <- rle(!real)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1
  is_gap <- runs$values
  gap_first <- starts[is_gap]
  gap_last <- ends[is_gap]
  before <- previous_real[gap_first]
  after <- next_real[gap_last]
  gaps <- data.frame(
    start = dates[pmax(before, 1L)],
    end = dates[pmin(after, n_grid)],
    length_s = (pmin(after, n_grid) - pmax(before, 1L)) * interval_s,
    missing_records = gap_last - gap_first + 1,
    filled = !long_gap[gap_first]
  )
  # Gaps at the start or end of the record run from or to midnight
  if (nrow(gaps) > 0) {
    gaps$start[before == 0L] <- dates[gap_first[before == 0L]]
    gaps$end[after > n_grid] <- end
    gaps$length_s[before == 0L | after > n_grid] <- gaps$missing_records[before == 0L | after > n_grid] * interval_s
  }

  # Report what was filled and left out
  short <- gaps[gaps$filled, , drop = FALSE]
  long <- gaps[!gaps$filled, , drop = FALSE]
  if (nrow(short) > 0) {
    warning(
      "Filled ", format(nrow(short), big.mark = ","), " gap", if (nrow(short) > 1) "s",
      " of up to ", format_seconds(max(short$length_s)), " by interpolation (",
      format(sum(short$missing_records), big.mark = ","), " records). These are used in the analysis.",
      call. = FALSE
    )
  }
  if (nrow(long) > 0) {
    warning(
      format(nrow(long), big.mark = ","), " gap", if (nrow(long) > 1) "s", " longer than ",
      max_fill_mins, " min (the longest ", format_seconds(max(long$length_s)), ") ",
      if (nrow(long) > 1) "were" else "was", " not filled for analysis: ",
      if (nrow(long) > 1) "they are" else "it is", " left out of the depth statistics, and the wavelet ",
      "power near ", if (nrow(long) > 1) "them" else "it", " is left out of the wavelet statistics.",
      call. = FALSE
    )
  }
  if (any(!day_coverage$kept)) {
    dropped <- day_coverage[!day_coverage$kept, , drop = FALSE]
    shown <- utils::head(paste0(dropped$date, " (", round(dropped$coverage, 1), "%)"), 10)
    warning(
      format(nrow(dropped), big.mark = ","), " day", if (nrow(dropped) > 1) "s have" else " has",
      " less than ", min_coverage, "% of ", if (nrow(dropped) > 1) "their" else "its",
      " records and ", if (nrow(dropped) > 1) "are" else "is", " left out of the statistics: ",
      paste(shown, collapse = ", "), if (nrow(dropped) > 10) paste0(" and ", nrow(dropped) - 10, " more"),
      call. = FALSE
    )
  }
  if (verbose) {
    message(
      "Regular grid: every ", interval_s, " s, ", format(n_grid, big.mark = ","), " records. Filled ",
      format(sum(short$missing_records), big.mark = ","), " records in short gaps; ",
      format(sum(long$missing_records), big.mark = ","), " in long gaps (not analysed). ",
      sum(day_coverage$kept), " of ", n_days, " days kept (at least ", min_coverage, "% real records)."
    )
  }

  attr(out, "time_zone") <- time_zone
  attr(out, "interval_s") <- interval_s
  attr(out, "max_fill_mins") <- max_fill_mins
  attr(out, "min_coverage") <- min_coverage
  attr(out, "day_coverage") <- day_coverage
  attr(out, "gaps") <- gaps
  attr(out, "sampling_report") <- report
  out
}

#' Attributes set by regularise_archive(), to carry them to a changed copy
#' @noRd
regular_attr_names <- c("interval_s", "max_fill_mins", "min_coverage", "day_coverage", "gaps", "sampling_report")

#' @noRd
copy_regular_attrs <- function(to, from) {
  for (a in regular_attr_names) attr(to, a) <- attr(from, a)
  to
}

#' Carry the long gaps and day coverage of a regular archive to its wavelet
#'
#' The long gaps are kept as runs of the wavelet's columns (first and last).
#' @noRd
add_gap_attrs <- function(wavelet, archive) {
  if ("long_gap" %in% names(archive)) {
    runs <- rle(as.logical(archive$long_gap))
    ends <- cumsum(runs$lengths)
    starts <- ends - runs$lengths + 1
    attr(wavelet, "long_gaps") <- data.frame(first = starts[runs$values], last = ends[runs$values])
    # WaveletComp divides the record by its standard deviation, which the flat
    # fills of long gaps lower. Power is rescaled to the variance of the real
    # records, so it is comparable with records without gaps.
    real <- !as.logical(archive$long_gap)
    if (any(!real) && sum(real) > 1) {
      attr(wavelet, "power_scale") <- stats::var(archive$depth) / stats::var(archive$depth[real])
    }
  }
  attr(wavelet, "day_coverage") <- attr(archive, "day_coverage")
  attr(wavelet, "min_coverage") <- attr(archive, "min_coverage")
  attr(wavelet, "time_zone") <- attr(archive, "time_zone")
  wavelet
}

#' Days kept for analysis by regularise_archive(), or NULL if not regularised
#' @noRd
kept_days <- function(x) {
  coverage <- attr(x, "day_coverage")
  if (is.null(coverage)) NULL else coverage$date[coverage$kept]
}
