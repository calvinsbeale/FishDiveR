#' Report irregular sampling in a depth record
#'
#' `sampling_report` checks that a depth record has a constant sampling
#' interval, and describes any irregularities: duplicate times, gaps, changes
#' of sampling rate, intervals shorter than expected, times out of order, and
#' days with fewer records than expected.
#'
#' @details
#' The analysis steps of FishDiveR assume a constant sampling interval. Use
#' this function on the output of [import_tag_data()], which warns when the
#' interval is not constant, to see where and what the irregularities are.
#'
#' Each interval between consecutive records is compared with the expected
#' interval, by default the most common one:
#' \itemize{
#'   \item \strong{Duplicates}: two records with the same time.
#'   \item \strong{Out of order}: a record timed before the previous record.
#'   \item \strong{Rate changes}: at least \code{min_run} consecutive intervals
#'     of the same, different length, e.g. a tag switching from 5 to 75 second
#'     sampling.
#'   \item \strong{Gaps}: other intervals longer than expected, i.e. missing
#'     records. \code{on_grid} is FALSE if the gap is not a whole number of
#'     expected intervals.
#'   \item \strong{Short intervals}: other intervals shorter than expected,
#'     e.g. from timing errors.
#' }
#'
#' Days are counted in the record's time zone, from the 'date_only' column if
#' present. A day's \code{percent} compares its number of records with the
#' number expected at the expected interval.
#'
#' @name sampling_report
#'
#' @param archive Data frame of time series depth data with a POSIXct 'date'
#'   column, e.g. from [import_tag_data()]. If it has a 'depth' column, records
#'   with no depth are left out.
#' @param interval_s Numerical. Expected sampling interval in seconds. Defaults
#'   to the most common interval.
#' @param min_run Numerical. Minimum number of consecutive intervals of the same
#'   length to count as a change of sampling rate. Defaults to 10.
#'
#' @returns A list of class 'sampling_report', printed as a summary, with:
#' \describe{
#'   \item{interval_s}{The expected sampling interval in seconds.}
#'   \item{regular}{TRUE if every interval equals \code{interval_s}.}
#'   \item{n_records}{Number of records checked.}
#'   \item{n_no_depth}{Number of records left out for having no depth.}
#'   \item{time_zone}{Time zone used for days.}
#'   \item{intervals}{Each interval length, with its number and percentage of
#'     intervals.}
#'   \item{duplicates, out_of_order, rate_changes, gaps, short}{One row per
#'     irregularity, as described in Details.}
#'   \item{days}{Records, expected records and percent complete for each day.}
#' }
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
#' # A regular record
#' sampling_report(archive_days)
#'
#' # Remove 5 records to make a gap
#' report <- sampling_report(archive_days[-(5000:5004), ])
#' report
#' report$gaps
#'
sampling_report <- function(archive, interval_s = NULL, min_run = 10) {
  if (!is.data.frame(archive)) {
    stop("archive must be a data frame.")
  }
  if (!"date" %in% names(archive) || !inherits(archive$date, "POSIXct")) {
    stop("archive must have a 'date' column of type POSIXct.")
  }
  if (!is.null(interval_s) && (!is.numeric(interval_s) || length(interval_s) != 1 || interval_s <= 0)) {
    stop("interval_s must be NULL or a positive number of seconds.")
  }
  if (!is.numeric(min_run) || length(min_run) != 1 || min_run < 2) {
    stop("min_run must be a number of at least 2.")
  }

  # Leave out records with no time or depth
  keep <- !is.na(archive$date)
  if ("depth" %in% names(archive)) keep <- keep & !is.na(archive$depth)
  n_no_depth <- sum(!keep & !is.na(archive$date))
  archive <- archive[keep, , drop = FALSE]
  date <- archive$date
  if (length(date) < 2) {
    stop("archive needs at least two records with a date and depth.")
  }

  time_zone <- attr(archive, "time_zone")
  if (is.null(time_zone)) time_zone <- attr(date, "tzone")
  if (is.null(time_zone) || !nzchar(time_zone[1])) time_zone <- "UTC"
  time_zone <- time_zone[1]

  # Interval in seconds after each record but the last
  dt <- diff(as.numeric(date))

  # Count each interval length
  lengths <- sort(unique(dt))
  counts <- tabulate(match(dt, lengths), nbins = length(lengths))
  intervals <- data.frame(interval_s = lengths, intervals = counts, percent = 100 * counts / length(dt))

  if (is.null(interval_s)) {
    positive <- intervals[intervals$interval_s > 0, , drop = FALSE]
    if (nrow(positive) == 0) {
      stop("archive has no positive intervals between records.")
    }
    interval_s <- positive$interval_s[which.max(positive$intervals)]
  }

  # Runs of identical intervals; long runs of another positive length are rate changes
  runs <- rle(dt)
  run_end <- cumsum(runs$lengths)
  run_start <- run_end - runs$lengths + 1
  is_rate <- runs$values > 0 & runs$values != interval_s & runs$lengths >= min_run
  in_rate <- rep(is_rate, runs$lengths)

  rate_changes <- data.frame(
    start = date[run_start[is_rate]],
    end = date[run_end[is_rate] + 1],
    interval_s = runs$values[is_rate],
    intervals = runs$lengths[is_rate]
  )

  dup <- which(dt == 0)
  duplicates <- data.frame(row = dup + 1, date = date[dup])

  back <- which(dt < 0)
  out_of_order <- data.frame(row = back + 1, previous = date[back], date = date[back + 1])

  gap <- which(dt > interval_s & !in_rate)
  gaps <- data.frame(
    row = gap + 1,
    start = date[gap],
    end = date[gap + 1],
    length_s = dt[gap],
    missing_records = round(dt[gap] / interval_s) - 1,
    on_grid = dt[gap] %% interval_s == 0
  )

  short_rows <- which(dt > 0 & dt < interval_s & !in_rate)
  short <- data.frame(row = short_rows + 1, previous = date[short_rows], date = date[short_rows + 1], interval_s = dt[short_rows])

  # Records per day in the record's time zone, including days with none
  if ("date_only" %in% names(archive)) {
    day <- as.integer(archive$date_only)
  } else {
    first <- as.Date(min(date), tz = time_zone)
    midnights <- as.POSIXct(format(seq(first, as.Date(max(date), tz = time_zone) + 1, by = "day")), tz = time_zone)
    day <- as.integer(first) - 1L + findInterval(as.numeric(date), as.numeric(midnights))
  }
  first_day <- min(day)
  records <- tabulate(day - first_day + 1L)
  expected <- 86400 / interval_s
  days <- data.frame(
    date = as.Date(first_day + seq_along(records) - 1L),
    records = records,
    expected = expected,
    percent = 100 * records / expected
  )

  structure(
    list(
      interval_s = interval_s,
      regular = all(dt == interval_s),
      n_records = length(date),
      n_no_depth = n_no_depth,
      time_zone = time_zone,
      intervals = intervals,
      duplicates = duplicates,
      out_of_order = out_of_order,
      rate_changes = rate_changes,
      gaps = gaps,
      short = short,
      days = days
    ),
    class = "sampling_report"
  )
}

#' @rdname sampling_report
#' @param x A 'sampling_report' object.
#' @param max_rows Numerical. Maximum number of each kind of irregularity, and
#'   of incomplete days, to list when printing. Defaults to 5.
#' @param ... Not used.
#' @export
print.sampling_report <- function(x, max_rows = 5, ...) {
  fmt_time <- function(t) format(t, "%Y-%m-%d %H:%M:%S", tz = x$time_zone)
  fmt_num <- function(n) format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
  # Percentages rounded down, so that a nearly complete 99.999% shows as 99.99%, not 100%
  fmt_pct <- function(p) paste0(trimws(formatC(floor(p * 100) / 100, format = "fg", digits = 4)), "%")
  records <- function(n) paste(fmt_num(n), if (n == 1) "record" else "records")

  # Lists up to max_rows lines of one kind of irregularity under its heading
  section <- function(heading, lines) {
    if (length(lines) == 0) {
      return()
    }
    cat("  ", heading, ": ", fmt_num(length(lines)), "\n", sep = "")
    cat(paste0("    ", utils::head(lines, max_rows), "\n"), sep = "")
    if (length(lines) > max_rows) {
      cat("    ... and ", fmt_num(length(lines) - max_rows), " more\n", sep = "")
    }
  }

  main_pct <- x$intervals$percent[x$intervals$interval_s == x$interval_s]
  if (length(main_pct) == 0) main_pct <- 0

  cat("Sampling report: ", records(x$n_records), " over ", nrow(x$days), " days (",
    format(min(x$days$date)), " to ", format(max(x$days$date)), ", ", x$time_zone, ")\n",
    sep = ""
  )
  if (x$n_no_depth > 0) cat(records(x$n_no_depth), " with no depth were left out.\n", sep = "")
  cat("Expected interval: ", x$interval_s, " s (", fmt_pct(main_pct), " of intervals)\n", sep = "")

  if (x$regular) {
    cat("Regular: every interval is ", x$interval_s, " s.\n", sep = "")
  } else {
    cat("Irregular sampling found:\n")
    # paste() of zero-length columns gives one empty string, so skip empty tables
    if (nrow(x$duplicates) > 0) {
      section("Duplicate times", paste("Two records at", fmt_time(x$duplicates$date)))
    }
    if (nrow(x$out_of_order) > 0) {
      section("Out of order", paste(fmt_time(x$out_of_order$date), "comes after", fmt_time(x$out_of_order$previous)))
    }
    if (nrow(x$rate_changes) > 0) {
      section("Rate changes", sprintf(
        "%s s from %s to %s (%s intervals)",
        x$rate_changes$interval_s, fmt_time(x$rate_changes$start), fmt_time(x$rate_changes$end), fmt_num(x$rate_changes$intervals)
      ))
    }
    if (nrow(x$gaps) > 0) {
      section("Gaps", paste0(
        vapply(x$gaps$length_s, format_seconds, character(1)), " from ", fmt_time(x$gaps$start), " to ", fmt_time(x$gaps$end),
        " (", vapply(x$gaps$missing_records, records, character(1)), " missing",
        ifelse(x$gaps$on_grid, "", ", not a whole number of intervals"), ")"
      ))
    }
    if (nrow(x$short) > 0) {
      section("Short intervals", paste0(x$short$interval_s, " s from ", fmt_time(x$short$previous), " to ", fmt_time(x$short$date)))
    }
  }

  incomplete <- x$days[x$days$percent != 100, , drop = FALSE]
  if (nrow(incomplete) > 0) {
    cat("Days without the expected ", records(x$days$expected[1]), ": ", nrow(incomplete), " of ", nrow(x$days), "\n", sep = "")
    cat(paste0(
      "    ", utils::head(paste0(format(incomplete$date), ": ", vapply(incomplete$records, records, character(1)), " (", fmt_pct(incomplete$percent), ")"), max_rows),
      "\n"
    ), sep = "")
    if (nrow(incomplete) > max_rows) {
      cat("    ... and ", fmt_num(nrow(incomplete) - max_rows), " more\n", sep = "")
    }
  }
  invisible(x)
}

#' Format a number of seconds as seconds, minutes, hours or days
#' @noRd
format_seconds <- function(s) {
  if (s < 120) {
    paste0(s, " s")
  } else if (s < 7200) {
    paste0(signif(s / 60, 3), " min")
  } else if (s < 172800) {
    paste0(signif(s / 3600, 3), " h")
  } else {
    paste0(signif(s / 86400, 3), " days")
  }
}
