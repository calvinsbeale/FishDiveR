# Detecting and reading the format of dates and date-times, as in tag archives
# (import_tag_data()), GPS files (create_depth_stats()) and tag tables (the
# app).

#' Date and date-time formats that can be detected
#'
#' Dates are year-month-day, day/month/year, month/day/year or day month-name
#' year, separated by "-", "/", "." or spaces. Times are hh:mm:ss (with optional
#' decimal seconds) or hh:mm, before or after the date (ISO 8601 "T" and a
#' final "Z" are allowed).
#' @param type "datetime" for dates with times, "date" for dates (with or
#'   without times), or "any".
#' @returns A list of formats, each with a regular expression, a strptime()
#'   format, a description and its date order.
#' @noRd
date_format_candidates <- function(type = c("datetime", "date", "any")) {
  type <- match.arg(type)
  orders <- list(
    ymd = c(re = "\\d{4}SEP\\d{1,2}SEP\\d{1,2}", fmt = "%YSEP%mSEP%d", label = "year-month-day"),
    dmy = c(re = "\\d{1,2}SEP\\d{1,2}SEP\\d{4}", fmt = "%dSEP%mSEP%Y", label = "day/month/year"),
    mdy = c(re = "\\d{1,2}SEP\\d{1,2}SEP\\d{4}", fmt = "%mSEP%dSEP%Y", label = "month/day/year"),
    dbY = c(re = "\\d{1,2}SEP[A-Za-z]{3}SEP\\d{4}", fmt = "%dSEP%bSEP%Y", label = "day month-name year")
  )
  seps <- c("-", "/", ".", " ")
  times <- list(
    hms = c(re = "\\d{1,2}:\\d{2}:\\d{2}(\\.\\d+)?", fmt = "%H:%M:%OS", label = "hh:mm:ss"),
    hm = c(re = "\\d{1,2}:\\d{2}", fmt = "%H:%M", label = "hh:mm")
  )

  candidates <- list()
  add <- function(re, fmt, label, order) {
    candidates[[length(candidates) + 1]] <<- list(regex = paste0("^", re, "$"), format = fmt, label = label, order = order)
  }
  for (o in names(orders)) {
    for (sep in seps) {
      sep_re <- if (sep == ".") "\\." else if (sep == " ") " +" else sep
      date_re <- gsub("SEP", sep_re, orders[[o]][["re"]], fixed = TRUE)
      date_fmt <- gsub("SEP", sep, orders[[o]][["fmt"]], fixed = TRUE)
      label <- orders[[o]][["label"]]
      if (type %in% c("date", "any")) add(date_re, date_fmt, label, o)
      for (t in times) {
        # Date then time ("2024-03-05 08:30:00", "2024-03-05T08:30:00Z")
        add(paste0(date_re, "[ T]", t[["re"]], "Z?"), paste(date_fmt, t[["fmt"]]), paste(label, t[["label"]]), o)
        # Time then date, as in Wildlife Computers archives ("08:30:00 05-Mar-2024")
        add(paste0(t[["re"]], " +", date_re), paste(t[["fmt"]], date_fmt), paste(t[["label"]], label), o)
      }
    }
  }
  candidates
}

#' Read dates or date-times in a given strptime() format, in UTC
#'
#' Month names are read in English, whatever the computer's language. ISO 8601
#' "T" separators are read as spaces.
#' @noRd
parse_date_times <- function(x, format, tz = "UTC") {
  old <- Sys.getlocale("LC_TIME")
  on.exit(suppressWarnings(Sys.setlocale("LC_TIME", old)), add = TRUE)
  suppressWarnings(Sys.setlocale("LC_TIME", "C"))
  x <- sub("^(\\d{4}\\S*\\d)T(\\d)", "\\1 \\2", trimws(x))
  as.POSIXct(x, format = format, tz = tz)
}

#' Detect the format of a column of dates or date-times
#'
#' Tries each format of date_format_candidates() on a sample of the values:
#' the first 500, and 500 spread through the column. A format must match and
#' read at least 99% of them. Where both day/month and month/day orders read
#' every value (no day above 12), the one whose dates are in order and span the
#' time expected from the rows' spacing is used, if `ordered`. Otherwise
#' day/month is used, and `ambiguous` is TRUE.
#'
#' @param x Character vector of dates or date-times.
#' @param type "datetime", "date" or "any" (see date_format_candidates()).
#' @param ordered Whether the values are a time series in order, as in a tag
#'   archive, so their spacing can show the day and month order.
#' @param what Description of the values, for error messages.
#' @returns A list with the strptime() format, a description (label), an
#'   example value, and whether day and month order was ambiguous.
#' @noRd
detect_date_format <- function(x, type = "datetime", ordered = TRUE, what = "date-time column") {
  x <- trimws(as.character(x))
  n <- length(x)
  rows <- unique(c(seq_len(min(n, 500)), round(seq(1, n, length.out = min(n, 500)))))
  rows <- rows[!is.na(x[rows]) & nzchar(x[rows])]
  if (length(rows) == 0) stop("The ", what, " is empty.", call. = FALSE)
  sample <- x[rows]

  candidates <- Filter(function(cand) mean(grepl(cand$regex, sample)) >= 0.99, date_format_candidates(type))
  parsed <- lapply(candidates, function(cand) parse_date_times(sample, cand$format))
  readable <- vapply(parsed, function(p) mean(!is.na(p)) >= 0.99, logical(1))
  candidates <- candidates[readable]
  parsed <- parsed[readable]
  if (length(candidates) == 0) {
    stop(
      "Could not read the ", what, ", e.g. '", sample[1], "'. Dates must be year-month-day, day/month/year, ",
      "month/day/year or day month-name year (e.g. 2024-03-05, 05/03/2024, 05-Mar-2024), with times as hh:mm:ss ",
      "or hh:mm before or after the date. Check the column number.",
      call. = FALSE
    )
  }

  ambiguous <- FALSE
  if (length(candidates) > 1) {
    if (ordered) {
      score <- vapply(parsed, order_score, numeric(1), rows = rows)
      best <- which(score <= min(score) + 1e-9)
    } else {
      best <- seq_along(candidates)
    }
    ambiguous <- length(best) > 1
    # Prefer day/month where the order can't be told
    orders <- vapply(candidates[best], function(cand) cand$order, character(1))
    choice <- best[if ("dmy" %in% orders) which(orders == "dmy")[1] else 1]
    candidates <- candidates[choice]
  }
  list(format = candidates[[1]]$format, label = candidates[[1]]$label, example = sample[1], ambiguous = ambiguous)
}

#' How badly date-times fail to look like an ordered, evenly spaced series: the
#' share of the sample out of order, plus how far the time span differs from
#' the span expected from the spacing of the first rows. Lower is better.
#' @noRd
order_score <- function(parsed, rows) {
  t <- as.numeric(parsed)
  keep <- !is.na(t)
  t <- t[keep]
  rows <- rows[keep]
  if (length(t) < 2) return(0)
  o <- order(rows)
  t <- t[o]
  rows <- rows[o]
  out_of_order <- mean(diff(t) < 0)
  head_rows <- rows <= 500
  steps <- diff(t[head_rows]) / diff(rows[head_rows])
  steps <- steps[is.finite(steps) & steps > 0]
  if (length(steps) == 0) return(out_of_order)
  expected <- stats::median(steps) * (rows[length(rows)] - rows[1])
  actual <- t[length(t)] - t[1]
  if (actual <= 0) return(out_of_order + 10)
  out_of_order + abs(log(actual / expected))
}

#' Describe how dates were read, e.g. "day/month/year hh:mm:ss: '05/03/2024
#' 08:30:00' is 2024-03-05 08:30:00"
#' @noRd
describe_date_format <- function(detected) {
  example <- format(parse_date_times(detected$example, detected$format), "%Y-%m-%d %H:%M:%S")
  paste0(detected$label, ": '", detected$example, "' is ", example)
}
