archive_days <- readRDS(system.file("extdata", "data", "archive_days.rds", package = "FishDiveR"))

# Two days of 5 s records, as from import_tag_data()
regular_archive <- function() {
  date <- seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"), by = 5, length.out = 2 * 17280)
  archive <- data.frame(date = date, depth = 10, date_only = as.Date(date, tz = "UTC"))
  attr(archive, "time_zone") <- "UTC"
  archive
}

test_that("sampling_report() finds nothing wrong with a regular record", {
  report <- sampling_report(archive_days)
  expect_s3_class(report, "sampling_report")
  expect_true(report$regular)
  expect_equal(report$interval_s, 60)
  expect_equal(report$n_records, nrow(archive_days))
  expect_equal(nrow(report$intervals), 1)
  for (table in c("duplicates", "out_of_order", "rate_changes", "gaps", "short")) {
    expect_equal(nrow(report[[table]]), 0)
  }
  expect_true(all(report$days$percent == 100))
  expect_output(print(report), "Regular: every interval is 60 s")
  expect_output(print(report), "60 s (100% of intervals)", fixed = TRUE)
})

test_that("sampling_report() finds gaps and incomplete days", {
  report <- sampling_report(archive_days[-(5000:5004), ])
  expect_false(report$regular)
  expect_equal(nrow(report$gaps), 1)
  expect_equal(report$gaps$length_s, 360)
  expect_equal(report$gaps$missing_records, 5)
  expect_true(report$gaps$on_grid)
  expect_equal(report$gaps$start, archive_days$date[4999])
  expect_equal(sum(report$days$percent < 100), 1)

  # Printed with the times of each gap and the incomplete day
  printed <- paste(capture.output(print(report)), collapse = "\n")
  gap_start <- format(archive_days$date[4999], "%Y-%m-%d %H:%M:%S")
  expect_match(printed, "Gaps: 1\n", fixed = TRUE)
  expect_match(printed, paste0("6 min from ", gap_start, " to .* \\(5 records missing\\)"))
  expect_match(printed, "Days without the expected 1,440 records: 1 of 10")
  expect_match(printed, ": 1,435 records (99.65%)", fixed = TRUE)
  # Nearly all intervals are 60 s, but not shown as 100%
  expect_match(printed, "60 s (99.99% of intervals)", fixed = TRUE)
  expect_no_match(printed, "\\$")
})

test_that("sampling_report() prints at most max_rows of each irregularity", {
  report <- sampling_report(archive_days[-seq(100, 1000, by = 100), ])
  expect_equal(nrow(report$gaps), 10)
  printed <- paste(capture.output(print(report)), collapse = "\n")
  expect_match(printed, "Gaps: 10")
  expect_match(printed, "... and 5 more", fixed = TRUE)
  expect_equal(lengths(regmatches(printed, gregexpr("2 min from", printed))), 5)
  expect_no_match(paste(capture.output(print(report, max_rows = 20)), collapse = "\n"), "more")
})

test_that("sampling_report() finds the duplicate and gap of a shifted hour", {
  # As in some Wildlife Computers archives: two records share a time, and the
  # record before the next hour is missing
  archive <- regular_archive()
  hour <- which(archive$date == as.POSIXct("2020-01-01 20:00:00", tz = "UTC"))
  archive$date[hour + 1] <- archive$date[hour]
  archive$date[(hour + 2):(hour + 719)] <- archive$date[(hour + 2):(hour + 719)] - 5
  report <- sampling_report(archive)

  expect_false(report$regular)
  expect_equal(report$duplicates$date, as.POSIXct("2020-01-01 20:00:00", tz = "UTC"))
  expect_equal(report$duplicates$row, hour + 1)
  expect_equal(report$gaps$start, as.POSIXct("2020-01-01 20:59:50", tz = "UTC"))
  expect_equal(report$gaps$missing_records, 1)
  expect_equal(nrow(report$rate_changes), 0)
  printed <- paste(capture.output(print(report)), collapse = "\n")
  expect_match(printed, "Duplicate times: 1\n    Two records at 2020-01-01 20:00:00", fixed = TRUE)
  expect_match(printed, "Gaps: 1\n    10 s from 2020-01-01 20:59:50 to 2020-01-01 21:00:00 (1 record missing)", fixed = TRUE)
})

test_that("sampling_report() finds rate changes, short intervals and times out of order", {
  archive <- regular_archive()
  n <- nrow(archive)

  # The last 100 records at 75 s, as after a tag's release
  archive$date[(n - 99):n] <- archive$date[n - 100] + 75 * (1:100)
  # One record 2 s early, making a 3 s and a 7 s interval
  archive$date[1000] <- archive$date[1000] - 2
  # Two records swapped
  archive$date[2000:2001] <- archive$date[2001:2000]
  report <- sampling_report(archive)

  expect_equal(nrow(report$rate_changes), 1)
  expect_equal(report$rate_changes$interval_s, 75)
  expect_equal(report$rate_changes$intervals, 100)
  expect_equal(report$rate_changes$start, archive$date[n - 100])

  expect_equal(report$short$interval_s, 3)
  expect_equal(report$short$row, 1000)
  expect_false(report$gaps$on_grid[report$gaps$row == 1001])

  expect_equal(nrow(report$out_of_order), 1)
  expect_equal(report$out_of_order$row, 2001)

  printed <- paste(capture.output(print(report)), collapse = "\n")
  expect_match(printed, "Rate changes: 1")
  expect_match(printed, "75 s from")
  expect_match(printed, "Short intervals: 1")
  expect_match(printed, "Out of order: 1")
})

test_that("sampling_report() leaves out records with no depth and accepts an expected interval", {
  archive <- regular_archive()
  archive$depth[c(10, 20)] <- NA
  report <- sampling_report(archive)
  expect_equal(report$n_no_depth, 2)
  expect_equal(report$n_records, nrow(archive) - 2)
  expect_equal(nrow(report$gaps), 2)
  expect_output(print(report), "2 records with no depth were left out")

  # Records every 5 s checked against an expected 10 s
  report <- sampling_report(regular_archive(), interval_s = 10)
  expect_equal(report$interval_s, 10)
  expect_false(report$regular)
  expect_equal(report$days$expected[1], 8640)
  expect_equal(nrow(report$rate_changes), 1)
})

test_that("sampling_report() counts days in the time zone, including days with no records", {
  archive <- regular_archive()
  archive$date_only <- NULL
  attr(archive, "time_zone") <- NULL
  # Remove the second day and add a third
  archive <- archive[c(1:17280, 17280 + 1:17280), ]
  archive$date[17281:34560] <- archive$date[17281:34560] + 86400
  report <- sampling_report(archive)
  expect_equal(report$days$date, as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")))
  expect_equal(report$days$records, c(17280, 0, 17280))
  expect_equal(report$gaps$length_s, 86405)

  # Days follow the time zone attribute: each UTC day spans two days in Perth (UTC+8)
  attr(archive, "time_zone") <- "Australia/Perth"
  report <- sampling_report(archive)
  expect_equal(report$time_zone, "Australia/Perth")
  expect_equal(report$days$date, as.Date("2020-01-01") + 0:3)
  expect_equal(report$days$records, c(11520, 5760, 11520, 5760))
  expect_match(paste(capture.output(print(report)), collapse = "\n"), "Australia/Perth")
})

test_that("sampling_report() checks its arguments", {
  expect_error(sampling_report("archive"), "must be a data frame")
  expect_error(sampling_report(data.frame(date = 1:3)), "POSIXct")
  expect_error(sampling_report(archive_days[1, ]), "at least two records")
  expect_error(sampling_report(archive_days, interval_s = -1), "positive")
  expect_error(sampling_report(archive_days, min_run = 1), "at least 2")
})
