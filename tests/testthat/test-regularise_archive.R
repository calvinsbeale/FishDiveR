archive_days <- readRDS(system.file("extdata", "data", "archive_days.rds", package = "FishDiveR"))

# Regularise without the warnings about gaps
regular_quietly <- function(...) suppressWarnings(regularise_archive(...))

test_that("regularise_archive() leaves a regular record unchanged", {
  expect_no_warning(regular <- regularise_archive(archive_days))
  expect_equal(regular$date, archive_days$date)
  expect_equal(regular$depth, archive_days$depth)
  expect_equal(regular$date_only, archive_days$date_only)
  expect_false(any(regular$filled))
  expect_false(any(regular$long_gap))
  expect_equal(attr(regular, "interval_s"), 60)
  expect_true(all(attr(regular, "day_coverage")$coverage == 100))
  expect_equal(nrow(attr(regular, "gaps")), 0)
  expect_true(attr(regular, "sampling_report")$regular)
})

test_that("regularise_archive() fills short gaps and marks long ones", {
  # A 4 minute gap (3 records missing) and a 5 hour gap (301 missing)
  gappy <- archive_days[-c(100:102, 5000:5300), ]
  expect_warning(
    expect_warning(regular <- regularise_archive(gappy), "Filled 1 gap of up to 4 min by interpolation \\(3 records\\)"),
    "1 gap longer than 5 min \\(the longest 5.03 h\\) was not filled for analysis"
  )
  expect_equal(nrow(regular), nrow(archive_days))
  expect_equal(sum(regular$filled), 304)
  expect_equal(sum(regular$long_gap), 301)
  expect_true(all(regular$long_gap[5000:5300]))
  expect_false(any(regular$long_gap[100:102]))

  # Short gaps are filled by straight lines between the records either side
  expect_equal(regular$depth[101], mean(archive_days$depth[c(99, 103)]))
  # Real records are unchanged
  expect_equal(regular$depth[!regular$filled], gappy$depth)

  gaps <- attr(regular, "gaps")
  expect_equal(gaps$missing_records, c(3, 301))
  expect_equal(gaps$length_s, c(240, 18120))
  expect_equal(gaps$filled, c(TRUE, FALSE))
  expect_equal(gaps$start[2], archive_days$date[4999])

  # Day 4 has 1,139 of 1,440 records: kept at 70%, dropped at 95%
  coverage <- attr(regular, "day_coverage")
  expect_equal(coverage$records[4], 1139)
  expect_equal(coverage$coverage[4], 79.1)
  expect_true(all(coverage$kept))
  expect_false(attr(regular_quietly(gappy, min_coverage = 95), "day_coverage")$kept[4])
  warnings <- capture_warnings(regularise_archive(gappy, min_coverage = 95))
  expect_match(
    warnings, "1 day has less than 95% of its records and is left out of the statistics: 2000-01-04 \\(79.1%\\)",
    all = FALSE
  )

  # A longer fill limit fills the 5 hour gap too
  regular <- regular_quietly(gappy, max_fill_mins = 360)
  expect_false(any(regular$long_gap))
  expect_true(all(attr(regular, "gaps")$filled))
})

test_that("regularise_archive() resolves duplicates, rate changes and late starts", {
  archive <- archive_days[1:2880, ]
  # A duplicate time, as in some Wildlife Computers archives
  archive$date[500] <- archive$date[499]
  # The last 100 records at 75 s instead of 60 s
  n <- nrow(archive)
  archive$date[(n - 99):n] <- archive$date[n - 100] + 75 * (1:100)
  # Records out of order
  archive <- archive[c(1:1000, 1002, 1001, 1003:n), ]
  regular <- regular_quietly(archive)

  expect_false(is.unsorted(regular$date))
  expect_true(all(diff(as.numeric(regular$date)) == 60))
  expect_true(attr(regular, "sampling_report")$regular == FALSE)
  # The 75 s records are spread over the 60 s grid, filling the gaps between
  expect_gt(sum(regular$filled[2780:nrow(regular)]), 0)
  expect_false(any(regular$long_gap[2780:2880]))

  # A record starting at 06:00 has a long gap from midnight
  late <- archive_days[361:2880, ]
  regular <- regular_quietly(late)
  expect_equal(regular$date[1], archive_days$date[1])
  expect_true(all(regular$long_gap[1:360]))
  expect_equal(attr(regular, "day_coverage")$coverage[1], 75)
})

test_that("regularise_archive() interpolates temperature and checks its arguments", {
  archive <- archive_days[-(200:202), ]
  archive$temp <- seq(20, 25, length.out = nrow(archive))
  regular <- regular_quietly(archive)
  expect_false(anyNA(regular$temp))
  expect_equal(regular$temp[201], mean(archive$temp[c(199, 200)]))

  expect_error(regularise_archive(archive_days, max_fill_mins = 361), "between 0 and 360")
  expect_error(regularise_archive(archive_days, max_fill_mins = -1), "between 0 and 360")
  expect_error(regularise_archive(archive_days, min_coverage = 101), "between 0 and 100")
  expect_error(regularise_archive(archive_days[1, ]), "at least two records")
  expect_error(regularise_archive(data.frame(date = 1:3, depth = 1:3)), "POSIXct")
})

test_that("thin_archive() keeps the gap flags and day coverage", {
  regular <- regular_quietly(archive_days[-(5000:5300), ])
  thinned <- thin_archive(regular, 300)
  expect_equal(attr(thinned, "interval_s"), 300)
  expect_equal(attr(thinned, "day_coverage"), attr(regular, "day_coverage"))
  # 5 minute blocks with any record in the long gap are in it
  expect_equal(sum(thinned$long_gap), 61)
})

test_that("wavelet statistics leave out power near long gaps, and optionally the ends", {
  skip_on_cran()
  gappy <- regular_quietly(archive_days[-(5000:5300), ])
  wavelet <- suppressWarnings(create_wavelet(
    gappy, "gappy", lower_period_mins = 30, upper_period_hours = 24, plot_wavelet = FALSE, interactive_mode = FALSE
  ))
  expect_equal(attr(wavelet, "long_gaps"), data.frame(first = 5000L, last = 5300L))
  expect_equal(attr(wavelet, "min_coverage"), 70)

  stats <- create_wavelet_stats(wavelet, tag_ID = "gappy")
  dropped <- attr(stats, "dropped_days")
  # The gap is on day 4: its 24 h power reaches about 32 h either side
  expect_equal(dropped$date, as.Date(c("2000-01-03", "2000-01-04", "2000-01-05")))
  expect_match(dropped$reason[1], "24 h period")
  expect_equal(stats$date_only, as.Date(c("2000-01-01", "2000-01-02", paste0("2000-01-0", 6:9), "2000-01-10")))
  expect_false(anyNA(stats[, setdiff(names(stats), c("tag_ID", "date_only"))]))

  # The same days without the gap are unaffected by it
  regular <- regularise_archive(archive_days)
  full_wavelet <- suppressWarnings(create_wavelet(
    regular, "full", lower_period_mins = 30, upper_period_hours = 24, plot_wavelet = FALSE, interactive_mode = FALSE
  ))
  full <- create_wavelet_stats(full_wavelet, tag_ID = "full")
  expect_equal(nrow(full), 10)
  expect_equal(nrow(attr(full, "dropped_days")), 0)
  # WaveletComp scales the whole record by its standard deviation, which the
  # filled values change, so power is rescaled to the variance of the real
  # records. Far from the gap, power then differs from the full record's only
  # by the ratio of their variances.
  real_var <- var(gappy$depth[!gappy$long_gap])
  expect_equal(attr(wavelet, "power_scale"), var(gappy$depth) / real_var)
  ratio <- var(archive_days$depth) / real_var
  day8 <- function(s, col) s[[col]][s$date_only == as.Date("2000-01-08")]
  for (col in c("p1_Power_mean", "p30_Power_mean")) {
    expect_equal(day8(stats, col), day8(full, col) * ratio, tolerance = 1e-3)
  }
  expect_equal(day8(stats, "p30_Amplitude_mean"), day8(full, "p30_Amplitude_mean") * sqrt(ratio), tolerance = 1e-3)

  # Masking the ends leaves out the first and last two days
  ends <- create_wavelet_stats(full_wavelet, tag_ID = "full", mask_ends = TRUE)
  expect_equal(attr(ends, "dropped_days")$date, as.Date(c("2000-01-01", "2000-01-02", "2000-01-09", "2000-01-10")))

  # Days with too few records are left out, as set at import
  stats_95 <- create_wavelet_stats(wavelet, tag_ID = "gappy", min_coverage = 0)
  expect_equal(nrow(stats_95), 10)
  expect_error(create_wavelet_stats(wavelet, tag_ID = "gappy", mask_ends = NA), "mask_ends")
})

test_that("depth statistics leave out long gaps and low coverage days", {
  gappy <- archive_days[-(5000:5300), ]
  regular <- regular_quietly(gappy)
  stats <- create_depth_stats(regular, tag_ID = "gappy")
  day4 <- gappy[gappy$date_only == as.Date("2000-01-04"), ]
  expect_equal(stats$depth.mean[4], mean(day4$depth))
  expect_equal(stats$depth.max[4], max(day4$depth))

  # Vertical speed is not measured across the gap
  speeds <- diff(day4$depth) / 60
  speeds <- speeds[diff(as.numeric(day4$date)) == 60]
  expect_equal(stats$max.dsc_vv[4], max(c(0, speeds)))

  # Days below the coverage set at import are left out
  low <- regular_quietly(gappy, min_coverage = 95)
  expect_equal(nrow(create_depth_stats(low, tag_ID = "gappy")), 9)
})

test_that("combine_data() matches depth statistics to the PCA scores' days", {
  folder <- file.path(tempdir(), "combine_days")
  dir.create(file.path(folder, "x", "3_Stats"), recursive = TRUE, showWarnings = FALSE)
  depth <- data.frame(date_only = as.Date("2000-01-01") + 0:2, depth.mean = c(10, 20, 30))
  utils::write.csv(depth, file.path(folder, "x", "3_Stats", "x_depthStats.csv"), row.names = FALSE)

  # Scores for two of the days, in another order
  scores <- data.frame(PC1 = c(3, 1))
  attr(scores, "days") <- data.frame(tag_ID = "x", date_only = as.Date(c("2000-01-03", "2000-01-01")))
  combined <- combine_data(tag_vector = "x", data_folder = folder, pc_scores = scores)
  expect_equal(combined$depth.mean, c(30, 10))
  expect_equal(combined$PC1, c(3, 1))

  attr(scores, "days")$date_only[1] <- as.Date("2000-01-05")
  expect_error(combine_data(tag_vector = "x", data_folder = folder, pc_scores = scores), "missing for 1 day")

  # Without days, the rows must match in number
  attr(scores, "days") <- NULL
  expect_error(combine_data(tag_vector = "x", data_folder = folder, pc_scores = scores), "3 days but the PCA scores have 2")
})
