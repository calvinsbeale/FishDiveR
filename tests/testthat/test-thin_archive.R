test_that("thin_archive() averages records into longer intervals", {
  archive_days <- readRDS(system.file("extdata", "data", "archive_days.rds", package = "FishDiveR"))
  archive_days$temp <- seq_len(nrow(archive_days))

  thinned <- thin_archive(archive_days, interval_s = 300)

  expect_equal(nrow(thinned), nrow(archive_days) / 5)
  expect_equal(names(thinned), names(archive_days))
  expect_equal(attr(thinned, "time_zone"), attr(archive_days, "time_zone"))
  expect_equal(as.numeric(difftime(thinned$date[2], thinned$date[1], units = "secs")), 300)
  expect_equal(thinned$date[1], archive_days$date[1])
  expect_equal(thinned$depth[1], mean(archive_days$depth[1:5]))
  expect_equal(thinned$temp[2], mean(archive_days$temp[6:10]))

  # Days are kept whole
  expect_equal(as.vector(table(thinned$date_only)), rep(288L, length(unique(archive_days$date_only))))

  # Same interval returns the archive unchanged
  expect_identical(thin_archive(archive_days, 60), archive_days)

  expect_error(thin_archive(archive_days, 90), "multiple of the sampling interval")
  expect_error(thin_archive(archive_days, 420), "divide a day evenly")
  expect_error(thin_archive(archive_days, 0), "positive whole number")
})

test_that("wavelets and wavelet statistics work on a thinned archive", {
  archive_days <- readRDS(system.file("extdata", "data", "archive_days.rds", package = "FishDiveR"))
  thinned <- thin_archive(archive_days, interval_s = 120)
  output_folder <- file.path(tempdir(), "thin_wavelet")
  unlink(output_folder, recursive = TRUE)

  my.w <- suppressWarnings(create_wavelet(
    archive = thinned, tag_ID = "thin", lower_period_mins = 30, upper_period_hours = 24,
    output = TRUE, output_folder = output_folder, plot_wavelet = TRUE, interactive_mode = FALSE
  ))
  expect_equal(readRDS(file.path(output_folder, "thin", "1_Wavelets", "wavelet_meta.rds"))$sampling_s, 120)

  # Redrawing with the full-resolution archive keeps the wavelet's own interval
  suppressWarnings(create_wavelet(
    archive = archive_days, tag_ID = "thin", load_existing_wavelet = TRUE, lower_period_mins = 30,
    upper_period_hours = 24, output = TRUE, output_folder = output_folder, plot_wavelet = TRUE, interactive_mode = FALSE
  ))
  expect_equal(readRDS(file.path(output_folder, "thin", "1_Wavelets", "wavelet_meta.rds"))$sampling_s, 120)

  wave_stats <- create_wavelet_stats(my.w, tag_ID = "thin")
  expect_equal(length(unique(wave_stats$date_only)), length(unique(archive_days$date_only)))
})
