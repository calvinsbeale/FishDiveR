test_that("create_depth_stats() creates a csv file", {
  # Set test to use example dataset
  tag <- "data"

  # Read in example archive_days
  archive_days_file <- system.file("extdata", "data/archive_days.rds", package = "FishDiveR")
  archive_days <- readRDS(archive_days_file)

  # Run the function
  depthStats <- create_depth_stats(
    archive = archive_days,
    tag_ID = tag,
    diel = FALSE,
    sunrise_time = "06:00:00",
    sunset_time = "18:00:00",
    GPS = FALSE,
    sunset_type = NULL,
    output = TRUE,
    output_folder = tempdir(),
    verbose = TRUE
  )

  # Check that waveStats.csv file is created in the save directory
  expected_filepath <- file.path(tempdir(), tag, "3_Stats", paste0(tag, "_depthStats.csv"))
  expect_true(file.exists(expected_filepath))

  # Run the function
  depthStats <- create_depth_stats(
    archive = archive_days,
    tag_ID = tag,
    diel = TRUE,
    sunrise_time = "06:00:00",
    sunset_time = "18:00:00",
    GPS = FALSE,
    sunset_type = NULL,
    output = TRUE,
    output_folder = tempdir(),
    verbose = TRUE
  )

  # Check that waveStats.csv file is created in the save directory
  expected_filepath <- file.path(tempdir(), tag, "3_Stats", paste0(tag, "_depthStats.csv"))
  expect_true(file.exists(expected_filepath))

  GPS_file <- system.file("extdata", "data/GPS.csv", package = "FishDiveR")

  # Run the function
  depthStats <- create_depth_stats(
    archive = archive_days,
    tag_ID = tag,
    diel = TRUE,
    sunrise_time = "06:00:00",
    sunset_time = "18:00:00",
    GPS = GPS_file,
    sunset_type = "civil",
    output = TRUE,
    output_folder = tempdir(),
    verbose = TRUE
  )

  # Check that waveStats.csv file is created in the save directory
  expected_filepath <- file.path(tempdir(), tag, "3_Stats", paste0(tag, "_depthStats.csv"))
  expect_true(file.exists(expected_filepath))
})

test_that("diel statistics keep days spent at the surface, matched by date", {
  archive_days <- readRDS(system.file("extdata", "data", "archive_days.rds", package = "FishDiveR"))
  diel_stats <- function(archive) {
    create_depth_stats(archive, tag_ID = "data", diel = TRUE, sunrise_time = "06:00:00", sunset_time = "18:00:00")
  }
  # Without an output folder, nothing is saved
  expected <- diel_stats(archive_days)
  expect_equal(nrow(expected), 10)

  surface <- archive_days
  surface$depth[surface$date_only == as.Date("2000-01-03")] <- 0
  stats <- diel_stats(surface)
  expect_equal(nrow(stats), 10)
  diel_cols <- c("st_mean_diff", "st_sd_diff", "st_range_diff", "st_absVV_diff", "st_dscVV_diff", "st_ascVV_diff", "surf_prop_diff")
  expect_true(all(unlist(stats[3, diel_cols, with = FALSE]) == 0))
  # The other days keep their own diel statistics. (The first vertical speed
  # of the next day is from the surface day's last record, so changes.)
  depth_cols <- c("st_mean_diff", "st_sd_diff", "st_range_diff", "surf_prop_diff")
  expect_equal(stats[-3, depth_cols, with = FALSE], expected[-3, depth_cols, with = FALSE])
  expect_equal(stats[-(3:4), diel_cols, with = FALSE], expected[-(3:4), diel_cols, with = FALSE])
})
