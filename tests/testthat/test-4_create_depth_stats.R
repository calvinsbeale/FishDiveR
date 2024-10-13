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
    output_folder = tempdir()
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
    output_folder = tempdir()
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
    output_folder = tempdir()
  )

  # Check that waveStats.csv file is created in the save directory
  expected_filepath <- file.path(tempdir(), tag, "3_Stats", paste0(tag, "_depthStats.csv"))
  expect_true(file.exists(expected_filepath))
})
