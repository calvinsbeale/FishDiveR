test_that("process_tag_data() imports a tag archive", {
  # Load the file path to the tag data
  archive_file <- system.file("extdata", "data/data-Archive.csv", package = "FishDiveR")

  archive_days <- import_tag_data(
    tag_ID = "data",
    tag_deploy_UTC = "2000-01-01 00:00:00",
    tag_release_UTC = "2000-01-11 23:59:00",
    archive = archive_file,
    date_time_col = 1,
    depth_col = 2,
    temp_col = NA,
    time_zone = "UTC",
    output = TRUE,
    output_folder = tempdir(),
    verbose = TRUE
  )

  # Check if the output is a dataframe
  expect_true(is.data.frame(archive_days))

  # Check the dimensions of the dataframe
  expect_true(nrow(archive_days) >= 1) # one or more observations
  expect_length(names(archive_days), 5) # five columns

  # Check the names of the columns
  expect_equal(names(archive_days), c("date", "depth", "date_only", "filled", "long_gap"))
  # A regular record has no gaps
  expect_false(any(archive_days$filled))
  expect_true(all(attr(archive_days, "day_coverage")$kept))

  # Check that archive_days.Rds file is created in the save directory
  expected_filepath <- file.path(tempdir(), "data/archive_days.rds")
  expect_true(file.exists(expected_filepath))


  ## Test the plot_TDR() function

  # Set test to use example dataset
  rds <- "data/archive_days.rds"

  filepath <- system.file("extdata", package = "FishDiveR")

  TDR_plot <- plot_TDR(
    rds_file = rds,
    data_folder = filepath,
    every_nth = 10, # 1-minute depth sampling interval. Plot every 10th data point = 10 minutes.
    every_s = 0,
    plot_size = c(12, 6),
    X_lim = NULL,
    Y_lim = c(0, 275, 50),
    date_breaks = "48 hour",
    dpi = 30,
    output = TRUE,
    output_folder = file.path(tempdir(), "data"),
    verbose = TRUE
  )

  TDR_plot <- plot_TDR(
    rds_file = rds,
    data_folder = filepath,
    every_nth = 10,
    every_s = 600, # plot every 600th second (10 minutes)
    plot_size = c(12, 6),
    X_lim = c("2000-01-02", "2000-01-10"),
    Y_lim = c(0, 275, 50),
    date_breaks = "48 hour",
    dpi = 30,
    output = TRUE,
    output_folder = file.path(tempdir(), "data"),
    verbose = TRUE
  )

  # Check that the TDR file is created in the save directory
  expected_filepath <- file.path(tempdir(), "data", "tag_archive.png")
  expect_true(file.exists(expected_filepath))
})

test_that("plot_TDR() minmax_bins keeps the shallowest and deepest records", {
  filepath <- system.file("extdata", package = "FishDiveR")
  archive_days <- readRDS(file.path(filepath, "data/archive_days.rds"))

  plot_data <- plot_TDR(
    rds_file = "data/archive_days.rds",
    data_folder = filepath,
    Y_lim = c(0, 300, 50),
    date_breaks = "48 hour",
    minmax_bins = 100
  )

  expect_lte(nrow(plot_data), 200)
  expect_equal(max(plot_data$depth), max(archive_days$depth))
  expect_equal(min(plot_data$depth), min(archive_days$depth))
  expect_false(is.unsorted(plot_data$date))
  expect_s3_class(attr(plot_data, "TDR_plot"), "ggplot")

  expect_error(plot_TDR("data/archive_days.rds", data_folder = filepath, minmax_bins = -1), "minmax_bins")
})

test_that("import_tag_data() puts irregular records on a regular grid, with warnings", {
  archive_file <- system.file("extdata", "data/data-Archive.csv", package = "FishDiveR")

  import_args <- list(
    tag_ID = "data",
    tag_deploy_UTC = "2000-01-01 00:00:00",
    tag_release_UTC = "2000-01-11 23:59:00",
    archive = archive_file,
    time_zone = "UTC",
    verbose = FALSE
  )

  # Example data has a constant 60 s interval
  expect_no_warning(do.call(import_tag_data, import_args))

  # Remove records to create a gap in the time-series
  irregular_file <- file.path(tempdir(), "irregular-Archive.csv")
  archive <- utils::read.csv(archive_file)
  utils::write.csv(archive[-(5000:5004), ], irregular_file, row.names = FALSE)

  # The 6 minute gap is longer than the 5 minute fill limit
  import_args$archive <- irregular_file
  expect_warning(
    archive <- do.call(import_tag_data, import_args),
    "1 gap longer than 5 min \\(the longest 6 min\\) was not filled for analysis"
  )
  expect_true(all(diff(as.numeric(archive$date)) == 60))
  expect_equal(sum(archive$long_gap), 5)
  expect_false(attr(archive, "sampling_report")$regular)

  # Filled with a longer limit
  import_args$max_fill_mins <- 10
  expect_warning(archive <- do.call(import_tag_data, import_args), "Filled 1 gap of up to 6 min")
  expect_false(any(archive$long_gap))
  expect_equal(sum(archive$filled), 5)

  # The gap settings are saved with the import
  import_args$output <- TRUE
  import_args$output_folder <- file.path(tempdir(), "gap_settings")
  suppressWarnings(do.call(import_tag_data, import_args))
  settings <- readRDS(file.path(tempdir(), "gap_settings", "data", "import_settings.rds"))
  expect_equal(settings$max_fill_mins, 10)
  expect_equal(settings$min_coverage, 70)
})

test_that("import_tag_data() reports temperatures when imported", {
  temp_file <- file.path(tempdir(), "temp-log-Archive.csv")
  archive <- utils::read.csv(system.file("extdata", "data/data-Archive.csv", package = "FishDiveR"))
  archive$temp <- 20 + archive$depth / 100
  archive$temp[3] <- NA
  utils::write.csv(archive, temp_file, row.names = FALSE)

  import <- function() {
    import_tag_data(
      tag_ID = "data", tag_deploy_UTC = "2000-01-01 00:00:00", tag_release_UTC = "2000-01-11 23:59:00",
      archive = temp_file, temp_col = 3, time_zone = "UTC", verbose = TRUE
    )
  }
  messages <- paste(capture_messages(archive_days <- import()), collapse = "")
  expect_match(messages, "Mean temperature = [0-9.]+ Minimum = 20 Maximum = 22.6")
  expect_match(messages, "Records with no temperature: 1")
  expect_true("temp" %in% names(archive_days))
})
