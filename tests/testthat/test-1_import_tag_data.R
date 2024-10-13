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
    output_folder = tempdir()
  )

  # Check if the output is a dataframe
  expect_true(is.data.frame(archive_days))

  # Check the dimensions of the dataframe
  expect_true(nrow(archive_days) >= 1) # one or more observations
  expect_length(names(archive_days), 3) # three columns

  # Check the names of the columns
  expect_equal(names(archive_days), c("date", "depth", "date_only"))

  # Check that archive_days.Rds file is created in the save directory
  expected_filepath <- file.path(tempdir(), "data/archive_days.rds")
  expect_true(file.exists(expected_filepath))


  ## Test the plot_TDR() function

  # Set test to use example dataset
  tag <- "data"

  filepath <- system.file("extdata", package = "FishDiveR")

  TDR_plot <- plot_TDR(
    tag_ID = tag,
    data_folder = filepath,
    every_nth = 10, # 1-minute depth sampling interval. Plot every 10th data point = 10 minutes.
    every_s = 0,
    plot_size = c(12, 6),
    Y_lim = c(0, 275, 50),
    date_breaks = "48 hour",
    dpi = 30,
    output_folder = tempdir()
  )

  TDR_plot <- plot_TDR(
    tag_ID = tag,
    data_folder = filepath,
    every_nth = 10,
    every_s = 600, # plot every 600th second (10 minutes)
    plot_size = c(12, 6),
    Y_lim = c(0, 275, 50),
    date_breaks = "48 hour",
    dpi = 30,
    output_folder = tempdir()
  )

  # Check that the TDR file is created in the save directory
  expected_filepath <- file.path(tempdir(), "data", paste0(tag, "_archive.png"))
  expect_true(file.exists(expected_filepath))
})
