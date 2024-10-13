test_that("create_wavelet() creates a WaveletComp object", {
  # Set test to use example dataset
  tag <- "data"

  archive_days_file <- system.file("extdata", "data/archive_days.rds", package = "FishDiveR")
  archive_days <- readRDS(archive_days_file)

  # Check if 'archive_days' is a data frame
  expect_true(is.data.frame(archive_days))

  my.wavelet <- create_wavelet(
    archive = archive_days,
    tag_ID = tag,
    wv_period_hours = 24,
    sampling_frequency = 60,
    suboctaves = 3,
    lower_period_mins = 360,
    upper_period_hours = 24,
    pval = TRUE,
    output_folder = file.path(tempdir(), "temp"),
    plot_wavelet = FALSE,
    max_period_ticks = 6,
    plot_width = 800,
    plot_height = 400,
    interactive_mode = FALSE
  )

  my.w <- create_wavelet(
    archive = archive_days,
    tag_ID = tag,
    wv_period_hours = 24,
    sampling_frequency = NULL,
    suboctaves = 12,
    lower_period_mins = 30,
    upper_period_hours = 24,
    pval = FALSE,
    output_folder = tempdir(),
    plot_wavelet = TRUE,
    max_period_ticks = 6,
    plot_width = 800,
    plot_height = 400,
    interactive_mode = FALSE
  )

  # Check if 'my.w' is a list
  expect_true(is.list(my.w))

  # Check the length of the list
  expect_length(my.w, 22)

  # Check if 'my.w' is of class "analyze.wavelet"
  expect_s3_class(my.w, "analyze.wavelet")

  # Check that certain values are not NULL
  expect_false(is.null(my.w$Power))

  # Check that my.w file is created in the save directory
  expected_filepath <- file.path(tempdir(), "data/1_Wavelets/data_wavelet.rds")
  expect_true(file.exists(expected_filepath))

  # Check that .png file is created in the save directory
  expected_filepath <- file.path(tempdir(), "data/2_Wavelet_Figures/data_wavelet_spectrum_no.p.png")
  expect_true(file.exists(expected_filepath))
})
