test_that("create_wavelet_stats() creates a csv file", {
  # Set test to use example dataset
  tag <- "data"

  # Load the example wavelet
  wavelet_file <- system.file("extdata", "data/1_Wavelets/data_wavelet.rds", package = "FishDiveR")
  my.w <- readRDS(wavelet_file)

  waveStats <- create_wavelet_stats(
    wavelet = my.w,
    tag_ID = tag,
    output_folder = tempdir()
  )

  # Check that waveStats.csv file is created in the save directory
  expected_filepath <- file.path(tempdir(), tag, "3_Stats", paste0(tag, "_waveStats.csv"))
  expect_true(file.exists(expected_filepath))
})
