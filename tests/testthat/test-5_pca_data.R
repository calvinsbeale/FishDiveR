test_that("pca_data_inc_depth() creates a rds file", {
  # Set test to use example dataset
  tag <- "data"
  filepath <- system.file("extdata", package = "FishDiveR")

  # Run the function
  pc_data <- pca_data(
    tag_vector = tag,
    data_folder = filepath,
    phase_mean = TRUE,
    phase_variance = TRUE,
    power_mean = FALSE,
    power_variance = FALSE,
    mean_sq_power = TRUE,
    amplitude_mean = FALSE,
    amplitude_variance = TRUE,
    output_folder = tempdir()
  )

  # Run the function
  pc_data <- pca_data(
    tag_vector = tag,
    data_folder = filepath,
    phase_mean = FALSE,
    phase_variance = FALSE,
    power_mean = TRUE,
    power_variance = TRUE,
    mean_sq_power = FALSE,
    amplitude_mean = TRUE,
    amplitude_variance = FALSE,
    output_folder = tempdir()
  )

  # Check if the output is a dataframe
  expect_true(is.data.frame(pc_data))

  # Check that pc_data.Rds file is created in the save directory
  expected_filepath <- file.path(tempdir(), tag, "4_PCA/pc_data.rds")
  expect_true(file.exists(expected_filepath))
})
