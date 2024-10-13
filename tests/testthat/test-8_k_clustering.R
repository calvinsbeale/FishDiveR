test_that("k_clustering creates a rds file", {
  # Load combined data first using the example dataset
  kmeans_data_file <- system.file("extdata", "data/5_k-means/combined_stats.rds", package = "FishDiveR")
  kmeans_data <- readRDS(kmeans_data_file)

  # Set RGL to use null device to suppress graphical output to pass CRAN checks
  Sys.setenv(RGL_USE_NULL = TRUE)

  # Run k_clustering function
  kmeans_result <- k_clustering(
    kmeans_data = kmeans_data,
    standardise = FALSE,
    k = 4,
    polygon = FALSE,
    output_folder = tempdir()
  )

  # Check if the output is a dataframe
  expect_true(is.list(kmeans_result))

  # Run k_clustering function
  kmeans_result <- k_clustering(
    kmeans_data = kmeans_data,
    standardise = TRUE,
    k = 4,
    polygon = TRUE,
    output_folder = tempdir()
  )

  # Check if the output is a dataframe
  expect_true(is.list(kmeans_result))

  # Check that kmeans_result.rds file is created in the save directory
  expected_filepath <- file.path(tempdir(), "data/5_k-means/kmeans_result.rds")
  expect_true(file.exists(expected_filepath))

  # Set RGL to use null device to suppress graphical output to pass CRAN checks
  Sys.setenv(RGL_USE_NULL = FALSE)
})
