test_that("pca_results(), pca_scores() and combine_data create the correct files", {
  # Set test to use example dataset
  pc_data_file <- system.file("extdata", "data/4_PCA/pc_data.rds", package = "FishDiveR")
  pca_data <- readRDS(pc_data_file)

  # Run the function pca_results
  pc_results <- pca_results(
    pc_data = pca_data,
    standardise = FALSE,
    No_pcs = NULL,
    PCV = 99,
    plot_eigenvalues = FALSE,
    output_folder = tempdir(),
    interactive_mode = FALSE
  )

  # Check if the output is a list
  expect_true(is.list(pc_results))

  # Run the function pca_results
  pc_results <- pca_results(
    pc_data = pca_data,
    standardise = TRUE,
    No_pcs = 3,
    PCV = NULL,
    plot_eigenvalues = TRUE,
    output_folder = tempdir(),
    interactive_mode = FALSE
  )

  # Check if the output is a list
  expect_true(is.list(pc_results))

  # Check the length of the list
  expect_length(pc_results, 5)

  # Check that certain values are not NULL
  expect_false(is.null(pc_results$eig))

  # Check that pc_results.Rds file is created in the save directory
  expected_filepath <- file.path(tempdir(), "data/4_PCA/pc_results.rds")
  expect_true(file.exists(expected_filepath))

  ## Test pca_scores()

  # Read in example pc_results
  pc_results_file <- system.file("extdata", "data/4_PCA/pc_results.rds", package = "FishDiveR")
  pca_results <- readRDS(pc_results_file)

  # Run the function pca_scores_inc_depth
  pc_scores <- pca_scores(
    pc_results = pca_results,
    plot_loadings = FALSE,
    output_folder = tempdir()
  )

  # Run the function pca_scores_inc_depth
  pc_scores <- pca_scores(
    pc_results = pca_results,
    plot_loadings = TRUE,
    output_folder = tempdir()
  )

  # Check if the output is a data frame
  expect_true(is.data.frame(pc_scores))

  # Check that certain values are not NULL
  expect_false(is.null(pc_scores$PC1))

  # Check that pc_scores.Rds file is created in the save directory
  expected_filepath <- file.path(tempdir(), "data/4_PCA/pc_scores.rds")
  expect_true(file.exists(expected_filepath))

  ## Test combine_data()
  # Import the depth statistics being used in clustering
  combined_data <- combine_data(
    tag_vector = "data",
    data_folder = file.path(system.file("extdata", package = "FishDiveR")),
    pc_scores = pc_scores,
    output_folder = tempdir()
  )

  # Check that combined_data.rds file is created in the save directory
  expected_filepath <- file.path(tempdir(), "data/5_k-means/combined_stats.rds")
  expect_true(file.exists(expected_filepath))
})
