test_that("select_k() creates the appropriate plot", {
  # Load pc_data and pc_scores first using the example dataset
  pc_scores_file <- system.file("extdata", "data/4_PCA/pc_scores.rds", package = "FishDiveR")
  pc_scores <- readRDS(pc_scores_file)

  # Run the function selecting_k_plots
  selecting_k_plots <- select_k(
    kmeans_data = pc_scores,
    standardise = FALSE,
    Max.k = 8,
    v_line = 4,
    calc_gap = TRUE,
    plot_gap = TRUE,
    output_folder = tempdir()
  )

  # Check if the output is a list
  expect_true(is.list(selecting_k_plots))

  # Load combined data first using the example dataset
  kmeans_data_file <- system.file("extdata", "data/5_k-means/combined_stats.rds", package = "FishDiveR")
  kmeans_data <- readRDS(kmeans_data_file)

  # Run the function selecting_k_plots
  selecting_k_plots <- select_k(
    kmeans_data = kmeans_data,
    standardise = TRUE,
    Max.k = 8,
    v_line = 4,
    calc_gap = FALSE,
    plot_gap = FALSE,
    output_folder = tempdir()
  )

  # Check if the output is a list
  expect_true(is.list(selecting_k_plots))
})
