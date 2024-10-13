test_that("plot_clusters() creates a png file", {
  # Load pc_data, pc_scores, and kmeans_result using the example dataset
  tag <- "data"
  filepath <- system.file("extdata", package = "FishDiveR")

  kmeans_result_file <- system.file("extdata", "data/5_k-means/kmeans_result.rds", package = "FishDiveR")
  kmeans_result <- readRDS(kmeans_result_file)

  # Should test using this:
  k <- length(unique(kmeans_result$cluster))

  # Run k_clustering function
  plots <- plot_clusters(
    tag_vector = tag, # tag archive is read in
    data_folder = filepath,
    kmeans_result = kmeans_result,
    No_days = 2,
    every_nth = 10,
    every_s = 0,
    Y_lim = c(0, 300, 50),
    color = FALSE,
    diel_shade = FALSE,
    dpi = 30,
    output_folder = tempdir()
  )

  # Check that TDR.png is created in the save directory
  expected_filepath <- file.path(tempdir(), tag, paste0("6_Cluster-plots.K=", k), "Cluster_1.png")
  expect_true(file.exists(expected_filepath))

  # Run k_clustering function
  plots <- plot_clusters(
    tag_vector = tag, # tag archive is read in
    data_folder = filepath,
    kmeans_result = kmeans_result,
    No_days = 2,
    every_nth = 10,
    every_s = 600,
    Y_lim = c(0, 275, 50),
    color = TRUE,
    diel_shade = TRUE,
    dpi = 100,
    output_folder = tempdir()
  )

  # Check that TDR.png is created in the save directory
  expected_filepath <- file.path(tempdir(), tag, paste0("6_Cluster-plots.K=", k, "_shaded"), "Cluster_1.png")
  expect_true(file.exists(expected_filepath))

  # Clean up the temporary directory
  unlink(list.files(tempdir(), full.names = TRUE))
})
