test_that("additional_plots creates a png file", {
  # Load combined data first using the example dataset
  tag <- "data"
  filepath <- system.file("extdata", package = "FishDiveR")

  kmeans_result_file <- system.file("extdata", "data/5_k-means/kmeans_result.rds", package = "FishDiveR")
  kmeans_result <- readRDS(kmeans_result_file)

  # Run plot_clusters function
  plot_cluster_TDR(
    tag_ID = "data",
    data_folder = filepath,
    kmeans_result = kmeans_result,
    every_nth = 10,
    every_s = 0,
    Y_lim = c(0, 300, 50),
    date_breaks = "48 hour",
    legend = FALSE,
    plot_size = c(12, 6),
    dpi = 30,
    output_folder = paste0(tempdir(), "/", tag)
  )

  # Run plot_clusters function
  plot_cluster_TDR(
    tag_ID = "data",
    data_folder = filepath,
    kmeans_result = kmeans_result,
    every_nth = 10,
    every_s = 600,
    Y_lim = c(0, 300, 50),
    date_breaks = "48 hour",
    legend = TRUE,
    plot_size = c(12, 6),
    dpi = 300,
    output_folder = paste0(tempdir(), "/", tag)
  )

  # Check that the TDR file is created in the save directory
  expected_filepath <- file.path(tempdir(), "data", paste0(tag, "_TDR_k=4.png"))
  expect_true(file.exists(expected_filepath))
})
