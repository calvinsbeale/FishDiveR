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
    output = TRUE,
    output_folder = tempdir(),
    verbose = TRUE
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
    output = TRUE,
    output_folder = tempdir(),
    verbose = TRUE
  )

  # Check that TDR.png is created in the save directory
  expected_filepath <- file.path(tempdir(), tag, paste0("6_Cluster-plots.K=", k, "_shaded"), "Cluster_1.png")
  expect_true(file.exists(expected_filepath))

  # Clean up the temporary directory
  unlink(list.files(tempdir(), full.names = TRUE))
})

test_that("plot_clusters() combines tags with and without diel times", {
  archive_days <- readRDS(system.file("extdata", "data", "archive_days.rds", package = "FishDiveR"))
  folder <- file.path(tempdir(), "mixed_diel")
  unlink(folder, recursive = TRUE)
  dir.create(file.path(folder, "plain"), recursive = TRUE)
  # One tag with diel times in its archive, one without
  create_depth_stats(
    archive_days, tag_ID = "diel", diel = TRUE, sunrise_time = "06:00:00", sunset_time = "18:00:00",
    output = TRUE, output_folder = folder
  )
  plain <- archive_days[, c("date", "depth", "date_only")]
  attr(plain, "time_zone") <- attr(archive_days, "time_zone")
  saveRDS(plain, file.path(folder, "plain", "archive_days.rds"))

  dates <- rep(as.Date("2000-01-01") + 0:9, 2)
  kmeans_result <- list(
    # Equal distances for both tags, so each cluster plots days from both
    cluster = rep(c(1L, 2L), 10), distances = rep(seq(0.1, 1, length.out = 10), 2),
    tag_ID = rep(c("diel", "plain"), each = 10), date_only = dates
  )
  expect_message(
    plots <- plot_clusters(
      tag_vector = c("diel", "plain"), data_folder = folder, kmeans_result = kmeans_result, No_days = 4,
      every_nth = 10, diel_shade = TRUE, dpi = 30, output = TRUE, output_folder = folder
    ),
    "No night shading for plain"
  )
  expect_length(plots, 2)
  # Each tag's day has its own panel, even on the same date
  days <- unique(plots[[1]]$data[, c("tag_ID", "date_only", "date_number")])
  expect_equal(anyDuplicated(days$date_number), 0)
  expect_true(all(c("diel", "plain") %in% days$tag_ID))
  # Only the days with times are shaded
  shaded <- unique(plots[[1]]$layers[[1]]$data$date_number)
  days <- plots[[1]]$data[!duplicated(plots[[1]]$data$date_number), ]
  expect_setequal(shaded, days$date_number[days$tag_ID == "diel"])
})
