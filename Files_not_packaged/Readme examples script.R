# Development version (requires either the "remotes" or "devtools" package to install:
# install.packages("remotes")
# remotes::install_github("calvinsbeale/FishDiveR", auth_token = "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")

library(FishDiveR)

# Set file path
filepath <- system.file("extdata", package = "FishDiveR")

output_folder <- file.path(tempdir(), "/temp2")

# Examples from the readme.rmd

# Import the tag data and crop to deployment times
archive_days <- import_tag_data(
  tag_ID = "data",
  tag_deploy_UTC = "2000-01-01 00:00:00",
  tag_release_UTC = "2000-01-11 23:59:00",
  archive = file.path(filepath, "data/data-Archive.csv"),
  date_time_col = 1,
  depth_col = 2,
  temp_col = NA,
  time_zone = "UTC",
  output_folder = output_folder
)

# Plot the depth time-series record
TDR_plot <- plot_TDR(
  tag_ID = "data",
  data_folder = filepath,
  every_nth = 1 * 10, # 1 minute depth sampling frequency. Plot every 10th data point = 10 minutes.
  every_s = 0,
  plot_size = c(12, 6),
  Y_lim = c(0, 300, 75),
  date_breaks = "48 hour",
  output_folder = output_folder
)

# Create the wavelet and plot the wavelet spectrum
my.w <- create_wavelet(
  archive = archive_days,
  tag_ID = "data",
  wv_period_hours = 24,
  sampling_frequency = NULL,
  suboctaves = 12,
  lower_period_mins = 30,
  upper_period_hours = 24,
  pval = FALSE,
  output_folder = output_folder,
  plot_wavelet = TRUE,
  max_period_ticks = 10,
  plot_width = 800,
  plot_height = 400,
  interactive_mode = FALSE
)

# Create daily wavelet statistics
waveStats <- create_wavelet_stats(
  wavelet = my.w,
  tag_ID = "data",
  output_folder = output_folder
)

# Create daily and diel depth statistics
depthStats <- create_depth_stats(
  archive = archive_days,
  tag_ID = "data",
  diel = TRUE,
  sunrise_time = "06:00:00",
  sunset_time = "18:00:00",
  GPS = file.path(filepath, "data/GPS.csv"),
  sunset_type = "civil",
  output_folder = output_folder
)

# List the tags to perform PCA of daily wavelet statistics
tag_list <- c("data")
# Create the data frame of wavelet statistics for all tags in tag_list. Choose which wavelet statistics to include.
pc_data <- pca_data(
  tag_vector = tag_list,
  data_folder = filepath,
  phase_mean = FALSE,
  phase_variance = FALSE,
  power_mean = TRUE,
  power_variance = TRUE,
  mean_sq_power = FALSE,
  amplitude_mean = TRUE,
  amplitude_variance = FALSE,
  output_folder = output_folder
)

# Run Principal Component Analysis on the data frame to calculate PC scores
pc_results <- pca_results(
  pc_data = pc_data,
  standardise = TRUE,
  No_pcs = 3,
  PCV = 90,
  plot_eigenvalues = TRUE,
  output_folder = output_folder
)

# Extract the principal component scores.
pc_scores <- pca_scores(
  pc_results = pc_results,
  plot_loadings = TRUE,
  output_folder = output_folder
)

# Import the depth statistics for tags in tag_vector, combine them with pc_scores and standardise
kmeans_features <- combine_data(
  tag_vector = tag_list,
  data_folder = filepath,
  pc_scores = pc_scores,
  output_folder = output_folder
)

# Optionally at this stage the user may select to remove or add additional statistics. If data are modified, be sure to re-standardise the data frame.
# kmeans_features <- kmeans_features[,c(1:8, 10:20)]

# Plot elbow and silhouette width plots to inform the value of k to use (number of clusters)
selecting_k <- select_k(
  kmeans_data = kmeans_features,
  standardise = TRUE,
  Max.k = 8,
  v_line = 4,
  output_folder = output_folder
)

# Run k-means with the selected number of clusters
kmeans_result <- k_clustering(
  kmeans_data = kmeans_features,
  standardise = TRUE,
  k = 4,
  polygon = TRUE,
  output_folder = output_folder
)

# Plot the depth time-series record with each day coloured by the cluster assignment
TDR_plot <- plot_cluster_TDR(
  tag_ID = "data",
  data_folder = filepath,
  kmeans_result = kmeans_result,
  every_nth = 10,
  every_s = 600,
  Y_lim = c(0, 275, 75),
  date_breaks = "24 hour",
  legend = TRUE,
  plot_size = c(12, 6),
  dpi = 100,
  output_folder = output_folder
)

# Plot the 24-hour depth time-series of the 'No_days' closest to the centre of each cluster group
plots_list <- plot_clusters(
  tag_vector = "data",
  data_folder = filepath,
  kmeans_result = kmeans_result,
  No_days = 1,
  every_nth = 10,
  every_s = 300,
  Y_lim = c(0, 275, 50),
  color = TRUE,
  diel_shade = TRUE,
  dpi = 100,
  output_folder = output_folder
)

