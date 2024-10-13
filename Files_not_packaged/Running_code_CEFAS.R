# Test code file for FishDiveR package

devtools::install_github("calvinsbeale/FishDiveR", auth_token ="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")
library(FishDiveR)

# Builds the website locally for external to R information.
pkgdown::build_site()

#
##### Meta data #####
data_dir <- "E:/Ch 3 data"
meta <- data.frame(
  animal = c(
    "1.oceanic_manta", "2.oceanic_manta", "3.oceanic_manta", "4.oceanic_manta_80_good_days", "5.oceanic_manta",
    "6.bluefin_tuna", "7.Green_sawfish", "8.Largetooth_sawfish", "9.cod_10m", "10.cod_10m",
    "11.cod_10m", "12.cod_10s", "13.whale_shark", "14.oceanic_manta_nz", "15.reef_manta",
    "16.130970_example_for_down_sampling", "17.Simulated_180", "18.FishDiveR_data",
    "19.130970a 10 minute", "20. 75379 10 minute", "21. 333237 10 minute"
  ),
  tag_ID = c(
    "130968", "130969", "130970a", "130970b", "152928",
    "165375", "237522", "333237", "100872", "101186",
    "225510m", "225510s", "75379", "177767", "140899",
    "130970e", "test180", "data",
    "130970a10", "7537910m", "33323710m"
  ),
  diel = c(
    rep(TRUE, 5),
    FALSE, rep(TRUE, 4),
    rep(TRUE, 5),
    rep(TRUE, 3),
    TRUE, TRUE, TRUE
  ),
  gps = c(
    "E:/Ch 3 data/130968/GPS.csv", "E:/Ch 3 data/130969/GPS.csv", "E:/Ch 3 data/130970a/GPS.csv", "E:/Ch 3 data/130970b/GPS.csv", "E:/Ch 3 data/152928/GPS.csv",
    FALSE, "E:/Ch 3 data/237522/GPS.csv", "E:/Ch 3 data/333237/GPS.csv", "E:/Ch 3 data/100872/GPS.csv", "E:/Ch 3 data/101186/GPS.csv",
    "E:/Ch 3 data/225510m/GPS.csv", "E:/Ch 3 data/225510s/GPS.csv", "E:/Ch 3 data/75379/GPS.csv", "E:/Ch 3 data/177767/GPS.csv", FALSE,
    "E:/Ch 3 data/130970e/GPS.csv", FALSE, "E:/Ch 3 data/data/GPS.csv",
    "E:/Ch 3 data/130970a10/GPS.csv", "E:/Ch 3 data/7537910m/GPS.csv", "E:/Ch 3 data/33323710m/GPS.csv"
  ),
  tag_deploy_UTC = c(
    "2013-10-25 02:46:00", "2014-05-08 05:25:00", "2013-10-13 03:30:00", "2015-07-23 12:00:00", "2016-05-08 09:26:45",
    "2016-10-01 19:23:39", "2023-05-07 08:29:00", "2022-09-24 00:00:00", "2003-11-22 12:00:00", "2005-03-11 02:41:00",
    "2001-04-03 13:21:00", "2001-06-30 23:59:50", "2008-01-17 03:07:00", "2019-03-06 04:50:00", "2014-10-20 00:02:40",
    "2013-10-31 00:00:00", "2000-01-01 00:00:00", "2000-01-01 00:00:00",
    "2013-10-13 03:30:00", "2008-01-17 03:07:00", "2022-09-24 00:00:00"
  ),
  tag_release_UTC = c(
    "2014-04-23 23:17:35", "2014-11-04 22:33:30", "2014-01-09 13:51:30", "2015-10-11 21:48:00", "2016-10-18 08:32:45",
    "2017-07-21 00:03:39", "2023-06-06 14:22:01", "2022-11-25 06:45:00", "2005-05-14 10:11:00", "2006-01-19 10:00:00",
    "2002-02-06 20:41:00", "2001-08-01 00:01:00", "2008-04-14 12:30:00", "2019-05-10 07:44:00", "2014-12-17 15:00:00",
    "2013-12-01 00:00:00", "2000-12-30 20:00:00", "2000-01-11 23:59:00",
    "2014-01-09 13:51:30", "2008-04-14 12:30:00", "2022-11-25 06:45:00"
  ),
  date_time_col = c(rep(1, 21)),
  depth_col = c(rep(2, 21)),
  temp_col = c(rep(NA, 21)),
  time_zone = c(
    "Asia/Tokyo", "Asia/Tokyo", "Asia/Tokyo", "Asia/Tokyo", "Asia/Tokyo",
    "America/Toronto", "Australia/Perth", "Australia/Perth", "UTC", "UTC",
    "UTC", "UTC", "Asia/Tokyo", "Etc/GMT-12", "Asia/Tokyo",
    "Asia/Tokyo", "UTC", "UTC",
    "Asia/Tokyo", "Asia/Tokyo", "Australia/Perth"
  )
)

#
##### Import tag archives #####
i <- 1

# My csv data archives are located in a parent folder, then a folder by tag_ID, then named by tag_ID-Archive.csv

# Loop through the metadata and call process_tag_data
for (i in c(1:3, 5:15, 15:21)) {
  archive_days <- import_tag_data(
    tag_ID = meta$tag_ID[i],
    tag_deploy_UTC = meta$tag_deploy_UTC[i],
    tag_release_UTC = meta$tag_release_UTC[i],
    archive = file.path(data_dir, meta$tag_ID[i], paste0(meta$tag_ID[i], "-Archive.csv")),
    date_time_col = meta$date_time_col[i],
    depth_col = meta$depth_col[i],
    temp_col = meta$temp_col[i],
    time_zone = meta$time_zone[i],
    output_folder = data_dir
  )
}

#
##### Plot archive_days time-series depth data #####
i <- 1

TDR_plot <- plot_TDR(
  tag_ID = meta$tag_ID[i],
  data_folder = data_dir,
  every_Nth = 12, # Plot every 12th data point (12*5 second = every 60 seconds)
  plot_size = c(12,6), # Height & width
  Y_lim = c(0, 125, 25), # Min, Max, Interval
  date_breaks = "28 day", # Use 14 day not 2 week for consistency below
  output_folder = file.path(data_dir) # Parent folder of tag data
)

rm(i, TDR_plot)
#
##### Create wavelet and clustering statistics #####

# Run an individual tag or multiple tags in a for loop
# i <- 1
for (i in c(1:21)) {
  data_dir <- "E:/Ch 3 data"
  archive_days <- readRDS(file = file.path(data_dir, meta$tag_ID[i], "archive_days.rds"))

  my.w <- create_wavelet(
      archive = archive_days,
      tag_ID = meta$tag_ID[i],
      lower_period_mins = 5, # Typically 5 or 10 minutes
      upper_period_hours = 24, # Typically 24 hours
      output_folder = data_dir,
      plot_wavelet = TRUE,
      max_period_ticks = 10,
      plot_width = 800,
      plot_height = 400
    )

  # Create daily wavelet statistics
  waveStats <- create_wavelet_stats(
    wavelet = my.w, # Wavelet must be in R environment
    tag_ID = meta$tag_ID[i],
    output_folder = data_dir
  )

  # Create daily depth statistics (including sunrise & sunset OR a GPS location file for diel statistics where available)
  depthStats <- create_depth_stats(
    archive = archive_days, # Archive must be in R environment
    tag_ID = meta$tag_ID[i],
    sunrise_time = "06:00:00", # Optionally set a fixed sunrise time if GPS not available
    sunset_time = "18:00:00", # Optionally set a fixed sunset time if GPS not available
    diel = meta$diel[i], # Optionally create diel statistics
    GPS = meta$gps[i], # Optionally use a GPS csv file for daily sunrise and sunset times
    sunset_type = "civil", # Choose between civil, nautical, and astronomical times
    output_folder = data_dir
  )

  cat(paste0("i ", i, " completed"))

  rm(my.w, waveStats, depthStats, i, archive_days)
  gc()
}

#
##### Principal Component Analysis of wavelet statistics #####
data_dir <- "E:/Ch 3 data"

# Run a single tag through clustering, or cluster multiple tags together
tag_list <- "100872" # Cod 1 - 10 minute depth sampling frequency
tag_list <- "101186" # Cod 2 - 10 minute depth sampling frequency
tag_list <- "225510m" # Cod 3 - 10 minute depth sampling frequency
tag_list <- c("100872", "101186", "225510m") # Cod. 8 PCs. 10 minute datasets
tag_list <- "225510s" # Cod 3 - 10 second depth sampling frequency

# Create the PC dataset from one or more tags being analysed. Using all defaults.
pc_data <- pca_data(
  tag_vector = tag_list,
  data_folder = data_dir,
  output_folder = data_dir
)

# Run Principal Component Analysis on the dataset
pc_results <- pca_results(
  pc_data = pc_data,
  standardise = TRUE,
  #No_pcs = 9, # Either manually select the number of principal components to keep or...
  PCV = 75, # Select the percentage cumulative variance within the data that you want to keep
  plot_eigenvalues = TRUE,
  output_folder = data_dir
)

# Create the principal component scores data frame
pc_scores <- pca_scores(
  pc_results = pc_results,
  plot_loadings = TRUE,
  output_folder = data_dir
)

# Import the tags' depth statistics and combine with their PC scores. Then standardise the data ready to be used in k-means clustering
kmeans_data <- combine_data(
  tag_vector = tag_list,
  data_folder = data_dir,
  pc_scores = pc_scores,
  output_folder = data_dir
)

# Optional: Select which depth statistics to include in clustering. For example you might choose to remove the daily and diel surface proportion statistics:
# kmeans_data <- kmeans_data[, !names(kmeans_data) %in% c("surface_proportion", "surf_prop_diff")]

#
##### K-means. Selecting k and analysis #####

# Run k-means on k up to a maximum k to determine best fit from elbow and silhouette width plots
select_k(
  kmeans_data = kmeans_data,
  standardise = TRUE,
  Max.k = 15,
  #v_line = 4, # Add a vertical line to the plot if you want, to denote the chosen K
  output_folder = data_dir
)

# Run k means with the selected number of clusters. Create the 3D plot with or without polygons.
# 3D plot window needs to be resized then create the plot a second time to fix the legend size.
kmeans_result <- k_clustering(
  kmeans_data = kmeans_data,
  k = 5,
  polygon = TRUE,
  output_folder = data_dir
)

#
##### Metadata frame for parameters ##### # changed weeks to days #####

# Set the metadata for all tags
metadata <- data.frame(
  animal = c(
    "1.oceanic_manta", "2.oceanic_manta", "3.oceanic_manta", "4.oceanic_manta_80_good_days", "5.oceanic_manta",
    "6.bluefin_tuna", "7.Green_sawfish", "8.Largetooth_sawfish", "9.cod_10m", "10.cod_10m",
    "11.cod_10m", "12.cod_10s", "13.whale_shark", "14.oceanic_manta_nz", "15.reef_manta",
    "16.130970_example_for_down_sampling", "17.Simulated_180", "18.FishDiveR_data",
    "19.130970a 10 minute", "20. 75379 10 minute", "21. 333237 10 minute"
    ),
  tag_ID = c(
    "130968", "130969", "130970a", "130970b", "152928",
    "165375", "237522", "333237", "100872", "101186",
    "225510m", "225510s", "75379", "177767", "140899",
    "", "test180", "",
    "130970a10", "7537910m", "33323710m"
    ),
  every_nth_TDR = c(
    12 * 5, 12 * 5, 12 * 5, 12 * 5, 4 * 5,
    1, 60 * 5, 5, 1, 1,
    1, 6 * 5, 5, 6 * 5, 6 * 5,
    NA, 5, NA,
    10, 10, 10
    ),
  Y_lim_TDR_min = c(
    0, 0, 0, 0, 0,
    0, 0, 0, 10, 0,
    0, 0, 0, 0, 0,
    NA, 0, NA,
    0, 0, 0
    ),
  Y_lim_TDR_max = c(
    900, 200, 800, 125, 1000,
    900, 58, 80, 35, 140,
    105, 105, 1225, 1150, 500,
    NA, 280, NA,
    750, 1225, 79
    ),
  Y_lim_TDR_interval = c(
    200, 100, 200, 100, 200,
    200, 20, 20, 10, 20,
    20, 20, 200, 200, 200,
    NA, 50, NA,
    200, 200, 20
    ),
  date_breaks = c(
    "14 day", "14 day", "14 day", "14 day", "14 day",
    "28 day", "7 day", "14 day", "28 day", "28 day",
    "28 day", "7 day", "14 day", "7 day", "14 day",
    NA, "28 day", NA,
    "14 day", "14 day", "14 day"
    ),
  every_nth_clusters = c(
    12, 12, 12, 12, 4,
    1, 60, 5, 1, 1,
    1, 6 * 5, 5, 6, 6 * 5,
    NA, 1, NA,
    1, 1, 1
    ),
  Y_lim_clusters_min = c(
    0, 0, 0, 0, 0,
    0, 0, 0, 0, 0,
    0, 0, 0, 0, 0,
    0, 0, 0,
    0, 0, 0
    ),
  Y_lim_clusters_max = c(
    950, 110, 799, 300, 1000,
    900, 58, 79, 35, 140,
    70, 70, 550, 1250, 500,
    NA, 280, NA,
    750, 1250, 79
    ),
  Y_lim_clusters_interval = c(
    200, 100, 800, 100, 200,
    200, 20, 20, 10, 20,
    20, 20, 200, 200, 200,
    NA, 50, NA,
    200, 200, 20
    ),
  stringsAsFactors = FALSE
)

#
##### Plot TDR - tag archive coloured by cluster #####

# Set the output folder to the individual tag, or create a new 'Combined_tags' folder if more than one tag is being processed.
if (length(tag_list) > 1) {
  output_folder <- file.path(data_dir, "Combined_tags")
} else {
  output_folder <- file.path(data_dir, tag_list)
}

# Initialize TDR_plots as an empty list
TDR_plots <- list()

# Use the plot metadata to set up plot parameters
tag_ID <- metadata$tag_ID[i]
every_nth <- metadata$every_nth_TDR[i]
Y_lim <- c(metadata$Y_lim_TDR_min[i], metadata$Y_lim_TDR_max[i], metadata$Y_lim_TDR_interval[i])
date_breaks <- metadata$date_breaks[i]

# Create plots of each time series coloured by cluster by tag_ID
TDR_plots[[tag_ID]] <- plot_cluster_TDR(
  tag_ID = tag_ID,
  data_folder = data_dir,
  kmeans_result = kmeans_result,
  every_nth = every_nth,
  Y_lim = Y_lim,
  date_breaks = date_breaks,
  dpi = 300,
  output_folder = output_folder
)

#
##### Plot the individual clusters time-depth series #####
# Plot the days closest to the centre of each cluster

# ONE TAG
i <- 1
tag_vector <- metadata$tag_ID[i]
every_nth <- metadata$every_nth_clusters[i]
Y_lim <- c(metadata$Y_lim_clusters_min[i], metadata$Y_lim_clusters_max[i], metadata$Y_lim_clusters_interval[i])

# Multiple TAGS
tag_vector <- tag_list
every_nth <- 1
Y_lim <- c(0, 1200, 200)

# PLOT:
cluster_plot <- plot_clusters(
  tag_vector = tag_vector,
  data_folder = data_dir,
  kmeans_result = kmeans_result,
  No_days = 3, # The number of days
  every_nth = every_nth, # Carefully select the sampling frequency to plot.
  Y_lim = Y_lim,
  color = TRUE,
  diel_shade = FALSE, # Takes a long time for plots with high sampling frequency. Get plot size etc correct before turning on.
  dpi = 300,
  output_folder = data_dir
)

#
