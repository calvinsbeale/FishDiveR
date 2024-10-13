# Test code file for FishDiveR package

devtools::document()
devtools::load_all()
data_dir <- "E:/Ch 3 data"

devtools::build_readme()
devtools::build()

devtools::test()
devtools::check()
devtools::install()

styler::style_pkg()

pkgdown::build_site()

usethis::use_github_action()
use_github_action("test-coverage")

# Installation methods: Public repository
devtools::install_github("calvinsbeale/FishDiveR")
remotes::install_github("calvinsbeale/FishDiveR")

library(FishDiveR)

#
##### Meta data #####
data_dir <- "E:/Ch 3 data"
meta <- data.frame(
  animal = c(
    "1.oceanic_manta", "2.oceanic_manta", "3.oceanic_manta", "4.oceanic_manta_80_good_days", "5.oceanic_manta",
    "6.bluefin_tuna", "7.Green_sawfish", "8.Largetooth_sawfish", "9.cod_10m", "10.cod_10m",
    "11.cod_10m", "12.cod_10s", "13.whale_shark", "14.oceanic_manta_nz", "15.reef_manta",
    "16.130970_example_for_down_sampling", "17.Simulated_180", "18.FishDiveR_data",
    "19.130970a 10 minute", "20. 75379 10 minute", "21. 333237 10 minute", "22.peru.oceanic"
  ),
  tag_ID = c(
    "130968", "130969", "130970a", "130970b", "152928",
    "165375", "237522", "333237", "100872", "101186",
    "225510m", "225510s", "75379", "177767", "140899",
    "130970e", "test180", "data",
    "130970a10", "7537910m", "33323710m", "173435"
  ),
  diel = c(
    rep(TRUE, 5),
    FALSE, rep(TRUE, 4),
    rep(TRUE, 5),
    rep(TRUE, 3),
    TRUE, TRUE, TRUE, TRUE
  ),
  gps = c(
    "E:/Ch 3 data/130968/GPS.csv", "E:/Ch 3 data/130969/GPS.csv", "E:/Ch 3 data/130970a/GPS.csv", "E:/Ch 3 data/130970b/GPS.csv", "E:/Ch 3 data/152928/GPS.csv",
    FALSE, "E:/Ch 3 data/237522/GPS.csv", "E:/Ch 3 data/333237/GPS.csv", "E:/Ch 3 data/100872/GPS.csv", "E:/Ch 3 data/101186/GPS.csv",
    "E:/Ch 3 data/225510m/GPS.csv", "E:/Ch 3 data/225510s/GPS.csv", "E:/Ch 3 data/75379/GPS.csv", "E:/Ch 3 data/177767/GPS.csv", FALSE,
    "E:/Ch 3 data/130970e/GPS.csv", FALSE, "E:/Ch 3 data/data/GPS.csv",
    "E:/Ch 3 data/130970a10/GPS.csv", "E:/Ch 3 data/7537910m/GPS.csv", "E:/Ch 3 data/33323710m/GPS.csv", "E:/Ch 3 data/173435/GPS.csv"
  ),
  tag_deploy_UTC = c(
    "2013-10-25 02:46:00", "2014-05-08 05:25:00", "2013-10-13 03:30:00", "2015-07-23 12:00:00", "2016-05-08 09:26:45",
    "2016-10-01 19:23:39", "2023-05-07 08:29:00", "2022-09-24 00:00:00", "2003-11-22 12:00:00", "2005-03-11 02:41:00",
    "2001-04-03 13:21:00", "2001-06-30 23:59:50", "2008-01-17 03:07:00", "2019-03-06 04:50:00", "2014-10-20 00:02:40",
    "2013-10-31 00:00:00", "2000-01-01 00:00:00", "2000-01-01 00:00:00",
    "2013-10-13 03:30:00", "2008-01-17 03:07:00", "2022-09-24 00:00:00", "2018-05-08 19:11:39"
  ),
  tag_release_UTC = c(
    "2014-04-23 23:17:35", "2014-11-04 22:33:30", "2014-01-09 13:51:30", "2015-10-11 21:48:00", "2016-10-18 08:32:45",
    "2017-07-21 00:03:39", "2023-06-06 14:22:01", "2022-11-25 06:45:00", "2005-05-14 10:11:00", "2006-01-19 10:00:00",
    "2002-02-06 20:41:00", "2001-08-01 00:01:00", "2008-04-14 12:30:00", "2019-05-10 07:44:00", "2014-12-17 15:00:00",
    "2013-12-01 00:00:00", "2000-12-30 20:00:00", "2000-01-11 23:59:00",
    "2014-01-09 13:51:30", "2008-04-14 12:30:00", "2022-11-25 06:45:00", "2018-06-03 21:00:00"
  ),
  date_time_col = c(rep(1, 22)),
  depth_col = c(rep(2, 22)),
  temp_col = c(rep(NA, 22)),
  time_zone = c(
    "Asia/Tokyo", "Asia/Tokyo", "Asia/Tokyo", "Asia/Tokyo", "Asia/Tokyo",
    "America/Toronto", "Australia/Perth", "Australia/Perth", "UTC", "UTC",
    "UTC", "UTC", "Asia/Tokyo", "Etc/GMT-12", "Asia/Tokyo",
    "Asia/Tokyo", "UTC", "UTC",
    "Asia/Tokyo", "Asia/Tokyo", "Australia/Perth", "America/Lima"
  )
)

#
##### Import tag archives #####
i <- 22

# Loop through the metadata and call process_tag_data

# Note 333237 has been down-sampled for wavelet analysis

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
i <- 4

TDR_plot <- plot_TDR(
  tag_ID = meta$tag_ID[i],
  data_folder = data_dir,
  every_Nth = 20,
  plot_size = c(12,6),
  Y_lim = c(0, 100, 25),
  date_breaks = "7 day", # Use 14 day not 2 week for consistency below
  output_folder = file.path(data_dir) # Combined_tags
)

rm(i, TDR_plot)
#
##### Create wavelet and clustering statistics #####
i <- 3  # Oceanic in paper 130970a
i <- 13 # whale shark in paper 75379
i <- 11 # Atlantic cod in paper 225510m
i <- 8  # Largetooth sawfish in paper 333237 (down-sampled for wavelet spectrum plot)

for (i in c(19:21)) {
  data_dir <- "E:/Ch 3 data"
  archive_days <- readRDS(file = file.path(data_dir, meta$tag_ID[i], "archive_days.rds"))

  my.w <- create_wavelet(
      archive = archive_days,
      tag_ID = meta$tag_ID[i],
      wv_period_hours = 24,
      sampling_frequency = NULL,
      suboctaves = 12,
      lower_period_mins = 10,
      upper_period_hours = 24,
      pval = FALSE,
      output_folder = data_dir,
      plot_wavelet = TRUE,
      max_period_ticks = 10,
      plot_width = 800,
      plot_height = 400,
      interactive_mode = TRUE
    )

  # Create daily wavelet statistics
  waveStats <- create_wavelet_stats(
    wavelet = my.w,
    tag_ID = meta$tag_ID[i],
    output_folder = data_dir
  )

  # Create daily depth statistics (including sunrise & sunset OR a GPS location file for diel statistics where available)
  depthStats <- create_depth_stats(
    archive = archive_days,
    tag_ID = meta$tag_ID[i],
    sunrise_time = "06:00:00",
    sunset_time = "18:00:00",
    diel = meta$diel[i],
    GPS = meta$gps[i],
    sunset_type = "civil",
    output_folder = data_dir
  )

  # archive_days <- readRDS(file = file.path(data_dir, meta$tag_ID[i], "archive_days.rds"))

  cat(paste0("i ", i, " completed"))

  rm(my.w, waveStats, depthStats, i, archive_days)
  gc()
}

#
##### Create daily wavelet and depth statistics #####

# Create wavelet statistics
waveStats <- create_wavelet_stats(
  wavelet = my.w,
  tag_ID = tag,
  output_folder = data_dir
)

# Create depth statistics including GPS location for diel statistics
depthStats <- create_depth_stats(
  archive = archive_days,
  tag_ID = tag,
  sunrise_time = "06:00:00",
  sunset_time = "18:00:00",
  diel = meta$diel[i],
  GPS = meta$gps[i],
  sunset_type = "civil",
  output_folder = data_dir
)

rm(archive_days, my.w, waveStats, depthStats)

rm(i, tag)
#
##### PCA of wavelet statistics #####
# devtools::load_all()
library(FishDiveR)
data_dir <- "E:/Ch 3 data/10_min_comparison"
data_dir <- "E:/Ch 3 data"

## ALL TAGS NEED RERUNNING WITHOUT COMPOSITE INDEX

tag_list <- c("173435") # 3 second depth sampling frequency
# tag_list <- c("130968") # 5 second depth sampling frequency
# tag_list <- c("130969") # 5 second depth sampling frequency
tag_list <- c("130970a") # 5 second depth sampling frequency
# tag_list <- c("130970b") # 5 second depth sampling frequency
tag_list <- c("152928") # 15 second depth sampling frequency
# tag_list <- c("130968", "130969", "130970a", "152928") # Four Raja tags with good depth data. 609 days. 9 PCs. 5s, 5s, 5s, 15s
tag_list <- c("130968", "130969", "130970a", "130970b", "152928") # Five Raja Mantas
# tag_list <- "165375" # Bluefin Tuna
# tag_list <- "237522" # Green Sawfish. 3 PCs = 75%
tag_list <- "333237" # Largetooth sawfish. Pristis pristis. 6 PCs (>= 75%)
# tag_list <- c("237522", "333237") # Two Sawfish species.
# tag_list <- "100872" # Cod 1 - 10 minute depth sampling frequency
# tag_list <- "101186" # Cod 2 - 10 minute depth sampling frequency
tag_list <- "225510m" # Cod 3 - 10 minute depth sampling frequency
# tag_list <- c("100872", "101186", "225510m") # Cod. 8 PCs. 10 minute datasets
# tag_list <- "225510s" # Cod 3 - 10 second depth sampling frequency
tag_list <- "75379" # Whale Shark
# tag_list <- "177767" # Oceanic manta New Zealand
# tag_list <- "140899" # Reef manta Raja Ampat
# tag_list <- "test" # Simulated data
# tag_list <- "test_30d" # Simulated data 30 day dataset
tag_list <- "test180" # Simulated data 180 days 1 minute SF
tag_list <- "data" # New package test data
tag_list <- c("130970a10", "7537910m", "225510m", "33323710m")
tag_list <- c("130970a10", "7537910m")

# Create the PC dataset from one or more tags being analysed. DOES NOT INCLUDE DEPTH STATS
pc_data <- pca_data(
  tag_vector = tag_list,
  data_folder = data_dir,
  phase_mean = FALSE,
  phase_variance = FALSE,
  power_mean = TRUE,
  power_variance = TRUE,
  mean_sq_power = FALSE,
  amplitude_mean = TRUE,
  amplitude_variance = FALSE,
  output_folder = data_dir
)

# Run Principal Component Analysis on the dataset
pc_results <- pca_results(
  pc_data = pc_data,
  standardise = TRUE,
  #No_pcs = 9,
  PCV = 75,
  plot_eigenvalues = TRUE,
  output_folder = data_dir
)

# Create the principal component scores data frame
pc_scores <- pca_scores(
  pc_results = pc_results,
  plot_loadings = TRUE,
  output_folder = data_dir
)

# Import the depth statistics and combine with the PC scores, then standardise the data ready to be used in k-means clustering
kmeans_data <- combine_data(
  tag_vector = tag_list,
  data_folder = data_dir,
  pc_scores = pc_scores,
  output_folder = data_dir
)

# Optional: Select which depth statistics to include in clustering
# kmeans_data <- kmeans_data[,c(1:4, 10:16)]
# kmeans_data <- kmeans_data[, !names(kmeans_data) %in% c("surface_proportion", "surf_prop_diff")]

#
##### K-means. Selecting k and analysis #####
# Optionally read in combined_data for a specific tag
# kmeans_data <- readRDS(file = file.path(data_dir, "/130970a/5_k-means/combined_stats.rds"))
# kmeans_data <- readRDS(file = file.path(data_dir, "/333237/5_k-means/combined_stats.rds"))
# devtools::load_all()
# data_dir <- "E:/Ch 3 data"
# kmeans_data <- readRDS(file = file.path(data_dir, "test180/5_k-means/combined_stats.rds"))

# DONT RERUN TEST DATA! LOAD THE FILE!

# Run k-means on k up to a maximum k to determine best fit from elbow and silhouette width plots
select_k(
  kmeans_data = kmeans_data,
  standardise = TRUE,
  Max.k = 15,
  v_line = 4,
  output_folder = data_dir
)

# Run k means with the selected number of clusters. Plot with or without polygons. Resize window then plot a second time to fix legend size.
kmeans_result <- k_clustering(
  kmeans_data = kmeans_data,
  k =4,
  polygon = TRUE,
  output_folder = data_dir
)

#
##### Metadata frame for parameters ##### # changed weeks to days #####
# NOTE: Plots will change as weeks are now days

metadata <- data.frame(
  animal = c(
    "1.oceanic_manta", "2.oceanic_manta", "3.oceanic_manta", "4.oceanic_manta_80_good_days", "5.oceanic_manta",
    "6.bluefin_tuna", "7.Green_sawfish", "8.Largetooth_sawfish", "9.cod_10m", "10.cod_10m",
    "11.cod_10m", "12.cod_10s", "13.whale_shark", "14.oceanic_manta_nz", "15.reef_manta",
    "16.130970_example_for_down_sampling", "17.Simulated_180", "18.FishDiveR_data",
    "19.130970a 10 minute", "20. 75379 10 minute", "21. 333237 10 minute", "22. Peru oceanic"
    ),
  tag_ID = c(
    "130968", "130969", "130970a", "130970b", "152928",
    "165375", "237522", "333237", "100872", "101186",
    "225510m", "225510s", "75379", "177767", "140899",
    "", "test180", "",
    "130970a10", "7537910m", "33323710m", "173435"
    ),
  every_nth_TDR = c(
    12 * 5, 12 * 5, 12 * 5, 12 * 5, 4 * 5,
    1, 60 * 5, 5, 1, 1,
    1, 6 * 5, 5, 6 * 5, 6 * 5,
    NA, 5, NA,
    10, 10, 10, 20
    ),
  Y_lim_TDR_min = c(
    0, 0, 0, 0, 0,
    0, 0, 0, 10, 0,
    0, 0, 0, 0, 0,
    NA, 0, NA,
    0, 0, 0, 0
    ),
  Y_lim_TDR_max = c(
    900, 200, 800, 125, 1000,
    900, 58, 80, 35, 140,
    105, 105, 1225, 1150, 500,
    NA, 280, NA,
    750, 1225, 79, 100
    ),
  Y_lim_TDR_interval = c(
    200, 100, 200, 100, 200,
    200, 20, 20, 10, 20,
    20, 20, 200, 200, 200,
    NA, 50, NA,
    200, 200, 20, 25
    ),
  date_breaks = c(
    "14 day", "14 day", "14 day", "14 day", "14 day",
    "28 day", "7 day", "14 day", "28 day", "28 day",
    "28 day", "7 day", "14 day", "7 day", "14 day",
    NA, "28 day", NA,
    "14 day", "14 day", "14 day", "7 day"
    ),
  every_nth_clusters = c(
    12, 12, 12, 12, 4,
    1, 60, 5, 1, 1,
    1, 6 * 5, 5, 6, 6 * 5,
    NA, 1, NA,
    1, 1, 1, 20
    ),
  Y_lim_clusters_min = c(
    0, 0, 0, 0, 0,
    0, 0, 0, 0, 0,
    0, 0, 0, 0, 0,
    0, 0, 0,
    0, 0, 0, 0
    ),
  Y_lim_clusters_max = c(
    950, 110, 799, 300, 1000,
    900, 58, 79, 35, 140,
    70, 70, 550, 1250, 500,
    NA, 280, NA,
    750, 1250, 79, 100
    ),
  Y_lim_clusters_interval = c(
    200, 100, 800, 100, 200,
    200, 20, 20, 10, 20,
    20, 20, 200, 200, 200,
    NA, 50, NA,
    200, 200, 20, 25
    ),
  stringsAsFactors = FALSE
)

# kmeans_result <- readRDS("E:/Ch 3 data/test_30d/5_k-means/kmeans_result.rds")
# kmeans_result <- readRDS("E:/Ch 3 data/130970a/5_k-means/kmeans_result.rds") # Oceanic manta
# kmeans_result <- readRDS("E:/Ch 3 data/75379/5_k-means/kmeans_result.rds") # Whale shark
# kmeans_result <- readRDS("E:/Ch 3 data/225510m/5_k-means/kmeans_result.rds") # Cod 2255
# kmeans_result <- readRDS("E:/Ch 3 data/333237/5_k-means/kmeans_result.rds") # Largetooth sawfish

# i <- 3 # Oceanic in paper 130970a
# i <- 13 # whale shark in paper 75379
# i <- 11 # Atlantic cod in paper 225510m
# i <- 8 # Largetooth sawfish in paper 333237 (down-sampled for wavelet spectrum plot)

# i <- 1
# data_dir <- "E:/Ch 3 data"
# tag_list <- metadata$tag_ID[i]

# For tags with meta (10 min tags are 11, 19, 20, 21)
# tag_list <- c("130970a10", "7537910m", "225510m", "33323710m")
# tag_list <- c("130970a10", "7537910m")
# data_dir <- "E:/Ch 3 data/10_min_comparison"
# kmeans_result <- readRDS(file = file.path(data_dir, "Combined_tags/5_k-means/kmeans_result.rds"))
# archive <- readRDS(file = file.path(data_dir, metadata$tag_ID[i], "archive_days.rds"))

#
##### Plot TDR - tag archive coloured by cluster #####

# Create plots of each time series coloured by cluster by tag_ID and loaded 'kmeans_result' data frame
if (length(tag_list) > 1) {
  output_folder <- file.path(data_dir, "Combined_tags")
} else {
  output_folder <- file.path(data_dir, tag_list)
}

# Initialize TDR_plots as an empty list
TDR_plots <- list()

tag_ID <- metadata$tag_ID[i]
every_nth <- metadata$every_nth_TDR[i]
Y_lim <- c(metadata$Y_lim_TDR_min[i], metadata$Y_lim_TDR_max[i], metadata$Y_lim_TDR_interval[i])
date_breaks <- metadata$date_breaks[i]

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
##### Plot clusters time-depth series #####
# Plot the days closest to the centre of each cluster

# ONE TAG
tag_vector <- metadata$tag_ID[i]
every_nth <- metadata$every_nth_clusters[i]
Y_lim <- c(metadata$Y_lim_clusters_min[i], metadata$Y_lim_clusters_max[i], metadata$Y_lim_clusters_interval[i])

# Multiple TAGS (in paper)
tag_vector <- tag_list
every_nth <- 1
Y_lim <- c(0, 1200, 200)

# PLOT:
cluster_plot <- plot_clusters(
  tag_vector = tag_vector,
  data_folder = data_dir,
  kmeans_result = kmeans_result,
  No_days = 3,
  #every_nth = every_nth,
  every_s = 60,
  Y_lim = Y_lim,
  color = TRUE,
  diel_shade = FALSE,
  dpi = 300,
  output_folder = data_dir
)

#
###### End of package functions ######
