devtools::load_all()
#
##### Meta #####

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
##### Figure 2 (simulated) #####

behavior_sequence_data <- readRDS(file = "E:/Ch 3 data/test180/behavior_sequence_data.rds")

library(ggplot2)
# Calculate sampling interval
sampling_interval <- behavior_sequence_data$time_hours[2] * 60 - behavior_sequence_data$time_hours[1]  * 60

# Filter to every nth record
every_nth <- seq(0, nrow(behavior_sequence_data), by = 60)
quick_plot_data <- behavior_sequence_data[every_nth, ]

plot <- ggplot(data = quick_plot_data, aes(x = days, y = depth, group = group)) +
  geom_line() +
  scale_x_continuous(
    name = "Days Elapsed",
    breaks = seq(0, max(quick_plot_data$days), by = 10),
    expand = c(0, 0, 0, 0), position = "top"
  ) +
  scale_y_reverse(limits = c(270, 0), expand = c(0, 0, 0, 0)) +
  labs(
    y = "Depth (m)",
    color = "Behavior"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 16, face = "bold", color = "black"), # Bold or not bold?
    axis.title = element_text(size = 16, face = "bold", color = "black"), # Bold or not bold?
    legend.text = element_text(size = 16),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks = element_line(linewidth = 1.0, color = "black"),
    axis.line = element_line(size = 1.0)
  )
plot

ggsave(filename = "E:/Ch 3 data/test180/Archive_bold.png", plot = plot, dpi = 600, height = 3.5, width = 8)

#
##### Figure 3 - PCA data, results, scores (simulated) #####

data_dir <- "E:/Ch 3 data"
tag_list <- "test180" # Simulated data 180 days 1 minute SF

# Create the PC dataset from one or more tags being analysed. DOES NOT INCLUDE DEPTH STATS
wc_data <- pca_data(
  tag_vector = tag_list,
  data_folder = data_dir,
  output_folder = data_dir
)

# Run Principal Component Analysis on the wavelet statistics dataset
pc_results <- pca_results(
  pc_data = wc_data,
  PCV = 90,
  output_folder = data_dir
)

## pc_scores:

plot_loadings = TRUE
output_folder = data_dir

# The attribute exists, so use it with existing helper function by turning it into a data frame
unique_tag_ID <- as.data.frame(attr(pc_results, "unique_tag_ID"))

# Set the save folder location using helper function
save_folder <- set_scores_save_folder(
  output_folder = output_folder,
  data_frame = unique_tag_ID # converted to data frame
)


Max.C <- attr(pc_results, "No.Components")

# Extract all the principal component scores
pc_scores <- as.data.frame(pc_results$ind$coord)

# Rename columns from Dim to PC
colnames(pc_scores) <- paste0("PC", 1:ncol(pc_scores))

# Helper function to save grobs if there are any existing plots
save_grob <- function(existing_plots, c, name_suffix) {
  if (length(existing_plots) > 0) {
    # Facet with arrangeGrob from gridExtra
    grob <- do.call(gridExtra::arrangeGrob, c(existing_plots, list(ncol = 1)))
    ggsave(file.path(save_folder, paste0("PC", c, name_suffix)), plot = grob, width = 10, height = 15, dpi = 600)
  }
}

# Extract the loadings (variable coordinates) as a data frame
loadings_df <- as.data.frame(pc_results$var$coord)

# Assign the new column names to the data frame
colnames(loadings_df) <- paste0("PC", 1:ncol(loadings_df))

# Assign a variable column and rename the row names
loadings_df$Variable <- rownames(loadings_df)
rownames(loadings_df) <- seq(1:nrow(loadings_df))

# Create long data frame for plotting
loadings_long <- tidyr::gather(loadings_df, Principal.Component, Loading, -Variable)

# Specify the loading threshold
load <- 0.5

# Create a sequence order to plot by
loadings_long$seq <- seq(1:nrow(loadings_long))

# Reverse the sequence so that of periods goes from short to long along the x-axis
loadings_long$seq <- (max(loadings_long$seq) + 1) - loadings_long$seq

# Define the desired order of Principal.Components
pc_order <- paste0("PC", 1:Max.C)

# Convert Principal.Component to a factor with custom levels
loadings_long$Principal.Component <- factor(loadings_long$Principal.Component, levels = pc_order)

#### Plotting the Loading statistics per PC
if (plot_loadings == TRUE) {
  # Helper function to display every other tick mark
  every_other <- function(x) {
    inds <- seq(1, length(x), by = 2)
    labels <- rep("", length(x))
    labels[inds] <- x[inds]
    return(labels)
  }

  # Helper function to display every nth tick mark
  every_nth_label <- function(x) {
    inds <- seq(1, length(x), by = every_nth)
    labels <- rep("", length(x))
    labels[inds] <- x[inds]
    return(labels)
  }

  # Initialize empty lists to store the plots
  plot_list1 <- list()
  plot_list2 <- list()
  mean_power_list <- list()
  mean_power_data <- list()

  ### Calculate the x-axis labels from wavelet meta
  # Initial values
  UP_mins <- attr(pc_results, "UP") * 24 * 60 # Convert UP to minutes
  LP <- attr(pc_results, "LP") # Lower bound in minutes (assuming already in minutes)
  SO <- attr(pc_results, "SO") # Suboctave interval

  # Remove the prefixes (p and numbers) from the column names. Assumes no non-numerical columns remain
  cleaned_colnames <- gsub("^p[0-9]+_", "", colnames(pc_results$call$X))

  # Calculate the number of periods
  No_periods <- length(pc_results$call$X) / length(unique(cleaned_colnames))

  # Prepare a sequence for periods
  periods <- seq(1, No_periods)

  # Calculate the rate of change per period to reach half value at each SO
  # Assuming exponential decay formula: N(t) = N0 * exp(-lambda * t)
  # To halve the value at each SO, we find lambda when t = SO and N(t) = N0/2
  lambda <- log(2) / SO

  # Apply formula to generate labels adjusting to fit the scale
  labels <- UP_mins * exp(-lambda * (periods - 1))

  # Adjusting labels to fit the scale more precisely for your requirement
  adjusted_labels <- round(labels, 2)

  # Replace the last value with LP
  adjusted_labels[length(adjusted_labels)] <- LP

  # Convert and format the adjusted_labels based on the condition
  formatted_labels <- sapply(adjusted_labels, function(label) {
    if (label >= 60) {
      # Convert to hours and append 'hrs'
      paste0(round(label / 60, 2), " hrs")
    } else {
      # Keep as minutes and append 'mins'
      paste0(label, " mins")
    }
  })

  c <- 1
  # Create plots for all PCs
  for (c in 1:Max.C) {
    # Filter the data for only PC
    pcx <- paste0("PC", c)
    loadings <- loadings_long[loadings_long$Principal.Component == pcx, ]

    # Rename the variables and prepare for plotting
    loadings$CleanVariable <- as.factor(gsub("p[0-9]+_", "", loadings$Variable))
    loadings$Pnumber <- gsub("(p[0-9]+_).*", "\\1", loadings$Variable)
    loadings$Pnumber <- sub("^p", "P", loadings$Pnumber)
    loadings$Pnumber <- sub("_$", "", loadings$Pnumber)

    # Dynamically set variables to plot based on existing data
    variables <- levels(loadings$CleanVariable)

    # Create the correct length of labels
    formatted_labels2 <- rep(formatted_labels, each = length(unique(variables)))

    # Create period labels for whole loadings data frame
    loadings$period_labs <- formatted_labels2

    # Create a new column for descriptive names
    loadings$DescriptiveName <- loadings$CleanVariable # Initialize with existing clean variable names

    # Recode using dplyr::recode
    loadings$DescriptiveName <- dplyr::recode(loadings$DescriptiveName,
                                              "Amplitude_mean" = "Mean Amplitude",
                                              "Phase_mean" = "Mean Phase",
                                              "Phase_variance" = "Variance of Phase",
                                              "Power_mean" = "Mean Power",
                                              "Amplitude_variance" = "Variance of Amplitude",
                                              "Power_variance" = "Power Variance",
                                              "Mean_sq_power" = "Mean Squared Power"
    )

    # subset the data for the wavelet variable 'var_name'
    data_sub <- subset(loadings, CleanVariable == "Power_mean")

    # Set labels
    data_sub$period_labs <- formatted_labels

    power_plot_title <- paste0("Mean Wavelet Power PC", c)

    # Save the plot data for the line plot
    mean_power_data[[c]] <- data_sub
  }

  # Combine all data frames into a single data frame
  combined_mean_power_data <- do.call(rbind, mean_power_data)

  # Helper function to get indices of non-empty labels
  non_empty_labels <- function(labels) {
    which(labels != "")
  }

  # Create x-axis labels for one 'set' of periods
  plot_labels <- rev(every_nth_label(combined_mean_power_data$period_labs[c(1:No_periods)]))

  # Get the positions of non-empty labels
  positions <- non_empty_labels(plot_labels)

  # Convert plot_labels to factor with appropriate levels
  combined_mean_power_data$period_labs_factor <- factor(combined_mean_power_data$period_labs, levels = unique(combined_mean_power_data$period_labs))

  # Plotting mean wavelet power for each PC
  mean_power_plot <-
    ggplot(data = combined_mean_power_data, aes(x = as.numeric(stats::reorder(period_labs_factor, seq)), y = Loading, color = Principal.Component, group = Principal.Component)) +
    geom_line(linewidth = 1) + # Use geom_line for line plots
    geom_hline(yintercept = load, linetype = "dashed", color = "black", linewidth = 0.75) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.75) +
    geom_hline(yintercept = -load, linetype = "dashed", color = "black", linewidth = 0.75) +
    scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +
    scale_x_continuous(breaks = positions, labels = plot_labels[positions], expand = c(0, 0)) +
    theme_bw() +
    labs(
      x = "Wavelet Period",
      y = "Mean Wavelet Power Loading",
      color = "Principal\nComponent"
    ) +
    theme(
      axis.text.x = element_text(size = 16, face = "bold", color = "Black"),
      axis.text.y = element_text(size = 16, face = "bold", color = "Black"),
      axis.title = element_text(size = 16, face = "bold", color = "Black"),
      legend.title = element_text(size = 16, face = "bold", color = "Black"),
      legend.text = element_text(size = 14, face = "bold", color = "Black"),
      axis.ticks.length = unit(0.2, "cm"),
      axis.ticks = element_line(linewidth = 1.0, color = "black"),
      axis.line = element_line(linewidth = 1.0)
    )

  # Print the plot
  print(mean_power_plot)

  # Save the mean power line plot
  ggsave(file.path(save_folder, "PCA_mean_power_line_plot.png"), plot = mean_power_plot, width = 15, height = 10, dpi = 600)

  # Report save folder dependant on the number of tags
  cat(paste0("\nOutput folder: ", save_folder, "\n"))
}

# Add an attribute to pc_scores with unique Tag_ID if processing one tag
attr(pc_scores, "unique_tag_ID") <- attr(pc_results, "unique_tag_ID")

# Add an attribute for the number of PC's kept
attr(pc_scores, "No.Components") <- Max.C

#
##### Create combined stats (simulated) #####
data_dir <- "E:/Ch 3 data"
tag_list <- "test180" # Simulated data 180 days 1 minute SF

pc_scores <- readRDS(file = file.path(data_dir, tag_list, "4_PCA/pc_scores.rds"))

# Import the depth statistics and combine with the PC scores, then standardise the data ready to be used in k-means clustering
kmeans_data <- combine_data(
  tag_vector = tag_list,
  data_folder = data_dir,
  pc_scores = pc_scores,
  output_folder = data_dir
)

#
##### Figure 4 - select K (simulated) #####
data_dir <- "E:/Ch 3 data"
tag_list <- "test180" # Simulated data 180 days 1 minute SF

kmeans_data <- readRDS(file = file.path(data_dir, tag_list, "5_k-means/combined_stats.rds"))
standardise = TRUE
Max.k = 15
v_line = 5
output_folder = data_dir

# Set the random seed for reproducible results in the function
withr::local_options(set.seed(123))

# Check that Max.k does not exceed the number of kmeans_data
if (Max.k >= nrow(kmeans_data)) {
  message(paste0("Error: The maximum number of clusters cannot equal or exceed the number of days. Using Max.K = ", nrow(kmeans_data) - 1))
  Max.k <- nrow(kmeans_data) - 1
}

# Set unique_tag_ID as the unique tag id attribute
unique_tag_ID <- attr(kmeans_data, "unique_tag_ID")

cat("\n Standardising k-means input. \n")

# Remove any non-numerical columns in kmeans_data. Identifying numeric columns
numeric_cols <- sapply(kmeans_data, is.numeric)

# Subset the dataframe to keep only numeric columns
kmeans_data <- kmeans_data[, numeric_cols]

# Apply scaling only to columns that have more than one unique value to avoid division by zero
for (i in 1:ncol(kmeans_data)) {
  if (length(unique(kmeans_data[[i]])) > 1) {
    kmeans_data[[i]] <- scale(kmeans_data[[i]])
  } else {
    cat("\nCaution - cannot standardise a column with a unique value: ", colnames(kmeans_data[i]), "\n")
  }
}

# set up data and loop for k-means
k_values <- 1:Max.k

# Create objects of correct length
sse <- numeric(length(k_values))
sil <- numeric(length(k_values))

sil_values <- list() # create an empty list to store silhouette width

for (k in k_values) {
  # Run the k-means clustering
  kmeans_result <- kmeans(kmeans_data, centers = k, nstart = 1500, iter.max = 25)

  # Extract the Sum of Squared Errors for k
  sse[k] <- kmeans_result$tot.withinss

  # Calculate the silhouette scores, and average silhouette width for each k
  if (k > 1 && length(unique(kmeans_result$cluster)) > 1) {
    # Extract the summary of the silhouette widths using distance matrix computation on kmeans_data - the reduced dimension data
    sil_summary <- summary(cluster::silhouette(as.numeric(kmeans_result$cluster), dist(kmeans_data)))

    # Extract the average width for k
    sil[k] <- sil_summary$avg.width

    # Compute silhouette scores
    sil_scores <- cluster::silhouette(kmeans_result$cluster, dist(kmeans_data))

    # Average silhouette width (higher is better, closer to 1)
    avg_sil_width <- round(mean(sil_scores[, 3]), 3)
    cat(paste0("\n K = ", k, " Average silhouette width = ", avg_sil_width, "\n"))

    # Check number of days in each cluster
    print(table(kmeans_result$cluster))

    # Add to list
    sil_values[[paste0("K_", k)]] <- avg_sil_width
  }
}

# Create the within-cluster sum of squares plot "Elbow method"
Elbow_plot <- ggplot2::ggplot() +
  geom_line(data = data.frame(k_values, sse), aes(x = k_values, y = sse), color = "black") +
  geom_point(data = data.frame(k_values, sse), aes(x = k_values, y = sse), color = "black") +
  labs(x = "Number of Clusters", y = "Within-Cluster Sum of Squares") +
  scale_x_continuous(breaks = seq(1, k, 1)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 16, face = "bold", color = "Black"),
    axis.text.y = element_text(size = 16, face = "bold", color = "Black"),
    axis.title = element_text(size = 16, face = "bold", color = "Black"),
    legend.title = element_text(size = 16, face = "bold", color = "Black"),
    legend.text = element_text(size = 14, face = "bold", color = "Black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks = element_line(linewidth = 1.0, color = "black"),
    axis.line = element_line(linewidth = 1.0)
  )

# Add a vertical line
Elbow_plot <- Elbow_plot + geom_vline(xintercept = 4, linetype = "dashed", color = "red")
Elbow_plot <- Elbow_plot + geom_vline(xintercept = v_line, linetype = "dashed", color = "red")
Elbow_plot

# Create the silhouette width plot
Silhouette_plot <- ggplot2::ggplot() +
  geom_line(data = data.frame(k_values[-1], sil[-1]), aes(x = k_values[-1], y = sil[-1]), color = "black") +
  geom_point(data = data.frame(k_values[-1], sil[-1]), aes(x = k_values[-1], y = sil[-1]), color = "black") +
  labs(x = "Number of Clusters", y = "Silhouette Width") +
  scale_x_continuous(breaks = seq(1, k, 1)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 16, face = "bold", color = "Black"),
    axis.text.y = element_text(size = 16, face = "bold", color = "Black"),
    axis.title = element_text(size = 16, face = "bold", color = "Black"),
    legend.title = element_text(size = 16, face = "bold", color = "Black"),
    legend.text = element_text(size = 14, face = "bold", color = "Black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks = element_line(linewidth = 1.0, color = "black"),
    axis.line = element_line(linewidth = 1.0)
  )

# Add a vertical line
Silhouette_plot <- Silhouette_plot + geom_vline(xintercept = v_line, linetype = "dashed", color = "red")
Silhouette_plot

# Combine the elbow and silhouette width plots
combined_k_plot <- cowplot::plot_grid(Elbow_plot, Silhouette_plot, labels = c("A", "B"), label_size = 16)

# Print the combined plot
print(combined_k_plot)

# Count the number of columns starting with "PC" in combined_data
pc_columns_count <- length(grep("^PC", names(kmeans_data)))

# Save the combined plot
save_folder <- file.path(output_folder, unique_tag_ID, "5_k-means")

# Message
cat(paste0("Single tag output file: ", save_folder, "/Select_k.", pc_columns_count, "_PCs.png\n"))

# Create the directory if it doesn't exist
create_directory(save_folder)

# Save plot to combined data folder
ggsave(file.path(save_folder, paste0("Select_k.", pc_columns_count, "_PCs.png")), plot = combined_k_plot, width = 14.22, height = 5)

#
##### Figure 5 - Separation of clusters (simulated) #####
data_dir <- "E:/Ch 3 data"
tag_list <- "test180" # Simulated data 180 days 1 minute SF
kmeans_data <- readRDS(file = file.path(data_dir, tag_list, "5_k-means/combined_stats.rds"))
kmeans_result <- readRDS(file = file.path(data_dir, tag_list, "5_k-means/kmeans_result.rds"))

k = 5
polygon = TRUE
output_folder = data_dir

## Plotting using k_clustering

# Set the random seed for reproducable results in the function
withr::local_options(set.seed(123))

# Set unique_tag_ID as the unique tag id attribute if it exists
unique_tag_ID <- attr(kmeans_data, "unique_tag_ID")

# Create tag_IDs variable
tag_IDs <- kmeans_data$tag_ID

# Identify numeric columns
numeric_cols <- sapply(kmeans_data, is.numeric)

# Subset the dataframe to keep only numeric columns
numeric_data <- kmeans_data[, numeric_cols]

cat("\n Standardising k-means input. \n")

# Apply scaling only to columns that have more than one unique value to avoid division by zero
for (i in 1:ncol(numeric_data)) {
  if (length(unique(numeric_data[[i]])) > 1) {
    numeric_data[[i]] <- scale(numeric_data[[i]])
  }
}

# Extract the centroids and cluster assignments
centroids <- kmeans_result$centers
k_cluster_assignments <- kmeans_result$cluster

# Calculate distances between each point and corresponding cluster centroid:
kmeans_result$distances2 <- sapply(1:nrow(numeric_data), function(i) {
  euclidean_distance <- sqrt(sum((numeric_data[i, ] - centroids[k_cluster_assignments[i], ])^2))
  return(euclidean_distance)
})

# Assign tag_ID column
kmeans_result$tag_ID <- tag_IDs

# Create table of results
cluster_table <- as.data.frame(table(kmeans_result$cluster))

# Rename the columns
names(cluster_table) <- c("Cluster", "Days in Cluster")

# Print number of days in each cluster
print(cluster_table)
print(paste0("Total of ", sum(cluster_table[, 2]), " days of data"))

# Define the desired columns
desired_pcs <- c("PC1", "PC2", "PC3")

# Use intersect to find common column names between the dataframe and the desired columns
existing_pcs <- intersect(names(numeric_data), desired_pcs)

# Custom select colours
custom_palette <- c(
  "1" = "#E41A1C", # Red
  "2" = "#377EB8", # Blue
  "3" = "#FF7F00", # Orange
  "4" = "#984EA3", # Purple
  "5" = "#4DAF4A", # Green
  "6" = "#00CED1", # Dark Turquoise
  "7" = "#A65628", # Brown
  "8" = "#F781BF", # Pink
  "9" = "#999999", # Grey
  "10" = "#344111", # Dark green
  "11" = "#8A2BE2", # Blue Violet
  "12" = "#FF69B4", # Hot Pink
  "13" = "#CD5C5C", # Indian Red
  "14" = "#7FFF00", # Chartreuse
  "15" = "#D2691E" # Chocolate
)

# Function to generate additional distinct colors
generate_additional_colors <- function(n, existing_colors) {
  total_colors <- n
  additional_colors <- colorspace::qualitative_hcl(total_colors, palette = "Set3")
  additional_colors <- additional_colors[(length(existing_colors) + 1):total_colors]
  names(additional_colors) <- as.character((length(existing_colors) + 1):total_colors)
  combined_palette <- c(existing_colors, additional_colors)
  return(combined_palette)
}

# Generate a palette for k clusters
if (k > 15) {
  additional <- k - 15
  custom_palette <- generate_additional_colors(n = k, existing_colors = custom_palette)

  # Ensure custom_palette has names
  if (is.null(names(custom_palette))) {
    names(custom_palette) <- as.character(1:length(custom_palette))
  }
}

# Create the 3d plot of the top three PC's
if (length(existing_pcs) == length(desired_pcs)) {
  top_3_pcs <- numeric_data[, existing_pcs]

  # Extract cluster assignments from k-means result
  cluster_assignments <- kmeans_result$cluster

  # Create a vector of colours to represent each cluster
  colours <- custom_palette[as.character(cluster_assignments)]

  # Create the 3D scatter plot
  rgl::plot3d(top_3_pcs,
              col = colours,
              xlab = "PC1", ylab = "PC2", zlab = "PC3",
              size = 4
  )

  # Add legend
  unique_clusters <- sort(unique(cluster_assignments))
  legend_colors <- custom_palette[as.character(unique_clusters)]

  rgl::legend3d("topright", legend = paste("Cluster", unique_clusters), pch = 15, col = legend_colors)

  # Create polygon if required
  if (polygon == TRUE) {
    # Add a polygon for each cluster with more than 3 points
    for (cluster in unique(cluster_assignments)) {
      # Get the points belonging to the current cluster
      cluster_points <- top_3_pcs[cluster_assignments == cluster, ]

      # Only proceed if the cluster has at least 4 points
      if (nrow(cluster_points) >= 4) {
        # Compute the convex hull
        hull_indices <- geometry::convhulln(cluster_points)

        # Get the colour for the current cluster
        cluster_colour <- custom_palette[as.character(cluster)]

        # Loop through each row of hull_indices (i.e., each triangle)
        for (i in 1:nrow(hull_indices)) {
          # Extract the indices of the three vertices of the triangle
          triangle_indices <- hull_indices[i, ]

          # Get the coordinates of the three vertices
          triangle_vertices <- cluster_points[triangle_indices, , drop = FALSE]

          # Plot the triangle
          rgl::triangles3d(triangle_vertices, col = alpha(cluster_colour, 0.5), lit = FALSE)
        }
      }
    }
  }
}

# Create cluster_result data frame
cluster_result <- as.data.frame(kmeans_result$cluster)
cluster_result <- cbind(tag_day = rownames(cluster_result), cluster_result)
cluster_result$tag_ID <- kmeans_data$tag_ID
colnames(cluster_result)[2] <- "cluster"

# Save the cluster data
# Set single tag save folder
save_folder <- file.path(output_folder, unique_tag_ID, "5_k-means")

# Create the directory if it doesn't exist
create_directory(save_folder)

# Save the cluster output as an excel sheet
write.csv(cluster_result, file = file.path(save_folder, paste0("Cluster_results_k=", k, "_b.csv")), row.names = FALSE)

# Save the cluster table as an excel sheet
write.csv(cluster_table, file = file.path(save_folder, paste0("Cluster_table_k=", k, "_b.csv")), row.names = FALSE)

# Message folder
cat(paste0("Output folder: ", save_folder, "\n"))

## This section for plotting the cluster means

# Calculate the mean value of each variable within each cluster
cluster_means <- aggregate(numeric_data, by = list(cluster = kmeans_result$cluster), FUN = mean)

# Reshape data to long format for ggplot
cluster_means <- tidyr::pivot_longer(cluster_means, -cluster, names_to = "Variable", values_to = "Mean")

# Ensure 'Variable' is an ordered factor based on its unique appearance order
cluster_means$Variable <- factor(cluster_means$Variable, levels = unique(cluster_means$Variable))

# Output the data as a csv
write.csv(as.data.frame(cluster_means), file = file.path(save_folder, paste0("cluster_variables_k=", k, ".csv")), row.names = FALSE)

# Create a new column 'RenamedVariable' with original names
cluster_means$RenamedVariable <- as.character(cluster_means$Variable)

# Create a named vector for renaming the variables
rename_vars <- c(
  "depth.mean" = "Mean Depth",
  "depth.sd" = "S.D. Depth",
  "depth.min" = "Minimum Depth",
  "depth.max" = "Maximum Depth",
  "mean.abs_vv" = "Mean Absolute\nVertical Velocity",
  "max.dsc_vv" = "Max Descent\nVertical Velocity",
  "max.asc_vv" = "Max Ascent\nVertical Velocity",
  "skewness_depth" = "Depth Skewness",
  "kurtosis_depth" = "Depth Kurtosis",
  "surface_proportion" = "Surface Proportion",
  "st_mean_diff" = "Diel Diff. Mean Depth",
  "st_sd_diff" = "Diel Diff. S.D. Depth",
  "st_range_diff" = "Diel Diff. Depth Range",
  "st_absVV_diff" = "Diel Diff. Absolute\nVertical Velocity",
  "st_ascVV_diff" = "Diel Diff. Ascent\nVertical Velocity",
  "st_dscVV_diff" = "Diel Diff. Descent\nVertical Velocity",
  "surf_prop_diff" = "Diel Diff. Surface\nProportion"
)

# Rename variables based on rename_vars, keeping original names for PC1, PC2, and PC3
cluster_means$RenamedVariable <- sapply(cluster_means$Variable, function(x) {
  if (x %in% names(rename_vars)) {
    rename_vars[[x]]
  } else {
    as.character(x)
  }
})

# Ensure 'RenamedVariable' is an ordered factor based on its unique appearance order
cluster_means$RenamedVariable <- factor(cluster_means$RenamedVariable, levels = unique(cluster_means$RenamedVariable))

# Calculate the number of variables for each cluster
n_val <- length(unique(cluster_means$RenamedVariable))

# Function to create a gradient palette based on the cluster colours
generate_gradient_palette <- function(color, n) {
  lighter_color <- scales::muted(color, l = 95) # Create a lighter version of the color
  colorRampPalette(c(color, lighter_color))(n) # Use the original and lighter color to create the gradient
}

# Generate gradient palettes for each cluster
gradient_palettes <- lapply(custom_palette, generate_gradient_palette, n = n_val)

# Flatten the gradient palettes into a single vector with names
all_gradients <- unlist(gradient_palettes)

# Create names for the gradient colours that match the fill values in the plot
fill_levels <- interaction(rep(names(custom_palette), each = n_val), rep(levels(cluster_means$RenamedVariable), length(custom_palette)))
names(all_gradients) <- fill_levels

# Define the first n_val variables for the legend
legend_levels <- levels(cluster_means$RenamedVariable)[1:n_val]

# Create the custom legend breaks for cluster 1
custom_legend_breaks <- interaction(rep(1, n_val), legend_levels)

# Calculate the range of y-axis values to determine the breaks
y_range <- range(cluster_means$Mean, na.rm = TRUE)
y_breaks <- seq(floor(y_range[1]), ceiling(y_range[2]), by = 1)

# Plot colour version
plot_colour <- ggplot(data = cluster_means, aes(x = as.factor(cluster), y = Mean, fill = interaction(cluster, RenamedVariable))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Cluster",
    y = "Mean Standardised Value"
  ) +
  scale_x_discrete() +
  scale_y_continuous(breaks = y_breaks) + # Set y-axis breaks to every whole number
  theme_classic() +
  theme(
    text = element_text(size = 40),
    axis.text.x = element_text(face = "bold", color = "Black"),
    axis.text.y = element_text(face = "bold", color = "Black"),
    axis.title = element_text(face = "bold", color = "Black"),
    legend.title = element_text(face = "bold", color = "Black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks = element_line(linewidth = 1.0, color = "black"),
    axis.line = element_line(linewidth = 1.0)
  ) +
  guides(fill = guide_legend(title = "Variable", ncol = 1, override.aes = list(alpha = 1)))

# Create a custom fill scale that uses the gradient colours
plot_colour_scaled <- plot_colour +
  scale_fill_manual(
    values = all_gradients,
    breaks = custom_legend_breaks, # Only show legend for cluster 1
    labels = legend_levels # Labels without cluster prefix
  )
plot_colour_scaled

# Save the plot
ggsave(file.path(save_folder, paste0("cluster_variables_k=", k, "_colour.png")), plot = plot_colour_scaled, height = 12, width = 16, dpi = 600)

# Plot black and white version
plot_bw_scaled <- ggplot(cluster_means, aes(x = as.factor(cluster), y = Mean, fill = RenamedVariable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_grey(start = 0.2, end = 0.8) + # Use greyscale for filling bars
  labs(
    x = "Cluster",
    y = "Mean Standardised Value"
  ) +
  scale_x_discrete() +
  scale_y_continuous(breaks = y_breaks) + # Set y-axis breaks to every whole number
  theme_classic() +
  theme(
    #axis.text.x = element_text(hjust = 1),
    text = element_text(size = 40),
    axis.text.x = element_text(face = "bold", color = "Black"),
    axis.text.y = element_text(face = "bold", color = "Black"),
    axis.title = element_text(face = "bold", color = "Black"),
    legend.title = element_text(face = "bold", color = "Black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks = element_line(linewidth = 1.0, color = "black"),
    axis.line = element_line(linewidth = 1.0)
  ) +
  guides(fill = guide_legend(title = "Variable", ncol = 1, override.aes = list(alpha = 1)))

# Save the plot
ggsave(file.path(save_folder, paste0("cluster_variables_k=", k, "_bw.png")), plot = plot_bw_scaled, height = 12, width = 16, dpi = 600)

print(plot_bw_scaled)

kmeans_result$cluster_means <- cluster_means

#
##### Figure 6 - Four combined depth time-series and Wavelet power spectra combined #####
# For TDR's use code below.
# for wavelet spectra use the standard 'create_wavelet()'

library(ggplot2)

plot_TDR_modified <- function(tag_ID,
                     data_folder = data_dir,
                     every_nth = 10,
                     every_s = 60,
                     plot_size = c(12, 6),
                     Y_lim = c(0, 1500, 100),
                     date_breaks = "14 day",
                     dpi = 300,
                     output_folder = data_dir) {
  # Check format of inputs, on error stop
  if (!is.character(tag_ID)) {
    stop("tag_ID must be a character string.")
  }
  if (!is.numeric(every_nth) || every_nth <= 0) {
    stop("every_nth must be a positive integer.")
  }
  if (!is.numeric(every_s) || every_s <= 0) {
    stop("every_s must be a positive integer.")
  }
  # Check that plot_size is numeric, positive and exactly 2 elements long
  if (!is.numeric(plot_size) || length(plot_size) != 2 || any(plot_size <= 0)) {
    stop("plot_size must be a numeric vector of length 2 with positive values.")
  }
  # Check that Y_lim is numeric, positive and exactly 3 elements long
  if (!is.numeric(Y_lim) || length(Y_lim) != 3 || any(Y_lim < 0)) {
    stop("Y_lim must be a numeric vector of length 3 with non-negative values.")
  }
  if ((!is.numeric(dpi) || dpi <= 0)) {
    stop("dpi must be a positive integer.")
  }

  # Read in tag archive
  archive_days <- readRDS(file = file.path(data_folder, tag_ID, "/archive_days.rds"))

  # Calculate the time differences between consecutive records
  time_diffs <- diff(as.numeric(archive_days$date, units = "secs"))

  # Check if the original sampling frequency is consistent
  sampling_interval <- unique(time_diffs)

  if (length(sampling_interval) != 1) {
    stop("Error: The original data does not have a consistent sampling frequency.")
  }

  # Print sampling interval
  cat(paste0("\nData sampling interval is ", sampling_interval, " seconds\n"))

  if (every_s != 0) { # Using time, rather than number of rows to plot data.
    # Check if every_s is a multiple of the original sampling frequency
    if (every_s %% sampling_interval != 0) {
      stop(paste("Error: The specified sampling interval (", every_s, " seconds) is not a multiple of the original sampling interval (", sampling_interval, " seconds).", sep = ""))
    }

    # Get the first timestamp as a numeric value
    start_time <- as.numeric(archive_days$date[1])

    # Filter the dataset by checking if the difference between the current timestamp
    # and the start time is divisible by every_s (the desired interval)
    archive_days <- archive_days[as.numeric(archive_days$date - start_time) %% every_s == 0, ]

    # Print sampling interval
    cat("Plotting every", every_s, "seconds \n")

  } else {
    if (every_nth != 1) {
      # Subset to every nth record
      crop_sq <- seq(every_nth, nrow(archive_days), by = every_nth)
      archive_days <- archive_days[crop_sq, ]

      # Print sampling interval
      cat("Plotting every", every_nth, "records \n")
    } else {
      # Print sampling interval
      cat("Plotting every record \n")
    }
  }

  # Messages
  cat("\nMaximum depth is", max(archive_days$depth), "meters\n")

  # Select plot data
  plot_data <- archive_days[, c("date", "depth")]

  TDR_plot <- ggplot(plot_data, aes(x = date, y = depth)) +
    geom_path() +
    # scale_y_reverse(limits = c(Y_lim[2], Y_lim[1]), breaks = seq(Y_lim[1], Y_lim[2], Y_lim[3]), expand = c(0, 0, 0, 0)) +
    scale_y_reverse(limits = c(Y_lim[2], Y_lim[1]), breaks = seq(Y_lim[1], Y_lim[2], Y_lim[3]), expand = c(0, 0)) +
    scale_x_datetime(date_breaks = date_breaks, date_labels = "%Y-%m-%d", expand = c(0, 0, 0, 0), position = "top") +
    labs(
      x = "Date",
      y = "Depth (m)"
    ) +
    theme_classic() +
    theme(
      axis.text = element_text(size = 14, face = "bold", colour = "black"),
      axis.title = element_text(size = 16, face = "bold", colour = "black"),
      plot.margin = unit(c(0, 1.5, 0.1, 0.1), "lines"), # top, right, bottom, left
      axis.ticks.length = unit(0.2, "cm"),
      axis.ticks = element_line(linewidth = 1.0, color = "black"),
      axis.line = element_line(linewidth = 1.0)
    )
  print(TDR_plot)

  ggsave(file.path(output_folder, tag_ID, paste0(tag_ID, "_archive.png")), plot = TDR_plot,
         width = plot_size[1], height = plot_size[2], dpi = dpi, create.dir = TRUE)

  # These transformations reduce the plot vertically too strongly
  # plot_data$depth1 <- plot_data$depth + 1
  # Apply log10 transformation to depth + 1
  # plot_data$neg_log_depth <- -log10(plot_data$depth1)
  # Apply log(x + 1) transformation to depth + 1
  # plot_data$log1p_depth <- log1p(plot_data$depth1)  # log1p is log(x + 1)

  # summary(plot_data)

  # Correct for the 0.5 offset in 130970a
  if (i == 3) {
    # Subtract 0.5 from all depths that are greater than or equal to 0.5
    plot_data$depth[plot_data$depth >= 0.5] <- plot_data$depth[plot_data$depth >= 0.5] - 0.5
  }

  # Apply square root transformation to depth
  plot_data$sqrt_depth <- sqrt(plot_data$depth)

  # Define the original depth breaks and the corresponding sqrt-transformed values
  original_depths <- sqrt_scale
  sqrt_breaks <- sqrt(original_depths)  # Apply square root transformation to the breaks

  TDR_plot_sqrt <- ggplot(plot_data, aes(x = date, y = sqrt_depth)) +
    geom_path() +
    scale_y_reverse(
      breaks = sqrt_breaks,  # Use the transformed breaks
      labels = original_depths,  # Use the original depth values as labels
      expand = c(0, 0)
    ) +  # Reverse y-axis for depth
    scale_x_datetime(
      date_breaks = date_breaks,
      date_labels = "%Y-%m-%d",
      expand = c(0, 0),
      position = "top"
    ) +
    labs(
      x = "Date",
      y = "Depth (m)"
    ) +
    theme_classic() +
    theme(
      axis.text = element_text(size = 14, face = "bold", colour = "black"),
      axis.title = element_text(size = 16, face = "bold", colour = "black"),
      plot.margin = unit(c(0, 1.5, 0.1, 0.1), "lines"), # top, right, bottom, left
      axis.ticks.length = unit(0.2, "cm"),
      axis.ticks = element_line(linewidth = 1.0, color = "black"),
      axis.line = element_line(linewidth = 1.0)
    )

  print(TDR_plot_sqrt)

  ggsave(file.path(output_folder, tag_ID, paste0(tag_ID, "_archive_sqrt.png")), plot = TDR_plot_sqrt,
         width = plot_size[1], height = plot_size[2], dpi = dpi, create.dir = TRUE)

  cat("\nOutput file:", file.path(output_folder, tag_ID, paste0(tag_ID, "_archive.png")))

  return(plot_data)
}

# 130970a Oceanic manta
i <- 3
every_s <- 60
Y_lim <- c(0, 799, 200)
sqrt_scale <- c(0,200,400,600)
date_breaks <- "14 day"

# 333237
i <- 8
every_s <- 60
Y_lim <- c(0, 85, 25)
sqrt_scale <- c(0,25,50,75)
date_breaks <- "14 day"

# 2255 10m
i <- 11
every_s <- 600
Y_lim <- c(0, 105, 25)
sqrt_scale <- c(0,25,50,75, 100)
date_breaks <- "56 day"

# 75379
i <- 13
every_s <- 60
Y_lim <- c(0, 1249, 250)
sqrt_scale <- c(0,250,500,750,1000)
date_breaks <- "14 day"

TDR_plot <- plot_TDR_modified(
  tag_ID = meta$tag_ID[i],
  data_folder = data_dir,
  every_s = every_s,
  plot_size = c(12, 6),
  Y_lim = Y_lim,
  date_breaks = date_breaks,
  output_folder = file.path(data_dir)
)

rm(i, TDR_plot)


tag_ID = meta$tag_ID[i]
data_folder = data_dir
every_nth = 10
plot_size = c(12, 6)
dpi = 300
output_folder = file.path(data_dir)

#
##### Figure 7 - Four species plots of cluster days #####

data_dir <- "E:/Ch 3 data"

# Panel A. Oceanic manta ray 130970a
tag_list <- c("130970a")

tag_vector <- tag_list
data_folder = data_dir
kmeans_result <- readRDS(file = file.path(data_dir, tag_list, "5_k-means/kmeans_result.rds"))
No_days = 3
every_nth = 12 # 5 second depth sampling frequency
Y_lim = c(0, 250, 50)
color = TRUE
diel_shade = TRUE
dpi = 300
output_folder = "E:/My Drive/PhD work/4.3 Chapter Three/Fig 6/"

# Initialize an empty list for storing data frames from each tag
data_list <- list()

# Initialize an empty data frame for storing unique dates and tag IDs
id_data <- data.frame(date_only = as.Date(character()), tag_ID = character(), stringsAsFactors = FALSE)

# Load the archive data for the single tag
archive_days <- readRDS(file.path(data_folder, tag_vector, "archive_days.rds"))
archive_days$tag_ID <- tag_vector # Add tag_ID column

cat(paste0("\nMaximum depth is ", max(archive_days$depth)))
cat(paste0("\nSampling interval is ", as.numeric(archive_days$date[2] - archive_days$date[1], units = "secs"), " seconds\n"))

# Directly assign the unique dates and tag ID for the single tag
unique_dates <- unique(archive_days$date_only)
id_data <- data.frame(date_only = unique_dates, tag_ID = tag_vector, stringsAsFactors = FALSE)

# Filter to every nth record
crop_sq <- seq(every_nth, nrow(archive_days), by = every_nth)
all_archive_days <- archive_days[crop_sq, ]

# New sampling interval
sampling_interval <- as.numeric(all_archive_days$date[2] - all_archive_days$date[1], units = "secs")
cat("Plotting every", sampling_interval, "seconds")

# Wrap the single all_archive_days in a list to standardise the structure
data_list[[tag_vector]] <- all_archive_days

# Calculate distances between each point (day) and corresponding cluster centroid:
distances <- kmeans_result$distances

# Set all_archive_days as a data.table
data.table::setDT(all_archive_days)

# Count unique date_only values for each tag_ID
unique_dates_by_tag <- all_archive_days[, list(unique_dates = data.table::uniqueN(date_only)), by = tag_ID]

# Sum all the unique counts across all tag IDs
total_unique_dates <- sum(unique_dates_by_tag$unique_dates)

# Add all cluster assignment to id_data
id_data$cluster <- kmeans_result$cluster

# Set k as the number of unique clusters
k <- length(unique(kmeans_result$cluster))

# Create a data frame with cluster assignments and distances
cluster_distances <- data.frame(cluster = id_data$cluster, distance = distances, date_only = id_data$date_only)

# Sort the data frame by cluster and distance
sorted_cluster_distances <- cluster_distances[order(cluster_distances$cluster, cluster_distances$distance), ]

# Create a data frame to hold values
best_k_representatives <- data.frame()

# Find the 'No_days' days closest to the cluster centroid for each cluster (i)
for (i in 1:k) {
  cluster_representatives <- head(sorted_cluster_distances[sorted_cluster_distances$cluster == i, ], No_days)
  cluster_representatives$row <- as.numeric(rownames(cluster_representatives))


  # Add the date_only, tag_ID, and cluster columns
  cluster_representatives$date <- id_data[cluster_representatives$row, "date_only"]
  cluster_representatives$tag_ID <- id_data[cluster_representatives$row, "tag_ID"]

  best_k_representatives <- rbind(best_k_representatives, cluster_representatives)

  if (nrow(cluster_representatives) < No_days) {
    cat(paste0("\n There are less than ", No_days, " representatives available for cluster ", i, "\n"))
  }
}

# Set save folder name based on diel_shade
if (diel_shade == TRUE) {
  folder <- paste0("6_Cluster-plots.K=", k, "_shaded")
} else {
  folder <- paste0("6_Cluster-plots.K=", k)
}

# Set the save folder location
save_folder <- file.path(output_folder, tag_vector, folder)

# Create the directory if it doesn't exist
create_directory(save_folder)

# Save the list of clusters with dates (best_k_representatives, date and tag)
write.csv(best_k_representatives, file = file.path(save_folder, "cluster_dates.csv"))

# Save the tag list
write(tag_vector, file = file.path(save_folder, "tag_IDs.txt"))

# Custom select colours
custom_palette <- c(
  "1" = "#E41A1C", # Red
  "2" = "#377EB8", # Blue
  "3" = "#FF7F00", # Orange
  "4" = "#984EA3", # Purple
  "5" = "#4DAF4A", # Green
  "6" = "#00CED1" # Dark Turquoise
)

# Create the lists for the facet plots
plots_list <- list()

# Create a list of Y_lim values
Y_lim_list <- list(
  Y_lim1 = c(0, 150, 100),
  Y_lim2 = c(0, 150, 100),
  Y_lim3 = c(0, 750, 600),
  Y_lim4 = c(0, 275, 150),
  Y_lim5 = c(0, 150, 100)
)

cluster_no <- 1
# Iterate over each cluster in turn. Select the days closest to the cluster centroid, extract the data and plot them
for (cluster_no in 1:k) {
  # Select the best representatives for the cluster (i):
  selected_days <- best_k_representatives[best_k_representatives$cluster == cluster_no, ]

  # Extract the data to use
  dates <- id_data[selected_days$row, c("date_only", "tag_ID", "cluster")]

  #  Rename the column for matching
  dates$date <- as.Date(dates$date_only)

  cat(paste0("\n Cluster ", cluster_no, " dates ", dates$date, " Tag: ", dates$tag_ID, "\n"))

  # Initialize an empty list to store the extracted data
  data_list <- list()

  # Iterate over the rows of 'dates' and extract the matching rows from 'all_archive_days' using date and tag_ID (duplicated dates in tag_ID's)
  for (j in 1:nrow(dates)) {
    # For each day in dates where date and tag_ID match the cluster, create plot_data
    plot_data <- all_archive_days[all_archive_days$date_only == dates$date[j] & all_archive_days$tag_ID == dates$tag_ID[j], ]

    # Add plot_data to data_list
    data_list[[j]] <- plot_data
  }

  # Combine the extracted data into a single data frame
  plot_data <- do.call(rbind, data_list)

  # Assign a unique number to each date
  plot_data$date_number <- match(plot_data$date_only, unique(plot_data$date_only))

  # Create a time column and set all dates to the same date to facet
  plot_data$time <- format(as.POSIXct(plot_data$date), "%H:%M:%S")
  plot_data$time <- as.POSIXct(paste("1970-01-01", plot_data$time), format = "%Y-%m-%d %H:%M:%S")
  plot_data <- as.data.frame(plot_data)

  # Create a sequence of specific times incrementing by 6 hours
  time_breaks <- seq(
    from = as.POSIXct("1970-01-01 06:00:00"),
    to = as.POSIXct("1970-01-01 18:00:00"),
    by = "6 hours"
  )

  ## Change y-limits here
  summary(plot_data$depth)
  Y_lim <- Y_lim_list[[cluster_no]]

  # Plot the base TDR (in distance from cluster centre order)
  cluster_plot <-
    ggplot(data = plot_data, aes(x = time, y = depth)) +
    geom_line(colour = custom_palette[cluster_no], linewidth = 1) +
    scale_x_datetime(breaks = time_breaks, date_labels = "%H %M", position = "top", expand = c(0.001, 0, 0, 0)) +
    scale_y_reverse(limits = c(Y_lim[2], Y_lim[1]), breaks = seq(Y_lim[1], Y_lim[2], Y_lim[3]), expand = c(0, 0)) +
    labs(x = "Time of Day", y = "Depth (m)") +
    theme_bw() +
    theme(
      plot.margin = unit(c(5.5, 25, 10.5, 5.5), "points"),
      legend.position = "NONE",

      text = element_text(size = 40, face = "bold", color = "Black"),
      axis.text.x = element_text(face = "bold", color = "Black"),
      axis.text.y = element_text(face = "bold", color = "Black"),
      #axis.text.x = element_blank(),
      #axis.title = element_blank(),
      strip.text.y = element_blank(), # removes 'date_number' label
      axis.ticks.length.x = unit(0.4, "cm"),
      axis.ticks.length.y = unit(0.4, "cm"),
      axis.ticks = element_line(linewidth = 1.5, color = "black"),
      axis.line.x = element_line(linewidth = 1.5, color = "black"),
      axis.line.y = element_line(linewidth = 1.5, color = "black")
    ) +
    facet_grid(date_number ~ .,) # axes = "all")

  #cluster_plot
  fixed_Y_plot <- cluster_plot

  # Add shading limits to plot_data so facet doesn't alter them (uses last plot in list)
  plot_data$ymax <- Y_lim[2]
  plot_data$ymin <- Y_lim[1]

  # Plot with diel shading
  if (diel_shade) {
    fixed_Y_plot <- fixed_Y_plot +
      geom_rect(
        # Midnight to sunrise shading
        data = plot_data,
        aes(
          xmin = as.POSIXct("1970-01-01 00:00:00"),
          xmax = as.POSIXct(paste("1970-01-01", format(sunrise, "%H:%M:%S"))),
          ymin = ymin,
          ymax = ymax
        ),
        fill = "grey", alpha = 0.01
      ) +
      geom_rect(
        # Sunset to midnight shading
        data = plot_data,
        aes(
          xmin = as.POSIXct(paste("1970-01-01", format(sunset, "%H:%M:%S"))),
          xmax = as.POSIXct("1970-01-01 23:59:59"),
          ymin = ymin,
          ymax = ymax
        ),
        fill = "grey", alpha = 0.01
      )
  }

  # fixed_Y_plot

  # Add plot to list for all clusters facet
  plots_list[[cluster_no]] <- fixed_Y_plot

  # Save the plot
  ggsave(file.path(save_folder, paste0("Cluster_", cluster_no, ".png")), plot = fixed_Y_plot, width = 14.22, height = 10, dpi = dpi)
}

# Facet the plots using 'patchwork'
for (i in 1:length(plots_list)) {
  # Modify first cluster (plot) separately
  if (i == 1) {
    plots_list[[i]] <- plots_list[[i]]
      #labs(x = NULL, y = NULL) + # Turn off all labels in first cluster
  } else {
    # Modify subsequent plots
    plots_list[[i]] <- plots_list[[i]] +
      labs(x = NULL, y = NULL) + # Turn off all labels in subsequent clusters
      theme(
        axis.text.x = element_blank(), # No x-axis text (06:00, 12:00, 18:00)
        axis.ticks.x = element_blank(), # No x-axis ticks
        axis.title.y = element_blank(),
      )
  }
}

# Add the y-axis title using plot_annotation
facet_plot_k <- patchwork::wrap_plots(plots_list, ncol = 1) +
  patchwork::plot_layout(axis_titles = 'collect')
# facet_plot_k

# Save the plot
ggsave(file.path(save_folder, "Cluster_facet.png"), plot = facet_plot_k, width = 14, height = 21, dpi = dpi)

rm(list = ls())
gc()




## Whale shark
data_dir <- "E:/Ch 3 data"

# Panel B.
tag_list <- c("75379")

tag_vector <- tag_list
data_folder = data_dir
kmeans_result <- readRDS(file = file.path(data_dir, tag_list, "5_k-means/kmeans_result.rds"))
No_days = 3
every_nth = 1 # 60 second depth sampling frequency
color = TRUE
diel_shade = TRUE
dpi = 300
output_folder = "E:/My Drive/PhD work/4.3 Chapter Three/Fig 6/"

# Initialize an empty list for storing data frames from each tag
data_list <- list()

# Initialize an empty data frame for storing unique dates and tag IDs
id_data <- data.frame(date_only = as.Date(character()), tag_ID = character(), stringsAsFactors = FALSE)

# Load the archive data for the single tag
archive_days <- readRDS(file.path(data_folder, tag_vector, "archive_days.rds"))
archive_days$tag_ID <- tag_vector # Add tag_ID column

cat(paste0("\nMaximum depth is ", max(archive_days$depth)))
cat(paste0("\nSampling interval is ", as.numeric(archive_days$date[2] - archive_days$date[1], units = "secs"), " seconds\n"))

# Directly assign the unique dates and tag ID for the single tag
unique_dates <- unique(archive_days$date_only)
id_data <- data.frame(date_only = unique_dates, tag_ID = tag_vector, stringsAsFactors = FALSE)

# Filter to every nth record
crop_sq <- seq(every_nth, nrow(archive_days), by = every_nth)
all_archive_days <- archive_days[crop_sq, ]

# New sampling interval
sampling_interval <- as.numeric(all_archive_days$date[2] - all_archive_days$date[1], units = "secs")
cat("Plotting every", sampling_interval, "seconds")

# Wrap the single all_archive_days in a list to standardise the structure
data_list[[tag_vector]] <- all_archive_days

# Calculate distances between each point (day) and corresponding cluster centroid:
distances <- kmeans_result$distances

# Set all_archive_days as a data.table
data.table::setDT(all_archive_days)

# Count unique date_only values for each tag_ID
unique_dates_by_tag <- all_archive_days[, list(unique_dates = data.table::uniqueN(date_only)), by = tag_ID]

# Sum all the unique counts across all tag IDs
total_unique_dates <- sum(unique_dates_by_tag$unique_dates)

# Add all cluster assignment to id_data
id_data$cluster <- kmeans_result$cluster

# Set k as the number of unique clusters
k <- length(unique(kmeans_result$cluster))

# Create a data frame with cluster assignments and distances
cluster_distances <- data.frame(cluster = id_data$cluster, distance = distances, date_only = id_data$date_only)

# Sort the data frame by cluster and distance
sorted_cluster_distances <- cluster_distances[order(cluster_distances$cluster, cluster_distances$distance), ]

# Create a data frame to hold values
best_k_representatives <- data.frame()

# Find the 'No_days' days closest to the cluster centroid for each cluster (i)
for (i in 1:k) {
  cluster_representatives <- head(sorted_cluster_distances[sorted_cluster_distances$cluster == i, ], No_days)
  cluster_representatives$row <- as.numeric(rownames(cluster_representatives))


  # Add the date_only, tag_ID, and cluster columns
  cluster_representatives$date <- id_data[cluster_representatives$row, "date_only"]
  cluster_representatives$tag_ID <- id_data[cluster_representatives$row, "tag_ID"]

  best_k_representatives <- rbind(best_k_representatives, cluster_representatives)

  if (nrow(cluster_representatives) < No_days) {
    cat(paste0("\n There are less than ", No_days, " representatives available for cluster ", i, "\n"))
  }
}

# Set save folder name based on diel_shade
if (diel_shade == TRUE) {
  folder <- paste0("6_Cluster-plots.K=", k, "_shaded")
} else {
  folder <- paste0("6_Cluster-plots.K=", k)
}

# Set the save folder location
save_folder <- file.path(output_folder, tag_vector, folder)

# Create the directory if it doesn't exist
create_directory(save_folder)

# Save the list of clusters with dates (best_k_representatives, date and tag)
write.csv(best_k_representatives, file = file.path(save_folder, "cluster_dates.csv"))

# Save the tag list
write(tag_vector, file = file.path(save_folder, "tag_IDs.txt"))

# Custom select colours
custom_palette <- c(
  "1" = "#E41A1C", # Red
  "2" = "#377EB8", # Blue
  "3" = "#FF7F00", # Orange
  "4" = "#984EA3", # Purple
  "5" = "#4DAF4A", # Green
  "6" = "#00CED1" # Dark Turquoise
)

# Create the lists for the facet plots
plots_list <- list()

# Create a list of Y_lim values
Y_lim_list <- list(
  Y_lim1 = c(0, 550, 400),
  Y_lim2 = c(0, 550, 400),
  Y_lim3 = c(0, 325, 200),
  Y_lim4 = c(0, 500, 400)
)

cluster_no <- 1
# Iterate over each cluster in turn. Select the days closest to the cluster centroid, extract the data and plot them
for (cluster_no in 1:k) {
  # Select the best representatives for the cluster (i):
  selected_days <- best_k_representatives[best_k_representatives$cluster == cluster_no, ]

  # Extract the data to use
  dates <- id_data[selected_days$row, c("date_only", "tag_ID", "cluster")]

  #  Rename the column for matching
  dates$date <- as.Date(dates$date_only)

  cat(paste0("\n Cluster ", cluster_no, " dates ", dates$date, " Tag: ", dates$tag_ID, "\n"))

  # Initialize an empty list to store the extracted data
  data_list <- list()

  # Iterate over the rows of 'dates' and extract the matching rows from 'all_archive_days' using date and tag_ID (duplicated dates in tag_ID's)
  for (j in 1:nrow(dates)) {
    # For each day in dates where date and tag_ID match the cluster, create plot_data
    plot_data <- all_archive_days[all_archive_days$date_only == dates$date[j] & all_archive_days$tag_ID == dates$tag_ID[j], ]

    # Add plot_data to data_list
    data_list[[j]] <- plot_data
  }

  # Combine the extracted data into a single data frame
  plot_data <- do.call(rbind, data_list)

  # Assign a unique number to each date
  plot_data$date_number <- match(plot_data$date_only, unique(plot_data$date_only))

  # Create a time column and set all dates to the same date to facet
  plot_data$time <- format(as.POSIXct(plot_data$date), "%H:%M:%S")
  plot_data$time <- as.POSIXct(paste("1970-01-01", plot_data$time), format = "%Y-%m-%d %H:%M:%S")
  plot_data <- as.data.frame(plot_data)

  # Create a sequence of specific times incrementing by 6 hours
  time_breaks <- seq(
    from = as.POSIXct("1970-01-01 06:00:00"),
    to = as.POSIXct("1970-01-01 18:00:00"),
    by = "6 hours"
  )

  ## Change y-limits here
  summary(plot_data$depth)
  Y_lim <- Y_lim_list[[cluster_no]]

  # Plot the base TDR (in distance from cluster centre order)
  cluster_plot <-
    ggplot(data = plot_data, aes(x = time, y = depth)) +
    geom_line(colour = custom_palette[cluster_no], linewidth = 1) +
    scale_x_datetime(breaks = time_breaks, date_labels = "%H %M", position = "top", expand = c(0.001, 0, 0, 0)) +
    scale_y_reverse(limits = c(Y_lim[2], Y_lim[1]), breaks = seq(Y_lim[1], Y_lim[2], Y_lim[3]), expand = c(0, 0)) +
    labs(x = "Time of Day", y = "Depth (m)") +
    theme_bw() +
    theme(
      plot.margin = unit(c(5.5, 25, 10.5, 5.5), "points"),
      legend.position = "NONE",

      text = element_text(size = 40, face = "bold", color = "Black"),
      axis.text.x = element_text(face = "bold", color = "Black"),
      axis.text.y = element_text(face = "bold", color = "Black"),
      #axis.text.x = element_blank(),
      #axis.title = element_blank(),
      strip.text.y = element_blank(), # removes 'date_number' label
      axis.ticks.length.x = unit(0.4, "cm"),
      axis.ticks.length.y = unit(0.4, "cm"),
      axis.ticks = element_line(linewidth = 1.5, color = "black"),
      axis.line.x = element_line(linewidth = 1.5, color = "black"),
      axis.line.y = element_line(linewidth = 1.5, color = "black")
    ) +
    facet_grid(date_number ~ .,) # axes = "all")

  #cluster_plot
  fixed_Y_plot <- cluster_plot

  # Add shading limits to plot_data so facet doesn't alter them (uses last plot in list)
  plot_data$ymax <- Y_lim[2]
  plot_data$ymin <- Y_lim[1]

  # Plot with diel shading
  if (diel_shade) {
    fixed_Y_plot <- fixed_Y_plot +
      geom_rect(
        # Midnight to sunrise shading
        data = plot_data,
        aes(
          xmin = as.POSIXct("1970-01-01 00:00:00"),
          xmax = as.POSIXct(paste("1970-01-01", format(sunrise, "%H:%M:%S"))),
          ymin = ymin,
          ymax = ymax
        ),
        fill = "grey", alpha = 0.01
      ) +
      geom_rect(
        # Sunset to midnight shading
        data = plot_data,
        aes(
          xmin = as.POSIXct(paste("1970-01-01", format(sunset, "%H:%M:%S"))),
          xmax = as.POSIXct("1970-01-01 23:59:59"),
          ymin = ymin,
          ymax = ymax
        ),
        fill = "grey", alpha = 0.01
      )
  }

  # fixed_Y_plot

  # Add plot to list for all clusters facet
  plots_list[[cluster_no]] <- fixed_Y_plot

  # Save the plot
  ggsave(file.path(save_folder, paste0("Cluster_", cluster_no, ".png")), plot = fixed_Y_plot, width = 14.22, height = 10, dpi = dpi)
}

# Facet the plots using 'patchwork'
for (i in 1:length(plots_list)) {
  # Modify first cluster (plot) separately
  if (i == 1) {
    plots_list[[i]] <- plots_list[[i]]
    #labs(x = NULL, y = NULL) + # Turn off all labels in first cluster
  } else {
    # Modify subsequent plots
    plots_list[[i]] <- plots_list[[i]] +
      labs(x = NULL, y = NULL) + # Turn off all labels in subsequent clusters
      theme(
        axis.text.x = element_blank(), # No x-axis text (06:00, 12:00, 18:00)
        axis.ticks.x = element_blank(), # No x-axis ticks
        axis.title.y = element_blank(),
      )
  }
}

# Add the y-axis title using plot_annotation
facet_plot_k <- patchwork::wrap_plots(plots_list, ncol = 1) +
  patchwork::plot_layout(axis_titles = 'collect')
# facet_plot_k

# Save the plot
ggsave(file.path(save_folder, "Cluster_facet.png"), plot = facet_plot_k, width = 14, height = 21, dpi = dpi)

rm(list = ls())
gc()




## Atlantic cod 225510m
data_dir <- "E:/Ch 3 data"

# Panel C.
tag_list <- c("225510m")

tag_vector <- tag_list
data_folder = data_dir
kmeans_result <- readRDS(file = file.path(data_dir, tag_list, "5_k-means/kmeans_result.rds"))
No_days = 3
every_nth = 1 # 600 seconds sampling interval
color = TRUE
diel_shade = TRUE
dpi = 300
output_folder = "E:/My Drive/PhD work/4.3 Chapter Three/Fig 6/"

# Initialize an empty list for storing data frames from each tag
data_list <- list()

# Initialize an empty data frame for storing unique dates and tag IDs
id_data <- data.frame(date_only = as.Date(character()), tag_ID = character(), stringsAsFactors = FALSE)

# Load the archive data for the single tag
archive_days <- readRDS(file.path(data_folder, tag_vector, "archive_days.rds"))
archive_days$tag_ID <- tag_vector # Add tag_ID column

cat(paste0("\nMaximum depth is ", max(archive_days$depth)))
cat(paste0("\nSampling interval is ", as.numeric(archive_days$date[2] - archive_days$date[1], units = "secs"), " seconds\n"))

# Directly assign the unique dates and tag ID for the single tag
unique_dates <- unique(archive_days$date_only)
id_data <- data.frame(date_only = unique_dates, tag_ID = tag_vector, stringsAsFactors = FALSE)

# Filter to every nth record
crop_sq <- seq(every_nth, nrow(archive_days), by = every_nth)
all_archive_days <- archive_days[crop_sq, ]

# New sampling interval
sampling_interval <- as.numeric(all_archive_days$date[2] - all_archive_days$date[1], units = "secs")
cat("Plotting every", sampling_interval, "seconds")

# Wrap the single all_archive_days in a list to standardise the structure
data_list[[tag_vector]] <- all_archive_days

# Calculate distances between each point (day) and corresponding cluster centroid:
distances <- kmeans_result$distances

# Set all_archive_days as a data.table
data.table::setDT(all_archive_days)

# Count unique date_only values for each tag_ID
unique_dates_by_tag <- all_archive_days[, list(unique_dates = data.table::uniqueN(date_only)), by = tag_ID]

# Sum all the unique counts across all tag IDs
total_unique_dates <- sum(unique_dates_by_tag$unique_dates)

# Add all cluster assignment to id_data
id_data$cluster <- kmeans_result$cluster

# Set k as the number of unique clusters
k <- length(unique(kmeans_result$cluster))

# Create a data frame with cluster assignments and distances
cluster_distances <- data.frame(cluster = id_data$cluster, distance = distances, date_only = id_data$date_only)

# Sort the data frame by cluster and distance
sorted_cluster_distances <- cluster_distances[order(cluster_distances$cluster, cluster_distances$distance), ]

# Create a data frame to hold values
best_k_representatives <- data.frame()

# Find the 'No_days' days closest to the cluster centroid for each cluster (i)
for (i in 1:k) {
  cluster_representatives <- head(sorted_cluster_distances[sorted_cluster_distances$cluster == i, ], No_days)
  cluster_representatives$row <- as.numeric(rownames(cluster_representatives))


  # Add the date_only, tag_ID, and cluster columns
  cluster_representatives$date <- id_data[cluster_representatives$row, "date_only"]
  cluster_representatives$tag_ID <- id_data[cluster_representatives$row, "tag_ID"]

  best_k_representatives <- rbind(best_k_representatives, cluster_representatives)

  if (nrow(cluster_representatives) < No_days) {
    cat(paste0("\n There are less than ", No_days, " representatives available for cluster ", i, "\n"))
  }
}

# Set save folder name based on diel_shade
if (diel_shade == TRUE) {
  folder <- paste0("6_Cluster-plots.K=", k, "_shaded")
} else {
  folder <- paste0("6_Cluster-plots.K=", k)
}

# Set the save folder location
save_folder <- file.path(output_folder, tag_vector, folder)

# Create the directory if it doesn't exist
create_directory(save_folder)

# Save the list of clusters with dates (best_k_representatives, date and tag)
write.csv(best_k_representatives, file = file.path(save_folder, "cluster_dates.csv"))

# Save the tag list
write(tag_vector, file = file.path(save_folder, "tag_IDs.txt"))

# Custom select colours
custom_palette <- c(
  "1" = "#E41A1C", # Red
  "2" = "#377EB8", # Blue
  "3" = "#FF7F00", # Orange
  "4" = "#984EA3", # Purple
  "5" = "#4DAF4A", # Green
  "6" = "#00CED1" # Dark Turquoise
)

# Create the lists for the facet plots
plots_list <- list()

# Create a list of Y_lim values
Y_lim_list <- list(
  Y_lim1 = c(20, 55, 20),
  Y_lim2 = c(40, 70, 20),
  Y_lim3 = c(0, 30, 20)
)

cluster_no <- 1
# Iterate over each cluster in turn. Select the days closest to the cluster centroid, extract the data and plot them
for (cluster_no in 1:k) {
  # Select the best representatives for the cluster (i):
  selected_days <- best_k_representatives[best_k_representatives$cluster == cluster_no, ]

  # Extract the data to use
  dates <- id_data[selected_days$row, c("date_only", "tag_ID", "cluster")]

  #  Rename the column for matching
  dates$date <- as.Date(dates$date_only)

  cat(paste0("\n Cluster ", cluster_no, " dates ", dates$date, " Tag: ", dates$tag_ID, "\n"))

  # Initialize an empty list to store the extracted data
  data_list <- list()

  # Iterate over the rows of 'dates' and extract the matching rows from 'all_archive_days' using date and tag_ID (duplicated dates in tag_ID's)
  for (j in 1:nrow(dates)) {
    # For each day in dates where date and tag_ID match the cluster, create plot_data
    plot_data <- all_archive_days[all_archive_days$date_only == dates$date[j] & all_archive_days$tag_ID == dates$tag_ID[j], ]

    # Add plot_data to data_list
    data_list[[j]] <- plot_data
  }

  # Combine the extracted data into a single data frame
  plot_data <- do.call(rbind, data_list)

  # Assign a unique number to each date
  plot_data$date_number <- match(plot_data$date_only, unique(plot_data$date_only))

  # Create a time column and set all dates to the same date to facet
  plot_data$time <- format(as.POSIXct(plot_data$date), "%H:%M:%S")
  plot_data$time <- as.POSIXct(paste("1970-01-01", plot_data$time), format = "%Y-%m-%d %H:%M:%S")
  plot_data <- as.data.frame(plot_data)

  # Create a sequence of specific times incrementing by 6 hours
  time_breaks <- seq(
    from = as.POSIXct("1970-01-01 06:00:00"),
    to = as.POSIXct("1970-01-01 18:00:00"),
    by = "6 hours"
  )

  ## Change y-limits here
  summary(plot_data$depth)
  Y_lim <- Y_lim_list[[cluster_no]]

  # Plot the base TDR (in distance from cluster centre order)
  cluster_plot <-
    ggplot(data = plot_data, aes(x = time, y = depth)) +
    geom_line(colour = custom_palette[cluster_no], linewidth = 1) +
    scale_x_datetime(breaks = time_breaks, date_labels = "%H %M", position = "top", expand = c(0.001, 0, 0, 0)) +
    scale_y_reverse(limits = c(Y_lim[2], Y_lim[1]), breaks = seq(Y_lim[1], Y_lim[2], Y_lim[3]), expand = c(0, 0)) +
    labs(x = "Time of Day", y = "Depth (m)") +
    theme_bw() +
    theme(
      plot.margin = unit(c(5.5, 25, 10.5, 5.5), "points"),
      legend.position = "NONE",

      text = element_text(size = 40, face = "bold", color = "Black"),
      axis.text.x = element_text(face = "bold", color = "Black"),
      axis.text.y = element_text(face = "bold", color = "Black"),
      #axis.text.x = element_blank(),
      #axis.title = element_blank(),
      strip.text.y = element_blank(), # removes 'date_number' label
      axis.ticks.length.x = unit(0.4, "cm"),
      axis.ticks.length.y = unit(0.4, "cm"),
      axis.ticks = element_line(linewidth = 1.5, color = "black"),
      axis.line.x = element_line(linewidth = 1.5, color = "black"),
      axis.line.y = element_line(linewidth = 1.5, color = "black")
    ) +
    facet_grid(date_number ~ .,) # axes = "all")

  #cluster_plot
  fixed_Y_plot <- cluster_plot

  # Add shading limits to plot_data so facet doesn't alter them (uses last plot in list)
  plot_data$ymax <- Y_lim[2]
  plot_data$ymin <- Y_lim[1]

  # Plot with diel shading
  if (diel_shade) {
    fixed_Y_plot <- fixed_Y_plot +
      geom_rect(
        # Midnight to sunrise shading
        data = plot_data,
        aes(
          xmin = as.POSIXct("1970-01-01 00:00:00"),
          xmax = as.POSIXct(paste("1970-01-01", format(sunrise, "%H:%M:%S"))),
          ymin = ymin,
          ymax = ymax
        ),
        fill = "grey", alpha = 0.01
      ) +
      geom_rect(
        # Sunset to midnight shading
        data = plot_data,
        aes(
          xmin = as.POSIXct(paste("1970-01-01", format(sunset, "%H:%M:%S"))),
          xmax = as.POSIXct("1970-01-01 23:59:59"),
          ymin = ymin,
          ymax = ymax
        ),
        fill = "grey", alpha = 0.01
      )
  }

  # fixed_Y_plot

  # Add plot to list for all clusters facet
  plots_list[[cluster_no]] <- fixed_Y_plot

  # Save the plot
  ggsave(file.path(save_folder, paste0("Cluster_", cluster_no, ".png")), plot = fixed_Y_plot, width = 14.22, height = 10, dpi = dpi)
}

# Facet the plots using 'patchwork'
for (i in 1:length(plots_list)) {
  # Modify first cluster (plot) separately
  if (i == 1) {
    plots_list[[i]] <- plots_list[[i]]
    #labs(x = NULL, y = NULL) + # Turn off all labels in first cluster
  } else {
    # Modify subsequent plots
    plots_list[[i]] <- plots_list[[i]] +
      labs(x = NULL, y = NULL) + # Turn off all labels in subsequent clusters
      theme(
        axis.text.x = element_blank(), # No x-axis text (06:00, 12:00, 18:00)
        axis.ticks.x = element_blank(), # No x-axis ticks
        axis.title.y = element_blank(),
      )
  }
}

# Add the y-axis title using plot_annotation
facet_plot_k <- patchwork::wrap_plots(plots_list, ncol = 1) +
  patchwork::plot_layout(axis_titles = 'collect')
# facet_plot_k

# Save the plot
ggsave(file.path(save_folder, "Cluster_facet.png"), plot = facet_plot_k, width = 14, height = 21, dpi = dpi)

rm(list = ls())
gc()





## Largetooth Sawfish 333237
data_dir <- "E:/Ch 3 data"

# Panel D.
tag_list <- c("333237")

tag_vector <- tag_list
data_folder = data_dir
kmeans_result <- readRDS(file = file.path(data_dir, tag_list, "5_k-means/kmeans_result.rds"))
No_days = 3
every_nth = 1 # 60 second depth sampling frequency
color = TRUE
diel_shade = TRUE
dpi = 300
output_folder = "E:/My Drive/PhD work/4.3 Chapter Three/Fig 6/"

# Initialize an empty list for storing data frames from each tag
data_list <- list()

# Initialize an empty data frame for storing unique dates and tag IDs
id_data <- data.frame(date_only = as.Date(character()), tag_ID = character(), stringsAsFactors = FALSE)

# Load the archive data for the single tag
archive_days <- readRDS(file.path(data_folder, tag_vector, "archive_days.rds"))
archive_days$tag_ID <- tag_vector # Add tag_ID column

cat(paste0("\nMaximum depth is ", max(archive_days$depth)))
cat(paste0("\nSampling interval is ", as.numeric(archive_days$date[2] - archive_days$date[1], units = "secs"), " seconds\n"))

# Directly assign the unique dates and tag ID for the single tag
unique_dates <- unique(archive_days$date_only)
id_data <- data.frame(date_only = unique_dates, tag_ID = tag_vector, stringsAsFactors = FALSE)

# Filter to every nth record
crop_sq <- seq(every_nth, nrow(archive_days), by = every_nth)
all_archive_days <- archive_days[crop_sq, ]

# New sampling interval
sampling_interval <- as.numeric(all_archive_days$date[2] - all_archive_days$date[1], units = "secs")
cat("Plotting every", sampling_interval, "seconds")

# Wrap the single all_archive_days in a list to standardise the structure
data_list[[tag_vector]] <- all_archive_days

# Calculate distances between each point (day) and corresponding cluster centroid:
distances <- kmeans_result$distances

# Set all_archive_days as a data.table
data.table::setDT(all_archive_days)

# Count unique date_only values for each tag_ID
unique_dates_by_tag <- all_archive_days[, list(unique_dates = data.table::uniqueN(date_only)), by = tag_ID]

# Sum all the unique counts across all tag IDs
total_unique_dates <- sum(unique_dates_by_tag$unique_dates)

# Add all cluster assignment to id_data
id_data$cluster <- kmeans_result$cluster

# Set k as the number of unique clusters
k <- length(unique(kmeans_result$cluster))

# Create a data frame with cluster assignments and distances
cluster_distances <- data.frame(cluster = id_data$cluster, distance = distances, date_only = id_data$date_only)

# Sort the data frame by cluster and distance
sorted_cluster_distances <- cluster_distances[order(cluster_distances$cluster, cluster_distances$distance), ]

# Create a data frame to hold values
best_k_representatives <- data.frame()

# Find the 'No_days' days closest to the cluster centroid for each cluster (i)
for (i in 1:k) {
  cluster_representatives <- head(sorted_cluster_distances[sorted_cluster_distances$cluster == i, ], No_days)
  cluster_representatives$row <- as.numeric(rownames(cluster_representatives))


  # Add the date_only, tag_ID, and cluster columns
  cluster_representatives$date <- id_data[cluster_representatives$row, "date_only"]
  cluster_representatives$tag_ID <- id_data[cluster_representatives$row, "tag_ID"]

  best_k_representatives <- rbind(best_k_representatives, cluster_representatives)

  if (nrow(cluster_representatives) < No_days) {
    cat(paste0("\n There are less than ", No_days, " representatives available for cluster ", i, "\n"))
  }
}

# Set save folder name based on diel_shade
if (diel_shade == TRUE) {
  folder <- paste0("6_Cluster-plots.K=", k, "_shaded")
} else {
  folder <- paste0("6_Cluster-plots.K=", k)
}

# Set the save folder location
save_folder <- file.path(output_folder, tag_vector, folder)

# Create the directory if it doesn't exist
create_directory(save_folder)

# Save the list of clusters with dates (best_k_representatives, date and tag)
write.csv(best_k_representatives, file = file.path(save_folder, "cluster_dates.csv"))

# Save the tag list
write(tag_vector, file = file.path(save_folder, "tag_IDs.txt"))

# Custom select colours
custom_palette <- c(
  "1" = "#E41A1C", # Red
  "2" = "#377EB8", # Blue
  "3" = "#FF7F00", # Orange
  "4" = "#984EA3", # Purple
  "5" = "#4DAF4A", # Green
  "6" = "#00CED1" # Dark Turquoise
)

# Create the lists for the facet plots
plots_list <- list()

# Create a list of Y_lim values
Y_lim_list <- list(
  Y_lim1 = c(10, 60, 40),
  Y_lim2 = c(0, 75, 50),
  Y_lim3 = c(10, 65, 40),
  Y_lim4 = c(10, 70, 40)
)

cluster_no <- 1
# Iterate over each cluster in turn. Select the days closest to the cluster centroid, extract the data and plot them
for (cluster_no in 1:k) {
  # Select the best representatives for the cluster (i):
  selected_days <- best_k_representatives[best_k_representatives$cluster == cluster_no, ]

  # Extract the data to use
  dates <- id_data[selected_days$row, c("date_only", "tag_ID", "cluster")]

  #  Rename the column for matching
  dates$date <- as.Date(dates$date_only)

  cat(paste0("\n Cluster ", cluster_no, " dates ", dates$date, " Tag: ", dates$tag_ID, "\n"))

  # Initialize an empty list to store the extracted data
  data_list <- list()

  # Iterate over the rows of 'dates' and extract the matching rows from 'all_archive_days' using date and tag_ID (duplicated dates in tag_ID's)
  for (j in 1:nrow(dates)) {
    # For each day in dates where date and tag_ID match the cluster, create plot_data
    plot_data <- all_archive_days[all_archive_days$date_only == dates$date[j] & all_archive_days$tag_ID == dates$tag_ID[j], ]

    # Add plot_data to data_list
    data_list[[j]] <- plot_data
  }

  # Combine the extracted data into a single data frame
  plot_data <- do.call(rbind, data_list)

  # Assign a unique number to each date
  plot_data$date_number <- match(plot_data$date_only, unique(plot_data$date_only))

  # Create a time column and set all dates to the same date to facet
  plot_data$time <- format(as.POSIXct(plot_data$date), "%H:%M:%S")
  plot_data$time <- as.POSIXct(paste("1970-01-01", plot_data$time), format = "%Y-%m-%d %H:%M:%S")
  plot_data <- as.data.frame(plot_data)

  # Create a sequence of specific times incrementing by 6 hours
  time_breaks <- seq(
    from = as.POSIXct("1970-01-01 06:00:00"),
    to = as.POSIXct("1970-01-01 18:00:00"),
    by = "6 hours"
  )

  ## Change y-limits here
  summary(plot_data$depth)
  Y_lim <- Y_lim_list[[cluster_no]]

  # Plot the base TDR (in distance from cluster centre order)
  cluster_plot <-
    ggplot(data = plot_data, aes(x = time, y = depth)) +
    geom_line(colour = custom_palette[cluster_no], linewidth = 1) +
    scale_x_datetime(breaks = time_breaks, date_labels = "%H %M", position = "top", expand = c(0.001, 0, 0, 0)) +
    scale_y_reverse(limits = c(Y_lim[2], Y_lim[1]), breaks = seq(Y_lim[1], Y_lim[2], Y_lim[3]), expand = c(0, 0)) +
    labs(x = "Time of Day", y = "Depth (m)") +
    theme_bw() +
    theme(
      plot.margin = unit(c(5.5, 25, 10.5, 5.5), "points"),
      legend.position = "NONE",

      text = element_text(size = 40, face = "bold", color = "Black"),
      axis.text.x = element_text(face = "bold", color = "Black"),
      axis.text.y = element_text(face = "bold", color = "Black"),
      #axis.text.x = element_blank(),
      #axis.title = element_blank(),
      strip.text.y = element_blank(), # removes 'date_number' label
      axis.ticks.length.x = unit(0.4, "cm"),
      axis.ticks.length.y = unit(0.4, "cm"),
      axis.ticks = element_line(linewidth = 1.5, color = "black"),
      axis.line.x = element_line(linewidth = 1.5, color = "black"),
      axis.line.y = element_line(linewidth = 1.5, color = "black")
    ) +
    facet_grid(date_number ~ .,) # axes = "all")

  #cluster_plot
  fixed_Y_plot <- cluster_plot

  # Add shading limits to plot_data so facet doesn't alter them (uses last plot in list)
  plot_data$ymax <- Y_lim[2]
  plot_data$ymin <- Y_lim[1]

  # Plot with diel shading
  if (diel_shade) {
    fixed_Y_plot <- fixed_Y_plot +
      geom_rect(
        # Midnight to sunrise shading
        data = plot_data,
        aes(
          xmin = as.POSIXct("1970-01-01 00:00:00"),
          xmax = as.POSIXct(paste("1970-01-01", format(sunrise, "%H:%M:%S"))),
          ymin = ymin,
          ymax = ymax
        ),
        fill = "grey", alpha = 0.01
      ) +
      geom_rect(
        # Sunset to midnight shading
        data = plot_data,
        aes(
          xmin = as.POSIXct(paste("1970-01-01", format(sunset, "%H:%M:%S"))),
          xmax = as.POSIXct("1970-01-01 23:59:59"),
          ymin = ymin,
          ymax = ymax
        ),
        fill = "grey", alpha = 0.01
      )
  }

  # fixed_Y_plot

  # Add plot to list for all clusters facet
  plots_list[[cluster_no]] <- fixed_Y_plot

  # Save the plot
  ggsave(file.path(save_folder, paste0("Cluster_", cluster_no, ".png")), plot = fixed_Y_plot, width = 14.22, height = 10, dpi = dpi)
}

# Facet the plots using 'patchwork'
for (i in 1:length(plots_list)) {
  # Modify first cluster (plot) separately
  if (i == 1) {
    plots_list[[i]] <- plots_list[[i]]
    #labs(x = NULL, y = NULL) + # Turn off all labels in first cluster
  } else {
    # Modify subsequent plots
    plots_list[[i]] <- plots_list[[i]] +
      labs(x = NULL, y = NULL) + # Turn off all labels in subsequent clusters
      theme(
        axis.text.x = element_blank(), # No x-axis text (06:00, 12:00, 18:00)
        axis.ticks.x = element_blank(), # No x-axis ticks
        axis.title.y = element_blank(),
      )
  }
}

# Add the y-axis title using plot_annotation
facet_plot_k <- patchwork::wrap_plots(plots_list, ncol = 1) +
  patchwork::plot_layout(axis_titles = 'collect')
# facet_plot_k

# Save the plot
ggsave(file.path(save_folder, "Cluster_facet.png"), plot = facet_plot_k, width = 14, height = 21, dpi = dpi)

rm(list = ls())
gc()



#
##### Different code, not a figure i think #####

# Initialize an empty list for storing data frames from each tag
data_list <- list()

# Initialize an empty data frame for storing unique dates and tag IDs
id_data <- data.frame(date_only = as.Date(character()), tag_ID = character(), stringsAsFactors = FALSE)

# Load the archives of the tags in 'tag_vector'
if (length(tag_vector) > 1) { # Multiple tags

  for (tag in tag_vector) {
    # Load the archive data for each tag
    archive_days <- readRDS(file.path(data_folder, tag, "archive_days.rds"))
    archive_days$tag_ID <- tag # Add tag_ID column

    cat(paste0("\nTag", tag, " Maximum depth is ", max(archive_days$depth)))
    cat(paste0("\nTag", tag, " Sampling interval is ", as.numeric(archive_days$date[2] - archive_days$date[1], units = "secs"), " seconds\n"))

    # Filter to every nth record
    crop_sq <- seq(every_nth, nrow(archive_days), by = every_nth)
    archive_days <- archive_days[crop_sq, ]

    # New sampling interval
    sampling_interval <- as.numeric(archive_days$date[2] - archive_days$date[1], units = "secs")
    cat("Plotting every", sampling_interval, "seconds")

    # Append to the list
    data_list[[tag]] <- archive_days

    # Extract unique dates for the current tag and create a temp dataframe
    unique_dates <- unique(archive_days$date_only)
    temp_id_data <- data.frame(tag_ID = tag, date_only = unique_dates, stringsAsFactors = FALSE)

    # Combine with the id_data dataframe
    id_data <- rbind(id_data, temp_id_data)
  }

  # Combine all tag data frames into one after processing each tag
  all_archive_days <- do.call(rbind, data_list)
} else { # Single tag

  # Load the archive data for the single tag
  archive_days <- readRDS(file.path(data_folder, tag_vector, "archive_days.rds"))
  archive_days$tag_ID <- tag_vector # Add tag_ID column

  cat(paste0("\nMaximum depth is ", max(archive_days$depth)))
  cat(paste0("\nSampling interval is ", as.numeric(archive_days$date[2] - archive_days$date[1], units = "secs"), " seconds\n"))

  # Directly assign the unique dates and tag ID for the single tag
  unique_dates <- unique(archive_days$date_only)
  id_data <- data.frame(date_only = unique_dates, tag_ID = tag_vector, stringsAsFactors = FALSE)

  # Filter to every nth record
  crop_sq <- seq(every_nth, nrow(archive_days), by = every_nth)
  all_archive_days <- archive_days[crop_sq, ]

  # New sampling interval
  sampling_interval <- as.numeric(all_archive_days$date[2] - all_archive_days$date[1], units = "secs")
  cat("Plotting every", sampling_interval, "seconds")

  # Wrap the single all_archive_days in a list to standardise the structure
  data_list[[tag_vector]] <- all_archive_days
}

# Calculate distances between each point (day) and corresponding cluster centroid:
distances <- kmeans_result$distances

# Set all_archive_days as a data.table
data.table::setDT(all_archive_days)

# Count unique date_only values for each tag_ID
unique_dates_by_tag <- all_archive_days[, list(unique_dates = data.table::uniqueN(date_only)), by = tag_ID]

# Sum all the unique counts across all tag IDs
total_unique_dates <- sum(unique_dates_by_tag$unique_dates)

if (total_unique_dates != length(kmeans_result$cluster)) {
  cat("\n Data length and number of clusters do not match. Cluster assignment may be incorrect. This error should only be ignored for readme data. \n")

  # Get length of all_archive_days
  l <- length(unique(all_archive_days$date_only))

  # Assign cut cluster list
  kmeans_result$cluster <- kmeans_result$cluster[1:l]

  # Cut distances to match available data length
  distances <- distances[1:l]
}

# Add all cluster assignment to id_data
id_data$cluster <- kmeans_result$cluster

# Set k as the number of unique clusters
k <- length(unique(kmeans_result$cluster))

# Create a data frame with cluster assignments and distances
cluster_distances <- data.frame(cluster = id_data$cluster, distance = distances, date_only = id_data$date_only)

# Sort the data frame by cluster and distance
sorted_cluster_distances <- cluster_distances[order(cluster_distances$cluster, cluster_distances$distance), ]

# Create a data frame to hold values
best_k_representatives <- data.frame()

# Find the 'No_days' days closest to the cluster centroid for each cluster (i)
for (i in 1:k) {
  cluster_representatives <- head(sorted_cluster_distances[sorted_cluster_distances$cluster == i, ], No_days)
  cluster_representatives$row <- as.numeric(rownames(cluster_representatives))


  # Add the date_only, tag_ID, and cluster columns
  cluster_representatives$date <- id_data[cluster_representatives$row, "date_only"]
  cluster_representatives$tag_ID <- id_data[cluster_representatives$row, "tag_ID"]

  best_k_representatives <- rbind(best_k_representatives, cluster_representatives)

  if (nrow(cluster_representatives) < No_days) {
    cat(paste0("\n There are less than ", No_days, " representatives available for cluster ", i, "\n"))
  }
}

# Set save folder name based on diel_shade
if (diel_shade == TRUE) {
  folder <- paste0("6_Cluster-plots.K=", k, "_shaded")
} else {
  folder <- paste0("6_Cluster-plots.K=", k)
}

# Set the save folder location
if (length(tag_vector) > 1) {
  # Multiple tags
  save_folder <- file.path(output_folder, Combined_tags, folder)
} else {
  # Single tag - tag_vector contains one tag ID
  save_folder <- file.path(output_folder, tag_vector, folder)
}

# Create the directory if it doesn't exist
create_directory(save_folder)

# Save the list of clusters with dates (best_k_representatives, date and tag)
write.csv(best_k_representatives, file = file.path(save_folder, "cluster_dates.csv"))

# Save the tag list
write(tag_vector, file = file.path(save_folder, "tag_IDs.txt"))

# Warn user of small output
if (max(kmeans_result$cluster) >= 6) {
  message("\n More than 5 clusters, facet plots will be small. \n")
}

# Custom select colours
custom_palette <- c(
  "1" = "#E41A1C", "2" = "#377EB8", "3" = "#FF7F00", "4" = "#984EA3",
  "5" = "#4DAF4A", "6" = "#344111", "7" = "#A65628", "8" = "#F781BF",
  "9" = "black", "10" = "green"
)

# Create the lists for the facet plots
plots_list <- list()
plots_list_free_Y <- list()

i <- 1
# Iterate over each cluster in turn. Select the days closest to the cluster centroid, extract the data and plot them
for (i in 1:k) {
  # Select the best representatives for the cluster (i):
  selected_days <- best_k_representatives[best_k_representatives$cluster == i, ]

  # Extract the data to use
  dates <- id_data[selected_days$row, c("date_only", "tag_ID", "cluster")]

  #  Rename the column for matching
  dates$date <- as.Date(dates$date_only)

  cat(paste0("\n Cluster ", i, " dates ", dates$date, " Tag: ", dates$tag_ID, "\n"))

  # Initialize an empty list to store the extracted data
  data_list <- list()

  # Iterate over the rows of 'dates' and extract the matching rows from 'all_archive_days' using date and tag_ID (duplicated dates in tag_ID's)
  for (j in 1:nrow(dates)) {
    # For each day in dates where date and tag_ID match the cluster, create plot_data
    plot_data <- all_archive_days[all_archive_days$date_only == dates$date[j] & all_archive_days$tag_ID == dates$tag_ID[j], ]

    # Check if plot_data exists
    if (nrow(plot_data) == 0) {
      cat(paste0("\n Cluster ", i, " date ", dates$date[j], " does not exist in the tag archive. Skipping this plot.\n"))
    } else {
      # Add plot_data to data_list
      data_list[[j]] <- plot_data
    }
  }

  # Filter out any NULL entries from data_list to ensure it only contains actual data
  data_list <- Filter(NROW, data_list) # This keeps only non-empty elements

  # If data_list is empty, skip the current iteration of i
  if (length(data_list) == 0) {
    cat(paste0("\n No data collected for Cluster ", i, ". Skipping this cluster.\n"))
    next
  }

  # Combine the extracted data into a single data frame
  plot_data <- do.call(rbind, data_list)

  # If plot_data is empty, skip the current iteration of i
  if (length(plot_data) == 0) {
    cat(paste0("\n No data available for plotting Cluster ", i, ". Skipping this cluster.\n"))
    next
  }

  # Assign a unique number to each date
  plot_data$date_number <- match(plot_data$date_only, unique(plot_data$date_only))

  # Create a time column and set all dates to the same date to facet
  plot_data$time <- format(as.POSIXct(plot_data$date), "%H:%M:%S")
  plot_data$time <- as.POSIXct(paste("1970-01-01", plot_data$time), format = "%Y-%m-%d %H:%M:%S")
  plot_data <- as.data.frame(plot_data)

  # Create a sequence of specific times incrementing by 6 hours
  time_breaks <- seq(
    from = as.POSIXct("1970-01-01 06:00:00"),
    to = as.POSIXct("1970-01-01 23:59:59"),
    by = "6 hours"
  )

  # Plot the base TDR (in distance from cluster centre order)
  cluster_plot <-
    ggplot(data = plot_data, aes(x = time, y = depth)) +
    geom_line(colour = custom_palette[i], linewidth = 1) +
    scale_x_datetime(breaks = time_breaks, date_labels = "%H %M", position = "top", expand = c(0.001, 0, 0, 0)) +
    labs(x = "Time of day", y = "Depth (m)") +
    theme_bw() +
    theme(
      # axis.text.x = element_text(angle = 45, hjust = -0.05),
      text = element_text(size = 26), ## change all text size in figure inc. change axis text size - For PDF
      plot.margin = unit(c(5.5, 25, 10.5, 5.5), "points"),
      legend.position = "NONE",
      strip.text.y = element_blank()
    ) +
    facet_grid(date_number ~ .)

  fixed_Y_plot <- cluster_plot +
    scale_y_reverse(limits = c(Y_lim[2], Y_lim[1]), breaks = seq(Y_lim[1], Y_lim[2], Y_lim[3]), expand = c(0, 0))

  if (diel_shade) {
    fixed_Y_plot <- fixed_Y_plot +
      # Midnight to sunrise shading
      geom_rect(
        data = plot_data,
        aes(
          xmin = as.POSIXct("1970-01-01 00:00:00"),
          xmax = as.POSIXct(paste("1970-01-01", format(sunrise, "%H:%M:%S"))),
          ymin = Y_lim[2],
          ymax = Y_lim[1]
        ),
        fill = "grey", alpha = 0.01
      ) +
      # Sunset to midnight shading
      geom_rect(
        data = plot_data,
        aes(
          xmin = as.POSIXct(paste("1970-01-01", format(sunset, "%H:%M:%S"))),
          xmax = as.POSIXct("1970-01-01 23:59:59"),
          ymin = Y_lim[2],
          ymax = Y_lim[1]
        ),
        fill = "grey", alpha = 0.01
      )
  }

  # fixed_Y_plot

  # Add plot to list for all clusters facet
  plots_list[[i]] <- fixed_Y_plot #+ ggtitle(paste0("Cluster ", i))

  # Save the plot
  ggsave(file.path(save_folder, paste0("Cluster_", i, ".png")), plot = fixed_Y_plot, width = 14.22, height = 10, dpi = dpi)

  ## Plotting the free Y-axis version of TDR

  # Calculate the minimum, maximum, and difference in depth of this cluster (i) for diurnal shading with geom_rect
  max_depth <- max(plot_data$depth)
  min_depth <- min(plot_data$depth)
  depth_dif <- max_depth - min_depth

  # Calculate Y-axis limits
  if (max_depth < 10) {
    limits <- c(10, 0)
  } else if (depth_dif < 10) {
    if (min_depth < 10) {
      limits <- c(max_depth + (10 - depth_dif) / 2, 0)
    } else {
      limits <- c(max_depth + (10 - depth_dif) / 2, min_depth - (10 - depth_dif) / 2)
    }
  } else {
    if (min_depth < 10) {
      limits <- c(max_depth + 2, 0)
    } else {
      limits <- c(max_depth + 1, min_depth - 1)
    }
  }

  cluster_plot_free_y <- cluster_plot +
    scale_y_reverse(limits = limits, breaks = seq(Y_lim[1], Y_lim[2], Y_lim[3]), expand = c(0, 0))

  # Add shading limits to plot_data so facet doesn't alter them (uses last plot in list)
  plot_data$ymin <- limits[2]
  plot_data$ymax <- limits[1]

  if (diel_shade) {
    cluster_plot_free_y <- cluster_plot_free_y +
      geom_rect(
        # Midnight to sunrise shading
        data = plot_data,
        aes(
          xmin = as.POSIXct("1970-01-01 00:00:00"),
          xmax = as.POSIXct(paste("1970-01-01", format(sunrise, "%H:%M:%S"))),
          ymin = ymin,
          ymax = ymax
        ),
        fill = "grey", alpha = 0.01
      ) +
      geom_rect(
        # Sunset to midnight shading
        data = plot_data,
        aes(
          xmin = as.POSIXct(paste("1970-01-01", format(sunset, "%H:%M:%S"))),
          xmax = as.POSIXct("1970-01-01 23:59:59"),
          ymin = ymin,
          ymax = ymax
        ),
        fill = "grey", alpha = 0.01
      )
  }

  # cluster_plot_free_y

  # Add plot to list for facet
  plots_list_free_Y[[i]] <- cluster_plot_free_y

  # Save the plot
  ggsave(file.path(save_folder, paste0("FreeY.Cluster_", i, ".png")), plot = cluster_plot_free_y, width = 14.22, height = 10, dpi = dpi)
}

# ensure lists contain valid ggplot objects
plots_list <- Filter(function(x) !is.null(x), plots_list)
plots_list_free_Y <- Filter(function(x) !is.null(x), plots_list_free_Y)

if (length(plots_list) == 1) {
  cat("\n Only one cluster of plots exists. Cannot facet clusters. Check dates or number of clusters \n")
  # print(plots_list[[1]])
}

# If plots_list is empty, stop the function here
if (length(plots_list) == 0) {
  cat("\n Plot list is empty. Check dates exist in tag archive. \n")
  return(NULL)
}

# Facet the plots using 'patchwork'
for (i in 1:length(plots_list)) {
  if (i == 1) { # Keep x-axis ticks for the first plot
    plots_list[[i]] <- plots_list[[i]] +
      labs(x = NULL, y = NULL) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26)
      )
  } else { # Remove x-axis text and tick marks for all other plots
    plots_list[[i]] <- plots_list[[i]] +
      labs(x = NULL, y = NULL) +
      theme(
        legend.position = "none",
        axis.text.x = element_blank(), # No x-axis text
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 26)
      )
  }

  # Editing the free Y-axis plots for facet
  if (i == 1) { # Keep x-axis ticks for the first plot
    plots_list_free_Y[[i]] <- plots_list_free_Y[[i]] +
      labs(x = NULL, y = NULL) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26)
      )
  } else { # Remove x-axis ticks for all other plots
    plots_list_free_Y[[i]] <- plots_list_free_Y[[i]] +
      labs(x = NULL, y = NULL) +
      theme(
        legend.position = "none",
        axis.text.x = element_blank(), # No x-axis text
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 26)
      )
  }
}

# Combine the plots into a facet plot
if (max(kmeans_result$cluster) <= 6) {
  ncol <- 1
  width <- 14
} else {
  ncol <- 2
  width <- 21
}

# Facet the fixed Y-axis plots
facet_plot_k <- patchwork::wrap_plots(plots_list, ncol = ncol)
ggsave(file.path(save_folder, "Cluster_facet.png"), plot = facet_plot_k, width = width, height = 21, dpi = dpi)

# Facet the free Y-axis plots
facet_plot_k_freey <- patchwork::wrap_plots(plots_list_free_Y, ncol = ncol)
ggsave(file.path(save_folder, "Cluster_facet_freeY.png"), plot = facet_plot_k_freey, width = width, height = 21, dpi = dpi)

# print(facet_plot_k_freey)



##### Re-sample any tag's data at a new sampling frequency #####
# Required libraries
library(data.table)

# Define directories and tag ID
data_dir <- "E:/Ch 3 data"
tag_ID <- "130970a"

# Import archive
archive_days <- readRDS(file = file.path(data_dir, tag_ID, "archive_days.rds"))

# Set new folder
save_folder <- "130970a_downsampled"

saveRDS(archive_days, file = file.path(data_dir, save_folder, "archive_days_original.rds"))

# Save time_zone attribute
tz_attr <- attr(archive_days, "time_zone")

# Ensure 'archive_days' is treated as a data.table for efficient operations
setDT(archive_days)

# Set sample rate based on depth sampling frequency
sample_rate <- 12*30

# Convert to data table
setDT(archive_days)

# Down-sampling the data to sample_rate
new_archive_days <- archive_days[seq(1, nrow(archive_days), by = sample_rate), ]

saveRDS(new_archive_days, file = file.path(data_dir, save_folder, "archive_days.rds"))

archive <- readRDS(file = file.path(data_dir, save_folder, "archive_days.rds"))

#
##### UNUSED - all data creation for 130970a at different sampling frequencies #####
# Set data folder
data_dir <- "E:/Ch 3 data/130970a_downsampled"

# Set folder names
folder_name <- c(1, 5, 10, 15, 20, 25, 30)

i <- 1
for (i in 1:7) {
  tag_ID <- paste0(folder_name[i], "m")
  archive_days <- readRDS(file = file.path(data_dir, paste0("archive_days_", tag_ID, ".rds")))

  # Print sampling rate of this 'i'
  print(archive_days$date[2]-archive_days$date[1])

  # Set lower period
  lower_period <- folder_name[i]

  # Proceed with correct archive sample interval
  my.w <- create_wavelet(
    archive = archive_days,
    tag_ID = tag_ID,
    lower_period_mins = lower_period,
    upper_period_hours = 24,
    output_folder = data_dir,
    interactive_mode = FALSE
  )

  # Create wavelet statistics
  waveStats <- create_wavelet_stats(
    wavelet = my.w,
    tag_ID = tag_ID,
    output_folder = data_dir
  )

  # Create depth statistics including GPS location for diel statistics
  depthStats <- create_depth_stats(
    archive = archive_days,
    tag_ID = tag_ID,
    diel = TRUE,
    sunrise = NULL,
    sunset = NULL,
    sunset_type = "civil",
    GPS = file.path(data_dir, tag_ID, "GPS.csv"),
    output_folder = data_dir
  )

  rm(archive_days, my.w, lower_period, waveStats, depthStats)
  gc()

  # Create the PC dataset from one or more tags being analysed. DOES NOT INCLUDE DEPTH STATS
  pc_data <- pca_data(
    tag_vector = tag_ID,
    data_folder = data_dir,
    output_folder = data_dir
  )

  # Run Principal Component Analysis on the dataset
  pc_results <- pca_results(
    pc_data = pc_data,
    standardise = TRUE,
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
    tag_vector = tag_ID,
    data_folder = data_dir,
    pc_scores = pc_scores,
    output_folder = data_dir
  )

  # Run k-means on k up to a maximum k to determine best fit from elbow and silhouette width plots
  select_k(
    kmeans_data = kmeans_data,
    standardise = TRUE,
    Max.k = 15,
    v_line = 5,
    output_folder = data_dir
  )

  # Run k means with the selected number of clusters. Plot with or without polygons. Resize window then plot a second time to fix legend size.
  kmeans_result <- k_clustering(
    kmeans_data = kmeans_data,
    standardise = TRUE,
    k = 5,
    polygon = TRUE,
    output_folder = data_dir
  )

  rm(pc_data, pc_results, pc_scores, kmeans_data, kmeans_result, tag_ID)
}

metadata <- data.frame(
  tag_ID = c("1m", "5m", "10m", "15m", "20m", "25m", "30m"),
  every_nth_TDR = c(rep(1, 7)),
  Y_lim_TDR_min = c(rep(0, 7)),
  Y_lim_TDR_max = c(rep(750, 7)),
  Y_lim_TDR_interval = c(rep(200, 7)),
  date_breaks = c(rep("14 day", 7)),
  every_nth_clusters = c(rep(1, 7)),
  Y_lim_clusters_min = c(rep(0, 7)),
  Y_lim_clusters_max = c(rep(750, 7)),
  Y_lim_clusters_interval = c(rep(200, 7)),
  stringsAsFactors = FALSE
)

# Set data folder
data_dir <- "E:/Ch 3 data/130970a_downsampled"

i <- 1
for (i in 1:7) {
  tag_ID <- metadata$tag_ID[i]

  # Load the k-means results
  kmeans_result <- readRDS(file = file.path(data_dir, tag_ID, "5_k-means/kmeans_result.rds"))

  # Plot metadata
  every_nth <- metadata$every_nth_TDR[i]
  Y_lim <- c(metadata$Y_lim_TDR_min[i], metadata$Y_lim_TDR_max[i], metadata$Y_lim_TDR_interval[i])
  date_breaks <- metadata$date_breaks[i]

  TDR_plot <- plot_cluster_TDR(
    tag_ID = tag_ID,
    data_folder = data_dir,
    kmeans_result = kmeans_result,
    every_nth = every_nth,
    Y_lim = Y_lim,
    date_breaks = date_breaks,
    dpi = 300,
    output_folder = file.path(data_dir, tag_ID)
  )

  # Plot the days closest to the centre of each cluster
  tag_vector <- metadata$tag_ID[i]
  every_nth <- metadata$every_nth_clusters[i]
  Y_lim <- c(metadata$Y_lim_clusters_min[i], metadata$Y_lim_clusters_max[i], metadata$Y_lim_clusters_interval[i])

  # Tags in paper:
  cluster_plot <- plot_clusters(
    tag_vector = tag_vector,
    data_folder = data_dir,
    kmeans_result = kmeans_result,
    No_days = 3,
    every_nth = every_nth,
    Y_lim = Y_lim,
    color = TRUE,
    diel_shade = FALSE,
    dpi = 300,
    output_folder = data_dir
  )

  rm(kmeans_result, tag_ID, every_nth, Y_lim, date_breaks, tag_vector, TDR_plot, cluster_plot)
}

rm(metadata, data_dir, folder_name)

#
##### Figure 8 - Load all sampling frequency clusters, combine and remap #####
# read csv files for each k-means cluster assignment
data_dir <- "E:/Ch 3 data/130970a_multi"
sf1_min <- read.csv(file = file.path(data_dir, "1_min/5_k-means/Cluster_results_k=5.csv"))
sf5_min <- read.csv(file = file.path(data_dir, "5_min/5_k-means/Cluster_results_k=5.csv"))
sf10_min <- read.csv(file = file.path(data_dir, "10_min/5_k-means/Cluster_results_k=5.csv"))
sf15_min <- read.csv(file = file.path(data_dir, "15_min/5_k-means/Cluster_results_k=5.csv"))
sf20_min <- read.csv(file = file.path(data_dir, "20_min/5_k-means/Cluster_results_k=5.csv"))
sf25_min <- read.csv(file = file.path(data_dir, "25_min/5_k-means/Cluster_results_k=5.csv"))
sf30_min <- read.csv(file = file.path(data_dir, "30_min/5_k-means/Cluster_results_k=5.csv"))

# Create the cluster_data data frame
cluster_data <- data.frame(
  Day = 1:nrow(sf1_min),
  SF_1m = sf1_min$kmeans_result.cluster,
  SF_5m = sf5_min$kmeans_result.cluster,
  SF_10m = sf10_min$kmeans_result.cluster,
  SF_15m = sf15_min$kmeans_result.cluster,
  SF_20m = sf20_min$kmeans_result.cluster,
  SF_25m = sf25_min$kmeans_result.cluster,
  SF_30m = sf30_min$kmeans_result.cluster
)

# saveRDS(object = cluster_data, file = file.path(data_dir, "cluster_data.rds"))
# write.csv(cluster_data, file = file.path(data_dir, "cluster_data.csv"), row.names = FALSE)
rm(cluster_data, sf1_min, sf5_min, sf10_min, sf15_min, sf20_min, sf25_min, sf30_min)

## Read data
data_dir <- "E:/Ch 3 data/130970a_multi"
cluster_assignment <- readRDS(file = file.path(data_dir, "cluster_data.rds"))

# As clusters are randomly assigned in k-means, the clusters had to be mapped to the same cluster in the one-minute benchmark. We did this by
# choosing the mapping of each cluster that gave the highest F1 scores between the 1-minute and down-sampled datasets.
# Create cluster map for each sampling frequency
mapping <- list(
  SF_5m = c('2' = 1, '4' = 2, '3' = 3, '1' = 4, '5' = 5),
  SF_10m = c('5' = 1, '3' = 2, '1' = 3, '2' = 4, '4' = 5),
  SF_15m = c('4' = 1, '5' = 2, '1' = 3, '3' = 4, '2' = 5),
  SF_20m = c('2' = 1, '3' = 2, '1' = 3, '4' = 4, '5' = 5),
  SF_25m = c('4' = 1, '2' = 2, '3' = 3, '5' = 4, '1' = 5),
  SF_30m = c('5' = 1, '3' = 2, '1' = 3, '2' = 4, '4' = 5)
)

# Function to remap clusters
remap_clusters <- function(column, mapping) {
  return(sapply(column, function(x) mapping[as.character(x)]))
}

# Create the new cluster assignment data frame
new_cluster_assignment <- cluster_assignment

# Apply the remapping for each specified column
for (sf in names(mapping)) {
  new_cluster_assignment[[sf]] <- remap_clusters(cluster_assignment[[sf]], mapping[[sf]])
}

# Now match the clusters in the benchmark (and therefore other down-sampled data) to those in the example case study in the paper for consistency
mapping <- list(
  SF_1m = c('1' = 3, '2' = 2, '3' = 1, '4' = 5, '5' = 4),
  SF_5m = c('1' = 3, '2' = 2, '3' = 1, '4' = 5, '5' = 4),
  SF_10m = c('1' = 3, '2' = 2, '3' = 1, '4' = 5, '5' = 4),
  SF_15m = c('1' = 3, '2' = 2, '3' = 1, '4' = 5, '5' = 4),
  SF_20m = c('1' = 3, '2' = 2, '3' = 1, '4' = 5, '5' = 4),
  SF_25m = c('1' = 3, '2' = 2, '3' = 1, '4' = 5, '5' = 4),
  SF_30m = c('1' = 3, '2' = 2, '3' = 1, '4' = 5, '5' = 4)
)

# Apply the remapping for each specified column
for (sf in names(mapping)) {
  new_cluster_assignment[[sf]] <- remap_clusters(new_cluster_assignment[[sf]], mapping[[sf]])
}

# saveRDS(object = new_cluster_assignment, file = file.path(data_dir, "new_cluster_data.rds"))
# write.csv(new_cluster_assignment, file = file.path(data_dir, "new_cluster_data.csv"), row.names = FALSE)

rm(cluster_assignment, sf, remap_clusters, mapping)

#
### Figure 11 plot - Panels of 30d time series
# Required libraries
library(data.table)
library(ggplot2)
library(readr)
library(caret)
library(lubridate)

# Define directories and tag ID
data_dir <- "E:/Ch 3 data/130970a_multi"

# Import archive
archive_days <- readRDS(file = file.path(data_dir, "1_min/archive_days.rds"))

# Save time_zone attribute
tz_attr <- attr(archive_days, "time_zone")

# Ensure 'archive_days' is treated as a data.table for efficient operations
setDT(archive_days)

# Create a data frame or data.table mapping each unique date to its cluster assignment
unique_dates <- unique(archive_days$date_only)

# sampling frequencies in minutes
sampling_frequencies <- c(1, 10, 20, 30)

panel_letters <- c("A", "B", "C", "D")  # Panel letters

# Define a function to map dates to 'Days Elapsed'
date_to_elapsed <- function(date, days_mapping) {
  unlist(lapply(date, function(x) {
    days_mapping[days_mapping$date == x, days_elapsed]
  }))
}

# Custom select colours
custom_palette <- c(
  "1" = "#E41A1C",   # Red
  "2" = "#377EB8",   # Blue
  "3" = "#FF7F00",   # Orange
  "4" = "#984EA3",   # Purple
  "5" = "#4DAF4A",   # Green
  "6" = "#00CED1",  # Dark Turquoise
  "7" = "#A65628",   # Brown
  "8" = "#F781BF",   # Pink
  "9" = "#999999",   # Grey
  "10" = "#344111",   # Dark green
  "11" = "#8A2BE2",  # Blue Violet
  "12" = "#FF69B4",  # Hot Pink
  "13" = "#CD5C5C",  # Indian Red
  "14" = "#7FFF00",  # Chartreuse
  "15" = "#D2691E"   # Chocolate
)

# Read in new cluster assignments
new_clusters <- readRDS(file = file.path(data_dir, "new_cluster_data.rds"))

i <- 1
# Loop through each sampling frequency
for (i in seq_along(sampling_frequencies)) {
  interval <- sampling_frequencies[i]
  panel_letter <- panel_letters[i]

  # Set sample rate based on interval
  sample_rate <- 1 * interval

  # Select the matched cluster assignments
  cluster_assignments <- new_clusters[[paste0("SF_", interval, "m")]]

  # Check lengths of unique_dates and cluster map
  if (length(unique_dates) != length(cluster_assignments)) {
    stop("\nLength of unique dates in archive is not equal to cluster assignment length. Please check.\n")
  } else {
    date_cluster_mapping <- data.table::data.table(date_only = unique_dates, cluster = cluster_assignments)
  }

  # Join this mapping back to archive_days to assign each row the correct cluster
  plot_data <- merge(archive_days, date_cluster_mapping, by = "date_only", all.x = TRUE)

  # Re-apply time_zone attribute
  attr(plot_data, "time_zone") <- tz_attr

  # Re-order columns as necessary
  plot_data <- plot_data[, c("date", "depth", "date_only", "cluster")]

  # Crop to the desired 30-day period
  plot_data <- subset(plot_data, date_only >= "2013-11-01" & date_only < "2013-12-01")

  # Define the specific days you want as breaks
  x_axis_labels <- c(1, 5, 10, 15, 20, 25, 30)

  # Convert to data table
  setDT(plot_data) # Convert to data.table if it's not already

  # Down-sampling the data to sample_rate
  plot_data <- plot_data[seq(1, nrow(plot_data), by = sample_rate), ]

  # Calculate sampling frequency
  sampling_interval <- as.numeric(plot_data$date[2] - plot_data$date[1], units = "secs")
  cat("\nDepth sampling frequency is", sampling_interval, "seconds\n")

  # Add a group to plot with geom_path
  plot_data$group <- 1

  # Convert 'cluster' to factor if it's not already
  plot_data$cluster <- as.factor(plot_data$cluster)

  # Ensure `seq_date` starts from 0 and increments by 1 for each new day
  plot_data$days_elapsed <- as.numeric(plot_data$date_only - min(plot_data$date_only)) + 1

  # Create a unique mapping of `date` to `days_elapsed`
  days_mapping <- unique(plot_data[, .(date, days_elapsed)])

  # Convert desired days into actual datetime objects
  start_date <- min(plot_data$date_only)
  break_dates <- start_date + lubridate::days(x_axis_labels - 1)  # subtract 1 to correctly align (e.g., day 1 is 0 days after start)
  break_dates <- as.POSIXct(break_dates, tz = "UTC")  # or use your relevant timezone

  # Plot with Days Elapsed labels on x-axis
  plot <- ggplot(data = plot_data, aes(x = date, y = depth, color = cluster, group = group)) +
    geom_path() +
    scale_y_reverse(limits = c(750, 0), breaks = waiver(), expand = c(0, 0)) +
    scale_x_datetime(breaks = break_dates, labels = date_to_elapsed(date = break_dates, days_mapping = days_mapping), expand = c(0, 0, 0, 0), position = "top") +
    scale_color_manual(values = custom_palette) +
    theme_classic()

  plot <- plot +
    labs(
      x = NULL,
      y = "Depth (m)",
      color = "Cluster"
    ) +
    theme(
      legend.position = "NULL",
      plot.margin = margin(t = 0.25, r = 0.1, b = 0.4, l = 0.1, "cm"),

      text = element_text(size = 26),
      axis.text.x = element_blank(),
      axis.text.y = element_text(face = "bold", colour = "black"),
      axis.title = element_text(face = "bold", colour = "black"),
      legend.title = element_text(face = "bold", colour = "black"),
      axis.ticks.length = unit(0.2, "cm"),
      axis.ticks = element_line(linewidth = 1.0, color = "black"),
      axis.line = element_line(linewidth = 1.0)

    )

  # Display the plot
  print(plot)

  # Save the plot to a file
  ggsave(file = file.path(data_dir, sprintf(".30 day/%dmin.png", interval)), plot = plot, width = 10, height = 4, dpi = 600)
  cat("Plot saved to ", file.path(data_dir, sprintf(".30 day/%dmin.png", interval)))

  # Clean up for next iteration
  rm(interval, panel_letter, sample_rate, cluster_assignments, date_cluster_mapping, plot_data, sampling_interval, days_mapping, start_date, break_dates, plot)
}

# Clean up remaining variables
rm(data_dir, archive_days, tz_attr, unique_dates, sampling_frequencies, panel_letters, date_to_elapsed, custom_palette, x_axis_labels, i, new_clusters)

#
##### Figure 9 - Confusion Matrix #####
# Load necessary libraries
library(ggplot2)

data_dir <- "E:/Ch 3 data/130970a_multi"
cluster_data <- readRDS(file = file.path(data_dir, "new_cluster_data.rds"))

# Read the CSV file into a data frame
#cluster_data <- new_cluster_assignment

# View the first few rows of the data frame
head(cluster_data)
cluster_data$SF_1m <- factor(cluster_data$SF_1m)
cluster_data$SF_5m <- factor(cluster_data$SF_5m)
cluster_data$SF_10m <- factor(cluster_data$SF_10m)
cluster_data$SF_15m <- factor(cluster_data$SF_15m)
cluster_data$SF_20m <- factor(cluster_data$SF_20m)
cluster_data$SF_25m <- factor(cluster_data$SF_25m)
cluster_data$SF_30m <- factor(cluster_data$SF_30m)

# Helper function to calculate precision, recall, and F1-score for each class in the confusion matrix also handle non-square matrices correctly
calculate_f1_scores <- function(cm) {
  # Ensure that the confusion matrix is square
  if (nrow(cm) != ncol(cm)) {
    all_classes <- union(rownames(cm), colnames(cm))
    cm_new <- matrix(0, length(all_classes), length(all_classes), dimnames = list(all_classes, all_classes))
    cm_new[rownames(cm), colnames(cm)] <- cm
    cm <- cm_new
  }

  # Calculate precision and recall for each class
  precision <- diag(cm) / rowSums(cm)
  recall <- diag(cm) / colSums(cm)

  # Calculate F1-score for each class
  f1_scores <- 2 * (precision * recall) / (precision + recall)

  # Handling the case where precision and recall are both 0, which results in NaN for the F1-score
  f1_scores[is.na(f1_scores)] <- 0

  return(f1_scores)
}

# Confusion matrix between the 15-second interval and the 1-minute interval
cm_1m_1m <- table(cluster_data$`SF_1m`, cluster_data$`SF_1m`)
f1_scores_1m_1m <- calculate_f1_scores(cm_1m_1m)
#print(f1_scores_1m_1m)

# Confusion matrix between the 1-minute interval and the 5-minute interval
cm_1m_5m <- table(cluster_data$SF_1m, cluster_data$`SF_5m`)
f1_scores_1m_5m <- calculate_f1_scores(cm_1m_5m)
#print(f1_scores_1m_5m)

# Confusion matrix between the 15-second interval and the 1-minute interval
cm_1m_10m <- table(cluster_data$SF_1m, cluster_data$SF_10m)
f1_scores_1m_10m <- calculate_f1_scores(cm_1m_10m)
# print(f1_scores_1m_10m)

# Confusion matrix between the 15-second interval and the 1-minute interval
cm_1m_15m <- table(cluster_data$SF_1m, cluster_data$SF_15m)
f1_scores_1m_15m <- calculate_f1_scores(cm_1m_15m)
# print(f1_scores_1m_15m)

# Confusion matrix between the 15-second interval and the 1-minute interval
cm_1m_20m <- table(cluster_data$SF_1m, cluster_data$SF_20m)
f1_scores_1m_20m <- calculate_f1_scores(cm_1m_20m)
# print(f1_scores_1m_20m)

# Confusion matrix between the 15-second interval and the 1-minute interval
cm_1m_25m <- table(cluster_data$SF_1m, cluster_data$SF_25m)
f1_scores_1m_25m <- calculate_f1_scores(cm_1m_25m)
# print(f1_scores_1m_25m)

# Confusion matrix between the 15-second interval and the 1-minute interval
cm_1m_30m <- table(cluster_data$SF_1m, cluster_data$SF_30m)
f1_scores_1m_30m <- calculate_f1_scores(cm_1m_30m)
# print(f1_scores_1m_30m)

# Helper function - Combine F1 scores into a data frame with corresponding sampling frequencies and cluster labels
combine_f1_scores <- function(f1_scores, interval_label) {
  data.frame(
    Cluster = as.factor(1:length(f1_scores)),
    F1Score = f1_scores,
    Interval = as.factor(interval_label)
  )
}

# Create data frames for each set of F1-scores
df_1m_1m <- combine_f1_scores(f1_scores_1m_1m, "1")
df_1m_5m <- combine_f1_scores(f1_scores_1m_5m, "5")
df_1m_10m <- combine_f1_scores(f1_scores_1m_10m, "10")
df_1m_15m <- combine_f1_scores(f1_scores_1m_15m, "15")
df_1m_20m <- combine_f1_scores(f1_scores_1m_20m, "20")
df_1m_25m <- combine_f1_scores(f1_scores_1m_25m, "25")
df_1m_30m <- combine_f1_scores(f1_scores_1m_30m, "30")

# Combine all F1 score data frames into one
all_f1_scores <- rbind(df_1m_1m, df_1m_5m, df_1m_10m, df_1m_15m, df_1m_20m, df_1m_25m, df_1m_30m)

rm(f1_scores_1m_1m, f1_scores_1m_5m, f1_scores_1m_10m, f1_scores_1m_15m, f1_scores_1m_20m, f1_scores_1m_25m, f1_scores_1m_30m)
rm(df_1m_1m, df_1m_5m, df_1m_10m, df_1m_15m, df_1m_20m, df_1m_25m, df_1m_30m)

# Calculate overall accuracy for each confusion matrix (macro F1 score - averages the F1 scores across clusters, regardless of cluster size).
overall_accuracy <- function(cm) {
  sum(diag(cm)) / sum(cm)
}

# Compute overall accuracies for each interval
acc_1m <- overall_accuracy(cm_1m_1m)
acc_5m <- overall_accuracy(cm_1m_5m)
acc_10m <- overall_accuracy(cm_1m_10m)
acc_15m <- overall_accuracy(cm_1m_15m)
acc_20m <- overall_accuracy(cm_1m_20m)
acc_25m <- overall_accuracy(cm_1m_25m)
acc_30m <- overall_accuracy(cm_1m_30m)

rm(cm_1m_1m, cm_1m_5m, cm_1m_10m, cm_1m_15m, cm_1m_20m, cm_1m_25m, cm_1m_30m)

# Create a data frame for plotting accuracies
overall_accuracy <- data.frame(
  Interval = factor(c("1", "5", "10", "15", "20", "25", "30"),
                    levels = c("1", "5", "10", "15", "20", "25", "30")),
  Accuracy = c(acc_1m, acc_5m, acc_10m, acc_15m, acc_20m, acc_25m, acc_30m)
)

rm(acc_1m, acc_5m, acc_10m, acc_15m, acc_20m, acc_25m, acc_30m)

# Custom select colours
custom_palette <- c(
  "1" = "#E41A1C",   # Red
  "2" = "#377EB8",   # Blue
  "3" = "#FF7F00",   # Orange
  "4" = "#984EA3",   # Purple
  "5" = "#4DAF4A",   # Green
  "Overall Accuracy\n(Macro F1)" = "black"     # Black
)

# Custom shape assignment (ensure you have as many shapes as clusters)
custom_shapes <- c(15, 16, 17, 18, 23, 4, 11, 8, 9, 10)

# Modify the ggplot code
combined_plot <- ggplot() +
  geom_line(data = all_f1_scores, aes(x = Interval, y = F1Score, group = Cluster, color = Cluster), linewidth = 1.2) +
  geom_point(data = all_f1_scores, aes(x = Interval, y = F1Score, color = Cluster, shape = Cluster), size = 3) +
  geom_line(data = overall_accuracy, aes(x = Interval, y = Accuracy, group = 1, color = "Overall Accuracy\n(Macro F1)"), linewidth = 1.5, linetype = "dashed") +
  geom_point(data = overall_accuracy, aes(x = Interval, y = Accuracy, color = "Overall Accuracy\n(Macro F1)", shape = "Overall Accuracy\n(Macro F1)"), size = 6) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01,0)) +
  scale_x_discrete(expand = c(0.01,0.02)) +
  scale_color_manual(values = custom_palette) +
  scale_shape_manual(values = custom_shapes) +
  labs(
    x = "Depth Sampling Frequency (mins)",
    y = "F1 Score",
    color = "Clusters",
    shape = "Clusters") +
  theme_classic() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.2, 0.2),
    plot.margin = margin(5.5, 10, 5.5, 5.5),

    text = element_text(size = 30),
    axis.text.x = element_text(face = "bold", colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black"),
    axis.title = element_text(face = "bold", colour = "black"),
    legend.title = element_text(face = "bold", colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks = element_line(linewidth = 1.0, color = "black"),
    axis.line = element_line(linewidth = 1.0)

  )

# Display the plot
print(combined_plot)

# Save the combined plot
ggsave(filename = file.path(data_dir, "Performance_Metrics.png"), plot = combined_plot, width = 10, height = 10, dpi = 600)
write.csv(overall_accuracy, file = file.path(data_dir, "overall_accuracy.csv"))
write.csv(all_f1_scores, file = file.path(data_dir, "all_f1_scores.csv"))

rm(cluster_data, calculate_f1_scores, combine_f1_scores, custom_palette, custom_shapes, combined_plot, data_dir, overall_accuracy, all_f1_scores)
#

#####
#####
##### Supplementary Figures #####
# No code here, these figures are output directly from FishDiveR code then combined in powerpoint

#
