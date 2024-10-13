i <- 20

data_dir <- "E:/Ch 3 data"
archive_days <- readRDS(file = file.path(data_dir, meta$tag_ID[i], "archive_days.rds"))


  tag_ID = meta$tag_ID[i]
  data_folder = data_dir
  every_Nth = 1*60
  plot_size = c(12,6)
  Y_lim = c(0, 300, 75)
  date_breaks = "60 day"
  output_folder = file.path(data_dir) # Combined_tags

# Read in tag archive
archive_days <- readRDS(file = file.path(data_folder, tag_ID, "/archive_days.rds"))
plot_data <- archive_days[, c("date", "depth")]

# Calculate sampling interval
sampling_interval <- as.numeric(archive_days$date[2] - archive_days$date[1], units = "secs")

cat("\nSampling interval is", sampling_interval, "seconds\n")
cat("\nMaximum depth is", max(archive_days$depth), "meters\n")
cat("\nPlotting every", sampling_interval * every_Nth, "seconds \n")

# Filter to every Nth record
every_nth <- seq(every_Nth, nrow(plot_data), by = every_Nth)
plot_data <- plot_data[every_nth, ]

TDR_plot <- ggplot(plot_data, aes(x = date, y = depth)) +
  geom_path() +
  scale_y_reverse(limits = c(Y_lim[2], Y_lim[1]), breaks = seq(Y_lim[1], Y_lim[2], Y_lim[3]), expand = c(0.05, 0, 0, 0)) +
  scale_x_datetime(date_breaks = date_breaks, date_labels = "%Y-%m-%d", expand = c(0, 0, 0, 0), position = "top") +
  labs(
    x = "Date",
    y = "Depth (meters)"
  ) +
  theme_classic() +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = -0.05),
    axis.text = element_text(size = 22),
    axis.title = element_text(size = 22),
    plot.margin = unit(c(0, 3, 0.1, 0.1), "lines") # top, right, bottom, left
  )
print(TDR_plot)

ggsave(file.path(output_folder, tag_ID, paste0(tag_ID, "_archive.png")), plot = TDR_plot, width = plot_size[1], height = plot_size[2], dpi = 600, create.dir = TRUE)
cat("\nOutput file:", file.path(output_folder, tag_ID, paste0(tag_ID, "_archive.png")))

my.w <- create_wavelet(
  archive = archive_days,
  tag_ID = meta$tag_ID[i],
  wv_period_hours = 24,
  sampling_frequency = NULL,
  suboctaves = 12,
  lower_period_mins = 5,
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

data_dir <- "E:/Ch 3 data"
tag_list <- "test180" # Simulated data 180 days 1 minute SF

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
  No_pcs = 3,
  plot_eigenvalues = TRUE,
  output_folder = data_dir
)

##### PC scores #####
# Create the principal component scores data frame
pc_results = pc_results
plot_loadings = TRUE
output_folder = data_dir


# Check if the 'unique_tag_ID' attribute exists in pc_results
if (!is.null(attr(pc_results, "unique_tag_ID"))) {
  # The attribute exists, so use it with existing helper function by turning it into a data frame
  unique_tag_ID <- as.data.frame(attr(pc_results, "unique_tag_ID"))

  # Set the save folder location using helper function
  save_folder <- set_scores_save_folder(
    output_folder = output_folder,
    data_frame = unique_tag_ID # converted to data frame
  )
} else {
  # Attribute does not exist
  cat("\n No unique_tag_ID. Multiple tags included in pc_results. \n")

  # Set the save folder location
  save_folder <- file.path(output_folder, "Combined_tags/4_PCA")
}

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

# Helper function to display every other tick mark
every_other <- function(x) {
  inds <- seq(1, length(x), by = 2)
  labels <- rep("", length(x))
  labels[inds] <- x[inds]
  return(labels)
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

    # Create plot title
    plot_title <- paste0("Principal Component ", c)

    wavelet_loadings_plot <- ggplot2::ggplot(
      data = loadings,
      aes(
        x = stats::reorder(period_labs, seq), y = Loading, fill = Principal.Component, group = CleanVariable,
        colour = DescriptiveName
      )
    ) +
      geom_path(stat = "identity", linewidth = 1.25) +
      geom_hline(yintercept = load, linetype = "dashed", color = "black", linewidth = 0.75) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.75) +
      geom_hline(yintercept = -load, linetype = "dashed", color = "black", linewidth = 0.75) +
      scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +
      scale_x_discrete(labels = every_other, expand = c(0, 0, 0.001, 0)) +
      theme_bw() +
      labs(
        x = "Wavelet Period",
        y = "Loading",
        colour = "Wavelet Variable"
      ) +
      ggtitle(plot_title) +
      theme(
        axis.text.x = element_text(angle = 45, size = 14, color = "Black", vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 14),
        legend.position = "right",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)
      )

    # Print to 'Plots' tab
    print(wavelet_loadings_plot)

    ggsave(file.path(save_folder, paste0("PC", c, "_Loadings.png")), plot = wavelet_loadings_plot, width = 15, height = 10, dpi = 600)

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

  # Plotting mean wavelet power for each PC
  mean_power_plot <- ggplot(data = combined_mean_power_data, aes(x = stats::reorder(period_labs, seq), y = Loading, color = Principal.Component, group = Principal.Component)) +
    geom_line(linewidth = 1) + # Use geom_line for line plots
    geom_hline(yintercept = load, linetype = "dashed", color = "black", linewidth = 0.75) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.75) +
    geom_hline(yintercept = -load, linetype = "dashed", color = "black", linewidth = 0.75) +
    scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +
    scale_x_discrete(labels = every_other(rev(combined_mean_power_data$period_labs)), expand = c(0, 0)) +
    theme_bw() +
    labs(
      x = "Wavelet Period",
      y = "Mean Wavelet Power Loading",
      color = "Principal\nComponent"
    ) +
    # ggtitle(title) +
    theme(
      axis.text.x = element_text(angle = 45, size = 14, color = "Black", vjust = 0.5, hjust = 0.5),
      axis.text.y = element_text(size = 22),
      axis.title.x = element_text(size = 22), # Set font size and style for x-axis label
      axis.title.y = element_text(size = 22), # Set font size and style for y-axis label
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 22)
    )

  # Print the plot
  print(mean_power_plot)

  # Save the mean power line plot
  ggsave(file.path(save_folder, "PCA_mean_power_line_plot.png"), plot = mean_power_plot, width = 15, height = 10, dpi = 600)


# Add an attribute to pc_scores with unique Tag_ID if processing one tag
attr(pc_scores, "unique_tag_ID") <- attr(pc_results, "unique_tag_ID")

# Add an attribute for the number of PC's kept
attr(pc_scores, "No.Components") <- Max.C

# Save the 'pc_scores' object to output_folder
saveRDS(pc_scores, file = file.path(save_folder, "pc_scores.rds"))
cat(paste0("\nOutput file: ", save_folder, "/pc_scores.rds\n"))

# Import the depth statistics and combine with the PC scores, then standardise the data ready to be used in k-means clustering
kmeans_data <- combine_data(
  tag_vector = tag_list,
  data_folder = data_dir,
  pc_scores = pc_scores,
  output_folder = data_dir
)


# Run k means with the selected number of clusters. Plot with or without polygons. Resize window then plot a second time to fix legend size.
kmeans_result <- k_clustering(
  kmeans_data = kmeans_data,
  standardise = TRUE,
  k = 5,
  polygon = TRUE,
  colour = TRUE,
  output_folder = data_dir
)


# Run k means with the selected number of clusters. Plot with or without polygons. Resize window then plot a second time to fix legend size.
standardise = TRUE
k = 5
polygon = TRUE
colour = TRUE
output_folder = data_dir

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

if (standardise == TRUE) {
  cat("\n Standardising k-means input. \n")

  # Apply scaling only to columns that have more than one unique value to avoid division by zero
  for (i in 1:ncol(numeric_data)) {
    if (length(unique(numeric_data[[i]])) > 1) {
      numeric_data[[i]] <- scale(numeric_data[[i]])
    } else {
      cat("\nCaution - cannot standardise a column with a unique value: ", colnames(numeric_data[i]), "\n")
    }
  }
} else {
  cat("If kmeans_data is a combination of PC scores and depth data. Please standardise the data as this can greatly impact the reliability of the results. \n")
}

# Run K-means with selected number of clusters
kmeans_result <- stats::kmeans(numeric_data, centers = k, nstart = 1500, iter.max = 50)

# Extract the centroids and cluster assignments
centroids <- kmeans_result$centers
k_cluster_assignments <- kmeans_result$cluster

# Calculate distances between each point and corresponding cluster centroid:
kmeans_result$distances <- sapply(1:nrow(numeric_data), function(i) {
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
  "1" = "#E41A1C", "2" = "#377EB8", "3" = "#FF7F00", "4" = "#984EA3",
  "5" = "#4DAF4A", "6" = "#344111", "7" = "#A65628", "8" = "#F781BF",
  "9" = "black", "10" = "green"
)

# Extract only the existing desired columns
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
} else {
  cat("One or more of PC1, PC2, PC3 do not exist. Skipping 3d cluster plot.\n")
}

# Save the cluster data
if (is.null(unique_tag_ID) == TRUE) {
  # Set multiple tags save folder
  save_folder <- file.path(output_folder, "Combined_tags/5_k-means")

  # Create the directory if it doesn't exist
  create_directory(save_folder)

  # Save the cluster output as an excel sheet
  write.csv(as.data.frame(kmeans_result$cluster), file = file.path(save_folder, paste0("Cluster_results_k=", k, ".csv")))

  # Save the cluster table as an excel sheet
  write.csv(cluster_table, file = file.path(save_folder, paste0("Cluster_table_k=", k, ".csv")), row.names = FALSE)

  # Message folder
  cat(paste0("Output folder: ", save_folder, "\n"))
} else {
  # Set single tag save folder
  save_folder <- file.path(output_folder, unique_tag_ID, "5_k-means")

  # Create the directory if it doesn't exist
  create_directory(save_folder)

  # Save the cluster output as an excel sheet
  write.csv(as.data.frame(kmeans_result$cluster), file = file.path(save_folder, paste0("Cluster_results_k=", k, ".csv")))

  # Save the cluster table as an excel sheet
  write.csv(cluster_table, file = file.path(save_folder, paste0("Cluster_table_k=", k, ".csv")), row.names = FALSE)

  # Message folder
  cat(paste0("Output folder: ", save_folder, "\n"))
}

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
  "st_mean_diff" = "Diel Mean\nDepth",
  "st_sd_diff" = "Diel S.D.\nDepth",
  "st_range_diff" = "Diel Depth\nRange",
  "st_absVV_diff" = "Diel Absolute\nVertical Velocity",
  "st_ascVV_diff" = "Diel Ascent\nVertical Velocity",
  "st_dscVV_diff" = "Diel Descent\nVertical Velocity",
  #"composite_index" = "Diel Composite\nIndex",
  "surf_prop_diff" = "Diel Surface\nProportion"
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
  lighter_color <- scales::muted(color, l = 95)  # Create a lighter version of the color
  colorRampPalette(c(color, lighter_color))(n)  # Use the original and lighter color to create the gradient
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

  # Update the plot
  plot <- ggplot(cluster_means, aes(x = as.factor(cluster), y = Mean, fill = interaction(cluster, RenamedVariable))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      x = "Cluster",
      y = "Mean Standardised Value"
    ) +
    scale_x_discrete() +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 35, hjust = 1),
      axis.text.y = element_text(size = 35),
      text = element_text(size = 35) ## change all text size in figure
    ) +
    guides(fill = guide_legend(title = "Variable", ncol = 1, override.aes = list(alpha = 1)))

  # Create a custom fill scale that uses the gradient colours
  plot_scaled <- plot +
    scale_fill_manual(
      values = all_gradients,
      breaks = custom_legend_breaks,  # Only show legend for cluster 1
      labels = legend_levels  # Labels without cluster prefix
    )

  plot_scaled
  # Save the plot
  ggsave(file.path(save_folder, paste0("cluster_variables_k=", k, " colour.png")), plot = plot_scaled, height = 12, width = 16, dpi = 600)

  # Plot
  plot_scaled <- ggplot(cluster_means, aes(x = as.factor(cluster), y = Mean, fill = RenamedVariable)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_grey(start = 0.2, end = 0.8) + # Use greyscale for filling bars
    labs(
      x = "Cluster",
      y = "Mean Standardised Value"
    ) +
    scale_x_discrete() +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 35, hjust = 1),
      axis.text.y = element_text(size = 35),
      text = element_text(size = 35) ## change all text size in figure
    ) +
    guides(fill = guide_legend(title = "Variable", ncol = 1, override.aes = list(alpha = 1)))

print(plot_scaled)

# Save the plot
ggsave(file.path(save_folder, paste0("cluster_variables_k=", k, " BW.png")), plot = plot_scaled, height = 12, width = 16, dpi = 600)

kmeans_result$cluster_means <- cluster_means

# Save the 'kmeans_result' object as kmeans_result.rds to save_folder
saveRDS(kmeans_result, file = file.path(save_folder, "kmeans_result.rds"))
