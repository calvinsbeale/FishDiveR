#' Perform k-means
#'
#' `k_clustering` performs k-means clustering on the PC scores with the selected
#' value of k
#'
#' @name k_clustering
#'
#' @import ggplot2
#' @importFrom scales muted
#' @importFrom rgl plot3d
#' @importFrom rgl triangles3d
#' @importFrom rgl legend3d
#' @importFrom colorspace qualitative_hcl
#' @importFrom geometry convhulln
#' @importFrom withr local_options
#' @importFrom stats kmeans
#' @importFrom grDevices rainbow
#' @importFrom tidyr replace_na
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate
#' @importFrom grDevices colorRampPalette
#'
#' @inheritParams select_k
#' @param k Numerical. Value of k to use for analysis.
#' @param polygon TRUE or FALSE. Plot polygons for cluster with more than 3 data
#'   points. Defaults to FALSE.
#'
#' @returns An object of class 'kmeans' containing the k-means clustering data
#'   for the data frame. Additionally plots a 3D cluster plot of the top three
#'   Principal Components.
#'
#' @export
#'
#' @examples
#' # Set file path
#' filepath <- system.file("extdata", package = "FishDiveR")
#'
#' # Load kmeans_data
#' kmeans_data <- readRDS(file.path(filepath, "data/5_k-means/combined_stats.rds"))
#'
#' # Run k_clustering function
#' kmeans_result <- k_clustering(
#'   kmeans_data = kmeans_data,
#'   standardise = TRUE,
#'   k = 4,
#'   polygon = FALSE,
#'   output_folder = tempdir()
#' )
#'
# Function to perform k-means clustering on the PC scores with the selected value of K
k_clustering <- function(kmeans_data,
                         standardise = TRUE,
                         k,
                         polygon = FALSE,
                         output_folder = data_dir) {
  # Check if kmeans_data is a data frame
  if (!is.data.frame(kmeans_data)) {
    stop("kmeans_data must be a data frame. \n")
  }
  if (!is.logical(standardise)) {
    stop("standardise must be TRUE or FALSE.")
  }
  if ((!is.numeric(k) || k <= 0)) {
    stop("k must a positive integer.")
  }
  if (!is.logical(polygon)) {
    stop("polygon must be TRUE or FALSE.")
  }

  # Check if k > number of distinct data points
  if (k >= nrow(kmeans_data)) {
    stop("Error: K cannot be equal to or greater than the number of data points. \n")
  }

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
        cat("\nCaution - cannot standardise a column with a unique value: ", colnames(numeric_data[i]), "=", print(numeric_data[1, i]), "\n")
      }
    }
  } else {
    cat("\n If kmeans_data is a combination of PC scores and depth data. Please standardise the data as this can greatly impact the reliability of the results. \n")
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
  } else {
    cat("One or more of PC1, PC2, PC3 do not exist. Skipping 3d cluster plot.\n")
  }

  # Create cluster_result data frame
  cluster_result <- as.data.frame(kmeans_result$cluster)
  cluster_result <- cbind(tag_day = rownames(cluster_result), cluster_result)
  cluster_result$tag_ID <- kmeans_data$tag_ID
  colnames(cluster_result)[2] <- "cluster"

  # Create a complete data frame with all combinations of tag_ID and cluster
  unique_tag_ids <- unique(cluster_result$tag_ID)
  all_clusters <- expand.grid(tag_ID = unique_tag_ids, cluster = 1:k)

  # Summarise the cluster results
  cluster_summary <- aggregate(tag_day ~ tag_ID + cluster, data = cluster_result, FUN = length)
  colnames(cluster_summary)[3] <- "days_in_cluster"

  # Join the complete data frame with the summary data frame
  complete_cluster_summary <- merge(all_clusters, cluster_summary, by = c("tag_ID", "cluster"), all.x = TRUE)

  # Replace NA values with 0
  complete_cluster_summary$days_in_cluster[is.na(complete_cluster_summary$days_in_cluster)] <- 0

  # Sort the data frame by tag_ID and cluster
  complete_cluster_summary <- complete_cluster_summary[order(complete_cluster_summary$tag_ID, complete_cluster_summary$cluster), ]

  # print(complete_cluster_summary)
  print(cluster_summary)

  # Save the cluster data
  if (is.null(unique_tag_ID) == TRUE) {
    # Set multiple tags save folder
    save_folder <- file.path(output_folder, "Combined_tags/5_k-means")

    # Create the directory if it doesn't exist
    create_directory(save_folder)

    # Save the cluster output as an excel sheet
    write.csv(cluster_result, file = file.path(save_folder, paste0("Cluster_results_k=", k, ".csv")), row.names = FALSE)

    # Save the cluster table as an excel sheet
    write.csv(cluster_table, file = file.path(save_folder, paste0("Cluster_table_k=", k, ".csv")), row.names = FALSE)

    # Write the summary to a CSV file
    write.csv(complete_cluster_summary, file = file.path(save_folder, paste0("Cluster_summary_tag_k=", k, ".csv")), row.names = FALSE)
    write.csv(cluster_summary, file = file.path(save_folder, paste0("Cluster_summary_cluster_k=", k, ".csv")), row.names = FALSE)

    # Message folder
    cat(paste0("Output folder: ", save_folder, "\n"))
  } else {
    # Set single tag save folder
    save_folder <- file.path(output_folder, unique_tag_ID, "5_k-means")

    # Create the directory if it doesn't exist
    create_directory(save_folder)

    # Save the cluster output as an excel sheet
    write.csv(cluster_result, file = file.path(save_folder, paste0("Cluster_results_k=", k, ".csv")), row.names = FALSE)

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
      axis.text.x = element_text(size = 20, hjust = 1),
      axis.text.y = element_text(size = 20),
      text = element_text(size = 20) ## change all text size in figure
    ) +
    guides(fill = guide_legend(title = "Variable", ncol = 1, override.aes = list(alpha = 1)))

  # Create a custom fill scale that uses the gradient colours
  plot_colour_scaled <- plot_colour +
    scale_fill_manual(
      values = all_gradients,
      breaks = custom_legend_breaks, # Only show legend for cluster 1
      labels = legend_levels # Labels without cluster prefix
    )

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
      axis.text.x = element_text(size = 20, hjust = 1),
      axis.text.y = element_text(size = 20),
      text = element_text(size = 20) ## change all text size in figure
    ) +
    guides(fill = guide_legend(title = "Variable", ncol = 1, override.aes = list(alpha = 1)))

  # Save the plot
  ggsave(file.path(save_folder, paste0("cluster_variables_k=", k, "_bw.png")), plot = plot_bw_scaled, height = 12, width = 16, dpi = 600)

  print(plot_bw_scaled)

  kmeans_result$cluster_means <- cluster_means

  # Save the 'kmeans_result' object as kmeans_result.rds to save_folder
  saveRDS(kmeans_result, file = file.path(save_folder, "kmeans_result.rds"))

  return(kmeans_result)
}
