#' Perform k selection
#'
#' `select_k` creates the elbow plot and silhouette width plot for assistance
#' with selection of k
#'
#' @name select_k
#'
#' @import ggplot2
#' @importFrom cluster silhouette
#' @importFrom cluster clusGap
#' @importFrom stats dist
#' @importFrom cowplot plot_grid
#' @importFrom withr local_options
#'
#' @inheritParams pca_data
#' @param kmeans_data Data frame containing the combined PC scores and depth
#'   statistics to perform k-means on. Output from the 'combine_data()'
#'   function.
#' @param standardise TRUE or FALSE. Whether or not to standardise the data.
#'   Defaults to TRUE.
#' @param Max.k Numerical. Maximum value of k to try. Defaults to 15.
#' @param v_line Numerical. Option to add a vertical line to plot at a specific
#'   value of k. Defaults to NULL.
#' @param plot_gap TRUE or FALSE. Whether or not to plot the gap statistic.
#'   Defaults to FALSE.
#'
#' @returns A 'ggplot' class object and creates a figure containing both the
#'   within-cluster sum of squares plot (elbow) and the average silhouette width
#'   plot for 1 to 'Max.k' clusters.
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
#' # Run select_k function
#' selecting_k <- select_k(
#'   kmeans_data = kmeans_data,
#'   standardise = TRUE,
#'   Max.k = 8,
#'   v_line = 4,
#'   plot_gap = FALSE,
#'   output_folder = tempdir()
#' )
#'
# Function to create the elbow and silhouette width plots for selecting K
select_k <- function(kmeans_data,
                     standardise = TRUE,
                     Max.k = 15,
                     v_line = NULL,
                     plot_gap = FALSE,
                     output_folder = data_dir) {
  # Check if kmeans_data is a data frame
  if (!is.data.frame(kmeans_data)) {
    stop("kmeans_data must be a data frame. \n")
  }
  if (!is.logical(standardise)) {
    stop("standardise must be TRUE or FALSE.")
  }
  if (!is.null(Max.k) && (!is.numeric(Max.k) || Max.k <= 0)) {
    stop("Max.k must be NULL or a positive number.")
  }
  if (!is.null(v_line) && (!is.numeric(v_line) || v_line <= 0)) {
    stop("v_line must be NULL or a positive number.")
  }
  if (!is.logical(plot_gap)) {
    stop("standardise must be TRUE or FALSE.")
  }

  # Set the random seed for reproducible results in the function
  withr::local_options(set.seed(123))

  # Check that Max.k does not exceed the number of kmeans_data
  if (Max.k >= nrow(kmeans_data)) {
    message(paste0("Error: The maximum number of clusters cannot equal or exceed the number of days. Using Max.K = ", nrow(kmeans_data) - 1))
    Max.k <- nrow(kmeans_data) - 1
  }

  # Set unique_tag_ID as the unique tag id attribute
  unique_tag_ID <- attr(kmeans_data, "unique_tag_ID")

  if (standardise == TRUE) {
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
  } else {
    cat("\n If kmeans_data is a combination of PC scores and depth data. Please standardise the data as this can greatly impact the reliability of the results. \n")
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

  # Calculate gap statistic for all k
  gap_stat_result <- cluster::clusGap(kmeans_data, FUN = kmeans, K.max = Max.k, B = 100)
  gap_stat <- gap_stat_result$Tab[, "gap"]
  se_sim <- gap_stat_result$Tab[, "SE.sim"]

  # Identify the optimal number of clusters based on the gap statistic
  firstSEmax <- function(gap_stat, se_sim) {
    k <- 1
    for (i in 2:length(gap_stat)) {
      if (gap_stat[i] >= gap_stat[i - 1] - se_sim[i]) {
        k <- i
        break
      }
    }
    return(k)
  }
  optimal_k_gap <- firstSEmax(gap_stat, se_sim)
  cat(paste0("\nOptimal number of clusters according to gap statistic: ", optimal_k_gap, "\n"))

  # Create the within-cluster sum of squares plot "Elbow method"
  Elbow_plot <- ggplot2::ggplot() +
    geom_line(data = data.frame(k_values, sse), aes(x = k_values, y = sse), color = "black") +
    geom_point(data = data.frame(k_values, sse), aes(x = k_values, y = sse), color = "black") +
    labs(x = "Number of Clusters", y = "Within-Cluster Sum of Squares") +
    scale_x_continuous(breaks = seq(1, k, 1)) +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14)
    )

  # If v_line is not NULL, add a vertical line
  if (!is.null(v_line)) {
    Elbow_plot <- Elbow_plot + geom_vline(xintercept = v_line, linetype = "dashed", color = "red")
  }

  # Create the silhouette width plot
  Silhouette_plot <- ggplot2::ggplot() +
    geom_line(data = data.frame(k_values[-1], sil[-1]), aes(x = k_values[-1], y = sil[-1]), color = "black") +
    geom_point(data = data.frame(k_values[-1], sil[-1]), aes(x = k_values[-1], y = sil[-1]), color = "black") +
    labs(x = "Number of Clusters", y = "Silhouette Width") +
    scale_x_continuous(breaks = seq(1, k, 1)) +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14)
    )

  # If v_line is not NULL, add a vertical line
  if (!is.null(v_line)) {
    Silhouette_plot <- Silhouette_plot + geom_vline(xintercept = v_line, linetype = "dashed", color = "red")
  }

  if (plot_gap) {
    # Create the gap statistic plot
    Gap_plot <- ggplot2::ggplot() +
      geom_line(data = data.frame(k_values, gap_stat), aes(x = k_values, y = gap_stat), color = "black") +
      geom_point(data = data.frame(k_values, gap_stat), aes(x = k_values, y = gap_stat), color = "black") +
      labs(x = "Number of Clusters", y = "Gap Statistic") +
      scale_x_continuous(breaks = seq(1, Max.k, 1)) +
      theme_classic() +
      theme(
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
      )

    # If v_line is not NULL, add a vertical line
    if (!is.null(v_line)) {
      Gap_plot <- Gap_plot + geom_vline(xintercept = v_line, linetype = "dashed", color = "red")
    }

    # Combine the three plots
    combined_k_plot <- cowplot::plot_grid(Elbow_plot, Silhouette_plot, Gap_plot, labels = c("A", "B", "C"), label_size = 10)
  } else {
    # Combine the elbow and silhouette width plots
    combined_k_plot <- cowplot::plot_grid(Elbow_plot, Silhouette_plot, labels = c("A", "B"), label_size = 10)
  }

  # Print the combined plot
  print(combined_k_plot)

  # Count the number of columns starting with "PC" in combined_data
  pc_columns_count <- length(grep("^PC", names(kmeans_data)))

  # Save the combined plot
  if (is.null(unique_tag_ID) == TRUE) {
    # Multiple tags save folder 'Combined_tags'
    save_folder <- file.path(output_folder, "Combined_tags/5_k-means")

    # Message
    cat(paste0("Multiple tags output file: ", save_folder, "/Select_k.", pc_columns_count, "_PCs.png\n"))
  } else {
    # Single tag save folder - unique_unique_tag_ID
    save_folder <- file.path(output_folder, unique_tag_ID, "5_k-means")

    # Message
    cat(paste0("Single tag output file: ", save_folder, "/Select_k.", pc_columns_count, "_PCs.png\n"))
  }

  # Create the directory if it doesn't exist
  create_directory(save_folder)

  # Save plot to combined data folder
  ggsave(file.path(save_folder, paste0("Select_k.", pc_columns_count, "_PCs.png")), plot = combined_k_plot, width = 14.22, height = 5)

  return(combined_k_plot)
}
