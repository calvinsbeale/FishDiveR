#' Plot the time-series depth records of the selected tag. Colour days by
#' cluster
#'
#' `plot_cluster_TDR` plots the time-series depth record of the selected
#' archival tag. Each day of data is coloured by the assigned cluster, this
#' helps to visualise changes in vertical movement behaviour over time.
#'
#' @name plot_cluster_TDR
#'
#' @import ggplot2
#' @importFrom colorspace qualitative_hcl
#'
#' @inheritParams pca_data
#' @inheritParams plot_TDR
#' @param tag_ID Unique tag identification number in a vector of characters.
#'   E.g. "123456".
#' @param kmeans_result An object of class 'kmeans' containing the k-means
#'   clustering data. Output of 'k_clustering()' function.
#' @param legend TRUE or FALSE. Whether or not to plot the figure legend.
#'   Defaults to TRUE.
#'
#' @returns Returns the cluster TDR plot. Additionally prints to file the TDR
#'   plot. Additionally outputs a facet plot of all tag_IDs.
#'
#' @export
#'
#' @examples
#' # Set file path
#' filepath <- system.file("extdata", package = "FishDiveR")
#'
#' # Load kmeans_result
#' kmeans_result <- readRDS(file.path(filepath, "data/5_k-means/kmeans_result.rds"))
#'
#' # Run plot_clusters function
#' plot_cluster_TDR(
#'   tag_ID = "data",
#'   data_folder = filepath,
#'   kmeans_result = kmeans_result,
#'   every_nth = 10,
#'   every_s = 0,
#'   X_lim = NULL,
#'   Y_lim = c(0, 300, 50),
#'   date_breaks = "1 day",
#'   legend = TRUE,
#'   plot_size = c(12, 6),
#'   dpi = 100,
#'   output_folder = tempdir()
#' )
#'
# Function to print to file one figure for each cluster with a fixed y-axis. Additionally outputs a facet plot of all clusters, and a free y-axis version of all plots.
plot_cluster_TDR <- function(tag_ID,
                             data_folder = data_dir,
                             kmeans_result,
                             every_nth = 10,
                             every_s = 0,
                             X_lim = NULL,
                             Y_lim = c(0, 250, 50),
                             date_breaks = "14 day",
                             legend = TRUE,
                             plot_size = c(12, 6),
                             dpi = 300,
                             output_folder = data_dir) {
  # Check if tag_IDs is a character vector
  if (!is.character(tag_ID) || length(tag_ID) != 1) {
    stop("tag_ID must be a single character string.")
  }
  if (!is.list(kmeans_result)) {
    stop("kmeans_result must be a data frame. \n")
  }
  if ((!is.numeric(every_nth) || every_nth <= 0)) {
    stop("every_nth must be a positive integer.")
  }
  if ((!is.numeric(every_s) || every_s < 0)) {
    stop("every_s must be a positive integer.")
  }
  # Check that Y_lim is numeric, positive and exactly 3 elements long
  if (!is.numeric(Y_lim) || length(Y_lim) != 3 || any(Y_lim < 0)) {
    stop("Y_lim must be a numeric vector of length 3 with non-negative values.")
  }
  if (!is.logical(legend)) {
    stop("legend must be TRUE or FALSE.")
  }
  # Check that plot_size is numeric, positive and exactly 2 elements long
  if (!is.numeric(plot_size) || length(plot_size) != 2 || any(plot_size <= 0)) {
    stop("plot_size must be a numeric vector of length 2 with positive values.")
  }
  if ((!is.numeric(dpi) || dpi <= 0)) {
    stop("dpi must be a positive integer.")
  }
  # Check X_lim if provided
  if (!is.null(X_lim)) {
    if (!is.character(X_lim) || length(X_lim) != 2) {
      stop("X_lim must be a character vector of two dates in 'YYYY-MM-DD' format.")
    }
    # Convert to Date format for subsetting
    X_lim <- as.Date(X_lim)
    if (any(is.na(X_lim))) {
      stop("Invalid dates in X_lim. Ensure they are in 'YYYY-MM-DD' format.")
    }
  }

  # Tag info:
  cat(paste0("\n Loading tag ", tag_ID, " TDR \n"))

  # Load archive_days
  archive_days <- readRDS(file.path(data_folder, tag_ID, "archive_days.rds"))

  # Load k-means result
  if (length(unique(kmeans_result$tag_ID)) > 1) {
    cat("\n Combined tag k-means loaded \n")
  } else {
    cat("\n Single tag k-means loaded \n")
  }

  cat(paste0("\nMaximum depth is ", max(archive_days$depth)))

  # Extract the cluster and tag_ID
  tag_data <- as.data.frame(kmeans_result$cluster)
  tag_data$tag_ID <- kmeans_result$tag_ID

  # Split the dataframe by 'tag_ID'
  list_of_dataframes <- split(tag_data, tag_data$tag_ID)

  # Ensure 'archive_days' is treated as a data.table for efficient operations
  setDT(archive_days)

  # Create a data frame or data.table mapping each unique date to its cluster assignment
  unique_dates <- unique(archive_days$date_only)

  # Get cluster assignments
  cluster_assignments <- list_of_dataframes[[tag_ID]]$`kmeans_result$cluster`

  # Check lengths of unique_dates and cluster map
  if (length(unique_dates) != length(cluster_assignments)) {
    cat("\n Length of unique dates in archive is not equal to cluster assignment length. Please check.\n")
    l <- length(unique_dates)
    date_cluster_mapping <- data.table::data.table(date_only = unique_dates, cluster = cluster_assignments[l])
  } else {
    date_cluster_mapping <- data.table::data.table(date_only = unique_dates, cluster = cluster_assignments)
  }

  # Join this mapping back to archive_days to assign each row the correct cluster
  archive_days <- merge(archive_days, date_cluster_mapping, by = "date_only", all.x = TRUE)

  # Calculate the time differences between consecutive records
  time_diffs <- diff(as.numeric(archive_days$date, units = "secs"))

  # Check if the original sampling frequency is consistent
  sampling_interval <- unique(time_diffs)

  if (length(sampling_interval) != 1) {
    stop("Error: The original data does not have a consistent sampling frequency.")
  }

  # Print sampling interval
  cat(paste0("\nData sampling interval is ", sampling_interval, " seconds\n"))

  # Check X_lim if provided
  if (!is.null(X_lim)) {
    # Crop archive_days to the subset set by the limits
    archive_days <- subset(archive_days, date_only >= as.Date(X_lim[1]) & date_only <= as.Date(X_lim[2]))
    if (nrow(archive_days) == 0) {
      stop("No data available within the specified X_lim date range.")
    }
    cat("Data has been filtered between X-axis limits \n")
  }

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

  # Select the plot data
  quick_plot_data <- archive_days[, c("date", "depth", "cluster")]

  # Add a group to plot with geom_path
  quick_plot_data$group <- tag_ID

  # Convert 'cluster' to factor if it's not already
  quick_plot_data$cluster <- as.factor(quick_plot_data$cluster)

  # Get number of clusters
  k <- length(unique(kmeans_result$cluster))

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

  # Use the custom palette with scale_color_manual
  plot <- ggplot(data = quick_plot_data, aes(x = date, y = depth, colour = cluster, group = tag_ID)) +
    geom_path() +
    scale_y_reverse(limits = c(Y_lim[2], Y_lim[1]), breaks = seq(Y_lim[1], Y_lim[2], Y_lim[3]), expand = c(0, 0)) +
    scale_x_datetime(date_breaks = date_breaks, date_labels = "%Y-%m-%d", expand = c(0, 0, 0, 0), position = "top") +
    scale_color_manual(values = custom_palette) +
    labs(
      x = "Date",
      y = "Depth (m)",
      colour = "Cluster"
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, size = 14, colour = "black", vjust = 0.5, hjust = 0),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 16), # Set font size and style for x-axis label
      axis.title.y = element_text(size = 16), # Set font size and style for y-axis label
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14)
    )

  if (legend == FALSE) {
    plot <- plot +
      theme(
        legend.position = "none" # Disable the legend
      )
  }

  print(plot)

  # Create the directory if it doesn't exist
  create_directory(output_folder)

  # save the plot
  ggsave(filename = file.path(output_folder, paste0(tag_ID, "_TDR_k=", k, ".png")), plot = plot, width = plot_size[1], height = plot_size[2], dpi = dpi)

  # Message output
  cat(paste0("\n Output file: ", output_folder, "/", tag_ID, "_TDR_k=", k, ".png \n"))

  return(plot)
}
