#' Plot the time-series depth records of the days closest to the centre of each
#' cluster
#'
#' `plot_clusters` plots the time-depth records of the days closest to the
#' centre of each of the clusters. Each cluster is plotted both individually,
#' and faceted together, with both a fixed y-axis and a free y-axis (depth).
#'
#' @name plot_clusters
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom patchwork wrap_plots
#' @importFrom utils head
#' @importFrom data.table uniqueN
#' @importFrom colorspace qualitative_hcl
#'
#' @inheritParams pca_data
#' @inheritParams plot_cluster_TDR
#' @param No_days Numerical. Number of days of each cluster to plot. Defaults to
#'   1.
#' @param color TRUE or FALSE. Output clusters coloured by cluster assignment.
#'   Defaults to TRUE.
#' @param diel_shade TRUE or FALSE. Output plot with night-time shading. Can
#'   be slow! Defaults to FALSE.
#'
#' @returns A plot list of all plots created of each cluster in the data. Prints
#'   to file one figure for each Cluster with a fixed y-axis. Additionally
#'   outputs a facet plot of all clusters, and a free y-axis version of all
#'   plots.
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
#' plot_clusters(
#'   tag_vector = "data",
#'   data_folder = filepath,
#'   kmeans_result = kmeans_result,
#'   No_days = 3,
#'   every_nth = 10,
#'   every_s = 0,
#'   Y_lim = c(0, 300, 50),
#'   color = TRUE,
#'   diel_shade = FALSE,
#'   dpi = 100,
#'   output_folder = tempdir()
#' )
#'
# Function to prints to file one figure for each Cluster with a fixed y-axis. Additionally outputs a facet plot of all clusters, and a free y-axis version of all plots.
plot_clusters <- function(tag_vector = tag_list,
                          data_folder = data_dir,
                          kmeans_result,
                          No_days = 1,
                          every_nth = 10,
                          every_s = 0,
                          Y_lim = c(0, 250, 50),
                          color = TRUE,
                          diel_shade = FALSE,
                          dpi = 300,
                          output_folder = data_dir) {
  # Check if tag_vector is a character vector
  if (!is.character(tag_vector)) {
    stop("tag_vector should be a vector of characters. Check input.")
  }
  if (!is.list(kmeans_result)) {
    stop("kmeans_result must be a data frame. \n")
  }
  if ((!is.numeric(No_days) || No_days <= 0)) {
    stop("No_days must be a positive integer.")
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
  if (!is.logical(color)) {
    stop("color must be TRUE or FALSE.")
  }
  if (!is.logical(diel_shade)) {
    stop("color must be TRUE or FALSE.")
  }
  if ((!is.numeric(dpi) || dpi <= 0)) {
    stop("dpi must be a positive integer.")
  }

  # Check k-means and tag vector match
  unique_tags <- unique(kmeans_result$tag_ID)

  # Check if all elements of tag_vector are present in unique_tags
  tags_match <- all(tag_vector %in% unique_tags)

  # Check if all elements of unique_tags are present in tag_vector
  tags_match_reverse <- all(unique_tags %in% tag_vector)

  # Print the results of the checks
  if ((tags_match & tags_match_reverse) == FALSE) {
    # Identify any tags that are in tag_vector but not in unique_tags
    missing_from_kmeans <- setdiff(tag_vector, unique_tags)
    if (length(missing_from_kmeans) > 0) {
      cat("Tags in tag_vector but not in kmeans_result: ", missing_from_kmeans, "\n")
    }

    # Identify any tags that are in unique_tags but not in tag_vector
    missing_from_tag_vector <- setdiff(unique_tags, tag_vector)
    if (length(missing_from_tag_vector) > 0) {
      cat("Tags in kmeans_result but not in tag_vector: ", missing_from_tag_vector, "\n")
    }

    stop("There is a mismatch between tag_vector and the tags in kmeans_result.\n")
  }

  # Initialize an empty list for storing data frames from each tag
  data_list <- list()

  # Initialize an empty data frame for storing unique dates and tag IDs
  id_data <- data.frame(date_only = as.Date(character()), tag_ID = character(), stringsAsFactors = FALSE)

  # Load the archives of the tags in 'tag_vector'
  if (length(tag_vector) > 1) { # Multiple tags

    # tag <- tag_vector[1]
    for (tag in tag_vector) {
      # Load the archive data for each tag
      archive_days <- readRDS(file.path(data_folder, tag, "archive_days.rds"))
      archive_days$tag_ID <- tag # Add tag_ID column

      # Print maximum depth
      cat(paste0("\nTag ", tag, " Maximum depth is ", max(archive_days$depth)))

      # Calculate the time differences between consecutive records
      time_diffs <- diff(as.numeric(archive_days$date, units = "secs"))

      # Check if the original sampling frequency is consistent
      sampling_interval <- unique(time_diffs)

      if (length(sampling_interval) != 1) {
        stop("Error: The original data does not have a consistent sampling frequency.")
      }

      # Print sampling interval
      cat(paste0("\nData sampling interval is ", sampling_interval, " seconds\n"))

      if (every_s != 0) {
        # Using time, rather than number of rows to plot data.

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

      # Fix the time zone to UTC before adding to list to prevent sunrise and sunset changing
      archive_days$sunrise <- lubridate::force_tz(archive_days$sunrise, "UTC")
      archive_days$sunset <- lubridate::force_tz(archive_days$sunset, "UTC")

      # Append to the list
      data_list[[tag]] <- archive_days

      # Extract unique dates for the current tag and create a temp dataframe
      unique_dates <- unique(archive_days$date_only)
      temp_id_data <- data.frame(tag_ID = tag, date_only = unique_dates, stringsAsFactors = FALSE)

      # Combine with the id_data dataframe
      id_data <- rbind(id_data, temp_id_data)

      rm(archive_days, sampling_interval, unique_dates, temp_id_data)
    }

    # Combine all tag data frames into one after processing each tag. Now re-sampled at every_nth data points
    all_archive_ts <- do.call(rbind, data_list)
  } else { # Single tag

    # Load the archive data for the single tag
    archive_days <- readRDS(file.path(data_folder, tag_vector, "archive_days.rds"))
    archive_days$tag_ID <- tag_vector # Add tag_ID column

    cat(paste0("\nMaximum depth is ", max(archive_days$depth)))

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

    # Directly assign the unique dates and tag ID for the single tag
    unique_dates <- unique(archive_days$date_only)
    id_data <- data.frame(date_only = unique_dates, tag_ID = tag_vector, stringsAsFactors = FALSE)

    # Standardise the output dataframe name for single / multiple tags
    all_archive_ts <- archive_days
  }

  # Calculate distances between each point (day) and corresponding cluster centroid:
  distances <- kmeans_result$distances

  # Set all_archive_ts as a data.table
  data.table::setDT(all_archive_ts)

  # Count unique date_only values for each tag_ID
  unique_dates_by_tag <- all_archive_ts[, list(unique_dates = data.table::uniqueN(date_only)), by = tag_ID]

  # Sum all the unique counts across all tag IDs
  total_unique_dates <- sum(unique_dates_by_tag$unique_dates)

  if (total_unique_dates != length(kmeans_result$cluster)) {
    cat("\n Data length and number of clusters do not match. Cluster assignment may be incorrect. This error should only be ignored for readme data. \n")

    # Get length of all_archive_ts
    l <- length(unique(all_archive_ts$date_only))

    # Assign cut cluster list
    kmeans_result$cluster <- kmeans_result$cluster[1:l]

    # Cut distances to match available data length
    distances <- distances[1:l]
  }

  # Add all cluster assignment to id_data
  id_data$cluster <- kmeans_result$cluster

  # Set k as the number of unique clusters
  k <- length(unique(kmeans_result$cluster))

  # Create a data frame with cluster assignments and distances from cluster centroid
  cluster_distances <- data.frame(cluster = id_data$cluster, distance = distances, date_only = id_data$date_only)

  # Sort the data frame by cluster and distance
  sorted_cluster_distances <- cluster_distances[order(cluster_distances$cluster, cluster_distances$distance), ]

  # Create a data frame to hold values
  best_k_representatives <- data.frame()

  # Find the 'No_days' days closest to the cluster centroid for each cluster (i)
  for (i in 1:k) {
    # Subset data for the current cluster 'i' and the 'No_days' required
    cluster_representatives <- head(sorted_cluster_distances[sorted_cluster_distances$cluster == i, ], No_days)

    # Add row column, representing the row number in cluster_distances
    cluster_representatives$row <- as.numeric(rownames(cluster_representatives))

    # Add the date_only, tag_ID, and cluster columns
    cluster_representatives$date <- id_data[cluster_representatives$row, "date_only"]
    cluster_representatives$tag_ID <- id_data[cluster_representatives$row, "tag_ID"]

    best_k_representatives <- rbind(best_k_representatives, cluster_representatives)

    # Check if the required number of days exist
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
    save_folder <- file.path(output_folder, "Combined_tags", folder)
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

    # j <- 1
    # Iterate over the rows of 'dates' and extract the matching rows from 'all_archive_ts' using date and tag_ID (duplicated dates in tag_ID's)
    for (j in 1:nrow(dates)) {
      # For each day in dates where date and tag_ID match the cluster, create plot_data
      plot_data <- all_archive_ts[all_archive_ts$date_only == dates$date[j] & all_archive_ts$tag_ID == dates$tag_ID[j], ]

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
      to = as.POSIXct("1970-01-01 18:00:00"),
      by = "6 hours"
    )

    # Plot the base TDR (in distance from cluster centre order)
    cluster_plot <-
      ggplot(data = plot_data, aes(x = time, y = depth)) +
      geom_line(colour = custom_palette[i], linewidth = 1) +
      scale_x_datetime(breaks = time_breaks, date_labels = "%H %M", position = "top", expand = c(0.001, 0, 0, 0)) +
      labs(x = "Time of day", y = "Depth (meters)") +
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
            xmax = as.POSIXct("1970-01-01 24:00:00"),
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
      scale_y_reverse(limits = limits, expand = c(0, 0))

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

  cat(paste0("Output folder: ", save_folder, "\n"))

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

  print(facet_plot_k_freey)

  return(plots_list)
}
