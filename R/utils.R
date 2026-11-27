# Cluster of each of a tag's days in a k-means result from k_clustering(). By
# date, if the result has the days' dates (NA for days not clustered, e.g. left
# out for gaps); for older results without dates, in day order.
cluster_of_days <- function(kmeans_result, tag_ID, dates, verbose = FALSE) {
  rows <- which(as.character(kmeans_result$tag_ID) == tag_ID)
  clusters <- kmeans_result$cluster[rows]
  if (!is.null(kmeans_result$date_only)) {
    return(unname(clusters[match(as.Date(dates), as.Date(kmeans_result$date_only[rows]))]))
  }
  if (length(clusters) != length(dates) && verbose) {
    message(
      "The k-means result has ", length(clusters), " days for tag ", tag_ID, " but its archive has ", length(dates),
      ", and no dates to match them by. Run k_clustering() again to match days by date."
    )
  }
  unname(clusters[seq_along(dates)])
}

# Seconds since local midnight of each record ('day_secs'), and of its day's
# sunrise and sunset if present, to plot days on one time axis. Days with a
# clock change run 23 or 25 hours, rather than repeating or skipping an hour
# of clock time. Records filled in long gaps (see regularise_archive()) get no
# depth, so lines break there.
add_day_times <- function(archive, time_zone) {
  if (is.null(time_zone) || !nzchar(time_zone)) time_zone <- "UTC"
  midnight <- as.numeric(lubridate::floor_date(lubridate::with_tz(archive$date, time_zone), "day"))
  archive$day_secs <- as.numeric(archive$date) - midnight
  next_midnight <- as.numeric(lubridate::ceiling_date(
    lubridate::with_tz(archive$date, time_zone), "day", change_on_boundary = TRUE
  ))
  archive$day_length <- next_midnight - midnight
  if (all(c("sunrise", "sunset") %in% names(archive))) {
    archive$sunrise_secs <- as.numeric(archive$sunrise) - midnight
    archive$sunset_secs <- as.numeric(archive$sunset) - midnight
  }
  if ("long_gap" %in% names(archive)) {
    archive$depth[as.logical(archive$long_gap)] <- NA
  }
  archive
}

# Rows to keep to plot a depth record quickly: the shallowest and deepest record
# in each of 'bins' equal time bins, so dive extremes are kept. Used by
# plot_TDR() and plot_cluster_TDR().
minmax_rows <- function(date, depth, bins) {
  rows <- split(seq_along(depth), cut(as.numeric(date), breaks = bins, labels = FALSE))
  keep <- c(
    vapply(rows, function(i) i[which.min(depth[i])], integer(1)),
    vapply(rows, function(i) i[which.max(depth[i])], integer(1))
  )
  sort(unique(keep))
}

# Utility function for creating directory if it doesn't exist. Used in all 10 .R files
create_directory <- function(folder) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
}

# Colour scale for temperature, from blue (cold) to red (warm). Used by
# plot_TDR() and plot_time_at().
temperature_scale <- function(aesthetics = "colour", ...) {
  ggplot2::scale_colour_distiller(palette = "RdYlBu", aesthetics = aesthetics, ...)
}

# Axis and legend title for temperature, with a degree sign
temperature_label <- paste0("Temperature (", intToUtf8(176), "C)")
