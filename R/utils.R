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
