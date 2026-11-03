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
