# Plot the time-series depth records of the selected tag. Colour days by cluster

`plot_cluster_TDR` plots the time-series depth record of the selected
archival tag. Each day of data is coloured by the assigned cluster, this
helps to visualise changes in vertical movement behaviour over time.

## Usage

``` r
plot_cluster_TDR(
  tag_ID,
  data_folder = NULL,
  kmeans_result,
  every_nth = 10,
  every_s = 0,
  X_lim = NULL,
  Y_lim = c(0, 250, 50),
  date_breaks = "14 day",
  legend = TRUE,
  plot_size = c(12, 6),
  dpi = 300,
  output = FALSE,
  output_folder = NULL,
  verbose = FALSE,
  minmax_bins = 0
)
```

## Arguments

- tag_ID:

  Unique tag identification number in a vector of characters. E.g.
  "123456".

- data_folder:

  Parent folder path with separate folders for each tag data. E.g.
  "C:/Tag data". Defaults to 'data_dir'

- kmeans_result:

  An object of class 'kmeans' containing the k-means clustering data.
  Output of 'k_clustering()' function.

- every_nth:

  Numerical. Optional down-sampling of data points to plot. Defaults to
  10, plotting every 10th record.

- every_s:

  Numerical. Alternative to every_nth. Optional down-sampling of data
  points to plot by number of seconds, as opposed to records. E.g. plots
  every 60th second, rather than 10th row of data. Must be a multiple of
  the sampling frequency. Overrides every_nth if != 0.

- X_lim:

  Optional. Vector with two dates delimiting the time-depth record to
  plot. E.g. c("2000-01-01", "2000-11-23")

- Y_lim:

  Character vector with minimum depth, maximum depth, and sequence for
  ticks on Y-axis. Must be numeric, positive and 3 elements long. E.g.
  c(0,1500,100).

- date_breaks:

  X-axis ggplot2 date breaks. E.g, "24 hour, "3 day", "2 week".

- legend:

  TRUE or FALSE. Whether or not to plot the figure legend. Defaults to
  TRUE.

- plot_size:

  ggSave height and width for saving the output plot. Must be numeric,
  positive and 2 elements long. Default to 'c(12,6)'

- dpi:

  Numerical. DPI to use for 'ggsave()' output. E.g, 600

- output:

  Logical. If TRUE, output is saved to `output_folder`. Defaults to
  FALSE.

- output_folder:

  Output folder path. If `output = TRUE`, `output_folder` must be
  provided. Defaults to NULL.

- verbose:

  Logical. If TRUE, progress messages are shown. Defaults to FALSE.

- minmax_bins:

  Numerical. Alternative to every_nth and every_s for fast plotting of
  large archives. Splits the record into this many equal time bins and
  plots only the shallowest and deepest record in each, so dive extremes
  are kept. E.g. 2000. Overrides every_nth and every_s if != 0. Defaults
  to 0.

## Value

Returns the cluster TDR plot. Additionally prints to file the TDR plot.
Additionally outputs a facet plot of all tag_IDs.

## Examples

``` r
# Set file path
filepath <- system.file("extdata", package = "FishDiveR")

# Load kmeans_result
kmeans_result <- readRDS(file.path(filepath, "data/5_k-means/kmeans_result.rds"))

# Run plot_clusters function
plot_cluster_TDR(
  tag_ID = "data",
  data_folder = filepath,
  kmeans_result = kmeans_result,
  every_nth = 10,
  every_s = 0,
  X_lim = NULL,
  Y_lim = c(0, 300, 50),
  date_breaks = "1 day",
  legend = TRUE,
  plot_size = c(12, 6),
  dpi = 100,
  output = TRUE,
  output_folder = tempdir(),
  verbose = TRUE
)
#> Loading tag data TDR
#> Single tag k-means loaded
#> Maximum depth is 262.2
#> Data sampling interval is 60 seconds
#> Plotting every 10 records
#> Output file: /tmp/Rtmp5l6iJ6/data_TDR_k=4.png

```
