# Perform k selection

`select_k` creates the elbow plot and silhouette width plot for
assistance with selection of k

## Usage

``` r
select_k(
  kmeans_data,
  standardise = TRUE,
  Max.k = 15,
  v_line = NULL,
  calc_gap = FALSE,
  plot_gap = FALSE,
  output = FALSE,
  output_folder = NULL,
  verbose = FALSE
)
```

## Arguments

- kmeans_data:

  Data frame containing the combined PC scores and depth statistics to
  perform k-means on. Output from the 'combine_data()' function.

- standardise:

  TRUE or FALSE. Whether or not to standardise the data. Defaults to
  TRUE.

- Max.k:

  Numerical. Maximum value of k to try. Defaults to 15.

- v_line:

  Numerical. Option to add a vertical line to plot at a specific value
  of k. Defaults to NULL.

- calc_gap:

  TRUE or FALSE. Whether or not to calculate the gap statistic. Defaults
  to FALSE

- plot_gap:

  TRUE or FALSE. Whether or not to plot the gap statistic. Defaults to
  FALSE.

- output:

  Logical. If TRUE, output is saved to `output_folder`. Defaults to
  FALSE.

- output_folder:

  Output folder path. If `output = TRUE`, `output_folder` must be
  provided. Defaults to NULL.

- verbose:

  Logical. If TRUE, progress messages are shown. Defaults to FALSE.

## Value

A 'ggplot' class object and creates a figure containing both the
within-cluster sum of squares plot (elbow) and the average silhouette
width plot for 1 to 'Max.k' clusters.

## Details

This function relies on random initialisation in k-means clustering. For
reproducible results, users may wish to set a random seed prior to
calling this function using
[`set.seed()`](https://rdrr.io/r/base/Random.html).

## Examples

``` r
# Set file path
filepath <- system.file("extdata", package = "FishDiveR")

# Load kmeans_data
kmeans_data <- readRDS(file.path(filepath, "data/5_k-means/combined_stats.rds"))

# Run select_k function
selecting_k <- select_k(
  kmeans_data = kmeans_data,
  standardise = TRUE,
  Max.k = 8,
  v_line = 4,
  calc_gap = FALSE,
  plot_gap = FALSE,
  output = TRUE,
  output_folder = tempdir(),
  verbose = TRUE
)
#> Standardising k-means input.
#> K = 2 Average silhouette width = 0.499
#> K = 3 Average silhouette width = 0.586
#> K = 4 Average silhouette width = 0.642
#> K = 5 Average silhouette width = 0.518
#> K = 6 Average silhouette width = 0.392
#> K = 7 Average silhouette width = 0.243
#> K = 8 Average silhouette width = 0.154
#> Single tag output file: /tmp/Rtmp5l6iJ6/data/5_k-means/Select_k.3_PCs.png
```
