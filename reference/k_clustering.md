# Perform k-means

`k_clustering` performs k-means clustering on the PC scores with the
selected value of k

## Usage

``` r
k_clustering(
  kmeans_data,
  standardise = TRUE,
  k,
  nstart = 50,
  polygon = FALSE,
  output = TRUE,
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

- k:

  Numerical. Value of k to use for analysis.

- nstart:

  Numerical. Value of nstart for k-means analysis.

- polygon:

  TRUE or FALSE. Plot polygons for cluster with more than 3 data points.
  Defaults to FALSE.

- output:

  TRUE or FALSE. Whether or not to output the results. Defaults to TRUE.

- output_folder:

  Output folder path. If `output = TRUE`, `output_folder` must be
  provided. Defaults to NULL.

- verbose:

  Logical. If TRUE, progress messages are shown. Defaults to FALSE.

## Value

An object of class 'kmeans' containing the k-means clustering data for
the data frame. Additionally plots a 3D cluster plot of the top three
Principal Components.

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

# \donttest{
# Full example using the complete dataset.
# Set output to TRUE for real use!

kmeans_result <- k_clustering(
  kmeans_data = kmeans_data,
  standardise = TRUE,
  k = 4,
  nstart = 50,
  polygon = FALSE,
  output = FALSE,
  output_folder = tempdir(),
  verbose = TRUE
)
#> Standardising k-means input.
#> Total of 10 days of data
# }
```
