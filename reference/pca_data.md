# Prepare all data for Principal Component Analysis

`pca_data` loads the wavelet statistics for each of the tags listed in
'tag_vector'. Performs various checks to ensure compatibility of
wavelets, and combines them into a data frame containing only the chosen
statistics.

## Usage

``` r
pca_data(
  tag_vector,
  data_folder = data_dir,
  phase_mean = FALSE,
  phase_variance = FALSE,
  power_mean = TRUE,
  power_variance = TRUE,
  mean_sq_power = FALSE,
  amplitude_mean = TRUE,
  amplitude_variance = FALSE,
  output = FALSE,
  output_folder = NULL,
  verbose = FALSE
)
```

## Arguments

- tag_vector:

  A character vector of tag IDs. E.g. 'c("123456", "456283", "AB98XJ").

- data_folder:

  Parent folder path with separate folders for each tag data. E.g.
  "C:/Tag data". Defaults to 'data_dir'

- phase_mean:

  TRUE or FALSE to include this wavelet statistic. Default FALSE

- phase_variance:

  TRUE or FALSE to include this wavelet statistic. Default FALSE

- power_mean:

  TRUE or FALSE to include this wavelet statistic. Default TRUE

- power_variance:

  TRUE or FALSE to include this wavelet statistic. Default TRUE

- mean_sq_power:

  TRUE or FALSE to include this wavelet statistic. Default FALSE

- amplitude_mean:

  TRUE or FALSE to include this wavelet statistic. Default TRUE

- amplitude_variance:

  TRUE or FALSE to include this wavelet statistic. Default FALSE

- output:

  Logical. If TRUE, output is saved to `output_folder`. Defaults to
  FALSE.

- output_folder:

  Output folder path. If `output = TRUE`, `output_folder` must be
  provided. Defaults to NULL.

- verbose:

  Logical. If TRUE, progress messages are shown. Defaults to FALSE.

## Value

A data frame with the combined data for all tag ID's listed, containing
the wavelet statistics to be used in Principal Component Analysis.

## Examples

``` r
# Set file path
filepath <- system.file("extdata", package = "FishDiveR")

# Run pca_data function
pc_data <- pca_data(
  tag_vector = c("data"),
  data_folder = filepath,
  phase_mean = FALSE,
  phase_variance = FALSE,
  power_mean = TRUE,
  power_variance = TRUE,
  mean_sq_power = FALSE,
  amplitude_mean = TRUE,
  amplitude_variance = FALSE,
  output = TRUE,
  output_folder = tempdir(),
  verbose = TRUE
)
#> Output file: /tmp/RtmpTKWEE7/data/4_PCA/pc_data.rds
```
