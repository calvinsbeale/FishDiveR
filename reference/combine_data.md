# Import depth statistics and combine with PC scores

This function imports the depth statistics from each of the tags listed
in tag_vector, and outputs a combined data frame then combines the depth
statistics from each tag with the principal component scores, and
outputs a data frame with the appropriate unique_tag_ID if necessary,
ready for use in k-means clustering.

## Usage

``` r
combine_data(
  tag_vector = tag_list,
  data_folder = NULL,
  pc_scores = scores,
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

- pc_scores:

  Data frame of principal component scores extracted through PCA on
  wavelet statistics. Output of 'pca_scores()' function.

- output:

  Logical. If TRUE, output is saved to `output_folder`. Defaults to
  FALSE.

- output_folder:

  Output folder path. If `output = TRUE`, `output_folder` must be
  provided. Defaults to NULL.

- verbose:

  Logical. If TRUE, progress messages are shown. Defaults to FALSE.

## Value

A data frame containing the combined depth statistics and principal
component scores from each of the tags listed in tag_vector

## Examples

``` r
# Set file path
filepath <- system.file("extdata", package = "FishDiveR")

# Load pc_results
pc_scores <- readRDS(file.path(filepath, "data/4_PCA/pc_scores.rds"))

# Run combine_data function
combined_stats <- combine_data(
  tag_vector = "data",
  data_folder = filepath,
  pc_scores = pc_scores,
  output = TRUE,
  output_folder = tempdir(),
  verbose = TRUE
)
#> Saving combined metrics to:/tmp/RtmpnmzaAr/data/5_k-means/combined_stats.rds
```
