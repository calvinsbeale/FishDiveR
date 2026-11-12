# Calculate Principal Component Analysis Scores not including depth statistics

This function extracts the PCA scores from the PCA results and plots the
loadings. This function is to be use on output from the
[`pca_data()`](https://calvinsbeale.github.io/FishDiveR/reference/pca_data.md)
function not including depth statistics.

## Usage

``` r
pca_scores(
  pc_results = results,
  plot_loadings = TRUE,
  every_nth = 12,
  output = FALSE,
  output_folder = NULL,
  verbose = FALSE
)
```

## Arguments

- pc_results:

  PCA class object containing the output from the 'pca_results()'
  function.

- plot_loadings:

  TRUE or FALSE. Plot PC loadings figures. Default TRUE.

- every_nth:

  Numeric. Sequence of labels to show on mean power plot. Default is 12.

- output:

  Logical. If TRUE, output is saved to `output_folder`. Defaults to
  FALSE.

- output_folder:

  Output folder path. If `output = TRUE`, `output_folder` must be
  provided. Defaults to NULL.

- verbose:

  Logical. If TRUE, progress messages are shown. Defaults to FALSE.

## Value

A data frame of pc scores containing one column for each Principal
Component kept. If processing just one tag, the attribute
'unique_tag_ID' is given to the data frame with the tag_ID. Plots the PC
loadings for each row of pc_data

## Examples

``` r
# Set file path
filepath <- system.file("extdata", package = "FishDiveR")

# Load pc_results
pc_results <- readRDS(file.path(filepath, "data/4_PCA/pc_results.rds"))

# Run pca_scores function
pc_scores <- pca_scores(
  pc_results = pc_results,
  plot_loadings = FALSE,
  every_nth = 12,
  output = TRUE,
  output_folder = tempdir(),
  verbose = TRUE
)
#> Output file: /tmp/RtmpnmzaAr/data/4_PCA/pc_scores.rds
```
