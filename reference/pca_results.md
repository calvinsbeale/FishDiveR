# Perform Principal Component Analysis

`pca_results` performs Principal Component Analysis on the pc_data data
frame containing statistics from wavelet analysis

## Usage

``` r
pca_results(
  pc_data,
  standardise = TRUE,
  No_pcs = NULL,
  PCV = NULL,
  plot_eigenvalues = TRUE,
  output = FALSE,
  output_folder = NULL,
  verbose = FALSE,
  interactive_mode = TRUE
)
```

## Arguments

- pc_data:

  Data frame containing the output of the pca_data() function.

- standardise:

  TRUE or FALSE. Whether or not to standardise the data. Default TRUE.

- No_pcs:

  Numerical. Number of principal components to retain. Null by default

- PCV:

  Numerical. Percentage of cumulative variance to retain. Null by
  default

- plot_eigenvalues:

  TRUE or FALSE. Plot PC eigenvalues and general loadings. Default TRUE.

- output:

  Logical. If TRUE, output is saved to `output_folder`. Defaults to
  FALSE.

- output_folder:

  Output folder path. If `output = TRUE`, `output_folder` must be
  provided. Defaults to NULL.

- verbose:

  Logical. If TRUE, progress messages are shown. Defaults to FALSE.

- interactive_mode:

  TRUE or FALSE. Used for testing the package. Default FALSE.

## Value

A PCA object from 'FactoMineR' package containing the output of the
Principal Component Analysis.

## Examples

``` r
# Set file path
filepath <- system.file("extdata", package = "FishDiveR")

# Load pc_data
pc_data <- readRDS(file.path(filepath, "data/4_PCA/pc_data.rds"))


# Run a minimal, fast pca_results example
pc_results <- pca_results(
  pc_data = pc_data,
  standardise = TRUE,
  No_pcs = 1,
  PCV = NULL,
  plot_eigenvalues = FALSE,
  output = TRUE,
  output_folder = tempdir(),
  verbose = TRUE,
  interactive_mode = FALSE
)
#> Non-numerical columns being excluded: tag_ID, date_only
#> 7 principal components of 9 have eigenvalues >= 1
#> Output file: /tmp/Rtmp5l6iJ6/data/4_PCA/eigenvalues_cum_var.csv
#> Keeping 1 PC's contributing to a cumulative 47.9402033916319% of variance
#> Output file: /tmp/Rtmp5l6iJ6/data/4_PCA/pc_results.rds contains the selected number of principal components.

# \donttest{
# Full example using the complete dataset
# Run pca_results function
pc_results <- pca_results(
  pc_data = pc_data,
  standardise = TRUE,
  No_pcs = 3,
  PCV = NULL,
  plot_eigenvalues = TRUE,
  output = TRUE,
  output_folder = tempdir(),
  verbose = TRUE,
  interactive_mode = FALSE
)
#> Non-numerical columns being excluded: tag_ID, date_only
#> 7 principal components of 9 have eigenvalues >= 1
#> Output file: /tmp/Rtmp5l6iJ6/data/4_PCA/eigenvalues_cum_var.csv
#> Keeping 3 PC's contributing to a cumulative 91.691872847357% of variance
#> Output folder: /tmp/Rtmp5l6iJ6/data/4_PCA
#> Output file: /tmp/Rtmp5l6iJ6/data/4_PCA/pc_results.rds contains the selected number of principal components.
# }
```
