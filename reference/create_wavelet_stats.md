# create_wavelet_stats

`create_wavelet_stats` aggregates the wavelet variables over the
specified time periods

## Usage

``` r
create_wavelet_stats(
  wavelet,
  tag_ID,
  output = FALSE,
  output_folder = NULL,
  verbose = FALSE
)
```

## Arguments

- wavelet:

  An object of class "analyze.wavelet" from package 'WaveletComp'

- tag_ID:

  Unique tag identification number in a vector of characters. E.g.
  "123456"

- output:

  Logical. If TRUE, output is saved to `output_folder`. Defaults to
  FALSE.

- output_folder:

  Output folder path. If `output = TRUE`, `output_folder` must be
  provided. Defaults to NULL.

- verbose:

  Logical. If TRUE, progress messages are shown. Defaults to FALSE.

## Value

A data frame containing the seven wavelet statistics for each period.
One observation is available per period per day:

- Amplitude_mean

- Amplitude_variance

- Mean_sq_power

- Power_mean

- Power_variance

- Phase_mean

- Phase_variance

## Examples

``` r
# Set file path
filepath <- system.file("extdata", package = "FishDiveR")

# Load my.w wavelet object
my.w <- readRDS(file.path(filepath, "data/1_Wavelets/data_wavelet.rds"))

# Run create_wavelet_stats function on wavelet object
waveStats <- create_wavelet_stats(
  wavelet = my.w,
  tag_ID = "data",
  output = TRUE,
  output_folder = tempdir(),
  verbose = TRUE
)
#> Running create_wavelet_stats() on tag ID data
#> 
#> Output file: /tmp/Rtmpxgg77y/data/3_Stats/data_waveStats.csv 
```
