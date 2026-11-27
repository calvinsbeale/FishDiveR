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
  verbose = FALSE,
  mask_ends = FALSE,
  min_coverage = NULL
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

- mask_ends:

  Logical. If TRUE, also leave out the wavelet power near the start and
  end of the record, as near a long gap. Defaults to FALSE.

- min_coverage:

  The percentage of a day's records, and of its wavelet power at every
  period, that must be valid for the day to be analysed. NULL (the
  default) uses the archive's setting from
  [`regularise_archive()`](https://calvinsbeale.github.io/FishDiveR/reference/regularise_archive.md),
  or 70.

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

## Details

Each day is the wavelet's records on that local date. Days with less
than `min_coverage` percent real records (see
[`regularise_archive()`](https://calvinsbeale.github.io/FishDiveR/reference/regularise_archive.md))
are left out.

Wavelet power near a gap too long to fill, where the wavelet ran over
filled values, is not valid. For each period, the power within the
e-folding time of its scale (the square root of 2 times the scale, about
1.35 times the period) either side of the gap is left out of the day's
statistics (Torrence & Compo 1998). With `mask_ends = TRUE`, the power
this close to the start and end of the record is left out too. A day is
left out if less than `min_coverage` percent of its power is valid at
any period, e.g. within about 1.4 days of a gap for a 24 hour upper
period.

The days left out, and why, are in the attribute 'dropped_days'.

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
#> Output file: /tmp/RtmpQAjQkT/data/3_Stats/data_waveStats.csv 
```
