# Create and plot the wavelet power spectrum

`create_wavelet` creates the a wavelet spectrum using WaveletComp
package. Optionally loads and plots an existing my.w object.

## Usage

``` r
create_wavelet(
  archive,
  tag_ID,
  wv_period_hours = 24,
  sampling_frequency = NULL,
  load_existing_wavelet = FALSE,
  suboctaves = 12,
  lower_period_mins = 5,
  upper_period_hours = 24,
  pval = FALSE,
  output = FALSE,
  output_folder = NULL,
  verbose = FALSE,
  plot_wavelet = TRUE,
  max_period_ticks = 10,
  plot_width = 800,
  plot_height = 400,
  interactive_mode = TRUE
)
```

## Arguments

- archive:

  Data frame containing processed time series depth data

- tag_ID:

  Unique tag identification number in a vector of characters. E.g.
  "123456"

- wv_period_hours:

  Time resolution in hours to calculate wavelet. Currently only supports
  the default of 24 hours as this package is created to investigate
  daily diving behaviour. Defaults to 24.

- sampling_frequency:

  Sampling frequency of depth data in seconds. Defaults to time between
  first and second depth record. Recommended to leave blank.

- load_existing_wavelet:

  Load an existing my.w wavelet object from the output_folder. Defaults
  to FALSE.

- suboctaves:

  number of suboctaves between each logarithmic period. E.g. between 24
  and 12 hours. Highly recommended to use 12, for easy of interpretation
  of hours and signal present (daily, diel, tidal).

- lower_period_mins:

  Lower period of the wavelet sampling in minutes. Cannot be less than
  sampling frequency. Defaults to 5 minutes.

- upper_period_hours:

  Upper period of the wavelet sampling in days. Defaults to 24 hours.

- pval:

  Produce p-values or not. True or False. Default set to FALSE, see
  [`WaveletComp::analyze.wavelet()`](https://rdrr.io/pkg/WaveletComp/man/analyze.wavelet.html)
  for further details. P-values not used in further analysis, and
  increase computation time and file size.

- output:

  Logical. If TRUE, output is saved to `output_folder`. Defaults to
  FALSE.

- output_folder:

  Output folder path. If `output = TRUE`, `output_folder` must be
  provided. Defaults to NULL.

- verbose:

  Logical. If TRUE, progress messages are shown. Defaults to FALSE.

- plot_wavelet:

  TRUE or FALSE. Plot the wavelet spectrum and mean power?

- max_period_ticks:

  Number of ticks displayed on the period (y) axis in plots.

- plot_width:

  Width of the wavelet spectrum plot output. Defaults to 800.

- plot_height:

  Height of the wavelet spectrum plot output. Defaults to 400.

- interactive_mode:

  Used for testing the package only. Defaults to TRUE.

## Value

When output = TRUE, returns an object of class "analyze.wavelet" from
package 'WaveletComp'. Additionally outputs a plot of the wavelet
spectrum, and a plot of the mean power per period.

## Details

Uses
[`WaveletComp::analyze.wavelet()`](https://rdrr.io/pkg/WaveletComp/man/analyze.wavelet.html)
to create a univariate wavelet power spectrum for the depth data
imported, see
[`WaveletComp::analyze.wavelet()`](https://rdrr.io/pkg/WaveletComp/man/analyze.wavelet.html)
for more details. Plots mean wavelet power using
[`WaveletComp::wt.avg()`](https://rdrr.io/pkg/WaveletComp/man/wt.avg.html).
If you have errors allocating large vectors try using library(bigmemory)
and create a big matrix with big_mat \<- big.matrix(nrow = 1e7, ncol =
10, type = "double") then run your code again. This allows greater range
between lower and upper periods

## Examples

``` r
# Set file path
filepath <- system.file("extdata", package = "FishDiveR")

# Load archive_days
archive_days <- readRDS(file.path(filepath, "data/archive_days.rds"))

# Run create_wavelet function
my.w <- create_wavelet(
  archive = archive_days,
  tag_ID = "data",
  wv_period_hours = 24,
  sampling_frequency = NULL,
  load_existing_wavelet = FALSE,
  suboctaves = 12,
  lower_period_mins = 30,
  upper_period_hours = 24,
  pval = FALSE,
  output = TRUE,
  output_folder = tempdir(),
  verbose = TRUE,
  plot_wavelet = FALSE,
  max_period_ticks = 10,
  plot_width = 800,
  plot_height = 400,
  interactive_mode = FALSE
)
#> 
#> Analysing tag ID data
#> 
#> Creating new wavelet.
#> Starting wavelet transformation...
#> Class attributes are accessible through following names:
#> series loess.span dt dj Wave Phase Ampl Power Power.avg Power.pval Power.avg.pval Ridge Period Scale nc nr coi.1 coi.2 axis.1 axis.2 date.format date.tz 
#> 
#> Wavelet saved to /tmp/Rtmpcux66d/data/1_Wavelets/data_wavelet.rds
```
