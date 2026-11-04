# Thin a depth record to a longer sampling interval

`thin_archive` averages a processed depth record into blocks of
`interval_s` seconds, e.g. from 1 second to 15 second sampling. Use it
on the archive passed to
[`create_wavelet()`](https://calvinsbeale.github.io/FishDiveR/reference/create_wavelet.md)
to reduce the memory and time the wavelet analysis needs for long,
high-frequency records.

## Usage

``` r
thin_archive(archive, interval_s, verbose = FALSE)
```

## Arguments

- archive:

  Data frame of processed time series depth data from
  [`import_tag_data()`](https://calvinsbeale.github.io/FishDiveR/reference/import_tag_data.md).

- interval_s:

  Numerical. New sampling interval in seconds. Must be a multiple of the
  current sampling interval and divide a day evenly, e.g. 5, 10, 15, 30
  or 60.

- verbose:

  Logical. If TRUE, progress messages are shown. Defaults to FALSE.

## Value

The thinned archive, with the same columns and 'time_zone' attribute.
Returned unchanged if `interval_s` equals the current sampling interval.

## Details

Each block of `interval_s` seconds becomes one record, timed at the
start of the block, with the mean depth (and mean temperature, if
present) of the block. Averaging, rather than keeping every nth record,
stops movements faster than the new interval appearing as false longer
cycles (aliasing).

Thinning only removes cycles shorter than about twice the new interval.
As a rule of thumb, keep at least 20 records per cycle of the wavelet's
lower period, i.e. `interval_s <= lower_period_mins * 60 / 20`: up to 15
seconds for a 5 minute lower period.

Keep using the full record for
[`create_depth_stats()`](https://calvinsbeale.github.io/FishDiveR/reference/create_depth_stats.md),
as averaging slightly reduces maximum depths. Tags clustered together
should all be thinned to the same interval.

## Examples

``` r
# Set file path
filepath <- system.file("extdata", package = "FishDiveR")

# Load archive_days, sampled every 60 seconds
archive_days <- readRDS(file.path(filepath, "data/archive_days.rds"))

# Thin to 5 minute sampling
archive_5min <- thin_archive(archive_days, interval_s = 300, verbose = TRUE)
#> Thinned from 60 to 300 second sampling: 14,400 to 2,880 records
```
