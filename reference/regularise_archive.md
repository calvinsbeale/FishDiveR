# Put a depth record on a regular time grid, filling short gaps

`regularise_archive()` puts a depth record on a regular time grid, so
that each record is the same time apart, as the wavelet analysis
assumes. It is run by
[`import_tag_data()`](https://calvinsbeale.github.io/FishDiveR/reference/import_tag_data.md),
so imported records are already regular.

## Usage

``` r
regularise_archive(
  archive,
  interval_s = NULL,
  max_fill_mins = 5,
  min_coverage = 70,
  verbose = FALSE
)
```

## Arguments

- archive:

  A depth record: a data frame with a POSIXct 'date' column, a numeric
  'depth' column and, optionally, a 'temp' column, such as from
  [`import_tag_data()`](https://calvinsbeale.github.io/FishDiveR/reference/import_tag_data.md).
  Its 'time_zone' attribute sets the days.

- interval_s:

  The sampling interval of the grid in seconds. NULL (the default) uses
  the record's most common interval.

- max_fill_mins:

  The longest gap, in minutes, to fill by interpolation for analysis
  (default 5). At most 360 (6 hours). 0 fills no gaps.

- min_coverage:

  The percentage of a day's records that must be real (not filled) for
  the day to be analysed (default 70).

- verbose:

  Print a summary.

## Value

The record on the regular grid: a data frame with 'date', 'depth',
'temp' (if present), 'date_only', and the logical columns 'filled'
(interpolated) and 'long_gap' (filled only for the wavelet). Its
attributes are:

- time_zone, interval_s, max_fill_mins and min_coverage

- day_coverage: for each day, its real records, expected records,
  percentage coverage, and whether it is kept for analysis

- gaps: each gap's start and end (the records either side), length in
  seconds, missing records, and whether it was filled for analysis

- sampling_report: the
  [`sampling_report()`](https://calvinsbeale.github.io/FishDiveR/reference/sampling_report.md)
  of the records before they were put on the grid

## Details

The grid runs from local midnight on the first day to midnight after the
last record, every `interval_s` seconds, in step with the first record
so that real records keep their times. Each grid time takes the nearest
record within half an interval. Duplicate times, times out of order and
records sampled at other rates are resolved this way.

Grid times with no record are gaps, filled by linear interpolation
between the records either side:

- Short gaps, up to `max_fill_mins` long (from one record to the next),
  are filled and used in the analysis, with a warning.

- Longer gaps, and any time before the first or after the last record,
  are filled only so that the wavelet can run, and marked in the
  `long_gap` column.
  [`create_depth_stats()`](https://calvinsbeale.github.io/FishDiveR/reference/create_depth_stats.md)
  leaves them out, and
  [`create_wavelet_stats()`](https://calvinsbeale.github.io/FishDiveR/reference/create_wavelet_stats.md)
  leaves out the wavelet power near them.

Each day's coverage is the percentage of its grid times that have a real
record. Days with less than `min_coverage` are left out of the wavelet
and depth statistics. A day with a filled gap of 6 hours has 75%
coverage.

## See also

[`sampling_report()`](https://calvinsbeale.github.io/FishDiveR/reference/sampling_report.md)
to check a record's sampling.

## Examples

``` r
archive_days <- readRDS(system.file("extdata", "data", "archive_days.rds", package = "FishDiveR"))

# Remove a few records to make two gaps
gappy <- archive_days[-c(100:102, 5000:5300), ]
regular <- suppressWarnings(regularise_archive(gappy, max_fill_mins = 5))

attr(regular, "gaps")
#>                 start                 end length_s missing_records filled
#> 1 2000-01-01 01:38:00 2000-01-01 01:42:00      240               3   TRUE
#> 2 2000-01-04 11:18:00 2000-01-04 16:20:00    18120             301  FALSE
head(attr(regular, "day_coverage"))
#>         date records expected coverage kept
#> 1 2000-01-01    1437     1440    99.79 TRUE
#> 2 2000-01-02    1440     1440   100.00 TRUE
#> 3 2000-01-03    1440     1440   100.00 TRUE
#> 4 2000-01-04    1139     1440    79.10 TRUE
#> 5 2000-01-05    1440     1440   100.00 TRUE
#> 6 2000-01-06    1440     1440   100.00 TRUE
```
