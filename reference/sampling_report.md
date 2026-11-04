# Report irregular sampling in a depth record

`sampling_report` checks that a depth record has a constant sampling
interval, and describes any irregularities: duplicate times, gaps,
changes of sampling rate, intervals shorter than expected, times out of
order, and days with fewer records than expected.

## Usage

``` r
sampling_report(archive, interval_s = NULL, min_run = 10)

# S3 method for class 'sampling_report'
print(x, max_rows = 5, ...)
```

## Arguments

- archive:

  Data frame of time series depth data with a POSIXct 'date' column,
  e.g. from
  [`import_tag_data()`](https://calvinsbeale.github.io/FishDiveR/reference/import_tag_data.md).
  If it has a 'depth' column, records with no depth are left out.

- interval_s:

  Numerical. Expected sampling interval in seconds. Defaults to the most
  common interval.

- min_run:

  Numerical. Minimum number of consecutive intervals of the same length
  to count as a change of sampling rate. Defaults to 10.

- x:

  A 'sampling_report' object.

- max_rows:

  Numerical. Maximum number of each kind of irregularity, and of
  incomplete days, to list when printing. Defaults to 5.

- ...:

  Not used.

## Value

A list of class 'sampling_report', printed as a summary, with:

- interval_s:

  The expected sampling interval in seconds.

- regular:

  TRUE if every interval equals `interval_s`.

- n_records:

  Number of records checked.

- n_no_depth:

  Number of records left out for having no depth.

- time_zone:

  Time zone used for days.

- intervals:

  Each interval length, with its number and percentage of intervals.

- duplicates, out_of_order, rate_changes, gaps, short:

  One row per irregularity, as described in Details.

- days:

  Records, expected records and percent complete for each day.

## Details

The analysis steps of FishDiveR assume a constant sampling interval. Use
this function on the output of
[`import_tag_data()`](https://calvinsbeale.github.io/FishDiveR/reference/import_tag_data.md),
which warns when the interval is not constant, to see where and what the
irregularities are.

Each interval between consecutive records is compared with the expected
interval, by default the most common one:

- **Duplicates**: two records with the same time.

- **Out of order**: a record timed before the previous record.

- **Rate changes**: at least `min_run` consecutive intervals of the
  same, different length, e.g. a tag switching from 5 to 75 second
  sampling.

- **Gaps**: other intervals longer than expected, i.e. missing records.
  `on_grid` is FALSE if the gap is not a whole number of expected
  intervals.

- **Short intervals**: other intervals shorter than expected, e.g. from
  timing errors.

Days are counted in the record's time zone, from the 'date_only' column
if present. A day's `percent` compares its number of records with the
number expected at the expected interval.

## Examples

``` r
# Set file path
filepath <- system.file("extdata", package = "FishDiveR")

# Load archive_days, sampled every 60 seconds
archive_days <- readRDS(file.path(filepath, "data/archive_days.rds"))

# A regular record
sampling_report(archive_days)
#> Sampling report: 14,400 records over 10 days (2000-01-01 to 2000-01-10, UTC)
#> Expected interval: 60 s (100% of intervals)
#> Regular: every interval is 60 s.

# Remove 5 records to make a gap
report <- sampling_report(archive_days[-(5000:5004), ])
report
#> Sampling report: 14,395 records over 10 days (2000-01-01 to 2000-01-10, UTC)
#> Expected interval: 60 s (99.99% of intervals)
#> Irregular sampling found:
#>   Gaps: 1
#>     6 min from 2000-01-04 11:18:00 to 2000-01-04 11:24:00 (5 records missing)
#> Days without the expected 1,440 records: 1 of 10
#>     2000-01-04: 1,435 records (99.65%)
report$gaps
#>    row               start                 end length_s missing_records on_grid
#> 1 5000 2000-01-04 11:18:00 2000-01-04 11:24:00      360               5    TRUE
```
