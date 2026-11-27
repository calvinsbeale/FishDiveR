# Load time-depth series data from csv file

`import_tag_data` processes the time-series depth data of marine animal
tags. Data to import should be a csv file with a 'date_time' column and
a depth column. Data is cropped by deployment and release times.

## Usage

``` r
import_tag_data(
  tag_ID,
  tag_deploy_UTC,
  tag_release_UTC,
  archive,
  date_time_col = 1,
  depth_col = 2,
  temp_col = NA,
  time_zone,
  date_format = NULL,
  max_fill_mins = 5,
  min_coverage = 70,
  output = FALSE,
  output_folder = NULL,
  verbose = FALSE
)
```

## Arguments

- tag_ID:

  Unique tag identification number in a vector of characters. E.g.
  "123456"

- tag_deploy_UTC:

  UTC deployment time in the allowed `POSIXct` format: E.g. "2013-10-25
  02:46:00"

- tag_release_UTC:

  UTC release time in the allowed `POSIXct` format: E.g. "2014-04-23
  23:17:35"

- archive:

  File path of the time-series depth archive. E.g. ("C:/Tag
  data/123456/123456-Archive.csv")

- date_time_col:

  Column number of the date time series. Date-times are in UTC, and
  their format is detected (see `date_format`).

- depth_col:

  Column number of the depth series

- temp_col:

  (Optional) Column number of temperature series

- time_zone:

  Time zone of the data. E.g. "Asia/Tokyo"

- date_format:

  NULL (the default) to detect the format of the date-times, or a
  [`strptime()`](https://rdrr.io/r/base/strptime.html) format, e.g.
  "%d/%m/%Y %H:%M:%S". Detected formats are dates as year-month-day,
  day/month/year, month/day/year or day month-name year (e.g.
  2024-03-05, 05/03/2024 or 05-Mar-2024), with times as hh:mm:ss or
  hh:mm, before or after the date, e.g. "00:01:00 01-Jan-2000" (Wildlife
  Computers) or "2000-01-01T00:01:00Z". Where no day is above 12, the
  order of day and month is taken from the spacing of the records.

- max_fill_mins:

  The longest gap, in minutes, to fill by interpolation for analysis
  (default 5, at most 360). See
  [`regularise_archive()`](https://calvinsbeale.github.io/FishDiveR/reference/regularise_archive.md).

- min_coverage:

  The percentage of a day's records that must be real (not filled) for
  the day to be analysed (default 70).

- output:

  Logical. If TRUE, output is saved to `output_folder`. Defaults to
  FALSE.

- output_folder:

  Output folder path. If `output = TRUE`, `output_folder` must be
  provided. Defaults to NULL.

- verbose:

  Logical. If TRUE, progress messages are shown. Defaults to FALSE.

## Value

A data frame of processed tag data. Columns kept are:

- 'date' a POSIXct date_time object in format "yyyy-mm-dd hh:mm:ss"

- 'depth' numerical depth data

- 'temp' numerical temperature data

- 'date_only' an as.Date version of the 'date' column

- 'filled' and 'long_gap', marking records filled by interpolation, and
  those in gaps too long to analyse

- An attribute 'time_zone' is added to the date frame containing the
  time zone of the 'date'

- The attributes of
  [`regularise_archive()`](https://calvinsbeale.github.io/FishDiveR/reference/regularise_archive.md),
  including each day's coverage and the gaps found

## Details

Data are cropped to full days from midnight to midnight in local time
based on the time zone supplied. If `output = TRUE`, the cropped data
are saved as `archive_days.rds` in the tag's folder within
`output_folder`, with the settings used (archive, deployment and release
times, columns and time zone) in `import_settings.rds`. The app uses
these to warn when a tag's settings have changed since it was imported.

The cropped data are then put on a regular time grid by
[`regularise_archive()`](https://calvinsbeale.github.io/FishDiveR/reference/regularise_archive.md),
as the wavelet analysis needs. Gaps of up to `max_fill_mins` are filled
by interpolation, with a warning. Longer gaps are filled only so that
the wavelet can run, and are left out of the statistics. Days with less
than `min_coverage` percent of their records are left out of the
statistics. The sampling check of the records before they were put on
the grid is kept as `attr(archive, "sampling_report")`.

## Examples

``` r
# Set file path
filepath <- system.file("extdata", package = "FishDiveR")

# Run import_tag_data function on tag archive csv file
archive_days <- import_tag_data(
  tag_ID = "data",
  tag_deploy_UTC = "2000-01-01 00:00:00",
  tag_release_UTC = "2000-01-11 23:59:00",
  archive = file.path(filepath, "data/data-Archive.csv"),
  date_time_col = 1,
  depth_col = 2,
  temp_col = NA,
  time_zone = "Asia/Tokyo",
  output = TRUE,
  output_folder = tempdir(),
  verbose = TRUE
)
#> 
#> Tag ID = data
#> Date-times read as hh:mm:ss day month-name year: '00:00:00 01-Jan-2000' is 2000-01-01 00:00:00
#> Depth sampling interval is 60 seconds
#> Number of depth values corrected (above 0):0
#> Mean depth = 75.5 SD = 81.9
#> Maximum depth = 256.6
#> Number of full days in dataset: 10
#> Regular grid: every 60 s, 14,400 records. Filled 0 records in short gaps; 0 in long gaps (not analysed). 10 of 10 days kept (at least 70% real records).
#> Output file: /tmp/RtmpQAjQkT/data/archive_days.rds
```
