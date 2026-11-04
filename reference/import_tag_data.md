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

  Column number of the date time series

- depth_col:

  Column number of the depth series

- temp_col:

  (Optional) Column number of temperature series

- time_zone:

  Time zone of the data. E.g. "Asia/Tokyo"

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

- An attribute 'time_zone' is added to the date frame containing the
  time zone of the 'date'

## Details

Data are cropped to full days from midnight to midnight in local time
based on the time zone supplied. If `output = TRUE`, the cropped data
are saved as `archive_days.rds` in the tag's folder within
`output_folder`, with the settings used (archive, deployment and release
times, columns and time zone) in `import_settings.rds`. The app uses
these to warn when a tag's settings have changed since it was imported.

A warning is given if the sampling interval is not constant throughout
the cropped data (e.g. gaps from missing records), listing the intervals
detected and how many records have each. Use
[`sampling_report()`](https://calvinsbeale.github.io/FishDiveR/reference/sampling_report.md)
on the imported data to see where the irregularities are.

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
#> Depth sampling interval is 60 seconds
#> Number of depth values corrected (above 0):0
#> Mean depth = 75.5 SD = 81.9
#> Maximum depth = 256.6
#> Number of full days in dataset: 10
#> Output file: /tmp/Rtmpcux66d/data/archive_days.rds
```
