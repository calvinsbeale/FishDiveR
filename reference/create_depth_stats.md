# Create depth statistics

`create_depth_stats` creates the various daily and diel depth statistics
for each day

## Usage

``` r
create_depth_stats(
  archive,
  tag_ID,
  diel = FALSE,
  sunrise_time = NULL,
  sunset_time = NULL,
  GPS = FALSE,
  sunset_type = "civil",
  output = FALSE,
  output_folder = NULL,
  verbose = FALSE
)
```

## Arguments

- archive:

  Data frame containing processed time series depth data

- tag_ID:

  Unique tag identification number in a vector of characters. E.g.
  "123456"

- diel:

  Include diel statistics when TRUE

- sunrise_time:

  Sunrise time (local time zone) in 24-hour clock. E.g. "05:45:00"

- sunset_time:

  Sunset time (local time zone) in 24-hour clock. E.g. "18:30:00"

- GPS:

  Either FALSE or the location of the GPS file containing columns
  'date', 'lat' (latitude) and 'lon' (longitude) if one exists. 'date'
  columns must be in a format readable by lubridate::dmy()

- sunset_type:

  Choose which type of sunset to include 'NULL', 'civil', 'nautical', or
  'astronomical'

- output:

  Logical. If TRUE, output is saved to `output_folder`. Defaults to
  FALSE.

- output_folder:

  Output folder path. If `output = TRUE`, `output_folder` must be
  provided. Defaults to NULL.

- verbose:

  Logical. If TRUE, progress messages are shown. Defaults to FALSE.

## Value

A set of statistics calculated daily for the depth data. If diel is
'TRUE', additional diel statistics will be returned. An attribute 'diel'
with value 'TRUE' is given when diel statistics are included.

## Examples

``` r
# Set file path
filepath <- system.file("extdata", package = "FishDiveR")

# Load archive_days
archive_days <- readRDS(file.path(filepath, "data/archive_days.rds"))

# Run create_depth_stats function
depthStats <- create_depth_stats(
  archive = archive_days,
  tag_ID = "data",
  diel = TRUE,
  sunrise_time = "06:00:00",
  sunset_time = "18:00:00",
  GPS = file.path(filepath, "data/GPS.csv"),
  sunset_type = "civil",
  output = TRUE,
  output_folder = tempdir(),
  verbose = TRUE
)
#> Running create_depth_stats() on tag ID data
#> Reading in GPS locations. Using actual sunrise and sunset times to calculate diel statistics
#> Output folder: /tmp/Rtmp27WBoj/data/3_Stats/data_depthStats.csv
```
