# Plot the time-series depth dataset

This function plots the time-series depth data from the imported tag.

## Usage

``` r
plot_TDR(
  rds_file,
  data_folder = NULL,
  every_nth = 20,
  every_s = 0,
  plot_size = c(12, 6),
  X_lim = NULL,
  Y_lim = c(0, 1500, 100),
  date_breaks = "14 day",
  dpi = 300,
  output = FALSE,
  output_folder = NULL,
  verbose = FALSE,
  minmax_bins = 0,
  colour_temp = FALSE
)
```

## Arguments

- rds_file:

  Character vector file path of rds file. E.g.
  ("E:/data/archive_days.rds")

- data_folder:

  Parent folder path with separate folders for each tag data. E.g.
  "C:/Tag data". Defaults to 'data_dir'

- every_nth:

  Numerical. Optional down-sampling of data points to plot. Defaults to
  10, plotting every 10th record.

- every_s:

  Numerical. Alternative to every_nth. Optional down-sampling of data
  points to plot by number of seconds, as opposed to records. E.g. plots
  every 60th second, rather than 10th row of data. Must be a multiple of
  the sampling frequency. Overrides every_nth if != 0.

- plot_size:

  ggSave height and width for saving the output plot. Must be numeric,
  positive and 2 elements long. Default to 'c(12,6)'

- X_lim:

  Optional. Vector with two dates delimiting the time-depth record to
  plot. E.g. c("2000-01-01", "2000-11-23")

- Y_lim:

  Character vector with minimum depth, maximum depth, and sequence for
  ticks on Y-axis. Must be numeric, positive and 3 elements long. E.g.
  c(0,1500,100).

- date_breaks:

  X-axis ggplot2 date breaks. E.g, "24 hour, "3 day", "2 week".

- dpi:

  Numerical. DPI to use for 'ggsave()' output. E.g, 600

- output:

  Logical. If TRUE, a plot file is saved to `output_folder`. Defaults to
  FALSE.

- output_folder:

  Output folder path used when `output = TRUE`. Defaults to NULL.

- verbose:

  Logical. If TRUE, progress messages are shown. Defaults to FALSE.

- minmax_bins:

  Numerical. Alternative to every_nth and every_s for fast plotting of
  large archives. Splits the record into this many equal time bins and
  plots only the shallowest and deepest record in each, so dive extremes
  are kept. E.g. 2000. Overrides every_nth and every_s if != 0. Defaults
  to 0.

- colour_temp:

  Logical. If TRUE, the depth record is coloured by temperature, from
  blue (cold) to red (warm). Needs a 'temp' column, i.e. the tag
  imported with `temp_col` in
  [`import_tag_data()`](https://calvinsbeale.github.io/FishDiveR/reference/import_tag_data.md).
  Defaults to FALSE.

## Value

A data frame of plot data. The ggplot object is attached as the
attribute 'TDR_plot'.

## Examples

``` r
# Set file path
filepath <- system.file("extdata", package = "FishDiveR")

# Run plot_TDR function
TDR_plot <- plot_TDR(
  rds_file = "data/archive_days.rds",
  data_folder = filepath,
  every_nth = 10,
  every_s = 0,
  plot_size = c(12, 6),
  X_lim = NULL,
  Y_lim = c(0, 300, 50),
  date_breaks = "24 hour",
  dpi = 100,
  output = TRUE,
  output_folder = tempdir(),
  verbose = TRUE
)
#> Data sampling interval is 60 seconds
#> Plotting every 10 records 
#> Maximum depth is 256.6 meters
#> Output file:/tmp/RtmpfhMJTI/tag_archive.png
```
