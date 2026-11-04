archive_days <- readRDS(system.file("extdata", "data", "archive_days.rds", package = "FishDiveR"))

# Three days of 60 s records with temperature falling with depth, as from import_tag_data()
temp_archive <- function() {
  date <- seq(as.POSIXct("2020-01-30 00:00:00", tz = "UTC"), by = 60, length.out = 3 * 1440)
  depth <- 100 + 90 * sin(seq_along(date) / 50)
  archive <- data.frame(date = date, depth = depth, temp = 28 - depth / 20, date_only = as.Date(date, tz = "UTC"))
  attr(archive, "time_zone") <- "UTC"
  archive
}

test_that("plot_time_at() gives the percentage of time in each depth range", {
  time_at <- plot_time_at(archive_days, breaks = seq(0, 300, by = 50))
  expect_s3_class(attr(time_at, "time_at_plot"), "ggplot")
  expect_equal(levels(time_at$range), paste(seq(0, 250, by = 50), "to", seq(50, 300, by = 50)))
  expect_equal(sum(time_at$records), nrow(archive_days))
  expect_equal(sum(time_at$percent), 100)
  expect_equal(time_at$hours, time_at$records * 60 / 3600)
  expect_equal(
    time_at$records[time_at$range == "0 to 50"],
    sum(archive_days$depth >= 0 & archive_days$depth < 50)
  )
  # Shallowest range at the top of the plot
  limits <- attr(time_at, "time_at_plot")$scales$get_scales("y")$limits
  expect_equal(limits[length(limits)], "0 to 50")
})

test_that("plot_time_at() groups by month and day", {
  by_month <- plot_time_at(archive_days, by = "month")
  expect_equal(levels(by_month$group), "Jan 2000")

  by_day <- plot_time_at(archive_days, by = "day")
  expect_equal(nlevels(by_day$group), 10)
  expect_equal(as.vector(tapply(by_day$percent, by_day$group, sum)), rep(100, 10))
  plot <- attr(by_day, "time_at_plot")
  expect_s3_class(plot$layers[[1]]$geom, "GeomTile")
  # Ranges with no time are left blank
  expect_true(all(is.na(plot$data$percent[by_day$records == 0])))

  archive <- temp_archive()
  by_month <- plot_time_at(archive, variable = "temp", by = "month")
  expect_equal(levels(by_month$group), c("Jan 2020", "Feb 2020"))
  expect_equal(as.vector(tapply(by_month$records, by_month$group, sum)), c(2, 1) * 1440)
})

test_that("plot_time_at() gives time at temperature", {
  archive <- temp_archive()
  time_at <- plot_time_at(archive, variable = "temp", breaks = 18:28)
  expect_equal(sum(time_at$percent), 100)
  plot <- attr(time_at, "time_at_plot")
  expect_match(plot$labels$y, "Temperature")
  # Warmest range at the top
  limits <- plot$scales$get_scales("y")$limits
  expect_equal(limits[length(limits)], "27 to 28")

  # Records outside the breaks are left out with a warning
  expect_warning(time_at <- plot_time_at(archive, variable = "temp", breaks = 20:28), "outside the breaks")
  expect_equal(sum(time_at$records), sum(archive$temp >= 20))
})

test_that("plot_time_at() default breaks cover the record with round values", {
  archive <- temp_archive()
  archive$depth[1] <- 900
  breaks <- time_at_breaks(archive$depth, "depth")
  expect_equal(breaks[1], 0)
  expect_gte(max(breaks), 900)
  # One wide range holds the extreme record rather than widening all ranges
  expect_lte(diff(breaks[1:2]), 20)

  time_at <- plot_time_at(archive)
  expect_equal(sum(time_at$records), nrow(archive))
  expect_equal(levels(time_at$range)[nlevels(time_at$range)], paste(breaks[length(breaks) - 1], "to", max(breaks)))
})

test_that("plot_time_at() saves its plot and checks its arguments", {
  folder <- file.path(tempdir(), "time_at")
  unlink(folder, recursive = TRUE)
  plot_time_at(temp_archive(), variable = "temp", by = "month", output = TRUE, output_folder = folder, dpi = 50)
  expect_true(file.exists(file.path(folder, "time_at_temp_by_month.png")))
  expect_equal(time_at_file("depth", "all"), "time_at_depth.png")

  expect_error(plot_time_at(archive_days, variable = "temp"), "no temperature data")
  expect_error(plot_time_at(archive_days, variable = "light"), "'depth' or 'temp'")
  expect_error(plot_time_at(archive_days, by = "week"), "'all', 'month' or 'day'")
  expect_error(plot_time_at(archive_days, breaks = c(50, 10)), "increasing")
  expect_error(plot_time_at(archive_days, output = TRUE), "output_folder")
})

test_that("plot_TDR() colours the depth record by temperature", {
  folder <- file.path(tempdir(), "tdr_temp")
  dir.create(file.path(folder, "tag"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(temp_archive(), file.path(folder, "tag", "archive_days.rds"))

  tdr <- plot_TDR("tag/archive_days.rds", data_folder = folder, every_nth = 1, Y_lim = c(0, 200, 50), colour_temp = TRUE)
  plot <- attr(tdr, "TDR_plot")
  expect_true("temp" %in% names(tdr))
  built <- ggplot2::ggplot_build(plot)$data[[1]]
  # Cold is blue and warm is red
  expect_equal(built$colour[which.min(tdr$temp)], "#4575B4")
  expect_equal(built$colour[which.max(tdr$temp)], "#D73027")

  expect_error(
    plot_TDR("data/archive_days.rds", data_folder = system.file("extdata", package = "FishDiveR"), colour_temp = TRUE),
    "needs temperature data"
  )
  expect_error(plot_TDR("tag/archive_days.rds", data_folder = folder, colour_temp = "yes"), "TRUE or FALSE")
})
