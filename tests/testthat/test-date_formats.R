times <- seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"), by = 60, length.out = 20000)

test_that("detect_date_format() reads common date-time formats", {
  formats <- c(
    "%H:%M:%S %d-%b-%Y", "%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%SZ", "%d/%m/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M", "%d.%m.%Y %H:%M:%S", "%Y/%m/%d %H:%M:%OS3", "%d %b %Y %H:%M:%S"
  )
  for (f in formats) {
    text <- format(times, f)
    detected <- detect_date_format(text)
    expect_false(detected$ambiguous)
    expect_equal(parse_date_times(text, detected$format), times, tolerance = 1e-3, info = f)
  }
  # Dates only, e.g. in GPS files
  days <- seq(as.Date("2013-10-26"), by = "day", length.out = 40)
  for (f in c("%d-%b-%Y", "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y")) {
    detected <- detect_date_format(format(days, f), type = "date")
    expect_equal(as.Date(parse_date_times(format(days, f), detected$format)), days, info = f)
  }
})

test_that("detect_date_format() tells day/month from month/day by the records' spacing", {
  # 3.5 days: no day is above 12, but only one order gives evenly spaced times
  short <- times[1:5000]
  expect_equal(detect_date_format(format(short, "%d/%m/%Y %H:%M:%S"))$format, "%d/%m/%Y %H:%M:%OS")
  expect_equal(detect_date_format(format(short, "%m/%d/%Y %H:%M:%S"))$format, "%m/%d/%Y %H:%M:%OS")

  # Unordered values, or too few to tell: day/month, marked ambiguous
  detected <- detect_date_format(c("1/2/2020 3:04", "1/2/2020 3:05"))
  expect_true(detected$ambiguous)
  expect_equal(detected$format, "%d/%m/%Y %H:%M")
  expect_true(detect_date_format(c("05/01/2020", "06/01/2020"), type = "date", ordered = FALSE)$ambiguous)

  expect_error(detect_date_format(c("hello", "world")), "Could not read the date-time column, e.g. 'hello'")
  expect_error(detect_date_format(c(NA, "")), "is empty")
})

test_that("import_tag_data() reads archives in other date formats", {
  archive_file <- system.file("extdata", "data/data-Archive.csv", package = "FishDiveR")
  original <- utils::read.csv(archive_file)
  import <- function(file, ...) {
    import_tag_data(
      tag_ID = "data", tag_deploy_UTC = "2000-01-01 00:00:00", tag_release_UTC = "2000-01-11 23:59:00",
      archive = file, time_zone = "UTC", ...
    )
  }
  expected <- import(archive_file)

  dates <- as.POSIXct(original[[1]], format = "%H:%M:%S %d-%b-%Y", tz = "UTC")
  for (f in c("%Y-%m-%d %H:%M:%S", "%d/%m/%Y %H:%M", "%m/%d/%Y %H:%M:%S")) {
    file <- file.path(tempdir(), "formats-Archive.csv")
    converted <- original
    converted[[1]] <- format(dates, f)
    utils::write.csv(converted, file, row.names = FALSE)
    archive <- import(file)
    expect_equal(archive$date, expected$date, info = f)
    expect_equal(archive$depth, expected$depth, info = f)
  }

  # The format found is reported
  expect_message(import(archive_file, verbose = TRUE), "Date-times read as hh:mm:ss day month-name year: '00:00:00 01-Jan-2000' is 2000-01-01 00:00:00")

  # A given format is used as is
  converted[[1]] <- format(dates, "%Y%m%d %H%M%S")
  utils::write.csv(converted, file, row.names = FALSE)
  expect_error(import(file), "Could not read the date-time column \\(1\\)")
  expect_equal(import(file, date_format = "%Y%m%d %H%M%S")$date, expected$date)
  expect_error(import(file, date_format = "ymd"), "strptime")

  # Rows whose date-times can't be read are left out, with a warning
  converted[[1]] <- format(dates, "%Y-%m-%d %H:%M:%S")
  converted[[1]][10] <- "not a date"
  utils::write.csv(converted, file, row.names = FALSE)
  warnings <- capture_warnings(archive <- import(file))
  expect_match(warnings, "1 date-time could not be read and was left out, e.g. 'not a date'", all = FALSE)
  # The record left out is a gap, filled
  expect_match(warnings, "Filled 1 gap of up to 2 min by interpolation (1 record)", all = FALSE, fixed = TRUE)
  expect_equal(sum(archive$filled), 1)
})

test_that("GPS dates are read in any detected format", {
  archive_days <- readRDS(system.file("extdata", "data", "archive_days.rds", package = "FishDiveR"))
  gps <- utils::read.csv(system.file("extdata", "data", "GPS.csv", package = "FishDiveR"))
  folder <- file.path(tempdir(), "gps_formats")
  dir.create(file.path(folder, "data"), recursive = TRUE, showWarnings = FALSE)
  stats <- function(gps_file) {
    create_depth_stats(archive_days, tag_ID = "data", diel = TRUE, GPS = gps_file, output_folder = folder)
  }
  expected <- stats(system.file("extdata", "data", "GPS.csv", package = "FishDiveR"))

  date_col <- grep("date", names(gps), ignore.case = TRUE)[1]
  days <- detect_date_format(gps[[date_col]], type = "date")
  dates <- as.Date(parse_date_times(gps[[date_col]], days$format))
  for (f in c("%m/%d/%Y", "%d.%m.%Y", "%d-%b-%Y")) {
    gps[[date_col]] <- format(dates, f)
    file <- file.path(folder, "GPS.csv")
    utils::write.csv(gps, file, row.names = FALSE)
    expect_equal(stats(file), expected, info = f)
  }
})

test_that("the app reads tag tables and archives with other date formats", {
  meta <- data.frame(
    tag_ID = c("a", "b"), archive = "x.csv", tag_deploy_UTC = c("25/10/2013 02:46", "08/05/2014 05:25:00"),
    tag_release_UTC = c("23/04/2014 23:17:35", "04/11/2014 22:33"), time_zone = "UTC"
  )
  file <- file.path(tempdir(), "tag_table_dates.csv")
  utils::write.csv(meta, file, row.names = FALSE)
  # Mixed hh:mm and hh:mm:ss can't be read as one format, so are left as they are
  expect_equal(read_tag_metadata(file)$tag_deploy_UTC, meta$tag_deploy_UTC)

  meta$tag_deploy_UTC <- c("25/10/2013 02:46:00", "08/05/2014 05:25:00")
  meta$tag_release_UTC <- c("23/04/2014 23:17:35", "04/11/2014 22:33:30")
  utils::write.csv(meta, file, row.names = FALSE)
  tags <- read_tag_metadata(file)
  expect_equal(tags$tag_deploy_UTC, c("2013-10-25 02:46:00", "2014-05-08 05:25:00"))
  expect_equal(tags$tag_release_UTC, c("2014-04-23 23:17:35", "2014-11-04 22:33:30"))
  expect_null(attr(tags, "date_note"))

  # No day above 12: read as day/month, with a note
  meta$tag_deploy_UTC <- c("05/01/2014 00:00:00", "08/05/2014 05:25:00")
  meta$tag_release_UTC <- c("03/04/2014 00:00:00", "04/11/2014 22:33:30")
  utils::write.csv(meta, file, row.names = FALSE)
  tags <- read_tag_metadata(file)
  expect_equal(tags$tag_deploy_UTC[1], "2014-01-05 00:00:00")
  expect_match(attr(tags, "date_note"), "were read as day/month/year hh:mm:ss")

  archive_file <- system.file("extdata", "data/data-Archive.csv", package = "FishDiveR")
  note <- archive_date_note(archive_file, 1)
  expect_true(note$ok)
  expect_equal(note$text, "Date-times read as hh:mm:ss day month-name year: '00:00:00 01-Jan-2000' is 2000-01-01 00:00:00.")
  note <- archive_date_note(archive_file, 2)
  expect_false(note$ok)
  expect_match(note$text, "Could not read the date-time column \\(2\\)")
})
