# Four days in Toronto across the end of daylight saving: on 2016-11-06 the
# clock goes back an hour at 02:00, so the day is 25 hours long
toronto_archive <- function() {
  date <- seq(as.POSIXct("2016-11-04 00:00:00", tz = "America/Toronto"), by = 60, length.out = 97 * 60)
  depth <- 50 + 40 * sin(2 * pi * as.numeric(date) / 3600)
  archive <- data.frame(date = date, depth = depth)
  attr(archive, "time_zone") <- "America/Toronto"
  regularise_archive(archive)
}

test_that("days with a clock change have their own length", {
  archive <- toronto_archive()
  coverage <- attr(archive, "day_coverage")
  expect_equal(coverage$date, as.Date("2016-11-04") + 0:3)
  expect_equal(coverage$expected, c(1440, 1440, 1500, 1440))
  expect_true(all(coverage$coverage == 100))
  expect_equal(as.vector(table(archive$date_only)), c(1440, 1440, 1500, 1440))
})

test_that("fixed sunrise and sunset times are local to the tag", {
  archive <- toronto_archive()
  folder <- file.path(tempdir(), "clock_change")
  unlink(folder, recursive = TRUE)
  dir.create(file.path(folder, "toronto"), recursive = TRUE)
  create_depth_stats(
    archive, tag_ID = "toronto", diel = TRUE, sunrise_time = "06:00:00", sunset_time = "18:00:00",
    output = TRUE, output_folder = folder
  )
  saved <- readRDS(file.path(folder, "toronto", "archive_days.rds"))
  # Daytime starts at 06:00 local time, whatever the computer's time zone
  first_day <- saved$daytime[saved$date_only == as.Date("2016-11-04")]
  expect_equal(which(first_day == 1)[1], 6 * 60 + 1)
  expect_equal(sum(first_day), 12 * 60)
  # The gap columns and attributes are kept
  expect_true(all(c("filled", "long_gap") %in% names(saved)))
  expect_equal(attr(saved, "day_coverage"), attr(archive, "day_coverage"))
})

test_that("cluster plots put each day on time since midnight, and match clusters by date", {
  folder <- file.path(tempdir(), "clock_change")
  # A k-means result with three of the four days: 2016-11-05 was left out
  kmeans_result <- list(
    cluster = c(1L, 1L, 2L), distances = c(0.1, 0.2, 0.1), tag_ID = rep("toronto", 3),
    date_only = as.Date(c("2016-11-04", "2016-11-06", "2016-11-07"))
  )
  plots <- plot_clusters(
    tag_vector = "toronto", data_folder = folder, kmeans_result = kmeans_result, No_days = 2,
    every_nth = 1, Y_lim = c(0, 100, 25), diel_shade = TRUE, dpi = 30, output = TRUE, output_folder = folder
  )
  cluster_1 <- plots[[1]]$data
  expect_setequal(unique(cluster_1$date_only), as.Date(c("2016-11-04", "2016-11-06")))

  # The 25 hour day runs from midnight to 25 hours, in order, without repeating
  # the hour after the clock change
  long_day <- cluster_1[cluster_1$date_only == as.Date("2016-11-06"), ]
  secs <- as.numeric(long_day$time)
  expect_equal(range(secs), c(0, 25 * 3600 - 60))
  expect_false(is.unsorted(secs))
  expect_equal(nrow(long_day), 1500)

  # Night shading runs to the end of each day, and sunrise is 06:00 local
  # time: 7 hours after midnight on the day the clock goes back
  night <- plots[[1]]$layers[[1]]$data
  expect_equal(sort(as.numeric(night$xmax)), c(6, 7, 24, 25) * 3600)

  # The cluster TDR plot leaves the unclustered day grey
  tdr <- plot_cluster_TDR(
    tag_ID = "toronto", data_folder = folder, kmeans_result = kmeans_result, every_nth = 1,
    Y_lim = c(0, 100, 25), date_breaks = "1 day", output = FALSE
  )
  plot_data <- tdr$data
  day <- as.Date(format(plot_data$date, "%Y-%m-%d", tz = "America/Toronto"))
  expect_true(all(is.na(plot_data$cluster[day == as.Date("2016-11-05")])))
  expect_true(all(plot_data$cluster[day == as.Date("2016-11-07")] == 2))
})

test_that("cluster_of_days() matches by date, or by order for older results", {
  dated <- list(cluster = c(3L, 1L), tag_ID = c("a", "a"), date_only = as.Date(c("2000-01-02", "2000-01-03")))
  expect_equal(cluster_of_days(dated, "a", as.Date("2000-01-01") + 0:2), c(NA, 3L, 1L))
  undated <- list(cluster = c(3L, 1L), tag_ID = c("a", "a"))
  expect_equal(cluster_of_days(undated, "a", as.Date("2000-01-01") + 0:1), c(3L, 1L))
})
