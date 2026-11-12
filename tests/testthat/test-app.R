test_that("tag tables round-trip through csv and fill optional columns", {
  file <- tempfile(fileext = ".csv")

  write_tag_metadata(example_tag(), file)
  expect_equal(read_tag_metadata(file), example_tag(), ignore_attr = TRUE)

  # Only the required columns
  utils::write.csv(example_tag()[, c("tag_ID", "archive", "tag_deploy_UTC", "tag_release_UTC", "time_zone")], file, row.names = FALSE)
  meta <- read_tag_metadata(file)
  expect_equal(names(meta), tag_metadata_columns)
  expect_equal(meta$date_time_col, 1L)
  expect_equal(meta$depth_col, 2L)
  expect_true(is.na(meta$temp_col))

  # Missing required column
  utils::write.csv(data.frame(tag_ID = "a"), file, row.names = FALSE)
  expect_error(read_tag_metadata(file), "missing column")

  expect_equal(nrow(read_tag_metadata(tempfile())), 0)
})

test_that("upsert_tags() replaces tags with the same ID", {
  tags <- upsert_tags(empty_tag_metadata(), example_tag())
  second <- example_tag()
  second$tag_ID <- "second"
  tags <- upsert_tags(tags, second)
  expect_equal(tags$tag_ID, c("example", "second"))

  changed <- example_tag()
  changed$time_zone <- "Asia/Tokyo"
  tags <- upsert_tags(tags, changed)
  expect_equal(nrow(tags), 2)
  expect_equal(tags$time_zone[tags$tag_ID == "example"], "Asia/Tokyo")
})

test_that("validate_tag() reports problems", {
  expect_length(validate_tag(example_tag()), 0)

  bad <- example_tag()
  bad$archive <- "no-such-file.csv"
  bad$tag_release_UTC <- "1999-12-31 00:00:00"
  bad$time_zone <- "Not/AZone"
  bad$depth_col <- NA_integer_
  problems <- validate_tag(bad)
  expect_true(any(grepl("archive file not found", problems)))
  expect_true(any(grepl("after deploy", problems)))
  expect_true(any(grepl("time zone", problems)))
  expect_true(any(grepl("depth column", problems)))
})

test_that("masked_input() makes a text box with its pattern, calendar and scripts", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("htmltools")

  box <- masked_input("deploy", "Deploy time (UTC)", value = "2024-03-05 08:30:00")
  html <- as.character(box)
  expect_match(html, 'id="deploy"', fixed = TRUE)
  expect_match(html, 'type="text"', fixed = TRUE)
  expect_match(html, 'data-pattern="yyyy-mm-dd hh:mm:ss"', fixed = TRUE)
  expect_match(html, 'maxlength="19"', fixed = TRUE)
  expect_match(html, 'value="2024-03-05 08:30:00"', fixed = TRUE)
  expect_match(html, 'data-fd-input="deploy"', fixed = TRUE)
  expect_match(html, "fd-calendar-anchor", fixed = TRUE)
  expect_match(html, "fd-calendar\"", fixed = TRUE)
  expect_match(html, "fd-clock-button", fixed = TRUE)
  # Tabbing goes from box to box, skipping the picker buttons
  expect_equal(lengths(regmatches(html, gregexpr('tabindex="-1"', html))), 3)

  deps <- vapply(htmltools::findDependencies(box), function(d) d$name, character(1))
  expect_true("fd-masked-input" %in% deps)
  expect_true("bootstrap-datepicker-js" %in% deps)
  script <- htmltools::findDependencies(box)[[which(deps == "fd-masked-input")]]
  expect_true(file.exists(file.path(script$src$file, script$script)))

  # A time has a clock, but no calendar
  html <- as.character(masked_input("sunrise", "Sunrise", pattern = "hh:mm:ss", value = "06:00:00"))
  expect_match(html, 'data-pattern="hh:mm:ss"', fixed = TRUE)
  expect_match(html, 'maxlength="8"', fixed = TRUE)
  expect_match(html, "fd-clock-button", fixed = TRUE)
  expect_no_match(html, "fd-calendar")
  expect_error(masked_input("x", "x", pattern = "dd/mm/yyyy"))
})

test_that("run_logged() collects messages, warnings and errors", {
  res <- run_logged({
    message("hello")
    warning("careful")
    42
  })
  expect_true(res$ok)
  expect_equal(res$value, 42)
  expect_equal(res$log, c("hello", "Warning: careful"))

  res <- run_logged(stop("broken"))
  expect_false(res$ok)
  expect_equal(res$log, "Error: broken")
})

test_that("app imports tags and plots the depth record", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  project <- file.path(tempdir(), "app_project")
  unlink(project, recursive = TRUE)
  dir.create(project)

  state <- shiny::reactiveValues(project = project, tags = example_tag(), refresh = 0)
  expect_equal(tag_status(example_tag(), project)$Imported, FALSE)

  shiny::testServer(mod_import_server, args = list(state = state), {
    session$setInputs(import_all = 1)
    expect_true(file.exists(file.path(project, "example", "archive_days.rds")))
    expect_match(paste(import_log(), collapse = "\n"), "example: imported")
    expect_equal(imported_tags(), "example")

    # Selecting an imported tag shows its sampling check
    session$setInputs(tag_table_rows_selected = 1)
    expect_match(paste(as.character(output$sampling_details), collapse = ""), "Regular: every interval is 60 s")
    expect_equal(sampling_status(tag_sampling("example")), "Regular (60 s)")
    session$setInputs(tag_table_rows_selected = NULL)

    # Changing a setting after import marks the tag as outdated until re-imported
    expect_equal(status()$Settings_changed, "")
    tags <- state$tags
    tags$tag_release_UTC <- "2000-01-10 23:59:00"
    state$tags <- tags
    session$flushReact()
    expect_equal(status()$Settings_changed, "release time")
    expect_match(paste(as.character(output$outdated_note), collapse = ""), "example: release time changed since import")
    session$setInputs(import_all = 2)
    expect_equal(status()$Settings_changed, "")
    expect_equal(nrow(readRDS(file.path(project, "example", "archive_days.rds"))), 9 * 1440)
    tags$tag_release_UTC <- example_tag()$tag_release_UTC
    state$tags <- tags
    session$setInputs(import_all = 3)
    expect_equal(status()$Settings_changed, "")

    session$setInputs(plot_tag = "example")
    session$setInputs(
      downsample = "n", every = 10, y_max = 300, y_step = 50,
      x_lim = archive_info()$dates, date_breaks = "2 day", no_axis_titles = TRUE
    )
    expect_null(plotted_settings())
    session$setInputs(create_plot = 1)
    plot <- tdr_plot()
    expect_s3_class(plot, "ggplot")
    expect_s3_class(plot$theme$axis.title, "element_blank")
    expect_identical(plotted_settings(), plot_settings())

    # Changing a setting does not redraw until Create plot is clicked
    session$setInputs(y_max = 350)
    expect_false(identical(plotted_settings(), plot_settings()))
    expect_identical(tdr_plot(), plot)

    # Fast preview plots at most two points per time bin
    session$setInputs(downsample = "minmax", create_plot = 2)
    expect_lte(nrow(tdr_plot()$data), 2 * preview_bins)

    # The example tag has no temperature to colour by
    session$setInputs(colour_temp = TRUE, create_plot = 3)
    expect_error(tdr_plot(), "no temperature data")
    session$setInputs(colour_temp = FALSE)
    expect_true(all(c("28 day", "2 month") %in% date_break_choices))

    # Time at depth for each day, with 25 m ranges
    session$setInputs(ta_tag = "example", ta_variable = "depth", ta_by = "day", ta_width = 25, ta_no_axis_titles = FALSE)
    expect_null(ta_plotted())
    session$setInputs(ta_create = 1)
    plot <- time_at_plot()
    expect_s3_class(plot, "ggplot")
    expect_equal(nlevels(plot$data$range), 11)
    session$setInputs(ta_save = 1)
    expect_true(file.exists(file.path(project, "example", "time_at_depth_by_day.png")))

    # Automatic ranges for the whole record; no temperature for time at temperature
    session$setInputs(ta_by = "all", ta_width = NA, ta_create = 2)
    expect_s3_class(time_at_plot(), "ggplot")
    session$setInputs(ta_variable = "temp", ta_create = 3)
    expect_error(time_at_plot(), "no temperature data")
    session$setInputs(ta_variable = "depth", ta_width = 0.01, ta_create = 4)
    expect_error(time_at_plot(), "Use a wider range")

    # Remove the tag after confirming
    session$setInputs(tag_table_rows_selected = 1, remove_selected = 1)
    session$setInputs(confirm_remove = 1)
    expect_equal(nrow(state$tags), 0)
    expect_equal(nrow(read_tag_metadata(file.path(project, "tag_metadata.csv"))), 0)
    expect_true(file.exists(file.path(project, "example", "archive_days.rds")))
  })
})

test_that("changed_import_settings() compares a tag with the settings it was imported with", {
  project <- file.path(tempdir(), "settings_project")
  unlink(project, recursive = TRUE)
  tag <- example_tag()
  expect_equal(changed_import_settings(tag, project), character())
  expect_true(is.na(imported_temperature(tag, project)))

  suppressMessages(import_tag_data(
    tag_ID = tag$tag_ID, tag_deploy_UTC = tag$tag_deploy_UTC, tag_release_UTC = tag$tag_release_UTC,
    archive = tag$archive, date_time_col = tag$date_time_col, depth_col = tag$depth_col, temp_col = tag$temp_col,
    time_zone = tag$time_zone, output = TRUE, output_folder = project
  ))
  expect_true(file.exists(file.path(project, "example", "import_settings.rds")))
  expect_equal(changed_import_settings(tag, project), character())
  expect_false(imported_temperature(tag, project))

  # Equivalent ways of writing the same settings are not changes
  same <- tag
  same$tag_deploy_UTC <- "2000-01-01 00:00"
  if (.Platform$OS.type == "windows") same$archive <- toupper(gsub("/", "\\\\", tag$archive))
  same$date_time_col <- "1"
  expect_equal(changed_import_settings(same, project), character())

  changed <- tag
  changed$tag_deploy_UTC <- "2000-01-02 00:00:00"
  changed$time_zone <- "Asia/Tokyo"
  changed$temp_col <- 3L
  expect_equal(changed_import_settings(changed, project), c("deployment time", "temperature column", "time zone"))

  # Without the settings used (imported before they were saved, or the archive
  # replaced since), the temperature column and time zone are checked against
  # the archive itself
  Sys.setFileTime(file.path(project, "example", "import_settings.rds"), Sys.time() - 3600)
  expect_equal(changed_import_settings(changed, project), c("temperature column", "time zone"))
  unlink(file.path(project, "example", "import_settings.rds"))
  expect_equal(changed_import_settings(tag, project), character())
  expect_false(imported_temperature(tag, project))

  # A tag imported with temperature
  temp_csv <- file.path(tempdir(), "temp-Archive.csv")
  archive <- utils::read.csv(tag$archive)
  archive$temp <- 20 + archive$depth / 100
  utils::write.csv(archive, temp_csv, row.names = FALSE)
  with_temp <- tag
  with_temp$archive <- temp_csv
  with_temp$temp_col <- 3L
  suppressMessages(import_tag_data(
    tag_ID = tag$tag_ID, tag_deploy_UTC = tag$tag_deploy_UTC, tag_release_UTC = tag$tag_release_UTC,
    archive = temp_csv, date_time_col = 1, depth_col = 2, temp_col = 3,
    time_zone = tag$time_zone, output = TRUE, output_folder = project
  ))
  expect_true(imported_temperature(with_temp, project))
  unlink(file.path(project, "example", "import_settings.rds"))
  expect_true(imported_temperature(with_temp, project))
  expect_equal(changed_import_settings(tag, project), "temperature column")
})

test_that("Browse opens in the project folder and fills the path", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  project <- file.path(tempdir(), "app_project_browse")
  dir.create(project, showWarnings = FALSE)
  archive <- example_tag()$archive

  start_dirs <- character()
  old <- options(FishDiveR.choose_path = function(type, caption, start_dir) {
    start_dirs <<- c(start_dirs, start_dir)
    archive
  })
  on.exit(options(old), add = TRUE)

  state <- shiny::reactiveValues(project = normalizePath(project, winslash = "/"), tags = empty_tag_metadata(), refresh = 0)
  shiny::testServer(mod_project_server, args = list(state = state), {
    session$setInputs(archive_browse = 1)
    session$setInputs(browse_ready = 1)
    expect_equal(start_dirs, state$project)
  })
})

test_that("Next buttons move through the steps", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")

  shiny::testServer(app_server(), {
    session$setInputs(next_project = 1)
    session$setInputs(next_import = 1)
    expect_true(TRUE)
  })
})

test_that("run_app() checks project_folder", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  expect_error(run_app(project_folder = file.path(tempdir(), "no-such-folder")), "does not exist")
})

test_that("wavelet size estimates match WaveletComp output", {
  wavelet <- readRDS(system.file("extdata", "data", "1_Wavelets", "data_wavelet.rds", package = "FishDiveR"))
  lp <- attr(wavelet, "LP")
  up_hours <- attr(wavelet, "UP") * 24
  so <- attr(wavelet, "SO")

  expect_equal(wavelet_periods(lp, up_hours, so), wavelet$nr)
  estimate <- wavelet_bytes(wavelet$nc, lp, up_hours, so)
  actual <- as.numeric(object.size(wavelet))
  expect_lt(abs(estimate - actual) / actual, 0.1)

  expect_equal(format_bytes(c(500, 2048, 5 * 1024^3)), c("500 B", "2 KB", "5 GB"))
})

test_that("app runs wavelets for imported tags", {
  # Runs whole wavelets: too slow for CRAN's check time, which the tests of
  # create_wavelet() cover
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  project <- file.path(tempdir(), "app_project_wavelet")
  unlink(project, recursive = TRUE)
  dir.create(project)
  tag <- example_tag()
  suppressMessages(import_tag_data(
    tag_ID = tag$tag_ID, tag_deploy_UTC = tag$tag_deploy_UTC, tag_release_UTC = tag$tag_release_UTC,
    archive = tag$archive, time_zone = tag$time_zone, output = TRUE, output_folder = project
  ))

  state <- shiny::reactiveValues(project = project, tags = tag, refresh = 0)
  shiny::testServer(mod_wavelet_server, args = list(state = state), {
    # Warnings about particular tags only appear for selected tags
    session$setInputs(lower_period_mins = 0.5, upper_period_hours = 24, suboctaves = 12)
    expect_false(isTRUE(grepl("shorter than the sampling interval", output$warnings$html)))
    session$setInputs(tag_table_rows_selected = 1)
    expect_match(output$warnings$html, "shorter than the sampling interval of: example")
    session$setInputs(tag_table_rows_selected = NULL)

    session$setInputs(lower_period_mins = 30, upper_period_hours = 24, suboctaves = 12)
    info <- tag_info()
    expect_equal(info$tag_ID, "example")
    expect_false(info$wavelet)
    expect_equal(info$sampling_s, 60)

    session$setInputs(run_all = 1)
    expect_true(file.exists(file.path(project, "example", "1_Wavelets", "example_wavelet.rds")))
    expect_true(file.exists(file.path(project, "example", "2_Wavelet_Figures", "example_wavelet_spectrum_no.p.png")))
    expect_match(paste(wavelet_log(), collapse = "\n"), "example: done")

    info <- tag_info()
    expect_true(info$wavelet)
    expect_equal(c(info$LP, info$UP_hours, info$SO), c(30, 24, 12))
    expect_equal(wavelet_tags(), "example")

    # Running again asks first, then replaces with the new settings
    session$setInputs(lower_period_mins = 60, run_all = 2)
    expect_equal(pending_run(), "example")
    session$setInputs(confirm_run = 1)
    expect_equal(tag_info()$LP, 60)

    # Redraw figures from the saved wavelet, keeping its settings
    figure <- file.path(project, "example", "2_Wavelet_Figures", "example_wavelet_spectrum_no.p.png")
    unlink(figure)
    session$setInputs(lower_period_mins = 30, tag_table_rows_selected = 1, redraw_selected = 1)
    expect_true(file.exists(figure))
    expect_equal(tag_info()$LP, 60)
    expect_match(paste(wavelet_log(), collapse = "\n"), "Loading existing wavelet")
  })
})

test_that("suggest_thin_interval() keeps enough records per cycle", {
  expect_equal(suggest_thin_interval(5, 1), 15)
  expect_equal(suggest_thin_interval(20, 1), 60)
  expect_equal(suggest_thin_interval(60, 1), 180)
  # Must also be a multiple of every more finely sampled tag's interval
  expect_equal(suggest_thin_interval(5, c(1, 4)), 12)
  expect_true(is.na(suggest_thin_interval(0.5, 1)))
})

test_that("app thins records before the wavelet", {
  # Runs a whole wavelet: too slow for CRAN's check time, which the tests of
  # thin_archive() cover
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  project <- file.path(tempdir(), "app_project_thin")
  unlink(project, recursive = TRUE)
  dir.create(project)
  tag <- example_tag()
  suppressMessages(import_tag_data(
    tag_ID = tag$tag_ID, tag_deploy_UTC = tag$tag_deploy_UTC, tag_release_UTC = tag$tag_release_UTC,
    archive = tag$archive, time_zone = tag$time_zone, output = TRUE, output_folder = project
  ))

  state <- shiny::reactiveValues(project = project, tags = tag, refresh = 0)
  shiny::testServer(mod_wavelet_server, args = list(state = state), {
    session$setInputs(lower_period_mins = 60, upper_period_hours = 24, suboctaves = 12, thin = FALSE, thin_s = 15)
    # No suggestion until a tag is selected
    expect_true(is.na(suggested_thin()))
    session$setInputs(tag_table_rows_selected = 1)
    expect_equal(suggested_thin(), 180)
    full_bytes <- tag_info()$bytes

    session$setInputs(use_suggestion = 1)
    session$setInputs(thin = TRUE, thin_s = 180)
    expect_equal(tag_info()$wave_sampling_s, 180)
    expect_equal(tag_info()$bytes, full_bytes / 3)
    expect_length(thin_problems(settings(), tag_info())$errors, 0)

    # A thinning interval that is not a multiple of 60 s is refused
    session$setInputs(thin_s = 90)
    expect_match(thin_problems(settings(), tag_info())$errors, "multiple of the sampling interval")
    session$setInputs(run_all = 1)
    expect_false(file.exists(file.path(project, "example", "1_Wavelets", "example_wavelet.rds")))

    session$setInputs(thin_s = 180, run_all = 2)
    expect_match(paste(wavelet_log(), collapse = "\n"), "Thinned from 60 to 180 second sampling")
    expect_equal(tag_info()$wavelet_sampling_s, 180)
    session$setInputs(view_tag = "example")
    expect_match(output$thinned_note$html, "Wavelet thinned to 180 seconds")

    # The imported record keeps its full resolution
    expect_equal(nrow(readRDS(file.path(project, "example", "archive_days.rds"))), 14400)
  })
})

test_that("suggest_n_pcs() keeps eigenvalues of 1 or more and at least 70% variance", {
  eig <- data.frame(eigenvalue = c(5, 2, 1.2, 0.8, 0.5), cumulative = c(50, 70, 82, 90, 95))
  expect_equal(suggest_n_pcs(eig), 3)
  eig$cumulative <- c(40, 55, 65, 75, 95)
  expect_equal(suggest_n_pcs(eig), 4)
})

test_that("app makes daily statistics and runs PCA across tags", {
  # Runs the whole workflow for two tags: too slow for CRAN's check time,
  # which the tests of each step's function cover
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  project <- file.path(tempdir(), "app_project_pca")
  unlink(project, recursive = TRUE)
  dir.create(project)
  tags <- example_tag()
  tags <- rbind(tags, transform(example_tag(), tag_ID = "example2"))
  for (i in seq_len(nrow(tags))) {
    archive <- suppressMessages(import_tag_data(
      tag_ID = tags$tag_ID[i], tag_deploy_UTC = tags$tag_deploy_UTC[i], tag_release_UTC = tags$tag_release_UTC[i],
      archive = tags$archive[i], time_zone = tags$time_zone[i], output = TRUE, output_folder = project
    ))
    utils::capture.output(suppressWarnings(create_wavelet(
      archive, tags$tag_ID[i], lower_period_mins = 60, upper_period_hours = 24, output = TRUE,
      output_folder = project, plot_wavelet = FALSE, interactive_mode = FALSE
    )))
  }
  state <- shiny::reactiveValues(project = project, tags = tags, refresh = 0)

  shiny::testServer(mod_stats_server, args = list(state = state), {
    session$setInputs(diel = TRUE, diel_source = "gps", sunset_type = "civil", sunrise_time = "06:00:00", sunset_time = "18:00:00")
    expect_equal(diel_source(tag_info()), c("GPS file", "GPS file"))
    session$setInputs(run_all = 1)
    expect_match(paste(stats_log(), collapse = "\n"), "example: done")
    expect_equal(tag_info()$wave_days, c(10L, 10L))
    expect_equal(tag_info()$depth_days, c(10L, 10L))

    session$setInputs(view_tag = "example", view_stats = "depth")
    expect_true(file.exists(view_file()))
  })

  shiny::testServer(mod_pca_server, args = list(state = state), {
    session$setInputs(tags = c("example", "example2"), stats = c("power_mean", "power_variance", "amplitude_mean"), standardise = TRUE)
    expect_length(errors(), 0)

    session$setInputs(calc_eigen = 1)
    expect_true(eigen_current())
    eig <- eigen()$eig
    n <- suggest_n_pcs(eig)
    expect_gte(eig$cumulative[n], 70)
    # All components are returned, not only those with eigenvalues of 1 or more
    expect_gt(nrow(eig), sum(eig$eigenvalue >= 1))

    # Changing the settings makes the eigenvalues out of date
    session$setInputs(standardise = FALSE)
    expect_false(eigen_current())
    session$setInputs(standardise = TRUE)

    session$setInputs(n_pcs = 3, run_pca = 1)
    expect_match(pca_log()[1], "PCA with 3 components: done")
    combined <- readRDS(file.path(project, "Combined_tags", "5_k-means", "combined_stats.rds"))
    expect_equal(nrow(combined), 20)
    expect_true(all(c("PC1", "PC2", "PC3") %in% names(combined)))
    expect_true("PC_Eigenvalues_cum_var.png" %in% figure_files())
  })

  shiny::testServer(mod_cluster_server, args = list(state = state), {
    combined <- file.path(project, "Combined_tags")
    expect_equal(unname(sources()), combined)
    session$setInputs(source = combined, standardise = TRUE, seed = 123, max_k = 5, calc_gap = FALSE, k = 3, nstart = 10, polygon = TRUE)

    session$setInputs(plot_k = 1)
    expect_s3_class(k_plot(), "ggplot")

    session$setInputs(run_kmeans = 1)
    expect_match(cluster_log()[1], "k = 3: done")
    expect_s3_class(scene(), "rglscene")
    expect_equal(length(unique(saved_kmeans()$cluster)), 3)
    expect_true(file.exists(variables_file()))
    counts <- cluster_day_counts(saved_kmeans())
    expect_equal(counts$Total, c(10, 10))
  })

  shiny::testServer(mod_results_server, args = list(state = state), {
    session$setInputs(source = file.path(project, "Combined_tags"))
    expect_equal(result_tags(), c("example", "example2"))
    expect_equal(k(), 3)

    session$setInputs(
      tdr_tag = "example", downsample = "minmax", every = 10, x_lim = as.Date(c("2000-01-01", "2000-01-10")),
      y_max = 300, y_step = 50, date_breaks = "2 day", legend = TRUE
    )
    session$setInputs(create_tdr = 1)
    expect_s3_class(tdr_plot(), "ggplot")
    expect_lte(nrow(tdr_plot()$data), 2 * preview_bins)

    session$setInputs(no_days = 1, days_every = 10, days_y_max = 300, days_y_step = 50, diel_shade = TRUE, dpi = 50)
    session$setInputs(create_days = 1)
    expect_match(results_log()[1], "Example days: done \\(")
    expect_match(days_time(), "Last plots took .* \\(with night shading\\)")
    expect_true("Cluster_facet.png" %in% days_files())
  })
})

test_that("diel statistics need a GPS file for every tag in GPS mode", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")

  project <- file.path(tempdir(), "app_project_gps")
  unlink(project, recursive = TRUE)
  dir.create(project)
  tag <- example_tag()
  tag$GPS <- NA_character_
  suppressMessages(import_tag_data(
    tag_ID = tag$tag_ID, tag_deploy_UTC = tag$tag_deploy_UTC, tag_release_UTC = tag$tag_release_UTC,
    archive = tag$archive, time_zone = tag$time_zone, output = TRUE, output_folder = project
  ))
  state <- shiny::reactiveValues(project = project, tags = tag, refresh = 0)

  shiny::testServer(mod_stats_server, args = list(state = state), {
    session$setInputs(diel = TRUE, diel_source = "gps", sunset_type = "civil", sunrise_time = "06:00:00", sunset_time = "18:00:00")
    expect_equal(diel_source(tag_info()), "No GPS file")
    expect_match(diel_problems(tag_info()), "No GPS file for example")
    session$setInputs(run_all = 1)
    expect_true(is.na(tag_info()$depth_days))

    session$setInputs(diel_source = "fixed")
    expect_equal(diel_source(tag_info()), "Fixed times")
    expect_length(diel_problems(tag_info()), 0)
  })
})
