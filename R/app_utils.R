# Helpers for the FishDiveR app (run_app). These do not use shiny, so they can
# be tested directly.

# Columns of the tag table. One row per tag, saved as tag_metadata.csv in the
# project folder. Names match the import_tag_data() arguments.
tag_metadata_columns <- c(
  "tag_ID", "archive", "tag_deploy_UTC", "tag_release_UTC",
  "date_time_col", "depth_col", "temp_col", "time_zone", "GPS"
)

#' Empty tag table
#' @noRd
empty_tag_metadata <- function() {
  data.frame(
    tag_ID = character(),
    archive = character(),
    tag_deploy_UTC = character(),
    tag_release_UTC = character(),
    date_time_col = integer(),
    depth_col = integer(),
    temp_col = integer(),
    time_zone = character(),
    GPS = character(),
    stringsAsFactors = FALSE
  )
}

#' Read a tag table from csv
#'
#' Required columns are tag_ID, archive, tag_deploy_UTC, tag_release_UTC and
#' time_zone. date_time_col, depth_col, temp_col and GPS are optional and take
#' the import_tag_data() defaults when missing.
#' @noRd
read_tag_metadata <- function(file) {
  if (!file.exists(file)) {
    return(empty_tag_metadata())
  }

  meta <- utils::read.csv(file, colClasses = "character", na.strings = c("", "NA"), check.names = FALSE)

  required <- c("tag_ID", "archive", "tag_deploy_UTC", "tag_release_UTC", "time_zone")
  missing_cols <- setdiff(required, names(meta))
  if (length(missing_cols) > 0) {
    stop("Tag table is missing column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # Fill optional columns with defaults
  if (!"date_time_col" %in% names(meta)) meta$date_time_col <- "1"
  if (!"depth_col" %in% names(meta)) meta$depth_col <- "2"
  if (!"temp_col" %in% names(meta)) meta$temp_col <- NA_character_
  if (!"GPS" %in% names(meta)) meta$GPS <- NA_character_

  meta <- meta[, tag_metadata_columns, drop = FALSE]
  meta$tag_ID <- trimws(meta$tag_ID)
  meta$date_time_col <- suppressWarnings(as.integer(meta$date_time_col))
  meta$depth_col <- suppressWarnings(as.integer(meta$depth_col))
  meta$temp_col <- suppressWarnings(as.integer(meta$temp_col))
  meta <- meta[!is.na(meta$tag_ID) & nzchar(meta$tag_ID), , drop = FALSE]
  rownames(meta) <- NULL

  meta
}

#' Write a tag table to csv
#' @noRd
write_tag_metadata <- function(meta, file) {
  utils::write.csv(meta[, tag_metadata_columns, drop = FALSE], file = file, row.names = FALSE, na = "")
  invisible(file)
}

#' Add or replace tags in a tag table, matching on tag_ID
#' @noRd
upsert_tags <- function(meta, new_tags) {
  meta <- meta[!meta$tag_ID %in% new_tags$tag_ID, , drop = FALSE]
  meta <- rbind(meta, new_tags[, tag_metadata_columns, drop = FALSE])
  rownames(meta) <- NULL
  meta
}

#' Check one row of the tag table
#'
#' @returns Character vector of problems. Empty if the tag is ready to import.
#' @noRd
validate_tag <- function(tag) {
  problems <- character()
  is_blank <- function(x) length(x) == 0 || is.na(x) || !nzchar(trimws(x))
  is_col <- function(x) !is.na(x) && x > 0

  if (is_blank(tag$tag_ID)) problems <- c(problems, "tag ID is missing")
  if (is_blank(tag$archive)) {
    problems <- c(problems, "archive file is missing")
  } else if (!file.exists(tag$archive)) {
    problems <- c(problems, "archive file not found")
  }

  parse_utc <- function(x) {
    if (is_blank(x)) return(NA)
    as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }
  deploy <- parse_utc(tag$tag_deploy_UTC)
  release <- parse_utc(tag$tag_release_UTC)
  if (is.na(deploy)) problems <- c(problems, "deploy time must be 'yyyy-mm-dd hh:mm:ss'")
  if (is.na(release)) problems <- c(problems, "release time must be 'yyyy-mm-dd hh:mm:ss'")
  if (!is.na(deploy) && !is.na(release) && release <= deploy) {
    problems <- c(problems, "release time must be after deploy time")
  }

  if (is_blank(tag$time_zone) || !tag$time_zone %in% OlsonNames()) {
    problems <- c(problems, "time zone not recognised")
  }
  if (!is_col(tag$date_time_col)) problems <- c(problems, "date-time column must be a positive number")
  if (!is_col(tag$depth_col)) problems <- c(problems, "depth column must be a positive number")
  if (!is.na(tag$temp_col) && tag$temp_col <= 0) problems <- c(problems, "temperature column must be blank or a positive number")
  if (!is_blank(tag$GPS) && !file.exists(tag$GPS)) problems <- c(problems, "GPS file not found")

  problems
}

#' Which pipeline outputs exist for each tag in the project folder
#' @noRd
tag_status <- function(meta, project_folder) {
  tags <- meta$tag_ID
  tag_file <- function(...) file.exists(file.path(project_folder, tags, ...))

  imported <- tag_file("archive_days.rds")
  data.frame(
    tag_ID = tags,
    Imported = imported,
    Settings_changed = vapply(seq_along(tags), function(i) {
      if (imported[i]) paste(changed_import_settings(meta[i, , drop = FALSE], project_folder), collapse = ", ") else ""
    }, character(1)),
    Wavelet = file.exists(file.path(project_folder, tags, "1_Wavelets", paste0(tags, "_wavelet.rds"))),
    Daily_stats = file.exists(file.path(project_folder, tags, "3_Stats", paste0(tags, "_waveStats.csv"))) &
      file.exists(file.path(project_folder, tags, "3_Stats", paste0(tags, "_depthStats.csv"))),
    stringsAsFactors = FALSE
  )
}

# Names of the tag settings that import_tag_data() saves in import_settings.rds
import_setting_names <- c(
  archive = "archive", tag_deploy_UTC = "deployment time", tag_release_UTC = "release time",
  date_time_col = "date-time column", depth_col = "depth column", temp_col = "temperature column",
  time_zone = "time zone"
)

#' Tag settings that differ from what an imported archive shows
#'
#' For archives with no record of the settings used: a temperature column set
#' for an archive without temperature (or the reverse), or a different time
#' zone. Dates are not compared, as a record can rightly start after deployment.
#' @noRd
changed_from_archive <- function(tag, archive_file) {
  summary <- archive_summary(archive_file)
  temp_col <- tag$temp_col
  has_temp_col <- !(length(temp_col) == 0 || is.na(temp_col) || !nzchar(trimws(temp_col)))
  changed <- c(
    temp_col = has_temp_col != summary$has_temp_col,
    time_zone = !is.na(summary$time_zone) && !identical(as.character(tag$time_zone), summary$time_zone)
  )
  unname(import_setting_names[names(changed)[changed]])
}

#' Tag settings that differ from those the tag was imported with
#'
#' Compares a tag's settings with the import_settings.rds saved by
#' import_tag_data() next to its archive_days.rds. If the settings used are
#' unknown (imported before they were saved, or the archive was replaced since),
#' checks what the archive itself shows: whether it has temperature, and its
#' time zone.
#'
#' @returns The names of the changed settings, or character(0) if none changed.
#' @noRd
changed_import_settings <- function(tag, project_folder) {
  settings_file <- file.path(project_folder, tag$tag_ID, "import_settings.rds")
  archive_file <- file.path(project_folder, tag$tag_ID, "archive_days.rds")
  if (!file.exists(archive_file)) {
    return(character())
  }
  # Both files are written together: a newer archive came from elsewhere
  if (!file.exists(settings_file) ||
    difftime(file.mtime(archive_file), file.mtime(settings_file), units = "secs") > 60) {
    return(changed_from_archive(tag, archive_file))
  }
  used <- readRDS(settings_file)

  is_blank <- function(x) length(x) == 0 || is.na(x) || !nzchar(trimws(x))
  as_path <- function(x) {
    if (is_blank(x)) return("")
    path <- normalizePath(x, winslash = "/", mustWork = FALSE)
    # Windows file names ignore case
    if (.Platform$OS.type == "windows") tolower(path) else path
  }
  as_time <- function(x) {
    if (is_blank(x)) return(NA_real_)
    t <- tryCatch(
      as.POSIXct(x, tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M", "%Y-%m-%d")),
      error = function(e) NA
    )
    as.numeric(t)
  }
  as_col <- function(x) if (is_blank(x)) NA_integer_ else suppressWarnings(as.integer(x))
  same <- function(a, b) identical(a, b)

  changed <- c(
    archive = !same(as_path(tag$archive), as_path(used$archive)),
    tag_deploy_UTC = !same(as_time(tag$tag_deploy_UTC), as_time(used$tag_deploy_UTC)),
    tag_release_UTC = !same(as_time(tag$tag_release_UTC), as_time(used$tag_release_UTC)),
    date_time_col = !same(as_col(tag$date_time_col), as_col(used$date_time_col)),
    depth_col = !same(as_col(tag$depth_col), as_col(used$depth_col)),
    temp_col = !same(as_col(tag$temp_col), as_col(used$temp_col)),
    time_zone = !same(as.character(tag$time_zone), as.character(used$time_zone))
  )
  unname(import_setting_names[names(changed)[changed]])
}

#' Evaluate an expression, collecting its messages, warnings and any error
#'
#' @returns A list with the value, a character log, and ok = FALSE if an error occurred.
#' @noRd
run_logged <- function(expr) {
  log <- character()
  ok <- TRUE

  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) {
      ok <<- FALSE
      log <<- c(log, paste("Error:", conditionMessage(e)))
      NULL
    }),
    message = function(m) {
      log <<- c(log, sub("\\s+$", "", conditionMessage(m)))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      log <<- c(log, paste("Warning:", conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )

  list(value = value, log = log[nzchar(log)], ok = ok)
}

#' Open a file or folder dialog on the user's computer
#'
#' Uses a tcltk dialog held in front of other windows (so it is not hidden
#' behind the browser), falling back to RStudio's dialogs. The option
#' FishDiveR.choose_path can supply a replacement function, used in tests.
#'
#' @param start_dir Folder the dialog opens in. Ignored if NULL or missing.
#' @returns The chosen path, or NULL if cancelled or no dialog is available.
#' @noRd
choose_path <- function(type = c("file", "dir"), caption = "Select", start_dir = NULL) {
  type <- match.arg(type)
  if (!is.null(start_dir) && !dir.exists(start_dir)) start_dir <- NULL

  path <- tryCatch(
    {
      chooser <- getOption("FishDiveR.choose_path")
      if (is.function(chooser)) {
        chooser(type, caption, start_dir)
      } else if (isTRUE(capabilities("tcltk")) && requireNamespace("tcltk", quietly = TRUE)) {
        choose_path_tcltk(type, caption, start_dir)
      } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        start <- if (is.null(start_dir)) rstudioapi::getActiveProject() else start_dir
        if (type == "file") rstudioapi::selectFile(caption = caption, path = start) else rstudioapi::selectDirectory(caption = caption, path = start)
      } else {
        NULL
      }
    },
    error = function(e) NULL
  )

  if (length(path) == 0 || is.na(path[1]) || !nzchar(path[1])) {
    return(NULL)
  }
  normalizePath(path[1], winslash = "/", mustWork = FALSE)
}

#' tcltk file or folder dialog owned by an always-on-top window
#' @noRd
choose_path_tcltk <- function(type, caption, start_dir) {
  # Small always-on-top owner window, so the dialog opens in front of the browser
  owner <- tcltk::tktoplevel()
  on.exit(tcltk::tkdestroy(owner))
  tcltk::tkwm.title(owner, "FishDiveR")
  tcltk::tkwm.geometry(owner, "1x1+200+200")
  tcltk::tcl("wm", "attributes", owner, "-topmost", 1)
  tcltk::tkraise(owner)
  tcltk::tkfocus("-force", owner)

  args <- list(parent = owner, title = caption)
  if (!is.null(start_dir)) args$initialdir <- start_dir

  path <- if (type == "dir") {
    do.call(tcltk::tkchooseDirectory, args)
  } else {
    do.call(tcltk::tkgetOpenFile, c(args, filetypes = "{{CSV files} {.csv}} {{All files} *}"))
  }
  tcltk::tclvalue(path)
}

#' Tag table row for the example data shipped with FishDiveR
#' @noRd
example_tag <- function() {
  data.frame(
    tag_ID = "example",
    archive = system.file("extdata", "data", "data-Archive.csv", package = "FishDiveR"),
    tag_deploy_UTC = "2000-01-01 00:00:00",
    tag_release_UTC = "2000-01-11 23:59:00",
    date_time_col = 1L,
    depth_col = 2L,
    temp_col = NA_integer_,
    time_zone = "UTC",
    GPS = system.file("extdata", "data", "GPS.csv", package = "FishDiveR"),
    stringsAsFactors = FALSE
  )
}

#' Sensible ggplot2 date_breaks for a record of n_days
#' @noRd
default_date_breaks <- function(n_days) {
  if (n_days <= 7) {
    "1 day"
  } else if (n_days <= 21) {
    "2 day"
  } else if (n_days <= 60) {
    "7 day"
  } else if (n_days <= 180) {
    "14 day"
  } else if (n_days <= 400) {
    "1 month"
  } else {
    "2 month"
  }
}

#' Number of wavelet periods for the create_wavelet() settings
#' @noRd
wavelet_periods <- function(lower_period_mins, upper_period_hours, suboctaves) {
  lower_days <- lower_period_mins / 1440
  upper_days <- upper_period_hours / 24
  floor(log2(upper_days / lower_days) * suboctaves) + 1
}

#' Approximate memory of a wavelet object in bytes
#'
#' WaveletComp stores the complex wave (16 bytes) and the phase, amplitude,
#' power and ridge matrices (8 bytes each) for every period and record.
#' @noRd
wavelet_bytes <- function(n_records, lower_period_mins, upper_period_hours, suboctaves) {
  n_records * wavelet_periods(lower_period_mins, upper_period_hours, suboctaves) * 48
}

# Thinning intervals offered as suggestions, in seconds. All divide a day evenly.
thin_interval_choices <- c(2, 3, 4, 5, 6, 10, 12, 15, 20, 30, 60, 120, 180, 240, 300, 600)

# Records per cycle of the lower period to keep when thinning
thin_records_per_cycle <- 20

#' Suggested thinning interval for a wavelet lower period
#'
#' The longest interval in thin_interval_choices that keeps
#' thin_records_per_cycle records per cycle of the lower period and is a
#' multiple of the sampling interval of every tag sampled more finely.
#' @returns Seconds, or NA if no interval fits.
#' @noRd
suggest_thin_interval <- function(lower_period_mins, sampling_s) {
  max_s <- lower_period_mins * 60 / thin_records_per_cycle
  ok <- thin_interval_choices[thin_interval_choices <= max_s]
  ok <- ok[vapply(ok, function(x) all(sampling_s >= x | x %% sampling_s == 0), logical(1))]
  if (length(ok) == 0) NA_real_ else max(ok)
}

#' Seconds as a short human-readable duration
#' @noRd
format_duration <- function(secs) {
  if (secs < 1) {
    "under 1 s"
  } else if (secs < 60) {
    paste(round(secs), "s")
  } else {
    paste(round(secs / 60, 1), "min")
  }
}

#' Bytes as a short human-readable string
#' @noRd
format_bytes <- function(bytes) {
  units <- c("B", "KB", "MB", "GB", "TB")
  i <- pmax(1, pmin(length(units), floor(log(pmax(bytes, 1), 1024)) + 1))
  paste(signif(bytes / 1024^(i - 1), 2), units[i])
}

# Archive summaries, cached by file path and modification time
archive_summary_cache <- new.env(parent = emptyenv())

#' Number of records, sampling interval, maximum depth and date range of an
#' imported archive
#' @noRd
archive_summary <- function(file) {
  key <- paste(file, file.mtime(file))
  if (!is.null(archive_summary_cache[[key]])) {
    return(archive_summary_cache[[key]])
  }
  archive <- readRDS(file)
  report <- sampling_report(archive)
  summary <- list(
    n_records = nrow(archive),
    sampling_s = report$interval_s,
    max_depth = max(archive$depth),
    dates = range(archive$date_only),
    sampling = report,
    has_temp_col = "temp" %in% names(archive),
    has_temp = has_temperature(archive),
    time_zone = if (is.null(attr(archive, "time_zone"))) NA_character_ else attr(archive, "time_zone")
  )
  archive_summary_cache[[key]] <- summary
  summary
}

#' One-line summary of a sampling_report(), e.g. "Irregular: 2 gaps, 1 duplicate"
#' @noRd
sampling_status <- function(report) {
  if (report$regular) {
    return(paste0("Regular (", report$interval_s, " s)"))
  }
  counts <- c(
    duplicate = nrow(report$duplicates),
    `out of order` = nrow(report$out_of_order),
    `rate change` = nrow(report$rate_changes),
    gap = nrow(report$gaps),
    `short interval` = nrow(report$short)
  )
  counts <- counts[counts > 0]
  paste0("Irregular: ", paste0(counts, " ", names(counts), ifelse(counts > 1, "s", ""), collapse = ", "))
}

#' Wavelet settings (LP in minutes, UP in days, SO) saved by create_wavelet()
#' @returns The settings list, or NULL if the tag has no wavelet.
#' @noRd
read_wavelet_meta <- function(project_folder, tag_ID) {
  file <- file.path(project_folder, tag_ID, "1_Wavelets", "wavelet_meta.rds")
  if (!file.exists(file)) {
    return(NULL)
  }
  readRDS(file)
}

#' Total memory of this computer in bytes, or NULL if unknown
#' @noRd
total_ram_bytes <- function() {
  bytes <- tryCatch(
    {
      sysname <- Sys.info()[["sysname"]]
      if (sysname == "Windows") {
        out <- system2("powershell", c("-NoProfile", "-Command", "(Get-CimInstance Win32_ComputerSystem).TotalPhysicalMemory"), stdout = TRUE, stderr = FALSE)
        as.numeric(out[nzchar(out)][1])
      } else if (sysname == "Darwin") {
        as.numeric(system2("sysctl", c("-n", "hw.memsize"), stdout = TRUE, stderr = FALSE)[1])
      } else {
        meminfo <- readLines("/proc/meminfo", n = 1)
        as.numeric(gsub("[^0-9]", "", meminfo)) * 1024
      }
    },
    error = function(e) NA_real_,
    warning = function(w) NA_real_
  )
  if (length(bytes) != 1 || is.na(bytes) || bytes <= 0) NULL else bytes
}

#' PCA results ready to cluster in a project
#'
#' combine_data() saves combined_stats.rds under Combined_tags for several
#' tags, or under the tag's own folder for one.
#' @returns Named character vector of result folders, named with the tags in
#'   each. Empty if none.
#' @noRd
kmeans_sources <- function(project_folder, tag_IDs) {
  folders <- file.path(project_folder, c("Combined_tags", tag_IDs))
  folders <- folders[file.exists(file.path(folders, "5_k-means", "combined_stats.rds"))]
  labels <- vapply(folders, function(folder) {
    tags <- unique(as.character(readRDS(file.path(folder, "5_k-means", "combined_stats.rds"))$tag_ID))
    if (basename(folder) == "Combined_tags") paste0("Tags together: ", paste(tags, collapse = ", ")) else paste("Tag", tags)
  }, character(1))
  stats::setNames(folders, labels)
}

#' Rounded maximum depth and tick step for a TDR Y axis
#' @noRd
default_depth_axis <- function(max_depth) {
  step <- if (max_depth <= 100) 10 else if (max_depth <= 500) 50 else 100
  c(max = ceiling(max_depth / step) * step, step = step)
}

#' Whether an imported archive has temperature data
#' @noRd
has_temperature <- function(archive) "temp" %in% names(archive) && any(!is.na(archive$temp))

# Shown when a plot needs temperature but the tag was imported without it
no_temperature_message <- "This tag has no temperature data. Set its temperature column in step 1, then re-import it."

#' plot_time_at() breaks of a given width covering all values (from 0 for depth)
#' @noRd
width_breaks <- function(values, width, variable) {
  values <- values[!is.na(values)]
  lower <- if (variable == "depth") 0 else floor(min(values) / width) * width
  upper <- max(ceiling(max(values) / width) * width, lower + width)
  seq(lower, upper, by = width)
}

#' Whether a tag's imported archive has temperature data, or NA if not imported
#'
#' Read from the import_settings.rds saved with the archive, or else from the
#' archive itself.
#' @noRd
imported_temperature <- function(tag, project_folder) {
  archive_file <- file.path(project_folder, tag$tag_ID, "archive_days.rds")
  settings_file <- file.path(project_folder, tag$tag_ID, "import_settings.rds")
  if (!file.exists(archive_file)) {
    return(NA)
  }
  if (file.exists(settings_file) &&
    difftime(file.mtime(archive_file), file.mtime(settings_file), units = "secs") <= 60) {
    temp_col <- readRDS(settings_file)$temp_col
    return(!(length(temp_col) == 0 || is.na(temp_col)))
  }
  archive_summary(archive_file)$has_temp
}

# Tick shown in tables, e.g. for tags with temperature data
tick_mark <- intToUtf8(10003)
