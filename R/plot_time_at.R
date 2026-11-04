#' Plot time at depth or time at temperature
#'
#' `plot_time_at` shows the percentage of time a tagged animal spent in each
#' depth or temperature range, for the whole record, each month, or each day.
#'
#' @details
#' Time is counted as the number of records in each range, so the record should
#' have a constant sampling interval (see [sampling_report()]). Percentages are
#' of the records in each group (the whole record, a month or a day) that have a
#' value. Ranges include their lower limit, and the last range also includes its
#' upper limit.
#'
#' Records outside the \code{breaks} are left out, with a warning.
#'
#' @name plot_time_at
#'
#' @param archive Data frame of time series depth data from
#'   [import_tag_data()]. Time at temperature needs a 'temp' column, i.e. the tag
#'   imported with \code{temp_col}.
#' @param variable Character. "depth" for time at depth, or "temp" for time at
#'   temperature. Defaults to "depth".
#' @param breaks Numerical. Limits of the depth (m) or temperature (degrees C)
#'   ranges, e.g. \code{seq(0, 300, by = 25)}. Defaults to NULL, which chooses
#'   about 12 ranges of round values covering the record.
#' @param by Character. "all" for one histogram of the whole record, "month" for
#'   one histogram per month, or "day" for a heat map of each day. Defaults to
#'   "all".
#' @param plot_size ggsave width and height for saving the plot. Must be numeric,
#'   positive and 2 elements long. Defaults to c(8, 6).
#' @param dpi Numerical. DPI to use for 'ggsave()' output. Defaults to 300.
#' @param output Logical. If TRUE, the plot is saved to \code{output_folder},
#'   named e.g. 'time_at_depth.png' or 'time_at_temp_by_month.png'. Defaults to
#'   FALSE.
#' @param output_folder Output folder path used when \code{output = TRUE}.
#'   Defaults to NULL.
#' @param verbose Logical. If TRUE, progress messages are shown. Defaults to
#'   FALSE.
#'
#' @returns A data frame with one row per group and range: 'group' ("All", the
#'   month, or the date), 'range' (e.g. "0 to 25"), its 'lower' and 'upper'
#'   limits, the number of 'records', the 'hours' they cover, and the 'percent'
#'   of the group's time. The ggplot object is attached as the attribute
#'   'time_at_plot'.
#'
#' @export
#'
#' @examples
#' # Set file path
#' filepath <- system.file("extdata", package = "FishDiveR")
#'
#' # Load archive_days
#' archive_days <- readRDS(file.path(filepath, "data/archive_days.rds"))
#'
#' # Time at depth over the whole record
#' tad <- plot_time_at(archive_days, variable = "depth", breaks = seq(0, 300, by = 25))
#' attr(tad, "time_at_plot")
#' head(tad)
#'
#' # Time at depth each day
#' tad_daily <- plot_time_at(archive_days, by = "day")
#' attr(tad_daily, "time_at_plot")
#'
plot_time_at <- function(archive,
                         variable = "depth",
                         breaks = NULL,
                         by = "all",
                         plot_size = c(8, 6),
                         dpi = 300,
                         output = FALSE,
                         output_folder = NULL,
                         verbose = FALSE) {
  if (!is.data.frame(archive) || !"date" %in% names(archive) || !inherits(archive$date, "POSIXct")) {
    stop("archive must be a data frame with a 'date' column of type POSIXct.")
  }
  if (!is.character(variable) || length(variable) != 1 || !variable %in% c("depth", "temp")) {
    stop("variable must be 'depth' or 'temp'.")
  }
  if (!variable %in% names(archive) || all(is.na(archive[[variable]]))) {
    stop(if (variable == "temp") {
      "archive has no temperature data: import the tag with temp_col in import_tag_data()."
    } else {
      "archive has no depth data."
    })
  }
  if (!is.character(by) || length(by) != 1 || !by %in% c("all", "month", "day")) {
    stop("by must be 'all', 'month' or 'day'.")
  }
  if (!is.null(breaks) && (!is.numeric(breaks) || length(breaks) < 2 || anyNA(breaks) || any(diff(breaks) <= 0))) {
    stop("breaks must be NULL or at least two increasing numbers.")
  }
  if (!is.numeric(plot_size) || length(plot_size) != 2 || any(plot_size <= 0)) {
    stop("plot_size must be a numeric vector of length 2 with positive values.")
  }
  if (!is.numeric(dpi) || dpi <= 0) {
    stop("dpi must be a positive number.")
  }
  if (isTRUE(output) && is.null(output_folder)) {
    stop("When output = TRUE, output_folder must be provided.")
  }

  archive <- as.data.frame(archive)
  values <- archive[[variable]]
  has_value <- !is.na(values)

  if (is.null(breaks)) breaks <- time_at_breaks(values, variable)

  range_labels <- paste(utils::head(breaks, -1), "to", breaks[-1])
  value_range <- cut(values, breaks = breaks, labels = range_labels, right = FALSE, include.lowest = TRUE)
  outside <- sum(has_value & is.na(value_range))
  if (outside > 0) {
    warning(
      signif(100 * outside / sum(has_value), 3), "% of records with a ", if (variable == "temp") "temperature" else "depth",
      " are outside the breaks and were left out.",
      call. = FALSE
    )
  }

  # Group by the whole record, month or day, in the record's time zone
  time_zone <- attr(archive, "time_zone")
  if (is.null(time_zone)) time_zone <- "UTC"
  day <- if ("date_only" %in% names(archive)) archive$date_only else as.Date(archive$date, tz = time_zone)
  # Label each distinct day once rather than every record, for speed
  days <- sort(unique(day))
  day_index <- match(day, days)
  group <- switch(by,
    all = factor(rep("All", nrow(archive))),
    month = {
      month_labels <- format(days, "%b %Y")
      factor(month_labels[day_index], levels = unique(month_labels))
    },
    day = factor(format(days)[day_index], levels = format(days))
  )

  # Records in each group and range, as a percentage of each group's records
  counts <- table(group = group, range = value_range)
  in_group <- rowSums(counts)
  sampling_s <- stats::median(diff(as.numeric(archive$date)))
  time_at <- data.frame(
    group = rep(rownames(counts), times = ncol(counts)),
    range = factor(rep(colnames(counts), each = nrow(counts)), levels = range_labels),
    lower = rep(utils::head(breaks, -1), each = nrow(counts)),
    upper = rep(breaks[-1], each = nrow(counts)),
    records = as.vector(counts),
    stringsAsFactors = FALSE
  )
  time_at$hours <- time_at$records * sampling_s / 3600
  time_at$percent <- 100 * time_at$records / rep(in_group, times = ncol(counts))
  time_at$percent[is.nan(time_at$percent)] <- 0
  time_at$group <- factor(time_at$group, levels = levels(group))
  time_at <- time_at[order(time_at$group, time_at$range), ]
  rownames(time_at) <- NULL

  # Shallowest depth at the top; warmest temperature at the top
  range_order <- if (variable == "depth") rev(range_labels) else range_labels
  value_label <- if (variable == "depth") "Depth (m)" else temperature_label

  if (by == "day") {
    plot_data <- time_at
    plot_data$group <- as.Date(as.character(plot_data$group))
    # Leave ranges with no time blank
    plot_data$percent[plot_data$percent == 0] <- NA
    time_at_plot <- ggplot(plot_data, aes(x = group, y = range, fill = percent)) +
      geom_tile() +
      scale_x_date(expand = c(0, 0)) +
      scale_y_discrete(limits = range_order) +
      scale_fill_viridis_c(name = "Time (%)", option = "magma", direction = -1, na.value = "transparent") +
      labs(x = "Date", y = value_label)
  } else {
    plot_data <- time_at
    plot_data$midpoint <- (plot_data$lower + plot_data$upper) / 2
    time_at_plot <- ggplot(plot_data, aes(x = percent, y = range)) +
      scale_y_discrete(limits = range_order) +
      scale_x_continuous(expand = c(0, 0, 0.05, 0)) +
      labs(x = "Time (%)", y = value_label)
    time_at_plot <- if (variable == "temp") {
      # Outlined, as the middle of the colour scale is pale yellow
      time_at_plot + geom_col(aes(fill = midpoint), width = 0.9, colour = "grey40", linewidth = 0.2) +
        temperature_scale("fill", guide = "none")
    } else {
      time_at_plot + geom_col(fill = "steelblue4", width = 0.9)
    }
    if (by == "month") {
      time_at_plot <- time_at_plot + facet_wrap(~group)
    }
  }

  time_at_plot <- time_at_plot +
    theme_classic() +
    theme(
      axis.text = element_text(size = 12, colour = "black"),
      # Smaller range labels to fit the smaller panels of each month
      axis.text.y = element_text(size = if (by == "month") 8 else 12),
      axis.title = element_text(size = 14),
      strip.text = element_text(size = 12),
      strip.background = element_blank()
    )

  if (isTRUE(verbose) && interactive()) {
    print(time_at_plot)
  }

  if (isTRUE(output)) {
    file <- file.path(output_folder, time_at_file(variable, by))
    ggplot2::ggsave(file, plot = time_at_plot, width = plot_size[1], height = plot_size[2], dpi = dpi, create.dir = TRUE)
    if (verbose) message("Output file: ", file)
  }

  attr(time_at, "time_at_plot") <- time_at_plot
  time_at
}

#' Default plot_time_at() breaks: about 12 ranges of round values
#'
#' The ranges span the middle 99.8% of values (from 0 for depth), so a few
#' extreme records don't widen them, with one wider range at each end to hold
#' any records beyond.
#' @noRd
time_at_breaks <- function(values, variable) {
  values <- values[!is.na(values)]
  limits <- stats::quantile(values, c(0.001, 0.999), names = FALSE)
  if (variable == "depth") limits[1] <- 0
  breaks <- pretty(limits, n = 12)
  if (length(breaks) < 2) {
    return(c(floor(min(values)), floor(min(values)) + 1))
  }
  step <- diff(breaks[1:2])
  if (min(values) < breaks[1]) breaks <- c(floor(min(values) / step) * step, breaks)
  if (max(values) > max(breaks)) breaks <- c(breaks, ceiling(max(values) / step) * step)
  breaks
}

#' File name for a plot_time_at() figure, e.g. "time_at_temp_by_month.png"
#' @noRd
time_at_file <- function(variable, by) {
  paste0("time_at_", variable, if (by != "all") paste0("_by_", by), ".png")
}
