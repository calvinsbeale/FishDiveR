#' Create and plot the wavelet power spectrum
#'
#' `create_wavelet` creates the a wavelet spectrum using WaveletComp package.
#' Skips creation if my.w is already in the global environment.
#'
#' @details Uses [WaveletComp::analyze.wavelet()] to create a univariate wavelet
#'   power spectrum for the depth data imported, see
#'   [WaveletComp::analyze.wavelet()] for more details. Plots mean wavelet power
#'   using [WaveletComp::wt.avg()]. If you have errors allocating large vectors
#'   try using library(bigmemory) and create a big matrix with
#'   big_mat <- big.matrix(nrow = 1e7, ncol = 10, type = "double") then run
#'   your code again. This allows greater range between lower and upper periods
#'
#'
#' @name create_wavelet
#'
#' @importFrom WaveletComp analyze.wavelet wt.image
#' @importFrom WaveletComp wt.avg
#' @importFrom grDevices png dev.off
#'
#' @inheritParams import_tag_data
#' @param archive Data frame containing processed time series depth data
#' @param wv_period_hours Time resolution in hours to calculate wavelet.
#'   Currently only supports the default of 24 hours as this package is created
#'   to investigate daily diving behaviour. Defaults to 24.
#' @param sampling_frequency Sampling frequency of depth data in seconds. Defaults
#'   to time between first and second depth record. Recommended to leave blank.
#' @param suboctaves number of suboctaves between each logarithmic period. E.g.
#'   between 24 and 12 hours. Highly recommended to use 12, for easy of
#'   interpretation of hours and signal present (daily, diel, tidal).
#' @param lower_period_mins Lower period of the wavelet sampling in minutes.
#'   Cannot be less than sampling frequency. Defaults to 5 minutes.
#' @param upper_period_hours Upper period of the wavelet sampling in days.
#'   Defaults to 24 hours.
#' @param pval Produce p-values or not. True or False. Default set to FALSE, see
#'   [WaveletComp::analyze.wavelet()] for further details. P-values not used in
#'   further analysis, and increase computation time and file size.
#' @param plot_wavelet TRUE or FALSE. Plot the wavelet spectrum and mean power?
#' @param max_period_ticks Number of ticks displayed on the period (y) axis in
#'   plots.
#' @param plot_width Width of the wavelet spectrum plot output. Defaults to 800.
#' @param plot_height Height of the wavelet spectrum plot output. Defaults to
#'   400.
#' @param interactive_mode Used for testing the package only. Defaults to TRUE.
#'
#' @returns An object of class "analyze.wavelet" from package 'WaveletComp'.
#'   Additionally outputs a plot of the wavelet spectrum, and a plot of the mean
#'   power per period
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
#' # Run create_wavelet function
#' my.w <- create_wavelet(
#'   archive = archive_days,
#'   tag_ID = "data",
#'   wv_period_hours = 24,
#'   sampling_frequency = NULL,
#'   suboctaves = 12,
#'   lower_period_mins = 30,
#'   upper_period_hours = 24,
#'   pval = FALSE,
#'   output_folder = tempdir(),
#'   plot_wavelet = FALSE,
#'   max_period_ticks = 10,
#'   plot_width = 800,
#'   plot_height = 400,
#'   interactive_mode = FALSE
#' )
#'
# Function to create a 'my.w' WaveletComp object
create_wavelet <- function(archive,
                           tag_ID,
                           wv_period_hours = 24,
                           sampling_frequency = NULL,
                           suboctaves = 12,
                           lower_period_mins = 5,
                           upper_period_hours = 24,
                           pval = FALSE,
                           output_folder = data_dir,
                           plot_wavelet = TRUE,
                           max_period_ticks = 10,
                           plot_width = 800,
                           plot_height = 400,
                           interactive_mode = TRUE) {
  # Tag info:
  cat(paste0("\nAnalysing tag ID ", tag_ID, "\n"))

  # 1. Check if archive is a data frame
  if (!is.data.frame(archive)) {
    stop("archive must be a data frame.")
  }
  # 1b. Check for the existence of required columns
  required_cols <- c("date", "depth", "date_only")
  if (!all(required_cols %in% names(archive))) {
    stop("archive is missing one or more required columns: 'date', 'depth', 'date_only'.")
  }
  # 1c. Check column types
  if (!inherits(archive$date, "POSIXct")) {
    stop("The 'date' column in archive must be of type POSIXct.")
  }
  if (!is.numeric(archive$depth)) {
    stop("The 'depth' column in archive must be numeric.")
  }
  if (!inherits(archive$date_only, "Date")) {
    stop("The 'date_only' column in archive must be of type Date.")
  }
  # 1d. Conditionally check the 'temp' column if it exists
  if ("temp" %in% names(archive) && !is.numeric(archive$temp)) {
    stop("The 'temp' column in archive, if present, must be numeric.")
  }
  # 1e. Check for time zone attribute
  if (is.null(attr(archive, "time_zone"))) {
    stop("archive does not have a 'time_zone' attribute. It is required to process days correctly.")
  } else {
    # Further checks to ensure the time_zone is valid or matches expected values
    expected_time_zones <- OlsonNames()
    if (!(attr(archive, "time_zone") %in% expected_time_zones)) {
      warning("The 'time_zone' attribute of archive is not a recognized R time zone.")
    }
  }
  # Check range of periods
  if (upper_period_hours == 24 && lower_period_mins < 5 && suboctaves == 12) {
    cat("Using an upper period of 24 hours and lower period of less than 5 minutes can result in a long processing time and very large wavelet file (>20 GB).
        Some computers will not be able to render the power spectrum plot at default size and resolution. If receiving vector allocation errors,
        see details in ?FishDiveR::create_wavelet \n")
  }

  # 2. Check other inputs
  # Validate 'tag_ID' (Must be a single character string)
  if (!is.character(tag_ID) || length(tag_ID) != 1) {
    stop("tag_ID must be a single character string.")
  }

  # Validate 'wv_period_hours' (Must be a whole number)
  if (!is.numeric(wv_period_hours) || wv_period_hours != as.integer(wv_period_hours) || wv_period_hours != 24) {
    stop("The package currently only supports 24 hours for wv_period_hours.")
  }

  # Validate 'sampling_frequency' (Can be NULL or a positive number)
  if (!is.null(sampling_frequency) && (!is.numeric(sampling_frequency) || sampling_frequency <= 0)) {
    stop("sampling_frequency must be NULL or a positive number.")
  }

  # Validate 'suboctaves' (Must be a whole number)
  if (!is.numeric(suboctaves) || suboctaves != as.integer(suboctaves)) {
    stop("suboctaves must be a whole number.")
  }

  # Validate 'lower_period_mins' & 'upper_period_hours' (Can be decimal, but must be positive)
  if (!is.numeric(lower_period_mins) || lower_period_mins <= 0) {
    stop("lower_period_mins must be a positive number.")
  }
  if (!is.numeric(upper_period_hours) || upper_period_hours <= 0) {
    stop("upper_period_hours must be a positive number.")
  }

  # Validate logical parameters 'pval', 'plot_wavelet', 'interactive_mode'
  if (!is.logical(pval)) {
    stop("pval must be TRUE or FALSE.")
  }
  if (!is.logical(plot_wavelet)) {
    stop("plot_wavelet must be TRUE or FALSE.")
  }
  # Validate numeric parameters 'max_period_ticks', 'plot_width', 'plot_height'
  if (!is.numeric(max_period_ticks) || max_period_ticks <= 0) {
    stop("max_period_ticks must be a positive number.")
  }
  if (!is.numeric(plot_width) || plot_width <= 0) {
    stop("plot_width must be a positive number.")
  }
  if (!is.numeric(plot_height) || plot_height <= 0) {
    stop("plot_height must be a positive number.")
  }
  if (!is.logical(interactive_mode)) {
    stop("interactive_mode must be TRUE or FALSE.")
  }

  # Helper function to handle readline based on interactive mode. Used for testing package functions
  get_response <- function(prompt, default_response, interactive_mode) {
    if (interactive_mode) {
      return(tolower(readline(prompt = prompt)))
    } else {
      return(default_response)
    }
  }

  # Helper function to check constant sampling frequency
  check_sampling_frequency <- function(archive) {
    # Create sample_length metric for each consecutive set of time records
    archive$sample_length <- c(NA, as.numeric(diff(archive$date), units = "secs"))

    # Warn user if sampling frequency is not constant
    if (length(unique(archive$sample_length)) != 2) {
      message("\n Warning: Sampling frequency is not constant over tag deployment.")
      filtered_archive_days <- archive[-1, ]
      time_frequency_table <- table(filtered_archive_days$sample_length)
      time_frequency_df <- as.data.frame(time_frequency_table)
      names(time_frequency_df) <- c("Time_frequency_in_Seconds", "Number_of_Records")
      print(time_frequency_df)

      # Ask user for input. Require the correct input
      while (TRUE) {
        # Set readline input for package testing purposes - Replaced with get_response
        response <- get_response(
          prompt = "Do you want to continue creating a new wavelet? (Yes/No): ",
          default_response = "no",
          interactive_mode = interactive_mode
        )

        if (response %in% c("yes", "no")) {
          break
        } else {
          cat("\n Please answer 'Yes' or 'No'.\n")
        }
      }
      if (response == "no") {
        stop("\nUser chose not to create a new wavelet.")
      }
    }
  }

  # Helper function to perform wavelet analysis
  perform_wavelet_analysis <- function() {
    # Set sampling frequency
    dt_sampling_frequency <- 1 / (wv_period_hours * 60 * 60 / sampling_frequency)

    # Run analyze.wavelet
    WaveletComp::analyze.wavelet(
      my.data = archive,
      my.series = "depth",
      loess.span = 0,
      dt = dt_sampling_frequency,
      dj = sub_octaves,
      lowerPeriod = 1 / (1440 / lower_period_mins),
      upperPeriod = upper_period_days,
      make.pval = pval,
      n.sim = 100
    )
  }

  # Helper function to save wavelet to output_folder
  save_wavelet <- function(my.w, tag_ID, pval) {
    wavelet_file_path <- if (pval) {
      file.path(output_folder, tag_ID, "1_Wavelets", paste0(tag_ID, "_wavelet_p_val.rds"))
    } else {
      file.path(output_folder, tag_ID, "1_Wavelets", paste0(tag_ID, "_wavelet.rds"))
    }

    # Create the directory if it doesn't exist
    create_directory(file.path(output_folder, tag_ID, "1_Wavelets"))
    saveRDS(my.w, file = wavelet_file_path)

    cat(paste0("\nWavelet saved to ", wavelet_file_path, "\n"))
  }

  # Calculate the wavelet upper period in days
  upper_period_days <- upper_period_hours / 24

  # Calculate the lower period in seconds
  lp_seconds <- lower_period_mins * 60

  # Calculate sampling_frequency if not provided
  if (is.null(sampling_frequency)) {
    if (nrow(archive) >= 2 && !is.null(archive$date) && any(class(archive$date) %in% c("POSIXct", "POSIXt", "Date"))) {
      sampling_frequency <- as.numeric(difftime(archive$date[2], archive$date[1], units = "secs"))

      if (sampling_frequency < 5) {
        cat(" Warning: Depth sampling frequency is < 5 seconds. This will increase computational time and file size.\n")
      }
    } else {
      stop("\nInsufficient data to calculate sampling_frequency or 'date' column missing/not in correct format.")
    }
  }

  # Check lower period length against sampling frequency
  if (lp_seconds < sampling_frequency) {
    cat(paste0("\n Warning: The lower period chosen (", lp_seconds, " seconds) is less than the depth data sampling frequency (", sampling_frequency, " seconds). \n"))
    # stop("Adjust lower period to equal or greater than the sampling frequency")
  }

  # Calculate actual sub_octaves from input
  sub_octaves <- (1 / suboctaves)

  # Check if my.w is in global environment
  if (!exists("my.w", where = globalenv())) {
    cat("\nNo 'my.w' in global environment. ")

    # Set the wavelet file path
    wavelet_file_path <- if (pval) {
      file.path(output_folder, tag_ID, "1_Wavelets", paste0(tag_ID, "_wavelet_p_val.rds"))
    } else {
      file.path(output_folder, tag_ID, "1_Wavelets", paste0(tag_ID, "_wavelet.rds"))
    }

    # Check if the wavelet exists in the output folder
    if (file.exists(wavelet_file_path)) {
      # Ask the user if they want to load the existing file. Require the correct input
      while (TRUE) {
        response <- get_response(
          prompt = "Do you want to load the existing wavelet spectrum? (Yes/No): ",
          default_response = "yes",
          interactive_mode = interactive_mode
        )

        if (response %in% c("yes", "no")) {
          break
        } else {
          cat("Please answer 'Yes' or 'No'.\n")
        }
      }

      if (response == "yes") {
        # Load the existing wavelet
        cat("\nLoading existing wavelet. ")
        my.w <- readRDS(file = wavelet_file_path)

        # Message
        cat("Existing wavelet loaded.\n")
      } else {
        # Message
        cat("\nCreating new wavelet.\n")

        # Check the sampling frequency is constant throughout the dataset
        check_sampling_frequency(archive)

        # Create new wavelet
        my.w <- perform_wavelet_analysis()

        # Add LP, UP and SO attributes to the wavelet
        attr(my.w, "LP") <- lower_period_mins
        attr(my.w, "UP") <- upper_period_days
        attr(my.w, "SO") <- suboctaves
        attr(my.w, "tag_ID") <- tag_ID

        # Save the wavelet
        save_wavelet(my.w, tag_ID, pval)
      }
    } else {
      # No wavelet in output_folder. Message
      cat("\nCreating new wavelet.\n")

      # Check sampling frequency is constant
      check_sampling_frequency(archive)

      # Create new wavelet
      my.w <- perform_wavelet_analysis()

      # Add LP, UP and SO attributes to the wavelet
      attr(my.w, "LP") <- lower_period_mins
      attr(my.w, "UP") <- upper_period_days
      attr(my.w, "SO") <- suboctaves
      attr(my.w, "tag_ID") <- tag_ID

      # Save the wavelet
      save_wavelet(my.w, tag_ID, pval)
    }
  } else {
    # Message
    cat("\nUsing 'my.w' in global environment.\n")
  }

  # Check for LP, UP and SO attributes
  if (lower_period_mins != attr(my.w, "LP")) {
    cat("\n Warning: 'lower_period_mins', is different from wavelet lower period:", attr(my.w, "LP"), ". Using lower period from wavelet. \n")
    lower_period_mins <- attr(my.w, "LP")
  }
  if (upper_period_days != attr(my.w, "UP")) {
    cat("\n Warning: 'upper_period_hours' is different from wavelet upper period:", attr(my.w, "UP") * 24, "hours. Using upper period from wavelet. \n")
    upper_period_days <- attr(my.w, "UP")
  }
  if (suboctaves != attr(my.w, "SO")) {
    cat("\n Warning: 'suboctaves' is different from wavelet suboctaves:", attr(my.w, "SO"), ". Using suboctaves from wavelet. \n")
    suboctaves <- attr(my.w, "SO")
    sub_octaves <- 1 / suboctaves
  }

  # Combine the attributes into a list
  wavelet_meta <- list(LP = attr(my.w, "LP"), UP = attr(my.w, "UP"), SO = attr(my.w, "SO"))

  # Save the list as an RDS file
  saveRDS(wavelet_meta, file = file.path(output_folder, tag_ID, "1_Wavelets/wavelet_meta.rds"))

  # Convert the list to a data frame & save as csv
  wavelet_meta_df <- as.data.frame(wavelet_meta)
  write.csv(wavelet_meta_df, file = file.path(output_folder, tag_ID, "1_Wavelets/wavelet_meta.csv"), row.names = FALSE)

  if (plot_wavelet == TRUE) {
    # Set the upper limit of period_range
    period_range_days <- c(upper_period_days)

    # Create lower period measure in days
    lower_period_days <- 1 / (1440 / lower_period_mins)

    # Create a vector of length max_period_ticks, by dividing the period in half until less than lower_period_days
    while ((period_range_days[length(period_range_days)] / 2) >= lower_period_days) {
      period_range_days <- c(period_range_days, period_range_days[length(period_range_days)] / 2)
    }

    # Ensure the last period is not lower than the lower_period_mins
    period_range <- period_range_days[period_range_days >= lower_period_days]

    # Generate human-readable labels for each period
    period_sequence <- sapply(period_range, function(p) {
      # Calculate total hours for the period
      hours <- p * 24
      # Determine the appropriate label based on the number of hours
      if (hours >= 1) {
        # Periods of one hour or more
        if (hours == 24) {
          return("24 hrs")
        } else {
          return(sprintf("%g hrs", hours))
        }
      } else {
        # Convert hours to minutes
        minutes <- hours * 60
        seconds <- minutes * 60
        if (minutes > 10) {
          # Periods of one minute or more but less than one hour
          return(sprintf("%.0f mins", minutes))
        } else if (minutes > 1) {
          # Periods of one minute or more but less than one hour
          return(sprintf("%.1f mins", minutes))
        } else {
          # Periods less than one minute displayed in seconds
          return(sprintf("%.0f secs", seconds))
        }
      }
    })

    # Calculate length of dataset
    days <- length(unique(archive$date_only))

    # Calculate time axis points and labels
    at_points <- seq(1, my.w$nc, by = 86400 / sampling_frequency)
    label_points <- seq(1, ((my.w$nc * sampling_frequency) / 86400), by = 1)

    if (length(at_points) != days) {
      at_points <- head(at_points, -1)
    }

    # Create the directory if it doesn't exist
    create_directory(file.path(output_folder, tag_ID, "2_Wavelet_Figures"))

    # Define the output file for saving
    if (pval == FALSE) {
      # Save wavelet spectrum without p values:
      # pdf("my_plot.pdf")
      png(file.path(output_folder, tag_ID, "2_Wavelet_Figures", paste0(tag_ID, "_wavelet_spectrum_no.p.png")), width = plot_width, height = plot_height, res = 72)
    } else {
      # Save wavelet spectrum with p values:
      # pdf("my_plot.pdf")
      png(file.path(output_folder, tag_ID, "2_Wavelet_Figures", paste0(tag_ID, "_wavelet_spectrum_p.png")), width = plot_width, height = plot_height, res = 72)
    }

    # Wrap the plotting code in a tryCatch block
    tryCatch(
      {
        if (max(my.w$Power) < 100) {
          label_digits <- 6
        } else if (max(my.w$Power) < 1000) {
          label_digits <- 3
        } else {
          label_digits <- 0
        }

        WaveletComp::wt.image(my.w,
          plot.coi = FALSE,
          plot.ridge = FALSE, # No ridge (p-value significance) will be plotted
          color.key = "quantile",
          n.levels = 100, # or #color.palette = "gray((n.levels):1/n.levels)", col.ridge = "blue",
          useRaster = TRUE, max.contour.segments = 250000,
          plot.legend = TRUE,

          # Legend
          legend.params = list(
            width = 1.2, shrink = 0.9, mar = 5.1,
            n.ticks = 6,
            label.digits = label_digits, label.format = "f",
            lab = "Wavelet power levels",
            lab.line = 2.5
          ),

          # date-time (x) axis:
          label.time.axis = TRUE, # show.date = TRUE,
          timelab = "Days elapsed",
          # timetck = 0.02, timetcl = 0.5, # control ticks
          spec.time.axis = list(
            at = at_points,
            labels = label_points
          ),

          # period (y) axis
          label.period.axis = TRUE,
          periodlab = "", # "Period",
          periodtck = 1, periodtcl = NULL,
          spec.period.axis = list(
            at = period_range,
            labels = period_sequence,
            las = 1, hadj = NA, padj = NA
          ),
          main = NULL,
          lwd = 2, # line width of contour lines. default 2
          lwd.axis = 2 # line width of axes. default 1
        )

        # Message
        cat(paste0("\nOutput folder: ", output_folder, "/", tag_ID, "/2_Wavelet_Figures/ \n"))
      },
      error = function(e) {
        cat("Error:", conditionMessage(e), "\n")
      },
      finally = {
        # Close the device in the finally block to ensure it happens regardless of error
        dev.off()
      }
    )

    # Create / define the output file for saving
    png(file.path(output_folder, tag_ID, "2_Wavelet_Figures", paste0(tag_ID, "_wavelet_mean_power.png")), width = plot_width, height = plot_height, res = 72)

    # Wrap the plotting code in a tryCatch block
    tryCatch(
      {
        WaveletComp::wt.avg(
          WT = my.w,
          label.avg.axis = TRUE,
          averagelab = "Mean wavelet power", averagetck = 0.02, averagetcl = 0.5,
          spec.avg.axis = list(at = NULL, labels = TRUE),
          label.period.axis = TRUE,
          periodlab = "",
          spec.period.axis = list(
            at = period_range,
            labels = period_sequence
          ),
          main = NULL
        )
      },
      error = function(e) {
        cat("Error:", conditionMessage(e), "\n")
      },
      finally = {
        # Close the device in the finally block to ensure it happens regardless of error
        dev.off()
      }
    )

    # Extract the Power matrix
    wavelet_power <- my.w$Power

    # Calculate mean wavelet power for each period
    mean_wavelet_power <- apply(wavelet_power, 1, mean, na.rm = TRUE)

    # Combine the periods and mean power into a data frame for easier interpretation
    mean_wavelet_power_df <- data.frame(
      Period_hours = my.w$Period * 24,
      Mean_Wavelet_Power = mean_wavelet_power
    )

    # Save mean wavelet power as a csv file
    write.csv(mean_wavelet_power_df, file = file.path(output_folder, tag_ID, "2_Wavelet_Figures", paste0(tag_ID, "_wavelet_mean_power.csv")))
  }

  return(my.w)
}
