#' Perform Principal Component Analysis
#'
#' `pca_results` performs Principal Component Analysis on the pc_data data frame
#' containing statistics from wavelet analysis
#'
#' @name pca_results
#'
#' @import ggplot2
#' @importFrom FactoMineR PCA
#' @importFrom stats reorder
#' @importFrom tidyr gather
#' @importFrom utils head
#'
#' @inheritParams pca_data
#' @param pc_data Data frame containing the output of the pca_data() function.
#' @param standardise TRUE or FALSE. Whether or not to standardise the data.
#'   Default TRUE.
#' @param No_pcs Numerical. Number of principal components to retain. Null by
#'   default
#' @param PCV Numerical. Percentage of cumulative variance to retain. Null by
#'   default
#' @param plot_eigenvalues TRUE or FALSE. Plot PC eigenvalues and general
#'   loadings. Default TRUE.
#' @param interactive_mode TRUE or FALSE. Used for testing the package. Default
#'   FALSE.
#'
#' @returns A PCA object from 'FactoMineR' package containing the output of the
#'   Principal Component Analysis.
#'
#' @export
#'
#' @examples
#' # Set file path
#' filepath <- system.file("extdata", package = "FishDiveR")
#'
#' # Load pc_data
#' pc_data <- readRDS(file.path(filepath, "data/4_PCA/pc_data.rds"))
#'
#' # Run pca_results function
#' pc_results <- pca_results(
#'   pc_data = pc_data,
#'   standardise = TRUE,
#'   No_pcs = 3,
#'   PCV = NULL,
#'   plot_eigenvalues = TRUE,
#'   output_folder = tempdir(),
#'   interactive_mode = FALSE
#' )
#'
# Function to run Principal Component Analysis on the wavelet statistics data frame
pca_results <- function(pc_data = data,
                        standardise = TRUE,
                        No_pcs = NULL,
                        PCV = NULL,
                        plot_eigenvalues = TRUE,
                        output_folder = data_dir,
                        interactive_mode = TRUE) {
  # Check if pc_data is a data frame
  if (!is.data.frame(pc_data)) {
    stop("pc_data must be a data frame. \n")
  }
  if (!is.logical(standardise)) {
    stop("standardise must be TRUE or FALSE.")
  }
  if (!is.null(No_pcs) && (!is.numeric(No_pcs) || No_pcs <= 0)) {
    stop("No_pcs must be NULL or a positive number.")
  }
  if (!is.null(PCV) && (!is.numeric(PCV) || PCV <= 0 || PCV > 100)) {
    stop("PCV must be NULL or a positive number between 0 and 100.")
  }
  if (!is.logical(plot_eigenvalues)) {
    stop("plot_eigenvalues must be TRUE or FALSE.")
  }
  if (!is.logical(interactive_mode)) {
    stop("interactive_mode must be TRUE or FALSE.")
  }

  # Check for required columns 'tag_ID' and 'date_only'
  required_columns <- c("tag_ID", "date_only")
  if (!all(required_columns %in% names(pc_data))) {
    stop("pc_data is missing one or more required columns: 'tag_ID', 'date_only'. \n")
  }

  # Check for at least one column starting with "p1_"
  if (!any(grepl("^p1_", names(pc_data)))) {
    stop("pc_data does not contain the correct wavelet data, containing a column starting with 'p1_'. \n")
  }

  # Check if pc_data has at least 10 rows
  if (nrow(pc_data) < 10) {
    warning(paste("Only", nrow(pc_data), " rows of data. PCA and clustering are unlikely to be effective.\n"))
  }

  # Check if the attribute unique_tag_ID exists and assign locally
  unique_tag_ID <- attr(pc_data, "unique_tag_ID")

  # Helper function for setting save folder (pca_data)
  set_save_folder <- function(output_folder, data_frame) {
    if (!is.null(attr(data_frame, "unique_tag_ID"))) {
      # Single tag
      return(file.path(output_folder, attr(data_frame, "unique_tag_ID"), "4_PCA"))
      print("unique")
    } else {
      # Multiple tags combined
      return(file.path(output_folder, "Combined_tags/4_PCA"))
    }
  }

  # Set the save folder location using helper function (detects multiple tags)
  save_folder <- set_save_folder(
    output_folder = output_folder,
    data_frame = pc_data
  )

  # Create the directory if it doesn't exist
  create_directory(save_folder)

  # Identify non-numerical columns
  non_numeric_cols <- sapply(pc_data, function(x) !is.numeric(x))

  # Ensure 'tag_ID' and 'date_only' are always excluded
  non_numeric_cols["tag_ID"] <- TRUE
  non_numeric_cols["date_only"] <- TRUE

  # Get the names of non-numerical columns, excluding 'tag_ID' and 'date_only'
  non_numeric_col_names <- names(pc_data)[non_numeric_cols]

  # Report the names of non-numerical columns being excluded
  message("\nNon-numerical columns being excluded: ")
  print(non_numeric_col_names)

  # Create a new data frame without non-numerical columns
  temp_data <- pc_data[, !non_numeric_cols]

  # Run PCA on the data frame, keeping the top 50 principal components
  pc_results <- FactoMineR::PCA(temp_data, scale.unit = standardise, graph = FALSE, ncp = 50)

  # Extract the eigenvalues and variance explained
  eigenvalues <- as.data.frame(pc_results$eig)

  # Count the number of PC's with eigenvalues >= 1 and print the result
  ev <- sum(eigenvalues$eigenvalue >= 1)

  # Print the eigenvalues >= 1 if in interactive_mode
  # Check if interactive_mode exists and is true, or if it does not exist
  if ((exists("interactive_mode") && interactive_mode) || !exists("interactive_mode")) {
    print(utils::head(eigenvalues, ev))
  }
  cat(paste0("\n", ev, " principal components of ", nrow(eigenvalues), " have eigenvalues >= 1 \n"))

  # Save the eigenvalues and cumulative variance in a csv in the data folder
  write.csv(head(eigenvalues, ev), file = file.path(save_folder, "eigenvalues_cum_var.csv"))
  cat(paste0("\nOutput file: ", save_folder, "/eigenvalues_cum_var.csv", "\n"))

  # Determine the number of principal components to keep
  if (!is.null(PCV)) {
    # Calculate the number of components required to reach the desired cumulative variance
    Max.C <- which(eigenvalues$`cumulative percentage of variance` >= PCV)[1]
    cat("Using cumulative variance threshold: Keeping", Max.C, "principal components to reach", PCV, "% variance\n")
  } else if (is.null(No_pcs)) {
    # Ask the user how many components to keep if No_pcs is NULL and PCV is not provided
    valid_input <- FALSE
    while (!valid_input) {
      # Prompt user
      Max.C_input <- readline(prompt = "Enter the number of components to keep: ")
      # Check input
      if (grepl("^\\d+$", Max.C_input)) {
        Max.C <- as.integer(Max.C_input)
        valid_input <- TRUE
      } else {
        cat("Invalid input. Please enter a valid integer.\n")
      }
    }
    # Print cumulative variance being kept
    cat("Keeping", Max.C, "PC's contributing to a cumulative", eigenvalues[Max.C, "cumulative percentage of variance"], "% of variance\n")
  } else {
    if (is.numeric(No_pcs)) {
      Max.C <- No_pcs
    } else {
      stop("Please enter a valid integer for No_pcs")
    }
    # Print cumulative variance being kept
    cat("Keeping", Max.C, "PC's contributing to a cumulative", eigenvalues[Max.C, "cumulative percentage of variance"], "% of variance\n")
  }

  # Run PCA on the data frame, keeping the top 'Max.c' principal components
  pc_results <- FactoMineR::PCA(temp_data, scale.unit = standardise, graph = FALSE, ncp = Max.C)

  # Plot the eigenvalues if plot_eigenvalues == TRUE
  if (plot_eigenvalues == TRUE) {
    # Extract the eigenvalues and variance explained
    eigenvalues <- as.data.frame(pc_results$eig)

    # Filter eigenvalues >= 1
    eigenvalues_filtered <- eigenvalues[eigenvalues$eigenvalue >= 1, ]

    # Prepare data for ggplot
    eigen_plot_data <- data.frame(
      PrincipalComponent = 1:nrow(eigenvalues_filtered),
      Eigenvalue = eigenvalues_filtered$eigenvalue
    )

    # Prepare the data
    eigen_plot_data$CumulativeVariance <- eigenvalues_filtered$`cumulative percentage of variance`

    # Extract the loadings (variable coordinates) as a data frame
    loadings_df <- as.data.frame(pc_results$var$coord)

    # Assign the new column names to the data frame
    colnames(loadings_df) <- paste0("PC", 1:ncol(loadings_df))

    # Assign a variable column and rename the row names
    loadings_df$Variable <- rownames(loadings_df)
    rownames(loadings_df) <- seq(1:nrow(loadings_df))

    # Create long data frame for plotting
    loadings_long <- tidyr::gather(loadings_df, Principal.Component, Loading, -Variable)

    # Create 'variable_type' column
    loadings_long$variable_type <- as.factor(gsub("^p[0-9]+_", "", loadings_df$Variable))

    # Specify the loading threshold
    load <- 0.5

    # Create a sequence order to plot by
    loadings_long$seq <- seq(1:nrow(loadings_long))

    # Reverse the sequence so that of periods goes from short to long along the x-axis
    loadings_long$seq <- (max(loadings_long$seq) + 1) - loadings_long$seq

    # Define the desired order of Principal.Components
    pc_order <- paste0("PC", 1:Max.C)

    # Convert Principal.Component to a factor with custom levels
    loadings_long$Principal.Component <- factor(loadings_long$Principal.Component, levels = pc_order)

    # Create the ggplot2 plot
    eigenplot <- ggplot2::ggplot(eigen_plot_data, aes(x = PrincipalComponent)) +
      geom_bar(aes(y = Eigenvalue), stat = "identity", fill = "#C7EAE5", color = "black") +
      geom_line(aes(y = CumulativeVariance * 1), linewidth = 1) +
      scale_y_continuous(
        name = "Eigenvalue",
        sec.axis = sec_axis(~ . / 1, name = "Cumulative Variance (%)")
      ) +
      scale_x_continuous(breaks = seq(1, ev, 2), expand = c(0, 0)) +
      labs(x = "Wavelet Period") +
      theme_bw() +
      theme(
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.line = element_line(linewidth = 0.5),
        legend.position = "bottom",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        plot.margin = unit(c(25.5, 5, 5.5, 5.5), "points")
      )

    # Print the plot to 'Plots' tab
    print(eigenplot)

    # Save the plot to the data folder
    ggsave(file.path(save_folder, "PC_Eigenvalues_cum_var.png"), plot = eigenplot, width = 14.22, height = 8.43, dpi = 600)

    # Create a new column for descriptive names
    loadings_long$DescriptiveName <- as.factor(gsub("p[0-9]+_", "", loadings_long$Variable))

    # Recode using dplyr::recode
    loadings_long$DescriptiveName <- dplyr::recode(loadings_long$DescriptiveName,
      "Amplitude_mean" = "Mean Amplitude",
      "Phase_mean" = "Mean Phase",
      "Phase_variance" = "Variance of Phase",
      "Power_mean" = "Mean Power",
      "Amplitude_variance" = "Variance of Amplitude",
      "Power_variance" = "Power Variance",
      "Mean_sq_power" = "Mean Squared Power"
    )

    # Copy descriptive names (these are a factor though)
    cleaned_colnames <- loadings_long$DescriptiveName

    # Calculate the number of periods
    No_periods <- length(pc_results$call$X) / length(unique(cleaned_colnames))

    # Prepare a sequence for periods
    periods <- seq(1, No_periods)

    # Initial values
    UP_mins <- attr(pc_data, "UP") * 24 * 60 # Convert UP to minutes
    LP <- attr(pc_data, "LP") # Lower bound in minutes (assuming already in minutes)
    SO <- attr(pc_data, "SO") # Suboctave interval

    # Calculate the rate of change per period to reach half value at each SO
    # Assuming exponential decay formula: N(t) = N0 * exp(-lambda * t)
    # To halve the value at each SO, we find lambda when t = SO and N(t) = N0/2
    lambda <- log(2) / SO

    # Apply formula to generate labels adjusting to fit the scale
    labels <- UP_mins * exp(-lambda * (periods - 1))

    # Adjusting labels to fit the scale more precisely for your requirement
    adjusted_labels <- round(labels, 2)

    # Replace the last value with LP
    adjusted_labels[length(adjusted_labels)] <- LP

    # Convert and format the adjusted_labels based on the condition
    formatted_labels <- sapply(adjusted_labels, function(label) {
      if (label >= 60) {
        # Convert to hours and append 'hrs'
        paste0(round(label / 60, 2), " hrs")
      } else {
        # Keep as minutes and append 'mins'
        paste0(label, " mins")
      }
    })

    # Dynamically set variables to plot based on existing data
    variables <- levels(loadings_long$DescriptiveName)

    # Create the correct length of labels
    formatted_labels2 <- rep(formatted_labels, each = length(unique(variables)))

    # Create period labels for whole loadings_long data frame
    loadings_long$period_labs <- formatted_labels2

    # Check number of wavelet variables
    waves <- length(unique(loadings_long$DescriptiveName))

    # Custom function to display every 3rd tick mark
    every_Nth <- function(x) {
      x <- loadings_long$period_labs
      inds <- seq(1, length(x), by = waves * 3)
      labels <- rep("", length(x))
      labels[inds] <- x[inds]
      labels <- rev(labels)
      return(labels)
    }

    # Create the loadings plot
    loadings_plot <-
      ggplot2::ggplot(
        data = loadings_long,
        aes(x = stats::reorder(Variable, seq), y = Loading, fill = DescriptiveName, colour = DescriptiveName)
      ) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = load, linetype = "dashed", color = "red", linewidth = 0.5) +
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = -load, linetype = "dashed", color = "red", linewidth = 0.5) +
      theme_bw() +
      labs(
        x = "Principal Components",
        fill = "Wavelet Variable",
        colour = "Wavelet Variable"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, size = 14, color = "black", vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.background = element_blank(),
        strip.placement = "inside",
        strip.text = element_text(size = 12, face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)
      ) +
      scale_y_continuous(breaks = seq(-1, 1, 0.25)) +
      scale_x_discrete(labels = every_Nth(loadings_long$period_labs)) +
      facet_wrap(~Principal.Component, ncol = 1, strip.position = "right")

    # Print the plot to 'Plots' tab
    print(loadings_plot)

    # Save the plot to the main folder
    ggsave(file.path(save_folder, "PC_Loadings_ALL_wavelet_variables.png"), plot = loadings_plot, width = 14.22, height = 14.22, dpi = 600)

    # Message for folder location
    cat(paste0("Output folder: ", save_folder), "\n")
  }

  # Add an attribute for the number of PC's kept
  attr(pc_results, "No.Components") <- Max.C

  # Move diel attribute forward
  attr(pc_results, "Includes diel") <- attr(pc_data, "Includes diel")

  # Move the wavelet meta attributes forward
  attr(pc_results, "LP") <- attr(pc_data, "LP")
  attr(pc_results, "UP") <- attr(pc_data, "UP")
  attr(pc_results, "SO") <- attr(pc_data, "SO")

  # Add an attribute to pc_results with unique tag_ID if processing one tag
  if (length(unique(pc_data$tag_ID)) == 1) {
    attr(pc_results, "unique_tag_ID") <- unique_tag_ID
  }

  # Save the 'pc_results' object with selected number of principal components
  saveRDS(pc_results, file = file.path(save_folder, "pc_results.rds"))
  cat(paste0("\nOutput file: ", save_folder, "/pc_results.rds contains the selected number of principal components.\n"))

  return(pc_results)
}

#' Calculate Principal Component Analysis Scores not including depth statistics
#'
#' This function extracts the PCA scores from the PCA results and plots the
#' loadings. This function is to be use on output from the `pca_data()` function
#' not including depth statistics.
#'
#' @name pca_scores
#'
#' @import ggplot2
#' @importFrom stats reorder
#' @importFrom tidyr gather
#' @importFrom gridExtra arrangeGrob
#'
#' @inheritParams pca_data
#' @param pc_results PCA class object containing the output from the
#'   'pca_results()' function.
#' @param plot_loadings TRUE or FALSE. Plot PC loadings figures. Default TRUE.
#' @param every_nth Numeric. Sequence of labels to show on mean power plot.
#'   Default is 12.
#'
#' @returns A data frame of pc scores containing one column for each Principal
#'   Component kept. If processing just one tag, the attribute 'unique_tag_ID'
#'   is given to the data frame with the tag_ID. Plots the PC loadings for each
#'   row of pc_data
#'
#' @export
#'
#' @examples
#' # Set file path
#' filepath <- system.file("extdata", package = "FishDiveR")
#'
#' # Load pc_results
#' pc_results <- readRDS(file.path(filepath, "data/4_PCA/pc_results.rds"))
#'
#' # Run pca_scores function
#' pc_scores <- pca_scores(
#'   pc_results = pc_results,
#'   plot_loadings = FALSE,
#'   every_nth = 12,
#'   output_folder = tempdir()
#' )
#'
# Function to extract the principal component scores and plot loadings
pca_scores <- function(pc_results = results,
                       plot_loadings = TRUE,
                       every_nth = 12,
                       output_folder = data_dir) {
  # Check if pc_results is from FactoMineR PCA
  if ("PCA" %in% class(pc_results)) {
    # Check for required components 'eig', 'var', 'ind'
    required_components <- c("eig", "var", "ind")
    if (!all(required_components %in% names(pc_results))) {
      stop("\n pc_results is missing one or more required components: 'eig', 'var', 'ind'.")
    }
  } else {
    stop("\n pc_results does not appear to be a PCA output from FactoMineR.")
  }
  if (!is.logical(plot_loadings)) {
    stop("\n plot_loadings must be TRUE or FALSE.")
  }
  if ((!is.numeric(every_nth) || every_nth <= 0)) {
    stop("every_nth must be a positive number.")
  }

  # Helper function for setting save folder for taglist data.
  set_scores_save_folder <- function(output_folder, data_frame) {
    # Single tag
    return(file.path(output_folder, data_frame[1, 1], "4_PCA"))
  }

  # Check if the 'unique_tag_ID' attribute exists in pc_results
  if (!is.null(attr(pc_results, "unique_tag_ID"))) {
    # The attribute exists, so use it with existing helper function by turning it into a data frame
    unique_tag_ID <- as.data.frame(attr(pc_results, "unique_tag_ID"))

    # Set the save folder location using helper function
    save_folder <- set_scores_save_folder(
      output_folder = output_folder,
      data_frame = unique_tag_ID # converted to data frame
    )
  } else {
    # Attribute does not exist
    cat("\n No unique_tag_ID. Multiple tags included in pc_results. \n")

    # Set the save folder location
    save_folder <- file.path(output_folder, "Combined_tags/4_PCA")
  }

  Max.C <- attr(pc_results, "No.Components")

  # Extract all the principal component scores
  pc_scores <- as.data.frame(pc_results$ind$coord)

  # Rename columns from Dim to PC
  colnames(pc_scores) <- paste0("PC", 1:ncol(pc_scores))

  # Helper function to save grobs if there are any existing plots
  save_grob <- function(existing_plots, c, name_suffix) {
    if (length(existing_plots) > 0) {
      # Facet with arrangeGrob from gridExtra
      grob <- do.call(gridExtra::arrangeGrob, c(existing_plots, list(ncol = 1)))
      ggsave(file.path(save_folder, paste0("PC", c, name_suffix)), plot = grob, width = 10, height = 15, dpi = 600)
    }
  }

  # Extract the loadings (variable coordinates) as a data frame
  loadings_df <- as.data.frame(pc_results$var$coord)

  # Assign the new column names to the data frame
  colnames(loadings_df) <- paste0("PC", 1:ncol(loadings_df))

  # Assign a variable column and rename the row names
  loadings_df$Variable <- rownames(loadings_df)
  rownames(loadings_df) <- seq(1:nrow(loadings_df))

  # Create long data frame for plotting
  loadings_long <- tidyr::gather(loadings_df, Principal.Component, Loading, -Variable)

  # Specify the loading threshold
  load <- 0.5

  # Create a sequence order to plot by
  loadings_long$seq <- seq(1:nrow(loadings_long))

  # Reverse the sequence so that of periods goes from short to long along the x-axis
  loadings_long$seq <- (max(loadings_long$seq) + 1) - loadings_long$seq

  # Define the desired order of Principal.Components
  pc_order <- paste0("PC", 1:Max.C)

  # Convert Principal.Component to a factor with custom levels
  loadings_long$Principal.Component <- factor(loadings_long$Principal.Component, levels = pc_order)

  #### Plotting the Loading statistics per PC
  if (plot_loadings == TRUE) {
    # Helper function to display every other tick mark
    every_other <- function(x) {
      inds <- seq(1, length(x), by = 2)
      labels <- rep("", length(x))
      labels[inds] <- x[inds]
      return(labels)
    }

    # Helper function to display every nth tick mark
    every_nth_label <- function(x) {
      inds <- seq(1, length(x), by = every_nth)
      labels <- rep("", length(x))
      labels[inds] <- x[inds]
      return(labels)
    }

    # Initialize empty lists to store the plots
    plot_list1 <- list()
    plot_list2 <- list()
    mean_power_list <- list()
    mean_power_data <- list()

    ### Calculate the x-axis labels from wavelet meta
    # Initial values
    UP_mins <- attr(pc_results, "UP") * 24 * 60 # Convert UP to minutes
    LP <- attr(pc_results, "LP") # Lower bound in minutes (assuming already in minutes)
    SO <- attr(pc_results, "SO") # Suboctave interval

    # Remove the prefixes (p and numbers) from the column names. Assumes no non-numerical columns remain
    cleaned_colnames <- gsub("^p[0-9]+_", "", colnames(pc_results$call$X))

    # Calculate the number of periods
    No_periods <- length(pc_results$call$X) / length(unique(cleaned_colnames))

    # Prepare a sequence for periods
    periods <- seq(1, No_periods)

    # Calculate the rate of change per period to reach half value at each SO
    # Assuming exponential decay formula: N(t) = N0 * exp(-lambda * t)
    # To halve the value at each SO, we find lambda when t = SO and N(t) = N0/2
    lambda <- log(2) / SO

    # Apply formula to generate labels adjusting to fit the scale
    labels <- UP_mins * exp(-lambda * (periods - 1))

    # Adjusting labels to fit the scale more precisely for your requirement
    adjusted_labels <- round(labels, 2)

    # Replace the last value with LP
    adjusted_labels[length(adjusted_labels)] <- LP

    # Convert and format the adjusted_labels based on the condition
    formatted_labels <- sapply(adjusted_labels, function(label) {
      if (label >= 60) {
        # Convert to hours and append 'hrs'
        paste0(round(label / 60, 2), " hrs")
      } else {
        # Keep as minutes and append 'mins'
        paste0(label, " mins")
      }
    })

    c <- 1
    # Create plots for all PCs
    for (c in 1:Max.C) {
      # Filter the data for only PC
      pcx <- paste0("PC", c)
      loadings <- loadings_long[loadings_long$Principal.Component == pcx, ]

      # Rename the variables and prepare for plotting
      loadings$CleanVariable <- as.factor(gsub("p[0-9]+_", "", loadings$Variable))
      loadings$Pnumber <- gsub("(p[0-9]+_).*", "\\1", loadings$Variable)
      loadings$Pnumber <- sub("^p", "P", loadings$Pnumber)
      loadings$Pnumber <- sub("_$", "", loadings$Pnumber)

      # Dynamically set variables to plot based on existing data
      variables <- levels(loadings$CleanVariable)

      # Create the correct length of labels
      formatted_labels2 <- rep(formatted_labels, each = length(unique(variables)))

      # Create period labels for whole loadings data frame
      loadings$period_labs <- formatted_labels2

      # Create a new column for descriptive names
      loadings$DescriptiveName <- loadings$CleanVariable # Initialize with existing clean variable names

      # Recode using dplyr::recode
      loadings$DescriptiveName <- dplyr::recode(loadings$DescriptiveName,
        "Amplitude_mean" = "Mean Amplitude",
        "Phase_mean" = "Mean Phase",
        "Phase_variance" = "Variance of Phase",
        "Power_mean" = "Mean Power",
        "Amplitude_variance" = "Variance of Amplitude",
        "Power_variance" = "Power Variance",
        "Mean_sq_power" = "Mean Squared Power"
      )

      # Create plot title
      plot_title <- paste0("Principal Component ", c)

      wavelet_loadings_plot <- ggplot2::ggplot(
        data = loadings,
        aes(
          x = stats::reorder(period_labs, seq), y = Loading, fill = Principal.Component, group = CleanVariable,
          colour = DescriptiveName
        )
      ) +
        geom_path(stat = "identity", linewidth = 1.25) +
        geom_hline(yintercept = load, linetype = "dashed", color = "black", linewidth = 0.75) +
        geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.75) +
        geom_hline(yintercept = -load, linetype = "dashed", color = "black", linewidth = 0.75) +
        scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +
        scale_x_discrete(labels = every_other, expand = c(0, 0, 0.001, 0)) +
        theme_bw() +
        labs(
          x = "Wavelet Period",
          y = "Loading",
          colour = "Wavelet Variable"
        ) +
        ggtitle(plot_title) +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(angle = 45, size = 16, color = "black", vjust = 0.5, hjust = 0.5, face = "bold"),
          axis.text.y = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 16, face = "bold"),
          legend.text = element_text(size = 14, face = "bold"),
          axis.ticks.length = unit(0.2, "cm"),
          axis.ticks = element_line(linewidth = 1.0, color = "black"),
          axis.line = element_line(linewidth = 1.0),
          legend.position = "right"
        )

      # Print to 'Plots' tab
      print(wavelet_loadings_plot)

      ggsave(file.path(save_folder, paste0("PC", c, "_Loadings.png")), plot = wavelet_loadings_plot, width = 15, height = 10, dpi = 600)

      # subset the data for the wavelet variable 'var_name'
      data_sub <- subset(loadings, CleanVariable == "Power_mean")

      # Set labels
      data_sub$period_labs <- formatted_labels

      power_plot_title <- paste0("Mean Wavelet Power PC", c)

      # Save the plot data for the line plot
      mean_power_data[[c]] <- data_sub
    }

    # Combine all data frames into a single data frame
    combined_mean_power_data <- do.call(rbind, mean_power_data)

    # Helper function to get indices of non-empty labels
    non_empty_labels <- function(labels) {
      which(labels != "")
    }

    # Create x-axis labels for one 'set' of periods
    plot_labels <- rev(every_nth_label(combined_mean_power_data$period_labs[c(1:No_periods)]))

    # Get the positions of non-empty labels
    positions <- non_empty_labels(plot_labels)

    # Convert plot_labels to factor with appropriate levels
    combined_mean_power_data$period_labs_factor <- factor(combined_mean_power_data$period_labs, levels = unique(combined_mean_power_data$period_labs))

    # Plotting mean wavelet power for each PC
    mean_power_plot <-
      ggplot(data = combined_mean_power_data, aes(x = as.numeric(stats::reorder(period_labs_factor, seq)), y = Loading, color = Principal.Component, group = Principal.Component)) +
      geom_line(linewidth = 1) + # Use geom_line for line plots
      geom_hline(yintercept = load, linetype = "dashed", color = "black", linewidth = 0.75) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.75) +
      geom_hline(yintercept = -load, linetype = "dashed", color = "black", linewidth = 0.75) +
      scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +
      scale_x_continuous(breaks = positions, labels = plot_labels[positions], expand = c(0, 0)) +
      theme_bw() +
      labs(
        x = "Wavelet Period",
        y = "Mean Wavelet Power Loading",
        color = "Principal\nComponent"
      ) +
      theme(
        axis.text.x = element_text(size = 16, color = "black", face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.ticks = element_line(linewidth = 1.0, color = "black"),
        axis.line = element_line(linewidth = 1.0)
      )

    # Print the plot
    print(mean_power_plot)

    # Save the mean power line plot
    ggsave(file.path(save_folder, "PCA_mean_power_line_plot.png"), plot = mean_power_plot, width = 15, height = 10, dpi = 600)

    # Report save folder dependant on the number of tags
    cat(paste0("\nOutput folder: ", save_folder, "\n"))
  }

  # Add an attribute to pc_scores with unique Tag_ID if processing one tag
  attr(pc_scores, "unique_tag_ID") <- attr(pc_results, "unique_tag_ID")

  # Add an attribute for the number of PC's kept
  attr(pc_scores, "No.Components") <- Max.C

  # Save the 'pc_scores' object to output_folder
  saveRDS(pc_scores, file = file.path(save_folder, "pc_scores.rds"))
  cat(paste0("\nOutput file: ", save_folder, "/pc_scores.rds\n"))

  return(pc_scores)
}

#' Import depth statistics and combine with PC scores
#'
#' This function imports the depth statistics from each of the tags listed in
#' tag_vector, and outputs a combined data frame then combines the depth
#' statistics from each tag with the principal component scores, and outputs a
#' data frame with the appropriate unique_tag_ID if necessary, ready for use in
#' k-means clustering.
#'
#' @name combine_data
#'
#' @inheritParams pca_data
#' @param pc_scores Data frame of principal component scores extracted through
#'   PCA on wavelet statistics. Output of 'pca_scores()' function.
#'
#' @returns A data frame containing the combined depth statistics and principal
#'   component scores from each of the tags listed in tag_vector
#'
#' @export
#'
#' @examples
#' # Set file path
#' filepath <- system.file("extdata", package = "FishDiveR")
#'
#' # Load pc_results
#' pc_scores <- readRDS(file.path(filepath, "data/4_PCA/pc_scores.rds"))
#'
#' # Run depth_data function
#' combined_stats <- combine_data(
#'   tag_vector = "data",
#'   data_folder = filepath,
#'   pc_scores = pc_scores,
#'   output_folder = tempdir()
#' )
#'
# Function to load the depth statistics from tags listed in tag_vector, to be added into k-means clustering directly
combine_data <- function(
    tag_vector = tag_list,
    data_folder = data_dir,
    pc_scores = scores,
    output_folder = data_dir) {
  # Check if tag_vector is a character vector
  if (!is.character(tag_vector)) {
    stop("tag_vector should be a vector of characters. Check input.")
  }

  # Initialize empty lists to store the data frames
  depth_data <- list()
  depth_length <- list() # List to store the length of each depth data frame

  # Initialize a variable to track the diel inclusion
  includes_diel <- FALSE

  tag_ID <- tag_vector[1]
  # Loop through each tag_ID
  for (tag_ID in tag_vector) {
    # Read in the depth statistics
    depth_stats_path <- file.path(data_folder, tag_ID, "3_Stats", paste0(tag_ID, "_depthStats.csv"))

    if (file.exists(depth_stats_path)) {
      depth_stats <- read.csv(depth_stats_path, header = TRUE)

      # List the lengths of depth and wave statistics for each tag_ID
      depth_length[[tag_ID]] <- ncol(depth_stats)

      # Check if depth_stats has more than 10 columns
      if (ncol(depth_stats) > 10) {
        includes_diel <- TRUE
      }

      # Combine the depth and wave statistics
      combined_stats <- cbind(tag_ID = tag_ID, depth_stats)

      # Append to the list
      depth_data[[tag_ID]] <- combined_stats
    } else {
      message("Files do not exist for tag ", tag_ID)
    }
  }

  # Check if there is more than 1 tag
  if (length(depth_data) > 1) {
    # More than one tag.
    # Check if includes_diel is true and if depth_length is uneven
    if (includes_diel && length(unique(unlist(depth_length))) != 1) {
      message("Diel data is not available for all tags. \n")
      for (tag in names(depth_data)) {
        # For each tag_ID check the number of columns against the minimum in the list
        if (ncol(depth_data[[tag]]) > min(unlist(depth_length)) + 1) {
          # Remove diel columns from list (13 - surface proportion:20 - surface proportion diel difference) from the data frame
          depth_data[[tag]] <- depth_data[[tag]][, -c(13:20)]
          message(paste0("Diel data removed from Tag ", tag, "\n"))
        }
      }
    }

    # Check if all data frames in depth_data have the same column names
    all_names <- lapply(depth_data, names)
    if (!all(sapply(all_names, identical, all_names[[1]]))) {
      stop("Data frames have inconsistent column names, cannot combine.\n")
    }

    # Combine all data frames
    depth_data <- do.call(rbind, depth_data)
  } else if (length(depth_data) == 1) {
    # Single tag. Call list into data frame
    depth_data <- do.call(rbind, depth_data)
  } else if (length(depth_data) == 0) {
    # No Tags - Error
    stop("No data available to combine.\n")
  }

  # Set date_only as a date
  depth_data$date_only <- as.Date(depth_data$date_only)

  # Combine the depth statistics with pc_scores
  combined_data <- cbind(depth_data, pc_scores)

  # Check length of tag_vector and assign 'unique_tag_ID' attribute if necessary
  if (length(tag_vector) == 1) {
    attr(combined_data, "unique_tag_ID") <- tag_vector
  }

  # Define the file path
  if (length(tag_vector) > 1) {
    folder_name <- "Combined_tags"
  } else {
    folder_name <- tag_vector
  }

  save_folder <- file.path(output_folder, folder_name, "5_k-means")
  create_directory(save_folder)

  # Save combined_data
  saveRDS(combined_data, file.path(output_folder, folder_name, "5_k-means/combined_stats.rds"))

  # Message about saving combined stats
  cat("\nSaving combined metrics to:", file.path(output_folder, folder_name, "5_k-means/combined_stats.rds\n"))

  return(combined_data)
}
