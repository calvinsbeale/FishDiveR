# App step 5: principal component analysis of the daily wavelet statistics.

# Wavelet statistics offered for the PCA, named as the pca_data() arguments
pca_stat_choices <- c(
  "Mean power" = "power_mean",
  "Power variance" = "power_variance",
  "Mean amplitude" = "amplitude_mean",
  "Amplitude variance" = "amplitude_variance",
  "Mean squared power" = "mean_sq_power",
  "Mean phase" = "phase_mean",
  "Phase variance" = "phase_variance"
)

# Cumulative variance the suggested number of components should reach
pca_min_variance <- 70

#' Folder the PCA and k-means results are saved under for a set of tags
#' @noRd
pca_folder <- function(project_folder, tag_IDs) {
  if (length(tag_IDs) > 1) file.path(project_folder, "Combined_tags") else file.path(project_folder, tag_IDs)
}

#' Suggested number of principal components
#'
#' Those with eigenvalues of at least 1, or more if needed to reach
#' pca_min_variance percent of the cumulative variance.
#' @noRd
suggest_n_pcs <- function(eig) {
  n_ev <- sum(eig$eigenvalue >= 1)
  n_var <- which(eig$cumulative >= pca_min_variance)[1]
  max(1, n_ev, n_var, na.rm = TRUE)
}

#' @noRd
mod_pca_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = bslib::breakpoints(sm = 12, xl = c(5, 7)),
    bslib::card(
      bslib::card_header("Principal component analysis"),
      shiny::p(
        "Wavelet statistics have many closely related periods. PCA combines them into a few principal components,",
        "which are then joined with the daily depth statistics for clustering."
      ),
      shiny::checkboxGroupInput(ns("tags"), "Tags to analyse together", choices = NULL),
      shiny::checkboxGroupInput(
        ns("stats"),
        shiny::tagList(
          "Wavelet statistics to include ",
          help_tip(paste(
            "Mean power, power variance and mean amplitude are recommended: they capture the most variance in the",
            "fewest principal components. The others add many components without usually improving the clustering."
          ))
        ),
        choices = pca_stat_choices,
        selected = c("power_mean", "power_variance", "amplitude_mean")
      ),
      shiny::checkboxInput(
        ns("standardise"),
        shiny::tagList(
          "Standardise the statistics ",
          help_tip("Scales each statistic to the same range, so statistics with large values don't dominate. Recommended.")
        ),
        value = TRUE
      ),
      shiny::uiOutput(ns("warnings")),
      shiny::div(shiny::actionButton(ns("calc_eigen"), "Calculate eigenvalues", class = "btn-primary")),
      shiny::uiOutput(ns("eigen_section")),
      shiny::h6("Log", class = "mt-3"),
      shiny::verbatimTextOutput(ns("log"))
    ),
    bslib::card(
      bslib::card_header("PCA figures"),
      shiny::selectInput(ns("figure"), "Figure", choices = NULL, width = "100%"),
      shiny::uiOutput(ns("figure_note")),
      shiny::imageOutput(ns("figure_image"), height = "auto", fill = FALSE)
    )
  )
}

#' @noRd
mod_pca_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pca_log <- shiny::reactiveVal(character())
    eigen <- shiny::reactiveVal(NULL)

    # Tags with both daily statistics files, and their wavelet settings
    tag_info <- shiny::reactive({
      state$refresh
      shiny::req(state$project)
      status <- tag_status(state$tags, state$project)
      tags <- status$tag_ID[status$Daily_stats]
      if (length(tags) == 0) {
        return(NULL)
      }
      rows <- lapply(tags, function(tag) {
        meta <- read_wavelet_meta(state$project, tag)
        files <- stats_files(state$project, tag)
        data.frame(
          tag_ID = tag,
          setting = if (is.null(meta)) NA else paste0(meta$LP, " min to ", meta$UP * 24, " h, ", meta$SO, " sub-oct."),
          sampling_s = if (is.null(meta$sampling_s)) NA else meta$sampling_s,
          wave_days = stats_days(files$wave),
          depth_days = stats_days(files$depth),
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, rows)
    })

    shiny::observe({
      info <- tag_info()
      choices <- if (is.null(info)) character() else info$tag_ID
      current <- shiny::isolate(input$tags)
      selected <- if (length(current) > 0) intersect(current, choices) else choices
      shiny::updateCheckboxGroupInput(session, "tags", choices = choices, selected = selected)
    })

    settings <- shiny::reactive(list(
      tags = sort(input$tags),
      stats = sort(input$stats),
      standardise = isTRUE(input$standardise)
    ))

    # Problems with the chosen tags that stop the PCA
    errors <- shiny::reactive({
      info <- tag_info()
      s <- settings()
      msgs <- character()
      if (is.null(info)) return("Create daily statistics in step 4 first.")
      if (length(s$tags) == 0) msgs <- c(msgs, "Choose at least one tag.")
      if (length(s$stats) == 0) msgs <- c(msgs, "Choose at least one wavelet statistic.")
      chosen <- info[info$tag_ID %in% s$tags, ]
      if (length(unique(chosen$setting)) > 1) {
        msgs <- c(msgs, "The chosen tags' wavelets use different periods or sub-octaves, so their statistics can't be combined. Run their wavelets again with the same settings.")
      }
      mismatch <- chosen$tag_ID[chosen$wave_days != chosen$depth_days]
      if (length(mismatch) > 0) {
        msgs <- c(msgs, paste0(
          "Wavelet and depth statistics cover different numbers of days for ", paste(mismatch, collapse = ", "),
          ". Run step 4 again for them."
        ))
      }
      msgs
    })

    output$warnings <- shiny::renderUI({
      info <- tag_info()
      s <- settings()
      msgs <- errors()
      if (!is.null(info)) {
        chosen <- info[info$tag_ID %in% s$tags, ]
        if (length(unique(stats::na.omit(chosen$sampling_s))) > 1) {
          msgs <- c(msgs, "The chosen tags' wavelets were made at different sampling intervals. Thin them to the same interval in step 3 for comparable statistics.")
        }
        if (sum(chosen$wave_days, na.rm = TRUE) < 10) {
          msgs <- c(msgs, "Fewer than 10 days in total. PCA and clustering are unlikely to be meaningful.")
        }
      }
      if (length(msgs) > 0) {
        shiny::div(class = "alert alert-warning mt-2", lapply(msgs, shiny::p, class = "mb-1"))
      }
    })

    output$log <- shiny::renderText({
      log <- pca_log()
      if (length(log) == 0) "No PCA run yet this session." else paste(log, collapse = "\n")
    })

    make_pc_data <- function(s, output) {
      flags <- stats::setNames(as.list(pca_stat_choices %in% s$stats), pca_stat_choices)
      do.call(pca_data, c(
        list(tag_vector = s$tags, data_folder = state$project),
        flags,
        list(output = output, output_folder = state$project, verbose = TRUE)
      ))
    }

    ## 1. Eigenvalues
    shiny::observeEvent(input$calc_eigen, {
      if (length(errors()) > 0) {
        shiny::showNotification(paste(errors(), collapse = " "), type = "error", duration = 10)
        return()
      }
      s <- settings()
      res <- shiny::withProgress(message = "Calculating eigenvalues", value = 0.5, run_logged({
        pc_data <- make_pc_data(s, output = FALSE)
        # FactoMineR only returns eigenvalues for the components kept, so keep
        # up to 50 (as pca_results() does internally) to see them all
        pc_res <- pca_results(pc_data, standardise = s$standardise, No_pcs = 50, plot_eigenvalues = FALSE, output = FALSE, interactive_mode = FALSE)
        eig <- as.data.frame(pc_res$eig)
        data.frame(
          component = seq_len(nrow(eig)),
          eigenvalue = eig$eigenvalue,
          variance = eig$`percentage of variance`,
          cumulative = eig$`cumulative percentage of variance`
        )
      }))
      pca_log(c("== Eigenvalues", res$log))
      if (res$ok) {
        eigen(list(settings = s, eig = res$value))
        shiny::updateNumericInput(session, "n_pcs", value = suggest_n_pcs(res$value), max = nrow(res$value))
      } else {
        eigen(NULL)
      }
    })

    eigen_current <- shiny::reactive({
      e <- eigen()
      !is.null(e) && identical(e$settings, settings())
    })

    output$eigen_section <- shiny::renderUI({
      e <- eigen()
      if (is.null(e)) return(NULL)
      eig <- e$eig
      n_ev <- sum(eig$eigenvalue >= 1)
      n_var <- which(eig$cumulative >= pca_min_variance)[1]

      shiny::tagList(
        shiny::hr(),
        if (!eigen_current()) {
          shiny::div(class = "alert alert-warning", "Settings have changed. Calculate the eigenvalues again before running the PCA.")
        },
        shiny::plotOutput(ns("eigen_plot"), height = "280px", fill = FALSE),
        shiny::p(
          class = "small",
          paste0(
            n_ev, " components have eigenvalues of 1 or more (", round(eig$cumulative[max(n_ev, 1)], 1), "% of the variance). ",
            if (is.na(n_var)) "" else paste0(n_var, " components reach ", pca_min_variance, "% of the variance. "),
            "Keep enough components for at least ", pca_min_variance, "% of the variance, and check the literature for your field."
          )
        ),
        shiny::numericInput(ns("n_pcs"), "Principal components to keep", value = suggest_n_pcs(eig), min = 1, max = nrow(eig), step = 1, width = "220px"),
        shiny::uiOutput(ns("n_pcs_note")),
        shiny::actionButton(ns("run_pca"), "Run PCA", class = "btn-primary")
      )
    })

    output$n_pcs_note <- shiny::renderUI({
      e <- eigen()
      shiny::req(e, input$n_pcs)
      n <- min(max(1, round(input$n_pcs)), nrow(e$eig))
      shiny::p(class = "text-muted small", paste0("Keeps ", round(e$eig$cumulative[n], 1), "% of the variance."))
    })

    output$eigen_plot <- shiny::renderPlot(
      {
        e <- eigen()
        shiny::req(e)
        chosen <- if (isTRUE(input$n_pcs >= 1)) round(input$n_pcs) else NA
        # Show every component with an eigenvalue of 1 or more, the chosen
        # number, and a few beyond, but at least 20
        n_show <- max(20, sum(e$eig$eigenvalue >= 1) + 3, chosen + 3, na.rm = TRUE)
        eig <- utils::head(e$eig, n_show)
        breaks <- if (nrow(eig) > 25) seq(1, nrow(eig), by = 2) else eig$component
        ggplot2::ggplot(eig, ggplot2::aes(x = component)) +
          ggplot2::geom_col(ggplot2::aes(y = variance), fill = "#C7EAE5", colour = "black") +
          ggplot2::geom_line(ggplot2::aes(y = cumulative)) +
          ggplot2::geom_point(ggplot2::aes(y = cumulative)) +
          ggplot2::geom_hline(yintercept = pca_min_variance, linetype = "dashed", colour = "grey40") +
          ggplot2::geom_vline(xintercept = chosen, colour = "#1B6A8F", linewidth = 1, na.rm = TRUE) +
          ggplot2::scale_x_continuous(breaks = breaks) +
          ggplot2::scale_y_continuous(limits = c(0, 100)) +
          ggplot2::labs(x = "Principal component", y = "Variance (%)", caption = "Bars: variance of each component. Line: cumulative variance.") +
          ggplot2::theme_bw()
      },
      res = 96
    )

    ## 2. Run the PCA, extract scores and combine with depth statistics
    shiny::observeEvent(input$run_pca, {
      if (!eigen_current()) {
        shiny::showNotification("Settings have changed. Calculate the eigenvalues again first.", type = "error")
        return()
      }
      s <- settings()
      n <- round(input$n_pcs)
      if (!isTRUE(n >= 1 && n <= nrow(eigen()$eig))) {
        shiny::showNotification("Choose a valid number of principal components.", type = "error")
        return()
      }

      res <- shiny::withProgress(message = "Running PCA", value = 0, {
        run_logged({
          shiny::setProgress(0.1, detail = "Combining wavelet statistics")
          pc_data <- make_pc_data(s, output = TRUE)
          shiny::setProgress(0.3, detail = "Principal components and figures")
          pc_res <- pca_results(
            pc_data, standardise = s$standardise, No_pcs = n, plot_eigenvalues = TRUE,
            output = TRUE, output_folder = state$project, verbose = TRUE, interactive_mode = FALSE
          )
          shiny::setProgress(0.6, detail = "Scores and loadings figures")
          pc_scores <- pca_scores(pc_res, plot_loadings = TRUE, output = TRUE, output_folder = state$project, verbose = TRUE)
          shiny::setProgress(0.9, detail = "Combining with depth statistics")
          combine_data(s$tags, data_folder = state$project, pc_scores = pc_scores, output = TRUE, output_folder = state$project, verbose = TRUE)
          invisible(NULL)
        })
      })
      pca_log(c(paste0("== PCA with ", n, " components: ", if (res$ok) "done" else "FAILED"), res$log))
      state$refresh <- state$refresh + 1
    })

    ## Figures
    figure_folder <- shiny::reactive({
      shiny::req(state$project, length(input$tags) > 0)
      file.path(pca_folder(state$project, input$tags), "4_PCA")
    })

    figure_files <- shiny::reactive({
      state$refresh
      files <- list.files(figure_folder(), pattern = "\\.png$")
      # Overview figures first
      first <- intersect(c("PC_Eigenvalues_cum_var.png", "PC_Loadings_ALL_wavelet_variables.png", "PCA_mean_power_line_plot.png"), files)
      c(first, setdiff(sort(files), first))
    })

    shiny::observe({
      files <- figure_files()
      names(files) <- gsub("_", " ", sub("\\.png$", "", files))
      current <- shiny::isolate(input$figure)
      shiny::updateSelectInput(session, "figure", choices = files, selected = if (isTRUE(current %in% files)) current else files[1])
    })

    output$figure_note <- shiny::renderUI({
      if (length(input$tags) == 0) {
        shiny::p(class = "text-muted", "Choose tags to see their PCA figures.")
      } else if (length(figure_files()) == 0) {
        shiny::p(class = "text-muted", "Run the PCA for the chosen tags to see its figures here.")
      }
    })

    output$figure_image <- shiny::renderImage(
      {
        state$refresh
        shiny::req(input$figure)
        file <- file.path(figure_folder(), input$figure)
        shiny::req(file.exists(file))
        list(src = file, contentType = "image/png", style = "max-width: 100%; max-height: 80vh;", alt = input$figure)
      },
      deleteFile = FALSE
    )
  })
}
