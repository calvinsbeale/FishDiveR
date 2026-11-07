# FishDiveR 1.3.0

## New features
- New `plot_time_at()` plots time at depth or time at temperature: the percentage of time in each depth or temperature range, for the whole record, each month, or as a heat map of each day. It returns the table of percentages, with the plot attached. In the app, step 2 has a new "Time at depth & temperature" tab.
- `plot_TDR()` has a new `colour_temp` argument to colour the depth record by temperature, from blue (cold) to red (warm). The app's depth record plot has a "Colour by temperature" option. Both need the tag imported with its temperature column.
- The app's date breaks now include "28 day" and "2 month", and records of over 400 days default to "2 month".
- `import_tag_data()` with `output = TRUE` now also saves the settings it used (archive, deployment and release times, columns and time zone) as `import_settings.rds` in the tag's folder. The app uses this to warn when a tag's settings change after it was imported: step 1 shows a warning when the tag is saved and in its Check column, and step 2 marks the tag "Outdated" and lists what changed until it is re-imported. For tags imported before the settings were saved, the app checks the temperature column and time zone against the imported record itself.
- `import_tag_data(verbose = TRUE)` now reports the mean, minimum and maximum temperature when a temperature column is imported.
- The app's table of tags in step 1 has a Temperature column, ticked for tags imported with temperature data.
- Printing a `sampling_report()` now lists each irregularity and incomplete day with its times, up to `max_rows` of each (5 by default; 20 in the app), and no longer rounds nearly regular records up to "100%" of intervals.

# FishDiveR 1.2.0 (not released on CRAN; included in 1.3.0)

## Breaking change
- `create_wavelet()` no longer has the `allow_irregular_sampling` argument (it was not functional), and no longer stops on non-constant sampling intervals.

## New features
- New `run_app()` opens a point-and-click app for FishDiveR that runs locally in your browser (in development). It manages a project folder and a table of tags, and covers the whole workflow: importing tags, plotting their depth records, the wavelet analysis (with memory estimates and optional thinning), daily statistics, PCA across tags, choosing k and k-means clustering (with an interactive 3D view), and plotting depth records by cluster and example days from each cluster. Needs the 'shiny', 'bslib' and 'DT' packages.
- New `thin_archive()` block-averages a depth record to a longer sampling interval (e.g. 1 s to 15 s) before `create_wavelet()`, cutting the wavelet's memory and run time for long, high-frequency records. In the app, step 3 has a thinning option with a suggested interval based on the lower period.
- `create_wavelet()` now records the wavelet's sampling interval as `sampling_s` in `wavelet_meta`, and with `load_existing_wavelet = TRUE` uses the saved wavelet's own sampling interval for the figures.
- `plot_TDR()` now attaches its ggplot to the returned data as the attribute `TDR_plot`.
- `plot_TDR()` has a new `minmax_bins` argument for fast plotting of large archives: it plots only the shallowest and deepest record in each of that many time bins, keeping dive extremes that `every_nth` can miss. `plot_cluster_TDR()` has the same argument.
- New `sampling_report()` checks a depth record for a constant sampling interval and reports any irregularities: duplicate times, gaps, changes of sampling rate, short intervals, times out of order, and days with fewer records than expected. In the app, step 2 shows each imported tag's sampling check.
- `import_tag_data()` now warns if the sampling interval is not constant, listing the intervals detected and their record counts, and suggests `sampling_report()` for details.

## Fixes
- `create_wavelet()`: the wavelet power spectrum figure no longer has its end cut off for long, high-frequency records (e.g. 29 days at 1 s). Records with millions of samples are now thinned for drawing only; the saved wavelet is unchanged. Redraw existing figures with `load_existing_wavelet = TRUE`.
- `k_clustering()` with `polygon = TRUE` no longer fails when a cluster's points are flat in PC1 to PC3 (e.g. duplicate days); that cluster is drawn without a polygon.
- `plot_clusters()` with `diel_shade = TRUE`: night is now shaded with one rectangle per night period, drawn beneath the depth line, instead of one semi-transparent rectangle per depth record drawn on top. Shading is now about as fast as no shading (e.g. 8 s instead of 19 minutes for 2 days per cluster at 1 s sampling), night-time dives are no longer hidden, and the shade no longer varies with sampling rate or `every_nth`.

## Docs/Infra
- New vignettes: "Getting started with FishDiveR" (the full workflow on the example data), "Using the FishDiveR app", and "Large, high-frequency records" (memory, thinning and fast plotting).
- Citation updated to the published article, with all 14 authors: Beale et al. (2026), *Movement Ecology* 14:8 <https://doi.org/10.1186/s40462-025-00622-w>.
- New package website at <https://calvinsbeale.github.io/FishDiveR/>, rebuilt automatically on every push to main. More about FishDiveR, including a tutorial and analysis services, at <https://calvinbeale.co.uk/fishdiver/>.
- README updated for the CRAN release; added CRAN badges.
- Documentation rebuilt with roxygen2 8.1.1.

# FishDiveR 1.1.0 — 2026-01-26

* First release on CRAN.

# FishDiveR 1.0.0.0

## Changes in version 1.0.0.0
Added optional X-axis limits for TDR plots

# FishDiveR 1.0.1 — 2025-09-25

## Breaking change
- `select_k()` now returns a single **ggplot** object (a `cowplot::plot_grid`) instead of a list. Update downstream code/tests accordingly.

## Fixes
- Silenced R CMD check NOTE by replacing data.table `.(...)` with `list(...)` in `create_depth_stats()`.
- Ensured reproducibility in `select_k()` by using `withr::local_seed()`.

## Docs/Infra
- Rebuilt documentation; `RoxygenNote` updated to 7.3.3.
- Tests updated to check for `ggplot` return class.
- Vignettes refreshed.
