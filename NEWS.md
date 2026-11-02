# FishDiveR (development version)

## Breaking change
- `create_wavelet()` no longer has the `allow_irregular_sampling` argument (it was not functional), and no longer stops on non-constant sampling intervals.

## New features
- New `run_app()` opens a point-and-click app for FishDiveR that runs locally in your browser (in development). It manages a project folder and a table of tags, and currently covers importing tags, plotting their depth records, and running the wavelet analysis with an estimate of the memory each tag needs. Later steps will follow. Needs the 'shiny', 'bslib' and 'DT' packages.
- `plot_TDR()` now attaches its ggplot to the returned data as the attribute `TDR_plot`.
- `plot_TDR()` has a new `minmax_bins` argument for fast plotting of large archives: it plots only the shallowest and deepest record in each of that many time bins, keeping dive extremes that `every_nth` can miss.
- `import_tag_data()` now warns if the sampling interval is not constant, listing the intervals detected and their record counts.

## Fixes
- `create_wavelet()`: the wavelet power spectrum figure no longer has its end cut off for long, high-frequency records (e.g. 29 days at 1 s). Records with millions of samples are now thinned for drawing only; the saved wavelet is unchanged. Redraw existing figures with `load_existing_wavelet = TRUE`.

## Docs/Infra
- Citation updated to the published article: Beale & Gleiss (2026), *Movement Ecology* 14:8 <doi:10.1186/s40462-025-00622-w>.
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
