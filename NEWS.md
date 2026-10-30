# FishDiveR (development version)

## Breaking change
- `create_wavelet()` no longer has the `allow_irregular_sampling` argument (it was not functional). Non-constant sampling intervals are now reported with a message rather than stopping.

## New features
- `import_tag_data()` now checks that the sampling frequency is constant and, when `verbose = TRUE`, reports the intervals detected.

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
