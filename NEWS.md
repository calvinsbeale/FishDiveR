# FishDiveR (development version)

* Initial CRAN submission.

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
