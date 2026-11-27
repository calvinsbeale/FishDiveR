# FishDiveR (development version)

## Records with gaps
- New `regularise_archive()` puts a depth record on a regular time grid from local midnight, as the wavelet analysis needs. Duplicate times, times out of order and changes of sampling rate are resolved. Gaps of up to `max_fill_mins` (5 minutes by default, at most 6 hours) are filled by linear interpolation, with a warning. Longer gaps are filled only so that the wavelet can run, and marked in a `long_gap` column. Each day's coverage by real records is recorded, and days below `min_coverage` (70% by default) are left out of the statistics.
- `import_tag_data()` now runs `regularise_archive()`, with new `max_fill_mins` and `min_coverage` arguments. Imported records have `filled` and `long_gap` columns, and keep the `sampling_report()` of the original records as an attribute. This replaces the warning about a non-constant sampling interval.
- `create_wavelet_stats()` now finds each day by its local date, rather than counting a day's worth of records from the start, so gaps no longer shift later days. Wavelet power within the e-folding time of each scale of a long gap (about 1.35 times the period either side; Torrence & Compo 1998) is left out, and days with too little valid power at any period are left out. Power is rescaled to the variance of the real records. New `mask_ends` treats the start and end of the record the same way. The days left out, and why, are in the attribute `dropped_days`.
- `create_depth_stats()` leaves out records filled in long gaps and days below the coverage threshold, and no longer measures vertical speed across a gap.
- The PCA scores now keep each row's tag and date, and `combine_data()` matches the depth statistics to them by date, as the wavelet statistics can leave out days that the depth statistics keep.
- `thin_archive()` keeps the gap flags and day coverage. `plot_TDR()` breaks the line at long gaps, and `plot_time_at()` leaves out their filled values.
- `k_clustering()` now keeps each day's date (`date_only`) with its tag, and `plot_clusters()` and `plot_cluster_TDR()` match clusters to days by date. Before, they matched them in order, which gave days the wrong cluster when any day had been left out. Days not clustered are grey in `plot_cluster_TDR()`. Older k-means results without dates are matched in order as before.
- `plot_clusters()` and `plot_cluster_TDR()` break the line at long gaps.
- In the app, step 2 has the gap fill limit and day coverage settings, a **Days kept** column, and lists the gaps filled and days left out for the selected tag. Tags imported before gaps were filled, or with other gap settings, are marked **Outdated**. Step 4 has the option to leave out wavelet power near the start and end of each tag.

## Daylight saving and time zones
- Days with a clock change are now 23 or 25 hours long throughout: in `regularise_archive()`'s day coverage, and in `create_wavelet_stats()`, which before split them into 24 hour blocks.
- `plot_clusters()` now plots each example day against the time since its local midnight, so a day with a clock change runs 23 or 25 hours instead of repeating or skipping an hour of clock time. Times are taken in each tag's own time zone, so tags from different time zones plot correctly together.
- `create_depth_stats()` with fixed `sunrise_time` and `sunset_time` now reads them in the tag's time zone. Before, they were read in the computer's time zone, which shifted day and night for tags in other time zones (e.g. by 9 hours for a tag in Tokyo analysed on a computer in the UK). Diel statistics from fixed times should be made again. Times from GPS files were not affected.

## New features
- In the app, the deployment and release times in step 1 are typed as digits only: the dashes, space and colons of `yyyy-mm-dd hh:mm:ss` are added as you type, dates pasted with other separators (e.g. `2024/3/5 8:30`) are tidied, a date without a time gets `00:00:00`, and impossible dates or times are marked. A calendar opens as soon as the box has the cursor, to pick the year, then the month, then the day; once the date is picked or typed, a clock follows, to pick the hour, then the minute, then the second. Calendar and clock buttons beside the box open them too. The fixed sunrise and sunset times in step 4 work the same way, as `hh:mm:ss` with the clock.

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
