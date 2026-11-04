## Update

This is an update from 1.1.0 to 1.2.0. The main changes are:

* A new point-and-click app, `run_app()`, that runs the workflow locally in the browser. Its packages ('shiny', 'bslib', 'DT', and optionally 'rstudioapi' and 'tcltk') are in Suggests and checked for at run time.
* A new function, `thin_archive()`, to reduce the memory needed for wavelets of long, high-frequency records.
* Three new vignettes.
* Bug fixes in `create_wavelet()`, `k_clustering()` and `plot_clusters()`.
* The citation now lists all 14 authors of the published article.

`create_wavelet()` no longer has the `allow_irregular_sampling` argument, which was not functional.

## Test environments

* Local: Windows 11 x64, R 4.6.1 (`R CMD check --as-cran`, including the PDF manual)
* win-builder: R-devel

## R CMD check results

0 errors | 0 warnings | 0 notes

All URLs were checked with `urlchecker::url_check()`.

## Reverse dependencies

There are currently no reverse dependencies.
