## Resubmission

This is a resubmission of the 1.2.0 submission, now as 1.3.0. The 1.2.0 check time was 13 min on Windows, mainly from the tests (575 s). In this version:

* The three slowest tests, which run the app's whole workflow (wavelets, statistics, PCA and clustering) on the example data, are skipped on CRAN with `skip_on_cran()`. Each step's function is still tested on CRAN on the small example data. Locally, the tests now take 35 s in CRAN mode, down from 226 s.

## Update

This is an update from 1.1.0 to 1.3.0. The main changes are:

* A new point-and-click app, `run_app()`, that runs the workflow locally in the browser. Its packages ('shiny', 'bslib', 'DT', and optionally 'rstudioapi' and 'tcltk') are in Suggests and checked for at run time.
* A new function, `thin_archive()`, to reduce the memory needed for wavelets of long, high-frequency records.
* A new function, `sampling_report()`, to check a depth record for irregular sampling.
* A new function, `plot_time_at()`, to plot time at depth or temperature, and an option in `plot_TDR()` to colour the depth record by temperature.
* Three new vignettes.
* Bug fixes in `create_wavelet()`, `k_clustering()` and `plot_clusters()`.
* The citation now lists all 14 authors of the published article.

`create_wavelet()` no longer has the `allow_irregular_sampling` argument, which was not functional.

## Test environments

* Local: Windows 11 x64, R 4.6.1 (`R CMD check --as-cran`, including the PDF manual)
* win-builder: R-devel

## R CMD check results

Local: 0 errors | 0 warnings | 0 notes

win-builder (R-devel): 0 errors | 0 warnings | 1 note

* Possibly misspelled words in DESCRIPTION: "et" and "al". These are from "et al." in the references to Sakamoto et al. (2009) and Beale et al. (2026), and are spelled correctly.
* Possibly invalid URL: <https://calvinbeale.co.uk/fishdiver/> ("SSL connect error ... Connection was reset"). The URL is valid: it returns status 200 in a browser, with `urlchecker::url_check()`, and with curl using both OpenSSL and Schannel. The site is served through Cloudflare, and the connection from win-builder was reset during the SSL handshake, which I could not reproduce from elsewhere.

## Reverse dependencies

There are currently no reverse dependencies.
