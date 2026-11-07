# Launch the FishDiveR app

`run_app` opens a point-and-click interface to FishDiveR in your web
browser. The app runs locally on your computer, so your tag data never
leave your machine.

## Usage

``` r
run_app(
  project_folder = NULL,
  launch.browser = TRUE,
  stop_on_close = FALSE,
  ...
)
```

## Arguments

- project_folder:

  Optional. Path of a project folder to open on launch. Defaults to
  NULL, choosing the folder in the app.

- launch.browser:

  Logical. If TRUE, the app opens in your web browser. Defaults to TRUE.

- stop_on_close:

  Logical. If TRUE, the app stops a few seconds after its last browser
  tab is closed, rather than running until R is stopped. Used by the
  standalone Windows app. Defaults to FALSE.

- ...:

  Further arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html), e.g.
  `port`.

## Value

Called for its side effect of running the app. Returns invisibly when
the app is closed.

## Details

The app works on a project folder. Tags are listed in a tag table, saved
as `tag_metadata.csv` in the project folder, and every step saves its
results there: one sub-folder per tag, plus `Combined_tags` for the
multi-tag PCA and k-means clustering. This is the same folder structure
the FishDiveR functions use when `output_folder` is the project folder,
so you can move between the app and R code at any point.

The app needs the 'shiny', 'bslib' and 'DT' packages. Install them with
`install.packages(c("shiny", "bslib", "DT"))`.

## Examples

``` r
if (interactive()) {
  run_app()
}
```
