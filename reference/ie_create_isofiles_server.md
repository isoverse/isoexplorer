# Create an isofiles server app for the isoexplorer GUI

Builds the full isoexplorer Shiny app – one navbar tab per measurement
type (continuous flow / dual inlet / scans), each with a file-selector
sidebar and plot – and returns it as a
[`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html)
object to run or deploy. Unlike the focused
[`ie_explore_continuous_flow()`](https://isoexplorer.isoverse.org/reference/ie_explore_continuous_flow.md)
explorers, this app does NOT take an `ir_isofiles` object – data arrives
at runtime via the navbar **Upload** button, any watched
`monitoring_folders`, and/or the **Load examples** button (a "get
started" prompt is shown until something is loaded). Loaded examples are
selected automatically; uploaded files are selected only when the upload
modal's "Select the uploaded files" box is checked.

## Usage

``` r
ie_create_isofiles_server(
  timezone = Sys.timezone(),
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url",
  default_theme = app_themes(),
  upload_folder = NULL,
  monitoring_folders = NULL,
  examples_folder = "examples",
  temporary_storage = FALSE,
  max_upload_size = NULL,
  log_level = "TRACE"
)
```

## Arguments

- timezone:

  the timezone to use for datetime display

- options:

  Named options that should be passed to the `runApp` call (these can be
  any of the following: "port", "launch.browser", "host", "quiet",
  "display.mode" and "test.mode"). You can also specify `width` and
  `height` parameters which provide a hint to the embedding environment
  about the ideal height/width for the app.

- uiPattern:

  A regular expression that will be applied to each `GET` request to
  determine whether the `ui` should be used to handle the request. Note
  that the entire request path must match the regular expression in
  order for the match to be considered successful.

- enableBookmarking:

  Can be one of `"url"`, `"server"`, or `"disable"`. The default value,
  `NULL`, will respect the setting from any previous calls to
  [`enableBookmarking()`](https://rdrr.io/pkg/shiny/man/enableBookmarking.html).
  See
  [`enableBookmarking()`](https://rdrr.io/pkg/shiny/man/enableBookmarking.html)
  for more information on bookmarking your app.

- default_theme:

  the default bslib Bootstrap 5 theme preset

- upload_folder:

  upload directory for the navbar upload button; `NULL` (the default)
  means no upload button, a path enables it; see
  [`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md)

- monitoring_folders:

  folders to watch for new isofiles, read and added automatically
  (`NULL` = off); see
  [`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md)

- examples_folder:

  directory the "Load examples" navbar button copies the isoreader2
  bundled example files into and loads; `"examples"` by default (`NULL`
  hides the button)

- temporary_storage:

  if `TRUE`, the upload dialog notes that uploaded files are stored only
  for the duration of the session (informational; default `FALSE`)

- max_upload_size:

  maximum per-file upload size in MB (sets the `shiny.maxRequestSize`
  option); `NULL` (the default) keeps Shiny's ~5 MB default. Raw
  isofiles are often larger, so raise this when allowing uploads.

- log_level:

  how verbosely the app logs; see
  [`ie_run_app()`](https://isoexplorer.isoverse.org/reference/ie_run_app.md).
  Defaults to `"TRACE"` (log everything) for this server app, unlike the
  focused explorers which default to `"WARN"`.

## Value

a [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html)
object (unrun)

## Details

Because it returns the app object unrun, launch it yourself (print it,
or wrap it in
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)) or hand
it to a deployment tool (e.g. shinyapps.io / ShinyProxy).

## Examples

``` r
if (interactive()) {
  # build the full multi-tab server app and run it; load data via the navbar
  # "Load examples" / "Upload" buttons at runtime
  ie_create_isofiles_server() |> shiny::runApp()

  # enable uploads (raise the per-file cap for large raw isofiles)
  app <- ie_create_isofiles_server(
    upload_folder = "uploads",
    max_upload_size = 200
  )
  shiny::runApp(app)
}
```
