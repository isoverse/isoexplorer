# Assemble and launch a custom isoexplorer app

Wraps your own module composition in the isoexplorer navbar shell (theme
picker, dark-mode toggle, about popup) and, by default, runs it. Build
your layout from the package's modules, instantiate the
[`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md)
and wire the modules in `setup_modules`. Supply EITHER `main` (a single
content area) OR `nav_panels` (a centered navbar tabset).

## Usage

``` r
ie_run_app(
  isofiles,
  main = NULL,
  setup_modules,
  timezone = Sys.timezone(),
  default_theme = app_themes(),
  nav_panels = NULL,
  selected = NULL,
  initial_selection = TRUE,
  upload_folder = NULL,
  monitoring_folders = NULL,
  examples_folder = NULL,
  temporary_storage = FALSE,
  max_upload_size = NULL,
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url",
  detached = FALSE,
  launch = TRUE,
  stop_on_close = detached,
  log_level = c("WARN", "INFO", "DEBUG", "TRACE", "ERROR", "FATAL")
)
```

## Arguments

- isofiles:

  the `ir_isofiles` to explore (handed to the
  [`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md))

- main:

  a single UI content area (use this OR `nav_panels`)

- setup_modules:

  `function(file, code)` that wires the app's server modules, given the
  [`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md)
  handle and the
  [`ie_code_server()`](https://isoexplorer.isoverse.org/reference/ie_code_server.md)
  handle (register each module's `get_code` with `code$register()`). A
  one-argument `function(file)` is still accepted (the code server is
  then not populated).

- timezone:

  timezone for datetime display

- default_theme:

  default bslib Bootstrap 5 theme preset

- nav_panels:

  a list of
  [`bslib::nav_panel()`](https://rstudio.github.io/bslib/reference/nav-items.html)s
  shown as a centered navbar tabset (use this OR `main`)

- selected:

  the value/title of the `nav_panels` tab to open initially (`NULL` =
  the first tab)

- initial_selection:

  initial file selection, see
  [`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md)

- upload_folder:

  upload directory for the navbar upload button; `NULL` (the default)
  means no upload button, see
  [`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md)

- monitoring_folders:

  folders to watch for new isofiles (`NULL` = off), see
  [`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md)

- examples_folder:

  directory for the "Load examples" navbar button (`NULL` = off), see
  [`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md)

- temporary_storage:

  note in the upload dialog that uploads are session-only
  (informational; default `FALSE`), see
  [`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md)

- max_upload_size:

  maximum per-file upload size in MB; sets the `shiny.maxRequestSize`
  option for the running app. `NULL` (the default) leaves Shiny's ~5 MB
  default (or a value you set yourself) untouched. Raw isofiles are
  often larger than 5 MB, so raise this when enabling uploads.

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

- detached:

  if `TRUE`, the app is launched in a separate R process (via
  [`callr::r_bg()`](https://callr.r-lib.org/reference/r_bg.html), the
  app handed over in a temporary `.rds`) and opened in your default
  browser, leaving the calling session free; closing the browser tab
  stops the app and the process (which is also killed if the calling
  session exits). Default `FALSE`.

- launch:

  only relevant when `detached = FALSE`: if `TRUE` (the default) the app
  is run in the current session (blocking) with
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html); if
  `FALSE` the
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html)
  object is returned unrun (for server deployment or manual launching).
  Ignored when `detached = TRUE` (a detached app is always run).

- stop_on_close:

  if `TRUE`, the app stops itself when the browser disconnects
  (`session$onSessionEnded()`); set automatically for the detached
  process so it exits when the tab is closed. Defaults to the same as
  `detached`.

- log_level:

  how verbosely the app logs, one of `"WARN"` (the default – only
  warnings and errors), `"INFO"`, `"DEBUG"`, `"TRACE"` (everything),
  `"ERROR"`, or `"FATAL"`. Sets the `LOG_LEVEL` environment variable
  that [rlog](https://rdrr.io/pkg/rlog/man/log_trace.html) reads (also
  applied in the detached process).

## Value

When `detached = TRUE`, the
[`callr::r_bg()`](https://callr.r-lib.org/reference/r_bg.html) process
(invisibly, with the app URL attached as an attribute); when
`detached = FALSE` and `launch = TRUE`, the result of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) (after
the app stops); when `detached = FALSE` and `launch = FALSE`, the
[`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html)
object.

## Details

The `detached` / `launch` arguments control how it runs:
`detached = TRUE` launches it in a separate R process (leaving this
session free); `detached = FALSE, launch = TRUE` (the default) runs it
in the current session (blocking) with
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html);
`detached = FALSE, launch = FALSE` returns the
[`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html)
object unrun (for deployment or manual launching). The focused explorers
([`ie_explore_continuous_flow()`](https://isoexplorer.isoverse.org/reference/ie_explore_continuous_flow.md)
etc.) and
[`ie_create_isofiles_server()`](https://isoexplorer.isoverse.org/reference/ie_create_isofiles_server.md)
are wrappers that call this with the appropriate `detached` / `launch`.

## Examples

``` r
if (interactive()) {
  # read the bundled isoreader2 examples (a mixed set of all types)
  iso <-
    isoreader2::ir_examples_folder() |>
    isoreader2::ir_find_isofiles() |>
    isoreader2::ir_read_isofiles()

  # a minimal custom app: a scans selector next to a scans plot
  ie_run_app(
    isofiles = iso,
    main = ie_type_explorer_ui("meta", ie_scans_plot_ui("scan")),
    setup_modules = function(file, code) {
      ie_scans_metadata_server("meta", file)
      ie_scans_plot_server("scan", file)
    }
  )
}
```
