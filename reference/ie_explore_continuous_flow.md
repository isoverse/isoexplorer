# Explore an ir_isofiles object by measurement type

Focused apps for exploring an already-read `ir_isofiles` object one
measurement type at a time: `ie_explore_continuous_flow()` /
`ie_explore_dual_inlet()` / `ie_explore_scans()` each show that type's
file-selector sidebar + plot, and `ie_explore_metadata()` shows just the
selector table. The generated example code (navbar **Show code**) refers
to the object by its `variable_name`. These take a fixed object only –
for upload / folder monitoring / load-examples use
[`ie_create_isofiles_server()`](https://isoexplorer.isoverse.org/reference/ie_create_isofiles_server.md).

## Usage

``` r
ie_explore_continuous_flow(
  isofiles,
  timezone = Sys.timezone(),
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url",
  default_theme = app_themes(),
  initial_selection = FALSE,
  variable_name = NULL,
  detached = TRUE,
  launch = TRUE,
  log_level = "WARN"
)

ie_explore_dual_inlet(
  isofiles,
  timezone = Sys.timezone(),
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url",
  default_theme = app_themes(),
  initial_selection = FALSE,
  variable_name = NULL,
  detached = TRUE,
  launch = TRUE,
  log_level = "WARN"
)

ie_explore_scans(
  isofiles,
  timezone = Sys.timezone(),
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url",
  default_theme = app_themes(),
  initial_selection = FALSE,
  variable_name = NULL,
  detached = TRUE,
  launch = TRUE,
  log_level = "WARN"
)

ie_explore_metadata(
  isofiles,
  timezone = Sys.timezone(),
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url",
  default_theme = app_themes(),
  initial_selection = FALSE,
  variable_name = NULL,
  detached = TRUE,
  launch = TRUE,
  log_level = "WARN"
)
```

## Arguments

- isofiles:

  the `ir_isofiles` object to explore (required)

- timezone:

  timezone for datetime display

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

  default bslib Bootstrap 5 theme preset

- initial_selection:

  what is selected on load, as a
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  expression on the aggregated metadata: `FALSE` (the default) selects
  nothing, `TRUE` selects everything, and any other expression (e.g.
  `grepl("std", file_name)`) selects the matching files/analyses. See
  [`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md).
  In `detached` mode the expression is re-evaluated in the separate
  process, so it must be self-contained (it cannot reference variables
  from your session).

- variable_name:

  the name used for the object in the generated example code. Defaults
  to the deparsed expression you passed (so
  `my_iso |> ie_explore_scans()` uses `"my_iso"`); set it explicitly to
  override.

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

- log_level:

  how verbosely the app logs; see
  [`ie_run_app()`](https://isoexplorer.isoverse.org/reference/ie_run_app.md)
  for the available levels. Defaults to `"WARN"` for the focused
  explorers.

## Value

the value of
[`ie_run_app()`](https://isoexplorer.isoverse.org/reference/ie_run_app.md)
for the chosen `detached` / `launch`: the
[`callr::r_bg()`](https://callr.r-lib.org/reference/r_bg.html) process
(detached), the
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) result
(in-session launch), or the
[`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html)
object (`launch = FALSE`).

## Details

By default the app runs **detached** in a separate R process (see
`detached`), so the calling session is not blocked, and it refuses to
launch while a document is being rendered (knitr / Quarto), showing a
message to run it interactively. These are thin wrappers over
[`ie_run_app()`](https://isoexplorer.isoverse.org/reference/ie_run_app.md),
which manages `detached` / `launch`.

## Functions

- `ie_explore_dual_inlet()`: focused app for the dual inlet plot.

- `ie_explore_scans()`: focused app for the scans plot.

- `ie_explore_metadata()`: focused app showing just the scans metadata
  selector table (handy for browsing/testing the selector).

## Examples

``` r
if (interactive()) {
  # read the bundled isoreader2 examples (a mixed set of all types); each
  # explorer filters the object to its own measurement type
  iso <-
    isoreader2::ir_examples_folder() |>
    isoreader2::ir_find_isofiles() |>
    isoreader2::ir_read_isofiles()

  # each focused explorer opens the object in a detached browser app
  ie_explore_continuous_flow(iso)
  ie_explore_dual_inlet(iso)
  ie_explore_scans(iso)
  ie_explore_metadata(iso) # just the selector table

  # preselect some files and run blocking in the current session instead
  ie_explore_continuous_flow(
    iso,
    initial_selection = grepl("gc", file_name),
    detached = FALSE
  )
}
```
