# Central file-management module

The single source of truth for the isofiles in an isoexplorer app, and
the hub every other module talks to. It maintains a *running* set of
read isofiles (seeded from `get_isofiles()`, grown by uploads and
watched folders), splits it into the three measurement types (scans /
continuous flow / dual inlet) with
[`isoreader2::ir_filter_for_scans()`](https://isoreader2.isoverse.org/reference/ir_filter_for.html)
and friends, owns the shared intensity-units selection and the per-type
file selection, and exposes the per-type metadata (for the
[`ie_metadata_server()`](https://isoexplorer.isoverse.org/reference/ie_metadata_server.md)
selector tables) and the selection-filtered aggregated data (for the
`*_plot_server()` modules).

## Usage

``` r
ie_file_server(
  id,
  get_isofiles,
  initial_selection = TRUE,
  upload_folder = NULL,
  monitoring_folders = NULL,
  examples_folder = NULL,
  temporary_storage = FALSE
)

ie_file_ui(id)
```

## Arguments

- id:

  the module id (namespace)

- get_isofiles:

  a reactive returning an `ir_isofiles` to seed the app with (already
  read); `reactive(NULL)` is fine when files only arrive via upload /
  monitoring

- initial_selection:

  what is selected, per type, before any selector pushes a selection. An
  expression evaluated as a
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  on the type's aggregated metadata tibble: `TRUE` (default) selects
  everything, `FALSE` selects nothing, and any other expression selects
  the matching rows (e.g. `grepl("std", file_name)`). Captured via tidy
  evaluation, so it may reference variables from the calling
  environment. Passed pre-quoted (a quosure) by the `ie_explore_*()` /
  [`ie_run_app()`](https://isoexplorer.isoverse.org/reference/ie_run_app.md)
  wrappers.

- upload_folder:

  directory where uploaded files are stored (created on demand); `NULL`
  (the default) means no upload button – set a directory path to enable
  uploads.

- monitoring_folders:

  character vector of folders to watch; isofiles found there with
  [`isoreader2::ir_find_isofiles()`](https://isoreader2.isoverse.org/reference/ir_find_isofiles.html)
  are read and added automatically. `NULL` (default) disables
  monitoring.

- examples_folder:

  directory the "Load examples" navbar button copies the isoreader2
  bundled example files into (and then loads). `NULL` (the default)
  means no examples button.

- temporary_storage:

  if `TRUE`, the upload dialog states that uploaded files are stored
  only for the duration of the session. Informational only – it does not
  change how or where files are stored (default `FALSE`).

## Value

The "file handle": a list of reactive accessors / setters. For each
`<type>` in `scans` / `cf` / `di`: `get_units()`/`set_units(units)`
(shared intensity units, default "mV"); `get_<type>_metadata()`;
`set_selected_<type>(rows)`; `get_<type>_selection()`;
`get_aggregated_<type>_data()`; plus `get_<type>_select_signal()` (file
paths a selector should select, fired by upload auto-select) and
`get_active_type()` (the type whose tab to activate after an
auto-select).

## Details

Selector tables read `get_<type>_metadata()` and push their selection
via `set_selected_<type>()`; plot modules read
`get_aggregated_<type>_data()` and drive the shared units via
`get_units()` / `set_units()`.

**Dynamic files.** New files (uploaded, or appearing in
`monitoring_folders`) are read with
[`isoreader2::ir_read_isofiles()`](https://isoreader2.isoverse.org/reference/ir_read_isofiles.html)
and appended; aggregation is incremental (only new files are
read/aggregated, then combined with
[`c()`](https://rdrr.io/r/base/c.html)), so already-read files are never
re-read.

**Upload.** When `upload_folder` is set the module owns a navbar upload
button (the `ie_file_ui()` placeholder) that opens a modal to upload
multiple files or whole folders bundled as `.zip` archives (the picker
allows `.zip`, the file types isoreader2 reads, and `.json` – which
covers their `.<type>.json` serializations such as `foo.cf.json`).
Uploaded files are stored in `upload_folder` (archives unpacked), and
**only the just-uploaded files are read** – files already present in
`upload_folder` when the app started are left untouched. An "Auto-select
the newly uploaded files" checkbox (off by default) exclusively selects
the new files in the relevant type's table and, in the full multi-tab
app, switches to that type's tab.

**Monitoring.** `monitoring_folders` are polled; any isofiles found
there with
[`isoreader2::ir_find_isofiles()`](https://isoreader2.isoverse.org/reference/ir_find_isofiles.html)
(including files already present at startup) are read and added.

**Getting started.** When `examples_folder` and/or `upload_folder` is
set but the app is launched without data (empty `get_isofiles()`), a
prompt is shown once inviting the user to load the examples and/or
upload their own files; an app launched with data never sees it.

## Functions

- `ie_file_ui()`: the navbar placeholders for the "Load examples" and
  "Upload" buttons (each rendered only when the server's
  `examples_folder` / `upload_folder` is set). Pair with
  `ie_file_server()` on the same `id`.

## Examples

``` r
if (interactive()) {
  library(shiny)
  # read the bundled isoreader2 examples for some scans data
  iso <-
    isoreader2::ir_examples_folder() |>
    isoreader2::ir_find_isofiles() |>
    isoreader2::ir_read_isofiles()

  ui <- bslib::page_fillable(
    ie_type_explorer_ui("meta", ie_scans_plot_ui("scan"))
  )
  server <- function(input, output, session) {
    # the hub: every other module reads/writes through this handle
    file <- ie_file_server("files", get_isofiles = reactive(iso))
    ie_scans_metadata_server("meta", file) # selection -> file server
    ie_scans_plot_server("scan", file)     # file server -> plot
  }
  shinyApp(ui, server)
}
```
