# Metadata selector module

Server for a file/analysis selector table (pair with
[`ie_metadata_ui()`](https://isoexplorer.isoverse.org/reference/ie_metadata_ui.md)).
It shows a row-grouped table over an already-aggregated metadata tibble
and pushes the current selection out via `set_selected`; double-clicking
a row selects just that analysis, double-clicking a file's group header
selects all of its analyses. Most users want the typed wrappers
[`ie_scans_metadata_server()`](https://isoexplorer.isoverse.org/reference/ie_typed_metadata_servers.md)
/
[`ie_cf_metadata_server()`](https://isoexplorer.isoverse.org/reference/ie_typed_metadata_servers.md)
/
[`ie_di_metadata_server()`](https://isoexplorer.isoverse.org/reference/ie_typed_metadata_servers.md),
which bind the right
[`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md)
accessors; `ie_metadata_server()` itself is the generic version for
wiring a custom metadata source.

## Usage

``` r
ie_metadata_server(
  id,
  get_metadata,
  set_selected = NULL,
  get_selection = NULL,
  get_select_signal = NULL
)
```

## Arguments

- id:

  the module id (must match the paired
  [`ie_metadata_ui()`](https://isoexplorer.isoverse.org/reference/ie_metadata_ui.md))

- get_metadata:

  a reactive returning the metadata tibble to browse (one row per
  analysis, with `uidx` + `analysis` columns)

- set_selected:

  optional `function(rows)` called with the selected metadata rows
  whenever the selection changes (e.g. `file$set_selected_scans`)

- get_selection:

  optional reactive of the current selection, used to reflect it in the
  table on load (e.g. `file$get_scans_selection`)

- get_select_signal:

  optional reactive carrying file paths the table should exclusively
  select (e.g. `file$get_scans_select_signal`, fired by upload
  auto-select); applied once the table contains those files

## Value

a list with the underlying selector-table handle plus the
`get_selected_row_id` / `get_selected_metadata` reactives

## Examples

``` r
if (interactive()) {
  library(shiny)
  iso <-
    isoreader2::ir_examples_folder() |>
    isoreader2::ir_find_isofiles() |>
    isoreader2::ir_read_isofiles()

  ui <- ie_metadata_ui("meta")
  server <- function(input, output, session) {
    file <- ie_file_server("files", get_isofiles = reactive(iso))
    # the generic selector wired to any aggregated-metadata accessors; most
    # users want the typed wrappers ie_scans_metadata_server() / _cf_ / _di_
    ie_metadata_server(
      "meta",
      get_metadata = file$get_scans_metadata,
      set_selected = file$set_selected_scans,
      get_selection = file$get_scans_selection
    )
  }
  shinyApp(ui, server)
}
```
