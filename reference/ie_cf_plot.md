# Continuous flow plot module

A plot view for continuous flow trace data, wired to a
[`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md):
it plots the selection-filtered aggregated `traces` with
[`isoreader2::ir_plot_traces()`](https://isoreader2.isoverse.org/reference/ir_plot_traces.html),
with unit, species/mass, legend, zoom and PDF-download controls. Pair
`ie_cf_plot_ui()` and `ie_cf_plot_server()` on one `id`.

## Usage

``` r
ie_cf_plot_ui(id)

ie_cf_plot_server(id, file)
```

## Arguments

- id:

  the module id (must match the paired
  [`ie_metadata_ui()`](https://isoexplorer.isoverse.org/reference/ie_metadata_ui.md))

- file:

  the
  [`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md)
  handle

## Value

`ie_cf_plot_ui()` returns a UI element; `ie_cf_plot_server()` returns a
list with a `get_code` generator for the code server (see
[`ie_code_server()`](https://isoexplorer.isoverse.org/reference/ie_code_server.md))

## See also

[`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md),
[`ie_cf_metadata_server()`](https://isoexplorer.isoverse.org/reference/ie_typed_metadata_servers.md)

## Examples

``` r
if (interactive()) {
  library(shiny)
  iso <-
    isoreader2::ir_examples_folder() |>
    isoreader2::ir_find_isofiles() |>
    isoreader2::ir_read_isofiles()

  ui <- ie_type_explorer_ui("meta", ie_cf_plot_ui("cf"))
  server <- function(input, output, session) {
    file <- ie_file_server("files", get_isofiles = reactive(iso))
    ie_cf_metadata_server("meta", file)
    ie_cf_plot_server("cf", file)
  }
  shinyApp(ui, server)
}
```
