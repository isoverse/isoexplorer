# Dual inlet plot module

A plot view for dual inlet cycle data, wired to a
[`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md):
it plots the selection-filtered aggregated `cycles` with
[`isoreader2::ir_plot_dual_inlet()`](https://isoreader2.isoverse.org/reference/ir_plot_dual_inlet.html),
with unit, species/mass, legend, zoom and PDF-download controls. Pair
`ie_di_plot_ui()` and `ie_di_plot_server()` on one `id`.

## Usage

``` r
ie_di_plot_ui(id)

ie_di_plot_server(id, file)
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

`ie_di_plot_ui()` returns a UI element; `ie_di_plot_server()` returns a
list with a `get_code` generator for the code server (see
[`ie_code_server()`](https://isoexplorer.isoverse.org/reference/ie_code_server.md))

## See also

[`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md),
[`ie_di_metadata_server()`](https://isoexplorer.isoverse.org/reference/ie_typed_metadata_servers.md)

## Examples

``` r
if (interactive()) {
  library(shiny)
  iso <-
    isoreader2::ir_examples_folder() |>
    isoreader2::ir_find_isofiles() |>
    isoreader2::ir_read_isofiles()

  ui <- ie_type_explorer_ui("meta", ie_di_plot_ui("di"))
  server <- function(input, output, session) {
    file <- ie_file_server("files", get_isofiles = reactive(iso))
    ie_di_metadata_server("meta", file)
    ie_di_plot_server("di", file)
  }
  shinyApp(ui, server)
}
```
