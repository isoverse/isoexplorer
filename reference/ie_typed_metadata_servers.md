# Typed metadata selectors

Selector-table servers wired to a
[`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md)
for one measurement type: they read that type's metadata and push the
selection back into the file server. Pair each with a
[`ie_metadata_ui()`](https://isoexplorer.isoverse.org/reference/ie_metadata_ui.md)
using the same `id`.

## Usage

``` r
ie_scans_metadata_server(id, file)

ie_cf_metadata_server(id, file)

ie_di_metadata_server(id, file)
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

a
[`ie_metadata_server()`](https://isoexplorer.isoverse.org/reference/ie_metadata_server.md)
handle

## See also

[`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md),
[`ie_metadata_ui()`](https://isoexplorer.isoverse.org/reference/ie_metadata_ui.md)

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
    # pushes the table's selection into the file server for this type
    ie_scans_metadata_server("meta", file)
  }
  shinyApp(ui, server)
}
```
