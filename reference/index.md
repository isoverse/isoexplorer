# Package index

## Explorer apps

These functions launch ready-to-run apps for exploring isofiles read
with isoreader2.

- [`ie_explore_continuous_flow()`](https://isoexplorer.isoverse.org/reference/ie_explore_continuous_flow.md)
  [`ie_explore_dual_inlet()`](https://isoexplorer.isoverse.org/reference/ie_explore_continuous_flow.md)
  [`ie_explore_scans()`](https://isoexplorer.isoverse.org/reference/ie_explore_continuous_flow.md)
  [`ie_explore_metadata()`](https://isoexplorer.isoverse.org/reference/ie_explore_continuous_flow.md)
  : Explore an ir_isofiles object by measurement type
- [`ie_create_isofiles_server()`](https://isoexplorer.isoverse.org/reference/ie_create_isofiles_server.md)
  : Create an isofiles server app for the isoexplorer GUI
- [`ie_run_app()`](https://isoexplorer.isoverse.org/reference/ie_run_app.md)
  : Assemble and launch a custom isoexplorer app

## File server

The hub every module is wired through - it holds the read isofiles,
splits them by measurement type, and owns the shared intensity units and
the file selection.

- [`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md)
  [`ie_file_ui()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md)
  : Central file-management module

## File selector modules

These modules show the metadata of the available files and push the
user’s selection into the file server.

- [`ie_metadata_ui()`](https://isoexplorer.isoverse.org/reference/ie_metadata_ui.md)
  : Metadata selector table UI
- [`ie_metadata_server()`](https://isoexplorer.isoverse.org/reference/ie_metadata_server.md)
  : Metadata selector module
- [`ie_scans_metadata_server()`](https://isoexplorer.isoverse.org/reference/ie_typed_metadata_servers.md)
  [`ie_cf_metadata_server()`](https://isoexplorer.isoverse.org/reference/ie_typed_metadata_servers.md)
  [`ie_di_metadata_server()`](https://isoexplorer.isoverse.org/reference/ie_typed_metadata_servers.md)
  : Typed metadata selectors
- [`ie_type_explorer_ui()`](https://isoexplorer.isoverse.org/reference/ie_type_explorer_ui.md)
  : File-selector + plot layout for one measurement type

## Plot modules

These modules plot the selected data, one per measurement type, with
unit, species/mass, zoom, plot option and PDF download controls.

- [`ie_cf_plot_ui()`](https://isoexplorer.isoverse.org/reference/ie_cf_plot.md)
  [`ie_cf_plot_server()`](https://isoexplorer.isoverse.org/reference/ie_cf_plot.md)
  : Continuous flow plot module
- [`ie_di_plot_ui()`](https://isoexplorer.isoverse.org/reference/ie_di_plot.md)
  [`ie_di_plot_server()`](https://isoexplorer.isoverse.org/reference/ie_di_plot.md)
  : Dual inlet plot module
- [`ie_scans_plot_ui()`](https://isoexplorer.isoverse.org/reference/ie_scans_plot.md)
  [`ie_scans_plot_server()`](https://isoexplorer.isoverse.org/reference/ie_scans_plot.md)
  : Scans plot module

## Code generation

The “Show code” module that assembles the isoreader2 code reproducing
whatever is on screen.

- [`ie_code_server()`](https://isoexplorer.isoverse.org/reference/ie_code_server.md)
  [`ie_code_ui()`](https://isoexplorer.isoverse.org/reference/ie_code_server.md)
  : Code-generation server

## Package

- [`isoexplorer`](https://isoexplorer.isoverse.org/reference/isoexplorer-package.md)
  [`isoexplorer-package`](https://isoexplorer.isoverse.org/reference/isoexplorer-package.md)
  : isoexplorer: GUI Components to Explore Stable Isotope Data Files
