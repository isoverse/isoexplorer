# isoexplorer 0.4.1

* Initial CRAN submission.
* Provides a Shiny GUI toolkit to browse, plot, and export stable isotope data
  files read with the isoreader2 package.
* Ready-to-run explorer apps: `ie_explore_continuous_flow()`,
  `ie_explore_dual_inlet()`, `ie_explore_scans()`, `ie_explore_metadata()`, and
  the multi-tab `ie_create_isofiles_server()`.
* Composable Shiny modules (`ie_file_server()`, `ie_metadata_ui()` /
  `*_metadata_server()`, `ie_*_plot_ui()` / `ie_*_plot_server()`,
  `ie_code_server()`) to build your own app via `ie_run_app()`.
* "Show code" feature that emits the isoreader2 code reproducing whatever is on
  screen, as plain R or a Quarto document.
