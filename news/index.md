# Changelog

## isoexplorer 0.5.0

- Requires **isoreader2 (\>= 0.7.0)** and adopts its new plotting API.

- Continuous flow plots now call
  [`ir_plot_traces()`](https://isoreader2.isoverse.org/reference/ir_plot_traces.html)
  (isoreader2 renamed
  [`ir_plot_continuous_flow()`](https://isoreader2.isoverse.org/reference/ir_plot_traces.html)),
  both for the plot itself and in the generated code.

- The **Color by** option gained a `(default)` choice, which is now the
  initial one: the colour aesthetic is left to isoreader2 entirely, so
  an intensity trace and its ratios share a colour and a single legend
  entry (`CO2: 45, 45/44`). Picking `trace` gives every trace its own
  colour and legend entry, as before. The generated code emits
  `color = ...` only when you override the default.

- The mass / ratio checkboxes no longer filter the plotted data in the
  app. The selection is passed to the plot functions as their own
  `species =` / `mass =` / `ratio =` arguments and isoreader2 does the
  sub-selecting, so the plot and the generated code can no longer
  disagree about what is shown. Both arguments default to
  `everything()`, so they are emitted only when the selection is
  narrowed.

- Fixed un-checking every **ratio** of a species still plotting all of
  its ratios. The `ratio` argument was simply omitted when nothing was
  selected, which the plot functions read as “all of them”; an empty
  selection is now passed explicitly as
  [`c()`](https://rdrr.io/r/base/c.html).

- Un-checking every **mass** while keeping a ratio checked now plots the
  ratios on their own (`mass = c()`) instead of an empty plot.
  Previously the app filtered the data by mass first, which removed the
  ratio rows too, since a ratio lives on the row of its numerator mass.

## isoexplorer 0.4.1

CRAN release: 2026-07-22

- Initial CRAN submission.
- Provides a Shiny GUI toolkit to browse, plot, and export stable
  isotope data files read with the isoreader2 package.
- Ready-to-run explorer apps:
  [`ie_explore_continuous_flow()`](https://isoexplorer.isoverse.org/reference/ie_explore_continuous_flow.md),
  [`ie_explore_dual_inlet()`](https://isoexplorer.isoverse.org/reference/ie_explore_continuous_flow.md),
  [`ie_explore_scans()`](https://isoexplorer.isoverse.org/reference/ie_explore_continuous_flow.md),
  [`ie_explore_metadata()`](https://isoexplorer.isoverse.org/reference/ie_explore_continuous_flow.md),
  and the multi-tab
  [`ie_create_isofiles_server()`](https://isoexplorer.isoverse.org/reference/ie_create_isofiles_server.md).
- Composable Shiny modules
  ([`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md),
  [`ie_metadata_ui()`](https://isoexplorer.isoverse.org/reference/ie_metadata_ui.md)
  / `*_metadata_server()`, `ie_*_plot_ui()` / `ie_*_plot_server()`,
  [`ie_code_server()`](https://isoexplorer.isoverse.org/reference/ie_code_server.md))
  to build your own app via
  [`ie_run_app()`](https://isoexplorer.isoverse.org/reference/ie_run_app.md).
- “Show code” feature that emits the isoreader2 code reproducing
  whatever is on screen, as plain R or a Quarto document.
