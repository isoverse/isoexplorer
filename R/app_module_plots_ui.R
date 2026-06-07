plots_ui <- function(id) {
  ns <- NS(id)

  bslib::accordion(
    id = ns("accordion"),
    multiple = TRUE,
    bslib::accordion_panel(
      "Data",
      icon = icon("chart-line"),
      bslib::navset_card_pill(
        id = ns("tabset"),
        bslib::nav_panel("Continuous Flow", cf_plot_ui(ns("cf"))),
        bslib::nav_panel("Dual Inlet", di_plot_ui(ns("di"))),
        bslib::nav_panel("Scans", scans_plot_ui(ns("scans")))
      ) |>
        div(class = "centered-pills") |>
        bslib::as_fill_carrier()
    )
  )
}
