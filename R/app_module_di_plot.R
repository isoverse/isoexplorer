# Dual Inlet plot UI + server =====
# thin wrapper over the shared data-plot framework (app_plots.R): plots the
# aggregated `cycles` with ir_plot_dual_inlet.

di_plot_ui <- function(id) {
  data_plot_view_ui(id)
}

di_plot_server <- function(id, get_isofiles) {
  setup_data_plot(
    id,
    get_isofiles = get_isofiles,
    dataset_key = "cycles",
    plot_fn = isoreader2::ir_plot_dual_inlet,
    no_data_message = "No dual inlet cycle data available.",
    download_basename = "dual_inlet"
  )
}
