# Continuous Flow plot UI + server =====
# thin wrapper over the shared data-plot framework (app_plots.R): plots the
# aggregated `traces` with ir_plot_continuous_flow.

cf_plot_ui <- function(id) {
  data_plot_view_ui(id)
}

cf_plot_server <- function(id, get_isofiles) {
  setup_data_plot(
    id,
    get_isofiles = get_isofiles,
    dataset_key = "traces",
    plot_fn = isoreader2::ir_plot_continuous_flow,
    no_data_message = "No continuous flow trace data available.",
    download_basename = "continuous_flow",
    zoom_arg = "time_window"
  )
}
