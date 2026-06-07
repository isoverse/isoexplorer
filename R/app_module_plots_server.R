plots_server <- function(id, get_aggregated_data, get_selected_metadata) {
  moduleServer(id, function(input, output, session) {
    cf <- cf_plot_server("cf", get_aggregated_data, get_selected_metadata)
    di <- di_plot_server("di", get_aggregated_data, get_selected_metadata)
    scans <- scans_plot_server("scans", get_aggregated_data, get_selected_metadata)
    list(cf = cf, di = di, scans = scans)
  })
}
