#' Continuous flow plot module
#'
#' A plot view for continuous flow trace data, wired to a [ie_file_server()]: it
#' plots the selection-filtered aggregated `traces` with
#' [isoreader2::ir_plot_continuous_flow()], with unit, species/mass, legend, zoom
#' and PDF-download controls. Pair `ie_cf_plot_ui()` and `ie_cf_plot_server()` on one `id`.
#'
#' @inheritParams ie_metadata_server
#' @return `ie_cf_plot_ui()` returns a UI element; `ie_cf_plot_server()` is called for
#'   its side effects
#' @seealso [ie_file_server()], [ie_cf_metadata_server()]
#' @name ie_cf_plot
#' @export
ie_cf_plot_ui <- function(id) {
  data_plot_view_ui(id)
}

#' @rdname ie_cf_plot
#' @export
ie_cf_plot_server <- function(id, file) {
  setup_data_plot(
    id,
    get_data = file$get_aggregated_cf_data,
    get_units = file$get_units,
    set_units = file$set_units,
    dataset_key = "traces",
    plot_fn = isoreader2::ir_plot_continuous_flow,
    no_data_message = "No continuous flow trace data available.",
    download_basename = "continuous_flow",
    zoom_arg = "time_window"
  )
}
