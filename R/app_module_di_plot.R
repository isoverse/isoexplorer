#' Dual inlet plot module
#'
#' A plot view for dual inlet cycle data, wired to a [ie_file_server()]: it plots the
#' selection-filtered aggregated `cycles` with
#' [isoreader2::ir_plot_dual_inlet()], with unit, species/mass, legend, zoom and
#' PDF-download controls. Pair `ie_di_plot_ui()` and `ie_di_plot_server()` on one `id`.
#'
#' @inheritParams ie_metadata_server
#' @return `ie_di_plot_ui()` returns a UI element; `ie_di_plot_server()` returns a
#'   list with a `get_code` generator for the code server (see [ie_code_server()])
#' @seealso [ie_file_server()], [ie_di_metadata_server()]
#' @name ie_di_plot
#' @export
ie_di_plot_ui <- function(id) {
  data_plot_view_ui(id)
}

#' @rdname ie_di_plot
#' @export
ie_di_plot_server <- function(id, file) {
  setup_data_plot(
    id,
    get_data = file$get_aggregated_di_data,
    get_units = file$get_units,
    set_units = file$set_units,
    dataset_key = "cycles",
    plot_fn = isoreader2::ir_plot_dual_inlet,
    plot_fn_name = "ir_plot_dual_inlet",
    no_data_message = "No dual inlet cycle data available.",
    download_basename = "dual_inlet",
    zoom_arg = "cycle_window",
    get_selection = file$get_di_selection,
    get_all_metadata = file$get_di_metadata
  )
}
