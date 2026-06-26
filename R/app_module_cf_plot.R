#' Continuous flow plot module
#'
#' A plot view for continuous flow trace data, wired to a [ie_file_server()]: it
#' plots the selection-filtered aggregated `traces` with
#' [isoreader2::ir_plot_continuous_flow()], with unit, species/mass, legend, zoom
#' and PDF-download controls. Pair `ie_cf_plot_ui()` and `ie_cf_plot_server()` on one `id`.
#'
#' @inheritParams ie_metadata_server
#' @return `ie_cf_plot_ui()` returns a UI element; `ie_cf_plot_server()` returns a
#'   list with a `get_code` generator for the code server (see [ie_code_server()])
#' @seealso [ie_file_server()], [ie_cf_metadata_server()]
#' @name ie_cf_plot
#' @export
ie_cf_plot_ui <- function(id) {
  ns <- NS(id)
  data_plot_view_ui(
    id,
    # short_time_labels is a continuous-flow-only ir_plot_continuous_flow() option
    extra_options = checkboxInput(
      ns("short_time_labels"),
      "Short time labels",
      value = FALSE
    )
  )
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
    plot_fn_name = "ir_plot_continuous_flow",
    no_data_message = "No continuous flow trace data selected/available.",
    download_basename = "continuous_flow",
    zoom_arg = "time_window.s",
    get_selection = file$get_cf_selection,
    get_all_metadata = file$get_cf_metadata
  )
}
