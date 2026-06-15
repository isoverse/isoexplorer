#' Metadata selector table UI
#'
#' The UI for a [ie_metadata_server()] file/analysis selector: a fullscreen-capable
#' card with a toolbar (select-all / deselect on the left, column / search
#' controls on the right) above a table that fills the rest of the card. Pair it
#' with one of the `*_metadata_server()` functions using the same `id`.
#'
#' @param id the module id (must match the paired `*_metadata_server()`)
#' @return a [bslib::card()] UI element
#' @seealso [ie_scans_metadata_server()], [ie_file_server()]
#' @export
ie_metadata_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_body(
      div(
        class = "d-flex align-items-center justify-content-between gap-2 mb-2",
        # left: selection controls
        div(
          class = "d-flex gap-2",
          module_selector_table_select_all_button(ns("metadata")),
          module_selector_table_deselect_all_button(ns("metadata"))
        ),
        # right: view / filter controls
        div(
          class = "d-flex gap-2",
          module_selector_table_columns_button(ns("metadata")),
          module_selector_table_search_button(ns("metadata"))
        )
      ),
      module_selector_table_ui(ns("metadata"))
    )
  )
}
