# toolbar above the selector table: selection controls on the left (single/multi
# toggle plus select-all / deselect, which only show in multiple mode), view +
# filter controls on the right; the table flexibly fills the remaining space
metadata_ui <- function(id) {
  ns <- NS(id)
  bslib::card_body(
    div(
      class = "d-flex align-items-center justify-content-between gap-2 mb-2",
      # left: selection controls
      div(
        class = "d-flex gap-2",
        module_selector_table_selection_button(ns("metadata")),
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
}
