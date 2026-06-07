metadata_ui <- function(id) {
  ns <- NS(id)

  bslib::accordion(
    id = ns("accordion"),
    multiple = TRUE,
    bslib::accordion_panel(
      "Metadata",
      icon = icon("file-lines"),
      bslib::card(
        full_screen = TRUE,
        min_height = 600,
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            position = "left",
            width = "160",
            module_selector_table_select_all_button(ns("metadata"), border = FALSE),
            module_selector_table_deselect_all_button(ns("metadata"), border = FALSE),
            module_selector_table_columns_button(ns("metadata"), border = FALSE),
            module_selector_table_search_button(ns("metadata"), border = FALSE)
          ),
          module_selector_table_ui(ns("metadata"))
        ),
        bslib::card_footer("Select the file(s) you want to work with.")
      )
    )
  )
}
