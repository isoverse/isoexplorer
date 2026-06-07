metadata_server <- function(id, get_aggregated_data) {
  moduleServer(id, function(input, output, session) {
    # selector table for the metadata dataset
    metadata_table <- module_selector_table_server(
      "metadata",
      get_data = reactive(
        get_aggregated_data()$metadata |>
          dplyr::mutate(row_id = dplyr::row_number(), .before = 1L)
      ),
      id_column = "row_id",
      available_columns = list(
        dplyr::across(dplyr::everything(), identity)
      ),
      # row grouping
      extensions = "RowGroup",
      rowGroup = list(dataSrc = 2),
      # make id & category column invisible
      columnDefs = list(
        list(visible = FALSE, targets = 0:2)
      ),
      visible_columns = c(),
      selection = "multiple",
      paging = FALSE,
      dom = "fltip"
    )

    list(
      table = metadata_table,
      get_selected_row_id = metadata_table$get_selected_ids,
      get_selected_metadata = metadata_table$get_selected_items
    )
  })
}
