metadata_server <- function(id, get_isofiles) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # aggregate just the metadata - the "metadata" aggregator needs no units
    get_metadata <- reactive({
      req(get_isofiles())
      out <- isoreader2::ir_aggregate_isofiles(
        get_isofiles(),
        aggregator = "metadata"
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      out$result$metadata
    })

    # selector table for the metadata dataset
    metadata_table <- module_selector_table_server(
      "metadata",
      get_data = reactive({
        metadata <- get_metadata()
        if (is.null(metadata)) {
          return(NULL)
        }
        dplyr::mutate(metadata, row_id = dplyr::row_number(), .before = 1L)
      }),
      id_column = "row_id",
      available_columns = list(
        dplyr::across(dplyr::everything(), identity)
      ),
      # row grouping
      extensions = "RowGroup",
      rowGroup = list(dataSrc = 2),
      # double-click a group's summary row to select all its rows (multiple mode)
      select_group_on_dblclick = TRUE,
      # make id & category column invisible
      columnDefs = list(
        list(visible = FALSE, targets = 0:2)
      ),
      visible_columns = c(),
      selection = "single",
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
