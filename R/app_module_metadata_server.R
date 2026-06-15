#' Metadata selector module
#'
#' Server for a file/analysis selector table (pair with [ie_metadata_ui()]). It
#' shows a row-grouped table over an already-aggregated metadata tibble and
#' pushes the current selection out via `set_selected`; double-clicking a row
#' selects just that analysis, double-clicking a file's group header selects all
#' of its analyses. Most users want the typed wrappers
#' [ie_scans_metadata_server()] / [ie_cf_metadata_server()] / [ie_di_metadata_server()],
#' which bind the right [ie_file_server()] accessors; `ie_metadata_server()` itself is
#' the generic version for wiring a custom metadata source.
#'
#' @param id the module id (must match the paired [ie_metadata_ui()])
#' @param get_metadata a reactive returning the metadata tibble to browse (one
#'   row per analysis, with `uidx` + `analysis` columns)
#' @param set_selected optional `function(rows)` called with the selected
#'   metadata rows whenever the selection changes (e.g.
#'   `file$set_selected_scans`)
#' @param get_selection optional reactive of the current selection, used to
#'   reflect it in the table on load (e.g. `file$get_scans_selection`)
#' @param file the [ie_file_server()] handle (for the typed wrappers)
#' @return a list with the underlying selector-table handle plus
#'   `get_selected_row_id` and `get_selected_metadata` reactives
#' @export
ie_metadata_server <- function(
  id,
  get_metadata,
  set_selected = NULL,
  get_selection = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # the (already aggregated) metadata plus a stable row_id the table keys on
    get_table_data <- reactive({
      metadata <- get_metadata()
      if (is.null(metadata)) {
        return(NULL)
      }
      dplyr::mutate(metadata, row_id = dplyr::row_number(), .before = 1L)
    })

    # selector table over the metadata
    metadata_table <- module_selector_table_server(
      "metadata",
      get_data = get_table_data,
      id_column = "row_id",
      available_columns = list(
        dplyr::across(dplyr::everything(), identity)
      ),
      # row grouping
      extensions = "RowGroup",
      rowGroup = list(dataSrc = 2),
      # double-click a row to select just that analysis, or a group summary row
      # to select that whole file's analyses (replacing the current selection)
      select_on_dblclick = TRUE,
      # make id & category column invisible
      columnDefs = list(
        list(visible = FALSE, targets = 0:2)
      ),
      visible_columns = c(),
      selection = "multiple",
      paging = FALSE,
      dom = "fltip"
    )

    # reflect the file server's (initial) selection in the table ONCE, when it
    # first loads, so the table visually matches what is plotted. select_rows is
    # keyed on row_id; we match the selection on uidx (file-level, like the data
    # filtering). After this the table drives the selection (pushed below).
    if (!is.null(get_selection)) {
      initial_synced <- reactiveVal(FALSE)
      observeEvent(metadata_table$is_table_reloaded(), {
        req(!initial_synced())
        data <- get_table_data()
        req(!is.null(data))
        metadata_table$select_rows(
          ids = initial_selection_row_ids(data, get_selection())
        )
        initial_synced(TRUE)
      })
    }

    # push the current selection into the file server (a NULL setter is a no-op).
    # The push is debounced so a double-click settles to its final selection
    # before anything downstream (the plot) reacts: a dblclick is two quick
    # single-clicks (each toggles the row via DT) plus the dblclick's exclusive
    # re-select, and without the delay those single clicks would push intermediate
    # selections (and re-render the plot) before the dblclick registers.
    # ignoreInit so the initial empty selection doesn't override the file server's
    # "nothing pushed yet -> show all" default; once the user (de)selects, the
    # exact selection (possibly empty -> empty plot) is pushed.
    if (!is.null(set_selected)) {
      selected_items <- debounce(metadata_table$get_selected_items, 300)
      observeEvent(
        selected_items(),
        set_selected(selected_items()),
        ignoreNULL = FALSE,
        ignoreInit = TRUE
      )
    }

    list(
      table = metadata_table,
      get_selected_row_id = metadata_table$get_selected_ids,
      get_selected_metadata = metadata_table$get_selected_items
    )
  })
}

#' Typed metadata selectors
#'
#' Selector-table servers wired to a [ie_file_server()] for one measurement type:
#' they read that type's metadata and push the selection back into the file
#' server. Pair each with a [ie_metadata_ui()] using the same `id`.
#'
#' @inheritParams ie_metadata_server
#' @return a [ie_metadata_server()] handle
#' @seealso [ie_file_server()], [ie_metadata_ui()]
#' @name ie_typed_metadata_servers
#' @export
ie_scans_metadata_server <- function(id, file) {
  ie_metadata_server(
    id,
    get_metadata = file$get_scans_metadata,
    set_selected = file$set_selected_scans,
    get_selection = file$get_scans_selection
  )
}

#' @rdname ie_typed_metadata_servers
#' @export
ie_cf_metadata_server <- function(id, file) {
  ie_metadata_server(
    id,
    get_metadata = file$get_cf_metadata,
    set_selected = file$set_selected_cf,
    get_selection = file$get_cf_selection
  )
}

#' @rdname ie_typed_metadata_servers
#' @export
ie_di_metadata_server <- function(id, file) {
  ie_metadata_server(
    id,
    get_metadata = file$get_di_metadata,
    set_selected = file$set_selected_di,
    get_selection = file$get_di_selection
  )
}
