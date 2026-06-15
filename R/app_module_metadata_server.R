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
#' @param get_select_signal optional reactive carrying file paths the table should
#'   exclusively select (e.g. `file$get_scans_select_signal`, fired by upload
#'   auto-select); applied once the table contains those files
#' @param file the [ie_file_server()] handle (for the typed wrappers)
#' @return a list with the underlying selector-table handle plus
#'   `get_selected_row_id` and `get_selected_metadata` reactives
#' @export
ie_metadata_server <- function(
  id,
  get_metadata,
  set_selected = NULL,
  get_selection = NULL,
  get_select_signal = NULL
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

    # Reflect a selection in the table, matching what is plotted. Two sources:
    # the one-time `get_selection` (initial selection) and `get_select_signal`
    # (file paths an upload wants exclusively selected). A selection can only be
    # applied once the table has actually RENDERED the target rows: `select_rows`
    # cleans ids against the table's rendered ids (`get_all_ids()`), so applying
    # before the DT re-renders with newly-added files would silently drop them
    # (the cause of "auto-select works the 1st upload but not the 2nd": the table
    # already existed, so the apply ran against the stale, pre-re-render ids).
    # We therefore gate on `get_all_ids()` (the current rendered ids) and only
    # apply once the targets are present; an upload request is held `pending`
    # until then and takes priority over the initial selection.
    if (!is.null(get_selection) || !is.null(get_select_signal)) {
      pending_paths <- reactiveVal(NULL)
      if (!is.null(get_select_signal)) {
        observeEvent(
          get_select_signal(),
          pending_paths(get_select_signal()),
          ignoreNULL = TRUE
        )
      }
      synced <- reactiveVal(FALSE)
      observe({
        paths <- pending_paths() # dependency (read first)
        data <- get_table_data() # dependency: re-run as the metadata grows
        rendered_ids <- metadata_table$get_all_ids() # halts until table renders
        req(!is.null(data))
        isolate({
          if (length(paths) > 0) {
            target <- data$row_id[data$file_path %in% paths]
            # only once the table has rendered these rows (else keep waiting)
            if (length(target) > 0 && all(target %in% rendered_ids)) {
              metadata_table$select_rows(ids = target)
              pending_paths(NULL)
              synced(TRUE)
            }
          } else if (!synced() && !is.null(get_selection)) {
            target <- initial_selection_row_ids(data, get_selection())
            if (all(target %in% rendered_ids)) {
              metadata_table$select_rows(ids = target)
              synced(TRUE)
            }
          }
        })
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
    get_selection = file$get_scans_selection,
    get_select_signal = file$get_scans_select_signal
  )
}

#' @rdname ie_typed_metadata_servers
#' @export
ie_cf_metadata_server <- function(id, file) {
  ie_metadata_server(
    id,
    get_metadata = file$get_cf_metadata,
    set_selected = file$set_selected_cf,
    get_selection = file$get_cf_selection,
    get_select_signal = file$get_cf_select_signal
  )
}

#' @rdname ie_typed_metadata_servers
#' @export
ie_di_metadata_server <- function(id, file) {
  ie_metadata_server(
    id,
    get_metadata = file$get_di_metadata,
    set_selected = file$set_selected_di,
    get_selection = file$get_di_selection,
    get_select_signal = file$get_di_select_signal
  )
}
