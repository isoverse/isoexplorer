#' Central file-management module
#'
#' The single source of truth for the isofiles in an isoexplorer app, and the hub
#' every other module talks to. It splits the provided isofiles into the three
#' measurement types (scans / continuous flow / dual inlet) with
#' [isoreader2::ir_filter_for_scans()] and friends, owns the shared
#' intensity-units selection and the per-type file selection, and exposes the
#' per-type metadata (for the [ie_metadata_server()] selector tables) and the
#' selection-filtered aggregated data (for the `*_plot_server()` modules).
#'
#' Selector tables read `get_<type>_metadata()` and push their selection via
#' `set_selected_<type>()`; plot modules read `get_aggregated_<type>_data()` and
#' drive the shared units via `get_units()` / `set_units()`. Aggregation is keyed
#' only on the units, so switching the selected files is a cheap filter rather
#' than a re-aggregation.
#'
#' @param id the module id (namespace)
#' @param get_isofiles a reactive returning the full `ir_isofiles` to manage (in
#'   the example apps simply `reactive(my_isofiles)`)
#' @param initial_selection what is selected, per type, before any selector pushes
#'   a selection -- i.e. what the plots show on load. One of `"all"` (default),
#'   `"none"`, or a `function(metadata)` called with a type's metadata tibble that
#'   returns the subset of rows to select (e.g.
#'   `\(m) dplyr::filter(m, grepl("std", file_name))`).
#' @return The "file handle": a list of reactive accessors and setters that the
#'   other modules use. For each `<type>` in `scans` / `cf` / `di`:
#'   \describe{
#'     \item{`get_units()`, `set_units(units)`}{the shared intensity units (default "mV")}
#'     \item{`get_<type>_metadata()`}{reactive metadata tibble for the selector table}
#'     \item{`set_selected_<type>(rows)`}{push the selected metadata rows (selector tables call this)}
#'     \item{`get_<type>_selection()`}{the resolved current selection (for reflecting it in a table)}
#'     \item{`get_aggregated_<type>_data()`}{reactive selection-filtered aggregated data (plot modules read this)}
#'   }
#' @export
ie_file_server <- function(id, get_isofiles, initial_selection = "all") {
  if (!is.function(initial_selection)) {
    initial_selection <- arg_match(initial_selection, c("all", "none"))
  }
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # shared intensity units (default mV) -- plot modules get/set this
    units <- reactiveVal("mV")
    get_units <- reactive(units())
    set_units <- function(new_units) units(new_units)

    # split the full isofiles into a single measurement type; NULL when that type
    # has no files (aggregating a 0-row isofiles errors)
    split_for <- function(filter_fn) {
      reactive({
        req(get_isofiles())
        out <- filter_fn(get_isofiles()) |> try_catch_cnds()
        out |> log_cnds(ns = ns)
        res <- out$result
        if (is.null(res) || nrow(res) == 0) NULL else res
      })
    }
    scans_isofiles <- split_for(isoreader2::ir_filter_for_scans)
    cf_isofiles <- split_for(isoreader2::ir_filter_for_continuous_flow)
    di_isofiles <- split_for(isoreader2::ir_filter_for_dual_inlet)

    # per-type metadata (the "metadata" aggregator needs no units); NULL when the
    # type is absent
    metadata_for <- function(get_type_isofiles) {
      reactive({
        isofiles <- get_type_isofiles()
        if (is.null(isofiles)) {
          return(NULL)
        }
        out <- isoreader2::ir_aggregate_isofiles(
          isofiles,
          aggregator = "metadata"
        ) |>
          try_catch_cnds()
        out |> log_cnds(ns = ns)
        out$result$metadata
      })
    }
    get_scans_metadata <- metadata_for(scans_isofiles)
    get_cf_metadata <- metadata_for(cf_isofiles)
    get_di_metadata <- metadata_for(di_isofiles)

    # per-type FULL aggregation, keyed on the shared units -> only re-runs when
    # the units change, NOT when the file selection changes. NULL when absent.
    agg_full_for <- function(get_type_isofiles) {
      reactive({
        isofiles <- get_type_isofiles()
        if (is.null(isofiles)) {
          return(NULL)
        }
        log_info(
          ns = ns,
          user_msg = paste("Aggregating with intensity units", get_units())
        )
        out <- isoreader2::ir_aggregate_isofiles(
          isofiles,
          intensity_units = get_units()
        ) |>
          try_catch_cnds()
        out |> log_cnds(ns = ns)
        out$result
      })
    }
    scans_agg_full <- agg_full_for(scans_isofiles)
    cf_agg_full <- agg_full_for(cf_isofiles)
    di_agg_full <- agg_full_for(di_isofiles)

    # resolve the configured `initial_selection` for a type, given its metadata:
    #   "all"  -> NULL          (no filter -> all files)
    #   "none" -> a 0-row tibble (-> empty)
    #   function(metadata) -> the subset of metadata rows to select
    resolve_initial_selection <- function(metadata) {
      if (is.function(initial_selection)) {
        if (is.null(metadata)) {
          return(NULL)
        }
        return(initial_selection(metadata))
      }
      switch(
        initial_selection,
        all = NULL,
        none = tibble::tibble()
      )
    }

    # per-type selection of metadata rows: a user override (NULL until a selector
    # table pushes something) layered on top of `initial_selection`. The setter
    # records the override; pushing NULL reverts to the initial selection. A
    # resolved selection of NULL means "all", a 0-row tibble means "none", any
    # other tibble filters to those rows' uidx.
    make_selection <- function(get_type_metadata) {
      override <- reactiveVal(NULL)
      list(
        get = reactive({
          if (!is.null(override())) {
            override()
          } else {
            resolve_initial_selection(get_type_metadata())
          }
        }),
        set = function(x) override(x)
      )
    }
    scans_selection <- make_selection(get_scans_metadata)
    cf_selection <- make_selection(get_cf_metadata)
    di_selection <- make_selection(get_di_metadata)

    # the selection-filtered aggregated data the plot modules consume
    aggregated_for <- function(get_agg_full, get_selected) {
      reactive({
        agg <- get_agg_full()
        if (is.null(agg)) {
          return(NULL)
        }
        selected <- get_selected()
        if (is.null(selected)) {
          return(agg) # nothing pushed yet -> all files
        }
        filter_agg_data_by_metadata(agg, selected) # NULL when 0 rows -> empty
      })
    }

    list(
      # shared intensity units (default mV)
      get_units = get_units,
      set_units = set_units,
      # per-type metadata for the selector tables
      get_scans_metadata = get_scans_metadata,
      get_cf_metadata = get_cf_metadata,
      get_di_metadata = get_di_metadata,
      # per-type selection setters (selector tables push their selection here)
      set_selected_scans = scans_selection$set,
      set_selected_cf = cf_selection$set,
      set_selected_di = di_selection$set,
      # per-type current (resolved) selection getters, so a selector table can
      # reflect the selection in its rows (NULL = all, 0-row = none, else subset)
      get_scans_selection = scans_selection$get,
      get_cf_selection = cf_selection$get,
      get_di_selection = di_selection$get,
      # per-type selection-filtered aggregated data for the plot modules
      get_aggregated_scans_data = aggregated_for(scans_agg_full, scans_selection$get),
      get_aggregated_cf_data = aggregated_for(cf_agg_full, cf_selection$get),
      get_aggregated_di_data = aggregated_for(di_agg_full, di_selection$get)
    )
  })
}
