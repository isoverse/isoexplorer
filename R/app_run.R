#' Assemble and launch a custom isoexplorer app
#'
#' Wraps your own module composition in the isoexplorer navbar shell (theme
#' picker, dark-mode toggle, about popup) and returns a [shiny::shinyApp()]. Build
#' your layout from the package's modules, instantiate the [ie_file_server()] and
#' wire the modules in `setup_modules`. Supply EITHER `main` (a single content
#' area) OR `nav_panels` (a centered navbar tabset).
#'
#' @param isofiles the `ir_isofiles` to explore (handed to the [ie_file_server()])
#' @param main a single UI content area (use this OR `nav_panels`)
#' @param setup_modules `function(file, code)` that wires the app's server
#'   modules, given the [ie_file_server()] handle and the [ie_code_server()] handle
#'   (register each module's `get_code` with `code$register()`). A one-argument
#'   `function(file)` is still accepted (the code server is then not populated).
#' @param timezone timezone for datetime display
#' @param default_theme default bslib Bootstrap 5 theme preset
#' @param nav_panels a list of [bslib::nav_panel()]s shown as a centered navbar
#'   tabset (use this OR `main`)
#' @param selected the value/title of the `nav_panels` tab to open initially
#'   (`NULL` = the first tab)
#' @param initial_selection initial file selection, see [ie_file_server()]
#' @param upload_folder upload directory for the navbar upload button; `NULL`
#'   (the default) means no upload button, see [ie_file_server()]
#' @param monitoring_folders folders to watch for new isofiles (`NULL` = off),
#'   see [ie_file_server()]
#' @param examples_folder directory for the "Load examples" navbar button
#'   (`NULL` = off), see [ie_file_server()]
#' @param temporary_storage note in the upload dialog that uploads are
#'   session-only (informational; default `FALSE`), see [ie_file_server()]
#' @param max_upload_size maximum per-file upload size in MB; sets the
#'   `shiny.maxRequestSize` option for the running app. `NULL` (the default)
#'   leaves Shiny's ~5 MB default (or a value you set yourself) untouched. Raw
#'   isofiles are often larger than 5 MB, so raise this when enabling uploads.
#' @inheritParams shiny::shinyApp
#' @return a [shiny::shinyApp()] object
#' @export
ie_run_app <- function(
  isofiles,
  main = NULL,
  setup_modules,
  timezone = Sys.timezone(),
  default_theme = app_themes()[1],
  nav_panels = NULL,
  selected = NULL,
  initial_selection = TRUE,
  upload_folder = NULL,
  monitoring_folders = NULL,
  examples_folder = NULL,
  temporary_storage = FALSE,
  max_upload_size = NULL,
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url"
) {
  # capture the initial-selection filter expression (tidy eval) to forward to the
  # file server through app_server (see ie_file_server())
  initial_selection <- rlang::enquo(initial_selection)
  # ensure isoreader2 is attached so .onAttach runs and registers the aggregators
  if (!"isoreader2" %in% .packages()) {
    library(isoreader2)
  }

  # raise the per-file upload cap when asked (shiny.maxRequestSize is in bytes);
  # Shiny reads this option per request, so setting it here covers the whole app
  if (!is.null(max_upload_size)) {
    options(shiny.maxRequestSize = max_upload_size * 1024^2)
  }

  ui <- app_ui(
    main = main,
    nav_panels = nav_panels,
    timezone = timezone,
    default_theme = default_theme,
    selected = selected
  )
  server <- app_server(
    isofiles = isofiles,
    setup_modules = setup_modules,
    initial_selection = initial_selection,
    upload_folder = upload_folder,
    monitoring_folders = monitoring_folders,
    examples_folder = examples_folder,
    temporary_storage = temporary_storage
  )

  shinyApp(
    ui = ui,
    server = server,
    options = options,
    enableBookmarking = enableBookmarking,
    uiPattern = uiPattern
  )
}

#' File-selector + plot layout for one measurement type
#'
#' Convenience UI combining a left file-selector sidebar (a [ie_metadata_ui()]) with
#' a plot to its right -- the building block each focused explorer and each tab of
#' [ie_start_isofiles_server()] uses. Wire the matching `*_metadata_server()` (on
#' `meta_id`) and `*_plot_server()` in your server function.
#'
#' @param meta_id the id for the [ie_metadata_ui()] / `*_metadata_server()` pair
#' @param plot_ui a plot module UI element, e.g. `ie_scans_plot_ui("scan")`
#' @return a [bslib::layout_sidebar()] UI element
#' @seealso [ie_file_server()], [ie_scans_plot_ui()], [ie_scans_metadata_server()]
#' @export
ie_type_explorer_ui <- function(meta_id, plot_ui) {
  bslib::layout_sidebar(
    fill = TRUE,
    sidebar = bslib::sidebar(
      position = "left",
      open = TRUE,
      width = "40%",
      fillable = TRUE,
      ie_metadata_ui(meta_id)
    ),
    plot_ui
  )
}

# the ir_filter_for_<type> name to put in a focused app's generated code, or NULL
# when `isofiles` already holds only that type (the filter would be a no-op then).
# `filter_fn` is the actual function (to test), `filter_fn_name` its name (for code)
app_focused_filter <- function(isofiles, filter_fn, filter_fn_name) {
  if (nrow(filter_fn(isofiles)) < nrow(isofiles)) filter_fn_name else NULL
}

#' Start an isofiles server in the isoexplorer GUI
#'
#' Launches the full isoexplorer Shiny app: one navbar tab per measurement type
#' (continuous flow / dual inlet / scans), each with a file-selector sidebar and
#' plot. Unlike the focused [ie_explore_continuous_flow()] explorers, this app does
#' NOT take an `ir_isofiles` object -- data arrives at runtime via the navbar
#' **Upload** button, any watched `monitoring_folders`, and/or the **Load
#' examples** button (a "get started" prompt is shown until something is loaded).
#' Loaded examples are selected automatically; uploaded files are selected only
#' when the upload modal's "Select the uploaded files" box is checked.
#'
#' @param timezone the timezone to use for datetime display
#' @param default_theme the default bslib Bootstrap 5 theme preset
#' @param upload_folder upload directory for the navbar upload button; `NULL`
#'   (the default) means no upload button, a path enables it; see [ie_file_server()]
#' @param monitoring_folders folders to watch for new isofiles, read and added
#'   automatically (`NULL` = off); see [ie_file_server()]
#' @param examples_folder directory the "Load examples" navbar button copies the
#'   isoreader2 bundled example files into and loads; `"examples"` by default
#'   (`NULL` hides the button)
#' @param temporary_storage if `TRUE`, the upload dialog notes that uploaded files
#'   are stored only for the duration of the session (informational; default
#'   `FALSE`)
#' @param max_upload_size maximum per-file upload size in MB (sets the
#'   `shiny.maxRequestSize` option); `NULL` (the default) keeps Shiny's ~5 MB
#'   default. Raw isofiles are often larger, so raise this when allowing uploads.
#' @inheritParams shiny::shinyApp
#' @return a [shiny::shinyApp()] object
#' @export
ie_start_isofiles_server <- function(
  timezone = Sys.timezone(),
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url",
  default_theme = app_themes(),
  upload_folder = NULL,
  monitoring_folders = NULL,
  examples_folder = "examples",
  temporary_storage = FALSE,
  max_upload_size = NULL
) {
  log_info("\n\n========================================================")
  log_info(
    "starting isoexplorer server GUI",
    if (shiny::in_devmode()) " in DEV mode"
  )
  timezone |>
    check_arg(
      is_scalar_character(timezone) && timezone %in% base::OlsonNames(),
      "must be an OlsonName"
    )
  default_theme <- arg_match(default_theme)

  # one centered navbar tab per measurement type, each the type's file-selector
  # sidebar + plot (the same content as the focused explorers)
  ie_run_app(
    isofiles = NULL,
    nav_panels = list(
      bslib::nav_panel(
        "Continuous Flow",
        ie_type_explorer_ui("cf_meta", ie_cf_plot_ui("cf"))
      ),
      bslib::nav_panel(
        "Dual Inlet",
        ie_type_explorer_ui("di_meta", ie_di_plot_ui("di"))
      ),
      bslib::nav_panel(
        "Scans",
        ie_type_explorer_ui("scans_meta", ie_scans_plot_ui("scans"))
      )
    ),
    # no data at startup -> open on the first tab (uploads switch tabs as needed)
    selected = NULL,
    setup_modules = function(file, code) {
      # each type registers its own read -> aggregate -> plot code chain, grouped
      # by tab title so the viewer shows only the active tab's code (the read step
      # finds files in the "data" folder; selectors drive which files/analyses the
      # plot step filters to)
      ie_cf_metadata_server("cf_meta", file)
      cf_plot <- ie_cf_plot_server("cf", file)
      code$register(
        "cf_read",
        "Read data files",
        code_read_step(
          "ir_find_continuous_flow",
          file$get_examples_loaded,
          file$get_uploads_loaded,
          examples_folder,
          upload_folder
        ),
        group = "Continuous Flow"
      )
      code$register(
        "cf_agg",
        "Aggregate data files",
        code_aggregate_step(file$get_units, "cf_data", cf_plot$get_ratio_calc),
        depends_on = "cf_read",
        group = "Continuous Flow"
      )
      code$register(
        "cf_plot",
        "Plot continuous flow",
        cf_plot$get_code,
        depends_on = "cf_agg",
        group = "Continuous Flow"
      )

      ie_di_metadata_server("di_meta", file)
      di_plot <- ie_di_plot_server("di", file)
      code$register(
        "di_read",
        "Read data files",
        code_read_step(
          "ir_find_dual_inlet",
          file$get_examples_loaded,
          file$get_uploads_loaded,
          examples_folder,
          upload_folder
        ),
        group = "Dual Inlet"
      )
      code$register(
        "di_agg",
        "Aggregate data files",
        code_aggregate_step(file$get_units, "di_data", di_plot$get_ratio_calc),
        depends_on = "di_read",
        group = "Dual Inlet"
      )
      code$register(
        "di_plot",
        "Plot dual inlet",
        di_plot$get_code,
        depends_on = "di_agg",
        group = "Dual Inlet"
      )

      ie_scans_metadata_server("scans_meta", file)
      scans_plot <- ie_scans_plot_server("scans", file)
      code$register(
        "scans_read",
        "Read data files",
        code_read_step(
          "ir_find_scans",
          file$get_examples_loaded,
          file$get_uploads_loaded,
          examples_folder,
          upload_folder
        ),
        group = "Scans"
      )
      code$register(
        "scans_agg",
        "Aggregate data files",
        code_aggregate_step(
          file$get_units,
          "scans_data",
          scans_plot$get_ratio_calc
        ),
        depends_on = "scans_read",
        group = "Scans"
      )
      code$register(
        "scans_plot",
        "Plot scans",
        scans_plot$get_code,
        depends_on = "scans_agg",
        group = "Scans"
      )
      # after an upload auto-select, switch to that measurement type's tab
      tab_titles <- c(
        scans = "Scans",
        cf = "Continuous Flow",
        di = "Dual Inlet"
      )
      observeEvent(file$get_active_type(), {
        at <- file$get_active_type()
        req(at, at$type %in% names(tab_titles))
        bslib::nav_select("ie_navbar", selected = tab_titles[[at$type]])
      })
    },
    timezone = timezone,
    default_theme = default_theme,
    # no seed data: selection is fully explicit (examples select on load, uploads
    # follow the upload checkbox) -- nothing selected by default
    initial_selection = FALSE,
    upload_folder = upload_folder,
    monitoring_folders = monitoring_folders,
    examples_folder = examples_folder,
    temporary_storage = temporary_storage,
    max_upload_size = max_upload_size,
    options = options,
    uiPattern = uiPattern,
    enableBookmarking = enableBookmarking
  )
}

#' Explore an ir_isofiles object by measurement type
#'
#' Focused apps for exploring an already-read `ir_isofiles` object one measurement
#' type at a time: `ie_explore_continuous_flow()` / `ie_explore_dual_inlet()` /
#' `ie_explore_scans()` each show that type's file-selector sidebar + plot, and
#' `ie_explore_metadata()` shows just the selector table. The generated example
#' code (navbar **Show code**) refers to the object by the name you passed in.
#' These take a fixed object only -- for upload / folder monitoring / load-examples
#' use [ie_start_isofiles_server()].
#'
#' @param isofiles the `ir_isofiles` object to explore (required)
#' @param timezone the timezone to use for datetime display
#' @param default_theme the default bslib Bootstrap 5 theme preset
#' @param initial_selection what is selected on load, as a [dplyr::filter()]
#'   expression on the aggregated metadata: `FALSE` (the default) selects nothing,
#'   `TRUE` selects everything, and any other expression (e.g. `grepl("std",
#'   file_name)`) selects the matching files/analyses. See [ie_file_server()].
#' @inheritParams shiny::shinyApp
#' @return a [shiny::shinyApp()] object
#' @export
ie_explore_continuous_flow <- function(
  isofiles,
  timezone = Sys.timezone(),
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url",
  default_theme = app_themes(),
  initial_selection = FALSE
) {
  obj_name <- paste(deparse(substitute(isofiles)), collapse = " ")
  initial_selection <- rlang::enquo(initial_selection)
  log_info("\n\n========================================================")
  log_info(
    "starting isoexplorer continuous flow GUI",
    if (shiny::in_devmode()) " in DEV mode"
  )
  isofiles |>
    check_arg(
      !is.null(isofiles) && inherits(isofiles, "ir_isofiles"),
      "must be an ir_isofiles object"
    )
  timezone |>
    check_arg(
      is_scalar_character(timezone) && timezone %in% base::OlsonNames(),
      "must be an OlsonName"
    )
  default_theme <- arg_match(default_theme)
  # only filter-to-type in the generated code when the object is actually mixed
  cf_filter <- app_focused_filter(
    isofiles,
    isoreader2::ir_filter_for_continuous_flow,
    "ir_filter_for_continuous_flow"
  )
  rlang::inject(ie_run_app(
    isofiles = isofiles,
    main = ie_type_explorer_ui("cf_meta", ie_cf_plot_ui("cf")),
    setup_modules = function(file, code) {
      ie_cf_metadata_server("cf_meta", file)
      cf_plot <- ie_cf_plot_server("cf", file)
      code$register(
        "cf_agg",
        "Aggregate data files",
        code_object_aggregate_step(
          obj_name,
          cf_filter,
          file$get_units,
          "cf_data",
          cf_plot$get_ratio_calc
        )
      )
      code$register(
        "cf_plot",
        "Plot continuous flow",
        cf_plot$get_code,
        depends_on = "cf_agg"
      )
    },
    timezone = timezone,
    default_theme = default_theme,
    initial_selection = !!initial_selection,
    options = options,
    uiPattern = uiPattern,
    enableBookmarking = enableBookmarking
  ))
}

#' @describeIn ie_explore_continuous_flow focused app for the dual inlet plot.
#' @export
ie_explore_dual_inlet <- function(
  isofiles,
  timezone = Sys.timezone(),
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url",
  default_theme = app_themes(),
  initial_selection = FALSE
) {
  obj_name <- paste(deparse(substitute(isofiles)), collapse = " ")
  initial_selection <- rlang::enquo(initial_selection)
  log_info("\n\n========================================================")
  log_info(
    "starting isoexplorer dual inlet GUI",
    if (shiny::in_devmode()) " in DEV mode"
  )
  isofiles |>
    check_arg(
      !is.null(isofiles) && inherits(isofiles, "ir_isofiles"),
      "must be an ir_isofiles object"
    )
  timezone |>
    check_arg(
      is_scalar_character(timezone) && timezone %in% base::OlsonNames(),
      "must be an OlsonName"
    )
  default_theme <- arg_match(default_theme)
  # only filter-to-type in the generated code when the object is actually mixed
  di_filter <- app_focused_filter(
    isofiles,
    isoreader2::ir_filter_for_dual_inlet,
    "ir_filter_for_dual_inlet"
  )
  rlang::inject(ie_run_app(
    isofiles = isofiles,
    main = ie_type_explorer_ui("di_meta", ie_di_plot_ui("di")),
    setup_modules = function(file, code) {
      ie_di_metadata_server("di_meta", file)
      di_plot <- ie_di_plot_server("di", file)
      code$register(
        "di_agg",
        "Aggregate data files",
        code_object_aggregate_step(
          obj_name,
          di_filter,
          file$get_units,
          "di_data",
          di_plot$get_ratio_calc
        )
      )
      code$register(
        "di_plot",
        "Plot dual inlet",
        di_plot$get_code,
        depends_on = "di_agg"
      )
    },
    timezone = timezone,
    default_theme = default_theme,
    initial_selection = !!initial_selection,
    options = options,
    uiPattern = uiPattern,
    enableBookmarking = enableBookmarking
  ))
}

#' @describeIn ie_explore_continuous_flow focused app for the scans plot.
#' @export
ie_explore_scans <- function(
  isofiles,
  timezone = Sys.timezone(),
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url",
  default_theme = app_themes(),
  initial_selection = FALSE
) {
  obj_name <- paste(deparse(substitute(isofiles)), collapse = " ")
  initial_selection <- rlang::enquo(initial_selection)
  log_info("\n\n========================================================")
  log_info(
    "starting isoexplorer scans GUI",
    if (shiny::in_devmode()) " in DEV mode"
  )
  isofiles |>
    check_arg(
      !is.null(isofiles) && inherits(isofiles, "ir_isofiles"),
      "must be an ir_isofiles object"
    )
  timezone |>
    check_arg(
      is_scalar_character(timezone) && timezone %in% base::OlsonNames(),
      "must be an OlsonName"
    )
  default_theme <- arg_match(default_theme)
  # only filter-to-type in the generated code when the object is actually mixed
  scans_filter <- app_focused_filter(
    isofiles,
    isoreader2::ir_filter_for_scans,
    "ir_filter_for_scans"
  )
  rlang::inject(ie_run_app(
    isofiles = isofiles,
    # the selector pushes its selection into the file server; the plot pulls the
    # selection-filtered scans data back out -- they only talk via the file server
    main = ie_type_explorer_ui("scans_meta", ie_scans_plot_ui("scans")),
    setup_modules = function(file, code) {
      ie_scans_metadata_server("scans_meta", file)
      scans_plot <- ie_scans_plot_server("scans", file)
      code$register(
        "scans_agg",
        "Aggregate data files",
        code_object_aggregate_step(
          obj_name,
          scans_filter,
          file$get_units,
          "scans_data",
          scans_plot$get_ratio_calc
        )
      )
      code$register(
        "scans_plot",
        "Plot scans",
        scans_plot$get_code,
        depends_on = "scans_agg"
      )
    },
    timezone = timezone,
    default_theme = default_theme,
    initial_selection = !!initial_selection,
    options = options,
    uiPattern = uiPattern,
    enableBookmarking = enableBookmarking
  ))
}

#' @describeIn ie_explore_continuous_flow focused app showing just the scans
#'   metadata selector table (handy for browsing/testing the selector).
#' @export
ie_explore_metadata <- function(
  isofiles,
  timezone = Sys.timezone(),
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url",
  default_theme = app_themes(),
  initial_selection = FALSE
) {
  obj_name <- paste(deparse(substitute(isofiles)), collapse = " ")
  initial_selection <- rlang::enquo(initial_selection)
  log_info("\n\n========================================================")
  log_info(
    "starting isoexplorer metadata GUI",
    if (shiny::in_devmode()) " in DEV mode"
  )
  isofiles |>
    check_arg(
      !is.null(isofiles) && inherits(isofiles, "ir_isofiles"),
      "must be an ir_isofiles object"
    )
  timezone |>
    check_arg(
      is_scalar_character(timezone) && timezone %in% base::OlsonNames(),
      "must be an OlsonName"
    )
  default_theme <- arg_match(default_theme)
  # only filter-to-type in the generated code when the object is actually mixed
  scans_filter <- app_focused_filter(
    isofiles,
    isoreader2::ir_filter_for_scans,
    "ir_filter_for_scans"
  )
  rlang::inject(ie_run_app(
    isofiles = isofiles,
    main = ie_metadata_ui("scans_meta"),
    setup_modules = function(file, code) {
      ie_scans_metadata_server("scans_meta", file)
      code$register(
        "scans_agg",
        "Aggregate data files",
        code_object_aggregate_step(
          obj_name,
          scans_filter,
          file$get_units,
          "scans_data"
        )
      )
    },
    timezone = timezone,
    default_theme = default_theme,
    initial_selection = !!initial_selection,
    options = options,
    uiPattern = uiPattern,
    enableBookmarking = enableBookmarking
  ))
}
