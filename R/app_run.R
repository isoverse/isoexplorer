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
#' @param setup_modules `function(file)` that wires the app's server modules,
#'   given the [ie_file_server()] handle
#' @param timezone timezone for datetime display
#' @param default_theme default bslib Bootstrap 5 theme preset
#' @param nav_panels a list of [bslib::nav_panel()]s shown as a centered navbar
#'   tabset (use this OR `main`)
#' @param initial_selection initial file selection, see [ie_file_server()]
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
  initial_selection = "all",
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url"
) {
  # ensure isoreader2 is attached so .onAttach runs and registers the aggregators
  if (!"isoreader2" %in% .packages()) {
    library(isoreader2)
  }

  ui <- app_ui(
    main = main,
    nav_panels = nav_panels,
    timezone = timezone,
    default_theme = default_theme
  )
  server <- app_server(
    isofiles = isofiles,
    setup_modules = setup_modules,
    initial_selection = initial_selection
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
#' [ie_explore_isofiles()] uses. Wire the matching `*_metadata_server()` (on
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

#' Explore isofiles in the isoexplorer GUI
#'
#' Launches a Shiny application for exploring stable isotope data files using
#' the isoreader2 package, including file/metadata selection and all plot types.
#'
#' @param isofiles optional `ir_isofiles` object to pre-load into the GUI
#' @param allow_upload whether to allow file uploads in the GUI
#' @param path starting directory path for file browsing (defaults to working directory)
#' @param timezone the timezone to use for datetime display
#' @param default_theme the default bslib Bootstrap 5 theme preset
#' @param initial_selection what is selected (per measurement type) on load:
#'   `"all"` (default), `"none"`, or a `function(metadata)` that is called with a
#'   type's metadata tibble and returns the subset of rows to select for it
#' @inheritParams shiny::shinyApp
#' @export
ie_explore_isofiles <- function(
  isofiles = NULL,
  allow_upload = FALSE,
  path = getwd(),
  timezone = Sys.timezone(),
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url",
  default_theme = app_themes(),
  initial_selection = "all"
) {
  log_info("\n\n========================================================")
  log_info("starting isoexplorer GUI", if (shiny::in_devmode()) " in DEV mode")

  isofiles |>
    check_arg(
      is.null(isofiles) || inherits(isofiles, "ir_isofiles"),
      "must be an ir_isofiles object or NULL"
    )
  allow_upload |>
    check_arg(is_scalar_logical(allow_upload), "must be TRUE or FALSE")
  path |>
    check_arg(
      is_scalar_character(path) && dir.exists(path),
      "must be a path to an existing directory"
    )
  timezone |>
    check_arg(
      is_scalar_character(timezone) && timezone %in% base::OlsonNames(),
      "must be an OlsonName"
    )
  default_theme <- arg_match(default_theme)

  # one centered navbar tab per measurement type, each the type's file-selector
  # sidebar + plot (the same content as the focused example apps)
  ie_run_app(
    isofiles = isofiles,
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
    setup_modules = function(file) {
      # every selector pushes its selection into the shared file server; every
      # plot pulls its selection-filtered data back out (units are shared too)
      ie_cf_metadata_server("cf_meta", file)
      ie_cf_plot_server("cf", file)
      ie_di_metadata_server("di_meta", file)
      ie_di_plot_server("di", file)
      ie_scans_metadata_server("scans_meta", file)
      ie_scans_plot_server("scans", file)
    },
    timezone = timezone,
    default_theme = default_theme,
    initial_selection = initial_selection,
    options = options,
    uiPattern = uiPattern,
    enableBookmarking = enableBookmarking
  )
}

#' Explore continuous flow data
#'
#' Launches a focused Shiny app showing the continuous flow plot for the provided
#' isofiles, with a file-selector sidebar.
#'
#' @param isofiles an `ir_isofiles` object to plot (required)
#' @inheritParams ie_explore_isofiles
#' @export
ie_explore_continuous_flow <- function(
  isofiles,
  timezone = Sys.timezone(),
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url",
  default_theme = app_themes(),
  initial_selection = "all"
) {
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
  ie_run_app(
    isofiles = isofiles,
    main = ie_type_explorer_ui("cf_meta", ie_cf_plot_ui("cf")),
    setup_modules = function(file) {
      ie_cf_metadata_server("cf_meta", file)
      ie_cf_plot_server("cf", file)
    },
    timezone = timezone,
    default_theme = default_theme,
    initial_selection = initial_selection,
    options = options,
    uiPattern = uiPattern,
    enableBookmarking = enableBookmarking
  )
}

#' Explore dual inlet data
#'
#' Launches a focused Shiny app showing the dual inlet plot for the provided
#' isofiles, with a file-selector sidebar.
#'
#' @param isofiles an `ir_isofiles` object to plot (required)
#' @inheritParams ie_explore_isofiles
#' @export
ie_explore_dual_inlet <- function(
  isofiles,
  timezone = Sys.timezone(),
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url",
  default_theme = app_themes(),
  initial_selection = "all"
) {
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
  ie_run_app(
    isofiles = isofiles,
    main = ie_type_explorer_ui("di_meta", ie_di_plot_ui("di")),
    setup_modules = function(file) {
      ie_di_metadata_server("di_meta", file)
      ie_di_plot_server("di", file)
    },
    timezone = timezone,
    default_theme = default_theme,
    initial_selection = initial_selection,
    options = options,
    uiPattern = uiPattern,
    enableBookmarking = enableBookmarking
  )
}

#' Explore scan data
#'
#' Launches a focused Shiny app showing the scans plot for the provided
#' isofiles, with a file-selector sidebar.
#'
#' @param isofiles an `ir_isofiles` object to plot (required)
#' @inheritParams ie_explore_isofiles
#' @export
ie_explore_scans <- function(
  isofiles,
  timezone = Sys.timezone(),
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url",
  default_theme = app_themes(),
  initial_selection = "all"
) {
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
  ie_run_app(
    isofiles = isofiles,
    # the selector pushes its selection into the file server; the plot pulls the
    # selection-filtered scans data back out -- they only talk via the file server
    main = ie_type_explorer_ui("scans_meta", ie_scans_plot_ui("scans")),
    setup_modules = function(file) {
      ie_scans_metadata_server("scans_meta", file)
      ie_scans_plot_server("scans", file)
    },
    timezone = timezone,
    default_theme = default_theme,
    initial_selection = initial_selection,
    options = options,
    uiPattern = uiPattern,
    enableBookmarking = enableBookmarking
  )
}

#' Explore file metadata
#'
#' Launches a focused Shiny app showing only the metadata selector table for the
#' provided isofiles (useful for testing the selector table).
#'
#' @param isofiles an `ir_isofiles` object whose metadata to browse (required)
#' @inheritParams ie_explore_isofiles
#' @export
ie_explore_metadata <- function(
  isofiles,
  timezone = Sys.timezone(),
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url",
  default_theme = app_themes()
) {
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
  ie_run_app(
    isofiles = isofiles,
    main = ie_metadata_ui("scans_meta"),
    setup_modules = function(file) {
      ie_scans_metadata_server("scans_meta", file)
    },
    timezone = timezone,
    default_theme = default_theme,
    options = options,
    uiPattern = uiPattern,
    enableBookmarking = enableBookmarking
  )
}
