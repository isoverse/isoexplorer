#' Assemble and launch a custom isoexplorer app
#'
#' Wraps your own module composition in the isoexplorer navbar shell (theme
#' picker, dark-mode toggle, about popup) and, by default, runs it. Build your
#' layout from the package's modules, instantiate the [ie_file_server()] and wire
#' the modules in `setup_modules`. Supply EITHER `main` (a single content area) OR
#' `nav_panels` (a centered navbar tabset).
#'
#' The `detached` / `launch` arguments control how it runs: `detached = TRUE`
#' launches it in a separate R process (leaving this session free);
#' `detached = FALSE, launch = TRUE` (the default) runs it in the current session
#' (blocking) with [shiny::runApp()]; `detached = FALSE, launch = FALSE` returns
#' the [shiny::shinyApp()] object unrun (for deployment or manual launching). The
#' focused explorers ([ie_explore_continuous_flow()] etc.) and
#' [ie_create_isofiles_server()] are wrappers that call this with the appropriate
#' `detached` / `launch`.
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
#' @param detached if `TRUE`, the app is launched in a separate R process (via
#'   [callr::r_bg()], the app handed over in a temporary `.rds`) and opened in your
#'   default browser, leaving the calling session free; closing the browser tab
#'   stops the app and the process (which is also killed if the calling session
#'   exits). Default `FALSE`.
#' @param launch only relevant when `detached = FALSE`: if `TRUE` (the default) the app is
#'   run in the current session (blocking) with [shiny::runApp()]; if `FALSE` the
#'   [shiny::shinyApp()] object is returned unrun (for server deployment or manual
#'   launching). Ignored when `detached = TRUE` (a detached app is always run).
#' @param stop_on_close if `TRUE`, the app stops itself when the browser
#'   disconnects (`session$onSessionEnded()`); set automatically for the detached
#'   process so it exits when the tab is closed. Defaults to the same as `detached`.
#' @param log_level how verbosely the app logs, one of `"WARN"` (the default --
#'   only warnings and errors), `"INFO"`, `"DEBUG"`, `"TRACE"` (everything),
#'   `"ERROR"`, or `"FATAL"`. Sets the `LOG_LEVEL` environment variable that
#'   [rlog][rlog::log_trace] reads (also applied in the detached process).
#' @inheritParams shiny::shinyApp
#' @return When `detached = TRUE`, the [callr::r_bg()] process (invisibly, with the
#'   app URL attached as an attribute); when `detached = FALSE` and `launch = TRUE`,
#'   the result of [shiny::runApp()] (after the app stops); when `detached = FALSE`
#'   and `launch = FALSE`, the [shiny::shinyApp()] object.
#' @examples
#' if (interactive()) {
#'   # read the bundled isoreader2 examples (a mixed set of all types)
#'   iso <-
#'     isoreader2::ir_examples_folder() |>
#'     isoreader2::ir_find_isofiles() |>
#'     isoreader2::ir_read_isofiles()
#'
#'   # a minimal custom app: a scans selector next to a scans plot
#'   ie_run_app(
#'     isofiles = iso,
#'     main = ie_type_explorer_ui("meta", ie_scans_plot_ui("scan")),
#'     setup_modules = function(file, code) {
#'       ie_scans_metadata_server("meta", file)
#'       ie_scans_plot_server("scan", file)
#'     }
#'   )
#' }
#' @export
ie_run_app <- function(
  isofiles,
  main = NULL,
  setup_modules,
  timezone = Sys.timezone(),
  default_theme = app_themes(),
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
  enableBookmarking = "url",
  detached = FALSE,
  launch = TRUE,
  stop_on_close = detached,
  log_level = c("WARN", "INFO", "DEBUG", "TRACE", "ERROR", "FATAL")
) {
  # capture the initial-selection filter expression (tidy eval) to forward to the
  # file server through app_server (see ie_file_server())
  initial_selection <- rlang::enquo(initial_selection)
  timezone |>
    check_arg(
      is_scalar_character(timezone) && timezone %in% base::OlsonNames(),
      "must be an OlsonName"
    )
  default_theme <- arg_match(default_theme)
  log_level <- arg_match(log_level)
  # rlog reads LOG_LEVEL from the environment; set it so this app logs at the
  # requested level (WARN by default -- only warnings/errors; the server app
  # defaults to TRACE, i.e. everything). Also forwarded in app_args so the
  # detached child sets the same level.
  Sys.setenv("LOG_LEVEL" = log_level)

  # snapshot the app-building arguments (every formal except the control flags), so
  # the detached path can hand the whole app spec to a separate process. Captured
  # up front, before any locals, so only the real arguments are included.
  app_args <- as.list(environment())
  app_args[c(
    "detached",
    "launch",
    "stop_on_close",
    "initial_selection"
  )] <- NULL

  # refuse to launch while a document is being rendered (knitr / Quarto) -- only
  # when we would actually start a server (detached, or launch here); returning an
  # unrun app object (launch = FALSE) is always fine
  if ((isTRUE(detached) || isTRUE(launch)) && app_in_rendering()) {
    return(app_rendering_notice(.call = rlang::caller_call()))
  }

  # info
  finish_info(
    "is creating the {col_magenta('graphical user interface (GUI)')}",
    if (shiny::in_devmode()) " in DEV mode",
    if (detached) {
      " in a separate R session (use {.code detached = FALSE} to run in the current session)..."
    }
  )

  # detached: run the app spec in a separate R process (returns the process)
  if (isTRUE(detached)) {
    cli::cli_inform(c(
      "i" = "the detached app is starting and will open in your browser; close the browser tab to stop the app."
    ))
    return(app_launch_detached(app_args, initial_selection, options = options))
  }

  # isoreader2 must be attached (not merely loaded) so its aggregators register
  # before the app runs; see ensure_isoreader2_attached().
  ensure_isoreader2_attached()

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
    temporary_storage = temporary_storage,
    stop_on_close = stop_on_close
  )
  app <- shinyApp(
    ui = ui,
    server = server,
    options = options,
    enableBookmarking = enableBookmarking,
    uiPattern = uiPattern
  )

  # launch = FALSE: hand back the shiny app object (e.g. for deployment); otherwise
  # run it in the current session (blocking) via runApp()
  if (!isTRUE(launch)) {
    return(app)
  }
  shiny::runApp(app)
}

#' File-selector + plot layout for one measurement type
#'
#' Convenience UI combining a left file-selector sidebar (a [ie_metadata_ui()]) with
#' a plot to its right -- the building block each focused explorer and each tab of
#' [ie_create_isofiles_server()] uses. Wire the matching `*_metadata_server()` (on
#' `meta_id`) and `*_plot_server()` in your server function.
#'
#' @param meta_id the id for the [ie_metadata_ui()] / `*_metadata_server()` pair
#' @param plot_ui a plot module UI element, e.g. `ie_scans_plot_ui("scan")`
#' @return a [bslib::layout_sidebar()] UI element
#' @seealso [ie_file_server()], [ie_scans_plot_ui()], [ie_scans_metadata_server()]
#' @examples
#' # a file-selector sidebar next to a scans plot (place inside a shiny UI)
#' ie_type_explorer_ui("meta", ie_scans_plot_ui("scan"))
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

#' Create an isofiles server app for the isoexplorer GUI
#'
#' Builds the full isoexplorer Shiny app -- one navbar tab per measurement type
#' (continuous flow / dual inlet / scans), each with a file-selector sidebar and
#' plot -- and returns it as a [shiny::shinyApp()] object to run or deploy. Unlike
#' the focused [ie_explore_continuous_flow()] explorers, this app does NOT take an
#' `ir_isofiles` object -- data arrives at runtime via the navbar **Upload**
#' button, any watched `monitoring_folders`, and/or the **Load examples** button (a
#' "get started" prompt is shown until something is loaded). Loaded examples are
#' selected automatically; uploaded files are selected only when the upload modal's
#' "Select the uploaded files" box is checked.
#'
#' Because it returns the app object unrun, launch it yourself (print it, or wrap
#' it in [shiny::runApp()]) or hand it to a deployment tool (e.g. shinyapps.io /
#' ShinyProxy).
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
#' @param log_level how verbosely the app logs; see [ie_run_app()]. Defaults to
#'   `"TRACE"` (log everything) for this server app, unlike the focused explorers
#'   which default to `"WARN"`.
#' @inheritParams shiny::shinyApp
#' @return a [shiny::shinyApp()] object (unrun)
#' @examples
#' if (interactive()) {
#'   # build the full multi-tab server app and run it; load data via the navbar
#'   # "Load examples" / "Upload" buttons at runtime
#'   ie_create_isofiles_server() |> shiny::runApp()
#'
#'   # enable uploads (raise the per-file cap for large raw isofiles)
#'   app <- ie_create_isofiles_server(
#'     upload_folder = "uploads",
#'     max_upload_size = 200
#'   )
#'   shiny::runApp(app)
#' }
#' @export
ie_create_isofiles_server <- function(
  timezone = Sys.timezone(),
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url",
  default_theme = app_themes(),
  upload_folder = NULL,
  monitoring_folders = NULL,
  examples_folder = "examples",
  temporary_storage = FALSE,
  max_upload_size = NULL,
  log_level = "TRACE"
) {
  # one centered navbar tab per measurement type, each the type's file-selector
  # sidebar + plot (the same content as the focused explorers). ie_run_app() with
  # launch = FALSE returns the app object unrun (timezone / default_theme /
  # log_level are validated there).
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
    enableBookmarking = enableBookmarking,
    log_level = log_level,
    # return the app object unrun -- the caller runs or deploys it
    detached = FALSE,
    launch = FALSE
  )
}

#' Explore an ir_isofiles object by measurement type
#'
#' Focused apps for exploring an already-read `ir_isofiles` object one measurement
#' type at a time: `ie_explore_continuous_flow()` / `ie_explore_dual_inlet()` /
#' `ie_explore_scans()` each show that type's file-selector sidebar + plot, and
#' `ie_explore_metadata()` shows just the selector table. The generated example
#' code (navbar **Show code**) refers to the object by its `variable_name`.
#' These take a fixed object only -- for upload / folder monitoring / load-examples
#' use [ie_create_isofiles_server()].
#'
#' By default the app runs **detached** in a separate R process (see `detached`),
#' so the calling session is not blocked, and it refuses to launch while a document
#' is being rendered (knitr / Quarto), showing a message to run it interactively.
#' These are thin wrappers over [ie_run_app()], which manages `detached` / `launch`.
#'
#' @param isofiles the `ir_isofiles` object to explore (required)
#' @param initial_selection what is selected on load, as a [dplyr::filter()]
#'   expression on the aggregated metadata: `FALSE` (the default) selects nothing,
#'   `TRUE` selects everything, and any other expression (e.g. `grepl("std",
#'   file_name)`) selects the matching files/analyses. See [ie_file_server()].
#'   In `detached` mode the expression is re-evaluated in the separate process, so
#'   it must be self-contained (it cannot reference variables from your session).
#' @param variable_name the name used for the object in the generated example code.
#'   Defaults to the deparsed expression you passed (so `my_iso |>
#'   ie_explore_scans()` uses `"my_iso"`); set it explicitly to override.
#' @param log_level how verbosely the app logs; see [ie_run_app()] for the
#'   available levels. Defaults to `"WARN"` for the focused explorers.
#' @inheritParams ie_run_app
#' @return the value of [ie_run_app()] for the chosen `detached` / `launch`: the
#'   [callr::r_bg()] process (detached), the [shiny::runApp()] result (in-session
#'   launch), or the [shiny::shinyApp()] object (`launch = FALSE`).
#' @examples
#' if (interactive()) {
#'   # read the bundled isoreader2 examples (a mixed set of all types); each
#'   # explorer filters the object to its own measurement type
#'   iso <-
#'     isoreader2::ir_examples_folder() |>
#'     isoreader2::ir_find_isofiles() |>
#'     isoreader2::ir_read_isofiles()
#'
#'   # each focused explorer opens the object in a detached browser app
#'   ie_explore_continuous_flow(iso)
#'   ie_explore_dual_inlet(iso)
#'   ie_explore_scans(iso)
#'   ie_explore_metadata(iso) # just the selector table
#'
#'   # preselect some files and run blocking in the current session instead
#'   ie_explore_continuous_flow(
#'     iso,
#'     initial_selection = grepl("gc", file_name),
#'     detached = FALSE
#'   )
#' }
#' @export
ie_explore_continuous_flow <- function(
  isofiles,
  timezone = Sys.timezone(),
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url",
  default_theme = app_themes(),
  initial_selection = FALSE,
  variable_name = NULL,
  detached = TRUE,
  launch = TRUE,
  log_level = "WARN"
) {
  variable_name <- variable_name %||%
    paste(deparse(substitute(isofiles)), collapse = " ")
  initial_selection <- rlang::enquo(initial_selection)
  isofiles |>
    check_arg(
      !is.null(isofiles) && inherits(isofiles, "ir_isofiles"),
      "must be an ir_isofiles object"
    )
  rlang::inject(ie_run_app(
    isofiles = isofiles,
    main = ie_type_explorer_ui("cf_meta", ie_cf_plot_ui("cf")),
    setup_modules = function(file, code) {
      ie_cf_metadata_server("cf_meta", file)
      cf_plot <- ie_cf_plot_server("cf", file)
      # only filter-to-type in the generated code when the object is actually
      # mixed (computed here, at app start, so it never touches data before then)
      cf_filter <- app_focused_filter(
        isofiles,
        isoreader2::ir_filter_for_continuous_flow,
        "ir_filter_for_continuous_flow"
      )
      code$register(
        "cf_agg",
        "Aggregate data files",
        code_object_aggregate_step(
          variable_name,
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
    detached = detached,
    launch = launch,
    log_level = log_level,
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
  initial_selection = FALSE,
  variable_name = NULL,
  detached = TRUE,
  launch = TRUE,
  log_level = "WARN"
) {
  variable_name <- variable_name %||%
    paste(deparse(substitute(isofiles)), collapse = " ")
  initial_selection <- rlang::enquo(initial_selection)
  isofiles |>
    check_arg(
      !is.null(isofiles) && inherits(isofiles, "ir_isofiles"),
      "must be an ir_isofiles object"
    )
  rlang::inject(ie_run_app(
    isofiles = isofiles,
    main = ie_type_explorer_ui("di_meta", ie_di_plot_ui("di")),
    setup_modules = function(file, code) {
      ie_di_metadata_server("di_meta", file)
      di_plot <- ie_di_plot_server("di", file)
      # only filter-to-type in the generated code when the object is actually
      # mixed (computed here, at app start, so it never touches data before then)
      di_filter <- app_focused_filter(
        isofiles,
        isoreader2::ir_filter_for_dual_inlet,
        "ir_filter_for_dual_inlet"
      )
      code$register(
        "di_agg",
        "Aggregate data files",
        code_object_aggregate_step(
          variable_name,
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
    detached = detached,
    launch = launch,
    log_level = log_level,
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
  initial_selection = FALSE,
  variable_name = NULL,
  detached = TRUE,
  launch = TRUE,
  log_level = "WARN"
) {
  variable_name <- variable_name %||%
    paste(deparse(substitute(isofiles)), collapse = " ")
  initial_selection <- rlang::enquo(initial_selection)
  isofiles |>
    check_arg(
      !is.null(isofiles) && inherits(isofiles, "ir_isofiles"),
      "must be an ir_isofiles object"
    )
  rlang::inject(ie_run_app(
    isofiles = isofiles,
    # the selector pushes its selection into the file server; the plot pulls
    # the selection-filtered scans data back out -- only via the file server
    main = ie_type_explorer_ui("scans_meta", ie_scans_plot_ui("scans")),
    setup_modules = function(file, code) {
      ie_scans_metadata_server("scans_meta", file)
      scans_plot <- ie_scans_plot_server("scans", file)
      # only filter-to-type in the generated code when the object is actually
      # mixed (computed here, at app start, so it never touches data before then)
      scans_filter <- app_focused_filter(
        isofiles,
        isoreader2::ir_filter_for_scans,
        "ir_filter_for_scans"
      )
      code$register(
        "scans_agg",
        "Aggregate data files",
        code_object_aggregate_step(
          variable_name,
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
    detached = detached,
    launch = launch,
    log_level = log_level,
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
  initial_selection = FALSE,
  variable_name = NULL,
  detached = TRUE,
  launch = TRUE,
  log_level = "WARN"
) {
  variable_name <- variable_name %||%
    paste(deparse(substitute(isofiles)), collapse = " ")
  initial_selection <- rlang::enquo(initial_selection)
  isofiles |>
    check_arg(
      !is.null(isofiles) && inherits(isofiles, "ir_isofiles"),
      "must be an ir_isofiles object"
    )
  rlang::inject(ie_run_app(
    isofiles = isofiles,
    main = ie_metadata_ui("scans_meta"),
    setup_modules = function(file, code) {
      ie_scans_metadata_server("scans_meta", file)
      # only filter-to-type in the generated code when the object is actually
      # mixed (computed here, at app start, so it never touches data before then)
      scans_filter <- app_focused_filter(
        isofiles,
        isoreader2::ir_filter_for_scans,
        "ir_filter_for_scans"
      )
      code$register(
        "scans_agg",
        "Aggregate data files",
        code_object_aggregate_step(
          variable_name,
          scans_filter,
          file$get_units,
          "scans_data"
        )
      )
    },
    timezone = timezone,
    default_theme = default_theme,
    initial_selection = !!initial_selection,
    detached = detached,
    launch = launch,
    log_level = log_level,
    options = options,
    uiPattern = uiPattern,
    enableBookmarking = enableBookmarking
  ))
}
