#' Explore isofiles in the isoexplorer GUI
#'
#' Launches a Shiny application for exploring stable isotope data files using
#' the isoreader2 package.
#'
#' @param isofiles optional `ir_isofiles` object to pre-load into the GUI
#' @param allow_upload whether to allow file uploads in the GUI
#' @param path starting directory path for file browsing (defaults to working directory)
#' @param timezone the timezone to use for datetime display
#' @param default_theme the default bslib Bootstrap 5 theme preset
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
  default_theme = c(
    "flatly",
    "cosmo",
    "lumen",
    "minty",
    "sandstone",
    "darkly",
    "cyborg",
    "slate",
    "superhero",
    "solar"
  )
) {
  log_info("\n\n========================================================")
  log_info("starting isoexplorer GUI", if (shiny::in_devmode()) " in DEV mode")

  # ensure isoreader2 is attached so .onAttach runs and registers the aggregators
  if (!"isoreader2" %in% .packages()) {
    library(isoreader2)
  }

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

  ui <- ie_ui(
    path = path,
    timezone = timezone,
    default_theme = default_theme
  )
  server <- ie_server(
    isofiles = isofiles,
    allow_upload = allow_upload,
    path = path,
    timezone = timezone
  )

  shinyApp(
    ui = ui,
    server = server,
    options = options,
    enableBookmarking = enableBookmarking,
    uiPattern = uiPattern
  )
}
