# launching the focused explorer apps =====
# The ie_explore_*() apps open a Shiny GUI. By default they run *detached* in a
# separate R process (via callr) so the calling session stays free, and they
# refuse to run while a document is being rendered (knitr / Quarto / R Markdown).

# TRUE while a knitr / Quarto / R Markdown document is being rendered
app_in_rendering <- function() {
  isTRUE(getOption("knitr.in.progress"))
}

# the message shown when the GUI is (incorrectly) launched during rendering. Names
# the launching function from the captured call (e.g. ie_explore_scans) so the user
# knows what to run interactively; `.call = caller_call()` picks up that caller.
app_rendering_notice <- function(.call = caller_call()) {
  fn <- tryCatch(as.character(.call[[1L]])[[1L]], error = function(e) {
    NA_character_
  })
  cli::cli_inform(c(
    "!" = "The {.pkg isoexplorer} GUI cannot run while a document is being rendered.",
    "i" = if (is.na(fn)) {
      "Call it interactively (in an R session) to launch the app."
    } else {
      "Call {.code {fn}()} interactively (in an R session) to launch the app."
    }
  ))
  invisible(NULL)
}

# the isoexplorer source path to hand the detached child WHEN this session is
# running the package under pkgload/devtools (load_all): the child then loads the
# same in-development version. NULL when installed normally, so the child just
# library()s the installed package.
app_dev_path <- function() {
  is_dev <- tryCatch(
    requireNamespace("pkgload", quietly = TRUE) &&
      pkgload::is_dev_package("isoexplorer"),
    error = function(e) FALSE
  )
  if (isTRUE(is_dev)) {
    tryCatch(getNamespaceInfo("isoexplorer", "path"), error = function(e) NULL)
  } else {
    NULL
  }
}

# the self-contained function run in the detached child process. Its environment
# is detached to baseenv() so callr can serialize it without pulling in (or
# depending on) the isoexplorer namespace, which the child loads itself. It reads
# the app spec (isofiles + UI + wiring closures) from `rds`, then re-invokes
# ie_run_app() in-process to build and run the app on the parent-chosen `port`.
app_detached_runner <- function() {
  runner <- function(rds, sel_expr, pkg_path, port) {
    if (!is.null(pkg_path)) {
      # parent is running the in-development package -> load the same source
      pkgload::load_all(pkg_path, quiet = TRUE)
    } else if (!requireNamespace("isoexplorer", quietly = TRUE)) {
      stop("isoexplorer is not installed")
    }
    # read the app-building arguments only AFTER the package is loaded, so the
    # closures they contain relink against its namespace
    app_args <- readRDS(rds)
    unlink(rds)
    # run on the parent-chosen port; the parent (interactive) session opens the
    # browser once the server is up -- this non-interactive child cannot
    app_args$options <- utils::modifyList(
      app_args$options %||% list(),
      list(port = port, host = "127.0.0.1", launch.browser = FALSE)
    )
    ie_run_app <- getExportedValue("isoexplorer", "ie_run_app")
    rlang::inject(ie_run_app(
      !!!app_args,
      initial_selection = !!str2lang(sel_expr),
      detached = FALSE,
      launch = TRUE,
      stop_on_close = TRUE
    ))
  }
  environment(runner) <- baseenv()
  runner
}

# the external-browser launcher command for the current platform (NULL on Windows,
# where shell.exec() is used instead). Kept separate so it can be unit-tested.
app_external_browser <- function() {
  if (.Platform$OS.type == "windows") {
    return(NULL)
  }
  if (identical(Sys.info()[["sysname"]], "Darwin")) "open" else "xdg-open"
}

# open `url` in the system's default *external* browser. Goes straight to the OS
# opener (macOS `open`, Linux `xdg-open`, Windows shell.exec) so it bypasses any
# R-level browser hook -- IDEs such as RStudio / Positron redirect
# getOption("browser") (and thus utils::browseURL()) to an internal viewer. Falls
# back to utils::browseURL() only if the external open fails.
app_browse_external <- function(url) {
  opened <- tryCatch(
    {
      if (.Platform$OS.type == "windows") {
        # shell.exec() exists only on Windows; fetch it dynamically so the
        # reference does not trip up checks on other platforms
        get("shell.exec", envir = baseenv())(url)
      } else {
        # system2() runs via the shell, so quote the URL
        system2(
          app_external_browser(),
          args = shQuote(url),
          wait = FALSE,
          stdout = FALSE,
          stderr = FALSE
        )
      }
      TRUE
    },
    error = function(e) FALSE
  )
  if (!isTRUE(opened)) {
    tryCatch(utils::browseURL(url), error = function(e) NULL)
  }
  invisible(opened)
}

# whether something is listening on a local TCP `port` yet
app_port_open <- function(port) {
  con <- tryCatch(
    suppressWarnings(socketConnection(
      "127.0.0.1",
      port,
      open = "r",
      blocking = TRUE,
      timeout = 1
    )),
    error = function(e) NULL
  )
  if (is.null(con)) {
    return(FALSE)
  }
  close(con)
  TRUE
}

# open `url` in the browser once the detached process `proc` is serving on `port`
# (poll briefly while the child starts up). Returns FALSE if the child died before
# coming up; opens the URL anyway as a best effort if the wait times out.
app_open_when_ready <- function(
  proc,
  port,
  url,
  tries = 75L,
  sleep = 0.2
) {
  for (i in seq_len(tries)) {
    if (!proc$is_alive()) {
      return(invisible(FALSE))
    }
    if (app_port_open(port)) {
      app_browse_external(url)
      return(invisible(TRUE))
    }
    Sys.sleep(sleep)
  }
  app_browse_external(url)
  invisible(FALSE)
}

# launch an app in a separate R process via callr: the whole app spec (`app_args`,
# i.e. the ie_run_app() arguments -- isofiles, UI, wiring closures, ...) is handed
# over in a temporary .rds and ie_run_app() is re-invoked there in-process. The
# child serves on a local port (the user's `options$port` if set, else a random
# one); when `browser`, this (interactive) session opens it once it is up. The app
# stops itself when the browser disconnects (stop_on_close -> onSessionEnded) and
# the supervised process is killed if this session exits -- so no process is left
# running. Returns the callr process (invisibly) with the app URL attached.
app_launch_detached <- function(
  app_args,
  initial_selection,
  options = list(),
  browser = TRUE
) {
  rlang::check_installed("callr", "to launch a detached isoexplorer app")
  rds <- tempfile("isoexplorer_", fileext = ".rds")
  saveRDS(app_args, rds)
  # the filter is re-evaluated in the child, so pass it as a (self-contained)
  # expression string rather than a quosure carrying this session's environment
  sel_expr <- paste(
    deparse(rlang::quo_get_expr(initial_selection)),
    collapse = " "
  )
  # the parent picks the port so it knows the URL to open; honor a user-set port
  port <- options$port %||% httpuv::randomPort()
  url <- sprintf("http://127.0.0.1:%d", port)
  proc <- callr::r_bg(
    func = app_detached_runner(),
    args = list(
      rds = rds,
      sel_expr = sel_expr,
      pkg_path = app_dev_path(),
      port = port
    ),
    supervise = TRUE
  )
  # open the browser from here (an interactive session); a non-interactive child
  # process cannot reliably launch one
  if (isTRUE(browser)) {
    app_open_when_ready(proc, port, url)
  }
  attr(proc, "url") <- url
  invisible(proc)
}
