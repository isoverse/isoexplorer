# launching the focused explorer apps =====
# The ie_explore_*() apps open a Shiny GUI. By default they run *detached* in a
# separate R process (via callr) so the calling session stays free, and they
# refuse to run while a document is being rendered (knitr / Quarto / R Markdown).

# TRUE while a knitr / Quarto / R Markdown document is being rendered
app_in_rendering <- function() {
  isTRUE(getOption("knitr.in.progress"))
}

# the message shown when an explorer is (incorrectly) called during rendering
app_rendering_notice <- function(fn_name) {
  cli::cli_inform(c(
    "!" = "The {.pkg isoexplorer} GUI cannot run while a document is being rendered.",
    "i" = "Call {.code {fn_name}()} interactively (in an R session) to launch the app."
  ))
  invisible(NULL)
}

# run a focused explorer: refuse during rendering, otherwise launch detached (in a
# separate process) when `detached`, or build + return the shiny app in-process.
# `build(stop_on_close)` constructs the shiny app; `fn_name` is the explorer's name
# (re-invoked in the detached child); `...` are the explorer's other arguments,
# forwarded to that child call.
app_run_focused <- function(
  fn_name,
  isofiles,
  variable_name,
  initial_selection,
  detached,
  stop_on_close,
  build,
  ...
) {
  if (app_in_rendering()) {
    return(app_rendering_notice(fn_name))
  }
  if (isTRUE(detached)) {
    return(app_launch_detached(
      fn_name,
      isofiles = isofiles,
      variable_name = variable_name,
      initial_selection = initial_selection,
      dots = list(...)
    ))
  }
  build(stop_on_close)
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
# depending on) the isoexplorer namespace, which the child loads itself.
app_detached_runner <- function() {
  runner <- function(
    rds,
    fn_name,
    variable_name,
    sel_expr,
    dots,
    pkg_path,
    port
  ) {
    if (!is.null(pkg_path)) {
      # parent is running the in-development package -> load the same source
      pkgload::load_all(pkg_path, quiet = TRUE)
    } else if (!requireNamespace("isoexplorer", quietly = TRUE)) {
      stop("isoexplorer is not installed")
    }
    obj <- readRDS(rds)
    unlink(rds)
    fn <- getExportedValue("isoexplorer", fn_name)
    app <- rlang::inject(fn(
      obj,
      variable_name = variable_name,
      detached = FALSE,
      .stop_on_close = TRUE,
      initial_selection = !!str2lang(sel_expr),
      !!!dots
    ))
    # the browser is opened by the parent (interactive) session once the server is
    # up -- this non-interactive child cannot reliably open one
    shiny::runApp(
      app,
      port = port,
      host = "127.0.0.1",
      launch.browser = FALSE
    )
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

# launch a focused explorer in a separate R process via callr: the isofiles are
# handed over in a temporary .rds and the explorer is re-invoked there by name
# (keeping the original `variable_name` for code generation). The child serves on a
# random local port; when `browser`, this (interactive) session opens it in the
# default browser once it is up. The app stops itself when the browser disconnects
# (.stop_on_close -> onSessionEnded) and the supervised process is killed if this
# session exits -- so no process is left running. Returns the callr process
# (invisibly) with the app URL attached.
app_launch_detached <- function(
  fn_name,
  isofiles,
  variable_name,
  initial_selection,
  dots = list(),
  browser = TRUE
) {
  rlang::check_installed("callr", "to launch a detached isoexplorer app")
  rds <- tempfile("isoexplorer_", fileext = ".rds")
  saveRDS(isofiles, rds)
  sel_expr <- paste(
    deparse(rlang::quo_get_expr(initial_selection)),
    collapse = " "
  )
  port <- httpuv::randomPort()
  url <- sprintf("http://127.0.0.1:%d", port)
  proc <- callr::r_bg(
    func = app_detached_runner(),
    args = list(
      rds = rds,
      fn_name = fn_name,
      variable_name = variable_name,
      sel_expr = sel_expr,
      dots = dots,
      pkg_path = app_dev_path(),
      port = port
    ),
    supervise = TRUE
  )
  cli::cli_inform(c(
    "v" = "{.pkg isoexplorer} is starting in a separate process at {.url {url}}.",
    "i" = "It will open in your browser; close the browser tab to stop it."
  ))
  # open the browser from here (an interactive session); a non-interactive child
  # process cannot reliably launch one
  if (isTRUE(browser)) {
    app_open_when_ready(proc, port, url)
  }
  attr(proc, "url") <- url
  invisible(proc)
}
