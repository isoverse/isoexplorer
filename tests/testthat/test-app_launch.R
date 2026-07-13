# Tests for the explorer launch behavior: the rendering guard, and the detached
# (separate-process) launch. The detached integration test reads the isoreader2
# example files and is skipped when that or callr is unavailable.

# a minimal stand-in that passes the ir_isofiles validation (no real data needed
# for the rendering guard, which returns before any data is touched)
fake_isofiles <- function() {
  structure(tibble::tibble(file_path = "x"), class = "ir_isofiles")
}

test_that("app_in_rendering reflects the knitr.in.progress option", {
  expect_false(app_in_rendering())
  old <- options(knitr.in.progress = TRUE)
  on.exit(options(old), add = TRUE)
  expect_true(app_in_rendering())
})

test_that("explorers refuse to launch while a document is being rendered", {
  old <- options(knitr.in.progress = TRUE)
  on.exit(options(old), add = TRUE)
  iso <- fake_isofiles()
  for (fn in list(
    ie_explore_continuous_flow,
    ie_explore_dual_inlet,
    ie_explore_scans,
    ie_explore_metadata
  )) {
    res <- NULL
    expect_message(
      res <- fn(iso),
      "cannot run while a document is being rendered"
    )
    # nothing is launched -- it just returns invisibly
    expect_null(res)
  }
})

test_that("a non-detached explorer with launch = FALSE returns a runnable shiny app", {
  iso <- read_cf_examples()
  # detached = FALSE, launch = FALSE hands back the app object without running it
  app <- ie_explore_scans(iso, detached = FALSE, launch = FALSE)
  expect_s3_class(app, "shiny.appobj")
})

test_that("ie_run_app dispatches on detached / launch", {
  iso <- read_cf_examples()
  build <- function(...) {
    ie_run_app(
      isofiles = iso,
      main = ie_type_explorer_ui("cf_meta", ie_cf_plot_ui("cf")),
      setup_modules = function(file, code) {
        ie_cf_metadata_server("cf_meta", file)
        ie_cf_plot_server("cf", file)
      },
      ...
    )
  }

  # launch = FALSE -> the shiny app object, unrun
  expect_s3_class(build(detached = FALSE, launch = FALSE), "shiny.appobj")

  # launch = TRUE -> run in-session via runApp() (mocked so nothing blocks)
  ran <- NULL
  testthat::local_mocked_bindings(
    runApp = function(app, ...) {
      ran <<- app
      invisible(NULL)
    },
    .package = "shiny"
  )
  build(detached = FALSE, launch = TRUE)
  expect_s3_class(ran, "shiny.appobj")
})

test_that("log_level sets the LOG_LEVEL env var (explorers WARN, server TRACE)", {
  iso <- read_cf_examples()
  # LOG_LEVEL is a global side effect -- snapshot and restore it
  old <- Sys.getenv("LOG_LEVEL", unset = NA)
  withr::defer(
    if (is.na(old)) Sys.unsetenv("LOG_LEVEL") else Sys.setenv(LOG_LEVEL = old)
  )

  Sys.setenv(LOG_LEVEL = "INFO")
  ie_create_isofiles_server()
  expect_identical(Sys.getenv("LOG_LEVEL"), "TRACE") # server default

  Sys.setenv(LOG_LEVEL = "INFO")
  ie_explore_scans(iso, detached = FALSE, launch = FALSE)
  expect_identical(Sys.getenv("LOG_LEVEL"), "WARN") # explorer default

  Sys.setenv(LOG_LEVEL = "INFO")
  ie_explore_scans(iso, detached = FALSE, launch = FALSE, log_level = "DEBUG")
  expect_identical(Sys.getenv("LOG_LEVEL"), "DEBUG")

  # invalid levels are rejected
  expect_error(ie_create_isofiles_server(log_level = "banana"))
})

test_that("app_external_browser picks the platform external opener", {
  cmd <- app_external_browser()
  if (.Platform$OS.type == "windows") {
    expect_null(cmd) # shell.exec() is used instead
  } else if (identical(Sys.info()[["sysname"]], "Darwin")) {
    expect_identical(cmd, "open")
  } else {
    expect_identical(cmd, "xdg-open")
  }
})

test_that("app_browse_external bypasses the IDE browser hook", {
  skip_on_os("windows")
  # if the (IDE) browser option were used, this would error
  old <- options(browser = function(url) {
    stop("IDE browser hook must not be used")
  })
  on.exit(options(old), add = TRUE)
  # swap in a harmless opener so nothing actually launches
  testthat::local_mocked_bindings(app_external_browser = function() "true")
  expect_silent(app_browse_external("http://127.0.0.1:9/x"))
})

test_that("app_dev_path points at the source when running under pkgload", {
  # this test suite runs via devtools::load_all(), i.e. as a dev package
  skip_if_not_installed("pkgload")
  skip_if_not(pkgload::is_dev_package("isoexplorer"))
  expect_true(dir.exists(app_dev_path()))
  expect_true(file.exists(file.path(app_dev_path(), "DESCRIPTION")))
})

test_that("detached launch starts a separate process that serves the app", {
  skip_on_cran()
  skip_if_not_installed("callr")
  iso <- read_cf_examples()

  # the whole app spec (ie_run_app() arguments) is handed to the child process
  app_args <- list(
    isofiles = iso,
    main = ie_type_explorer_ui("cf_meta", ie_cf_plot_ui("cf")),
    setup_modules = function(file, code) {
      ie_cf_metadata_server("cf_meta", file)
      ie_cf_plot_server("cf", file)
    }
  )
  proc <- app_launch_detached(app_args, rlang::quo(FALSE), browser = FALSE)
  on.exit(if (proc$is_alive()) proc$kill(), add = TRUE)

  url <- attr(proc, "url")
  expect_match(url, "^http://127\\.0\\.0\\.1:[0-9]+$")
  port <- as.integer(sub(".*:", "", url))

  # wait for the child to build the app and start listening on its port
  listening <- FALSE
  for (i in seq_len(80)) {
    if (!proc$is_alive()) {
      break
    }
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
    if (!is.null(con)) {
      close(con)
      listening <- TRUE
      break
    }
    Sys.sleep(0.25)
  }
  expect_true(proc$is_alive())
  expect_true(listening)

  # killing the process terminates it cleanly (no rogue process left)
  proc$kill()
  proc$wait(timeout = 5000)
  expect_false(proc$is_alive())
})
