# Tests for the file server's speed optimization (data-only aggregator + metadata
# re-use) and the initial_selection filter expression. The aggregator helper is
# checked directly; the initial_selection threading is checked by driving
# ie_file_server() the way app_server() does (read isoreader2 example files, skipped
# when that is not possible -- e.g. the external isoextract helper is unavailable).
# Shared helpers attach_isoreader2() / read_cf_examples() live in helper-examples.R.

test_that("data_only_aggregator drops metadata but keeps the data series + keys", {
  attach_isoreader2()
  agg <- data_only_aggregator()
  expect_s3_class(agg, "ir_aggregator")
  # no metadata aggregation (that is reused from the metadata aggregator) ...
  expect_false("metadata" %in% agg$dataset)
  # ... but the data series remain, and each carries its own analysis key (uidx is
  # added automatically), so the result can still be joined to the metadata
  expect_true(all(c("traces", "cycles", "scans") %in% agg$dataset))
  for (ds in c("traces", "cycles", "scans")) {
    cols <- agg$column[agg$dataset == ds]
    expect_true("analysis" %in% cols)
    expect_true("species" %in% cols)
    expect_true("mass" %in% cols)
  }
})

# a harness module that drives ie_file_server() exactly the way app_server() does:
# it receives the initial_selection as a quosure value and forwards it spliced
# (`!!`), returning the continuous-flow selection as a reactive
cf_selection_harness <- function(id, get_isofiles, sel) {
  shiny::moduleServer(id, function(input, output, session) {
    file <- rlang::inject(ie_file_server(
      "files",
      get_isofiles = get_isofiles,
      initial_selection = !!sel
    ))
    shiny::reactive(file$get_cf_selection())
  })
}

# the size of the default continuous-flow selection for an initial_selection
# filter: NA for "all" (the NULL sentinel), otherwise the number of selected rows.
# The summary is computed *inside* the testServer block and captured out (the
# reactive graph evaluates reliably there; testServer's own return value does not).
resolve_cf_selection_n <- function(iso, sel_quo) {
  n <- NULL
  shiny::testServer(
    cf_selection_harness,
    args = list(get_isofiles = shiny::reactive(iso), sel = sel_quo),
    {
      session$flushReact()
      sel <- session$returned()
      n <<- if (is.null(sel)) NA_integer_ else nrow(sel)
    }
  )
  n
}

test_that("initial_selection filter expression drives the default selection", {
  iso <- read_cf_examples()
  # TRUE -> everything (NA marks the all-selected NULL sentinel)
  expect_true(is.na(resolve_cf_selection_n(iso, rlang::quo(TRUE))))
  # FALSE -> nothing
  expect_equal(resolve_cf_selection_n(iso, rlang::quo(FALSE)), 0L)
  # an expression matching nothing -> nothing (and references its environment)
  missing_name <- "definitely_not_a_file"
  expect_equal(
    resolve_cf_selection_n(iso, rlang::quo(file_name == missing_name)),
    0L
  )
})
