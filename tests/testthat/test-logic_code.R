test_that("build_code_tree orders depth-first with correct depths", {
  # isofiles (root) -> {cf_select -> cf_plot, scans_select -> scans_plot}
  specs <- list(
    isofiles = list(heading = "Read", depends_on = NULL),
    cf_select = list(heading = "Select CF", depends_on = "isofiles"),
    cf_plot = list(heading = "Plot CF", depends_on = "cf_select"),
    scans_select = list(heading = "Select scans", depends_on = "isofiles"),
    scans_plot = list(heading = "Plot scans", depends_on = "scans_select")
  )
  nodes <- build_code_tree(specs)
  ids <- vapply(nodes, `[[`, character(1), "id")
  depths <- vapply(nodes, `[[`, integer(1), "depth")

  # depth-first, siblings in registration order
  expect_equal(
    ids,
    c("isofiles", "cf_select", "cf_plot", "scans_select", "scans_plot")
  )
  expect_equal(depths, c(1L, 2L, 3L, 2L, 3L))
})

test_that("build_code_tree treats unknown/NULL depends_on as roots", {
  specs <- list(
    a = list(heading = "A", depends_on = NULL),
    b = list(heading = "B", depends_on = "missing"), # unknown -> root
    c = list(heading = "C", depends_on = "a")
  )
  nodes <- build_code_tree(specs)
  ids <- vapply(nodes, `[[`, character(1), "id")
  depths <- vapply(nodes, `[[`, integer(1), "depth")
  # roots a, b in order (a then its child c, then b)
  expect_equal(ids, c("a", "c", "b"))
  expect_equal(depths, c(1L, 2L, 1L))
})

test_that("build_code_tree handles the empty registry", {
  expect_equal(build_code_tree(list()), list())
})

test_that("render_code_document sets heading levels and line numbers", {
  sections <- list(
    list(depth = 1L, heading = "Read", code = "iso_files <- ir_read_isofiles()"),
    list(depth = 2L, heading = "Select", code = "scans <- iso_files |>\n  ir_filter_for_scans()"),
    list(depth = 3L, heading = "Plot", code = "scans |> ir_plot_scans()")
  )
  doc <- render_code_document(sections, quarto = FALSE)
  lines <- strsplit(doc$script, "\n", fixed = TRUE)[[1]]

  # heading prefixes follow the depth
  expect_equal(doc$headings$level, c(1L, 2L, 3L))
  expect_equal(lines[doc$headings$line], c("# Read", "## Select", "### Plot"))
  # the recorded line numbers really point at the headings
  expect_true(all(grepl("^#+ ", lines[doc$headings$line])))
})

test_that("render_code_document wraps code in chunks + front matter for quarto", {
  sections <- list(
    list(depth = 1L, heading = "Read", code = "iso_files <- ir_read_isofiles()")
  )
  doc <- render_code_document(sections, quarto = TRUE)
  expect_match(doc$script, "```{r}", fixed = TRUE)
  expect_match(doc$script, "iso_files <- ir_read_isofiles()", fixed = TRUE)
  expect_match(doc$script, "format: html", fixed = TRUE) # YAML front matter
  # heading line number accounts for the front matter offset
  lines <- strsplit(doc$script, "\n", fixed = TRUE)[[1]]
  expect_equal(lines[doc$headings$line], "# Read")
})

test_that("render_code_document can omit the front matter (viewer) while keeping chunks", {
  sections <- list(
    list(depth = 1L, heading = "Read", code = "iso_files <- ir_read_isofiles()")
  )
  doc <- render_code_document(sections, quarto = TRUE, front_matter = FALSE)
  expect_match(doc$script, "```{r}", fixed = TRUE) # still chunked
  expect_false(grepl("format: html", doc$script, fixed = TRUE)) # no YAML
  lines <- strsplit(doc$script, "\n", fixed = TRUE)[[1]]
  expect_equal(lines[doc$headings$line], "# Read") # heading is now line 1
  expect_equal(doc$headings$line, 1L)
})

test_that("render_code_document handles no sections", {
  doc <- render_code_document(list())
  expect_equal(doc$script, "")
  expect_equal(nrow(doc$headings), 0L)
})

test_that("code_value formats scalars and vectors", {
  expect_equal(code_value("mV"), '"mV"')
  expect_equal(code_value(14), "14")
  expect_equal(code_value(TRUE), "TRUE")
  expect_equal(code_value(c("v44", "v45")), 'c("v44", "v45")')
  expect_equal(code_value(c(120, 480)), "c(120, 480)")
})

test_that("code_arg names arguments", {
  expect_equal(code_arg("units", "mV"), 'units = "mV"')
  expect_equal(code_arg(NULL, "mV"), '"mV"')
  expect_equal(code_arg("", "mV"), '"mV"')
})

test_that("code_call builds calls and breaks long ones", {
  expect_equal(code_call("f"), "f()")
  expect_equal(code_call("f", list("x")), 'f("x")')
  expect_equal(code_call("f", list(a = 1, b = 2)), "f(a = 1, b = 2)")
  long <- code_call("ir_plot_scans", list(
    scan_type = "highvoltage_scan",
    masses = c("v44", "v45", "v46"),
    legend = "bottom"
  ))
  expect_match(long, "\n", fixed = TRUE) # multi-line
  expect_match(long, "scan_type = \"highvoltage_scan\"", fixed = TRUE)
})

test_that("code_raw is emitted verbatim (not quoted)", {
  expect_equal(code_value(code_raw("ir_default_theme(text_size = 14)")), "ir_default_theme(text_size = 14)")
  expect_equal(
    code_call("ir_plot_scans", list(theme = code_raw("ir_default_theme()"))),
    "ir_plot_scans(theme = ir_default_theme())"
  )
})

test_that("code_aes_value handles columns, factors, and non-syntactic names", {
  expect_equal(code_aes_value("species"), "species")
  expect_equal(code_aes_value("analysis", c("analysis", "uidx")), "factor(analysis)")
  expect_equal(code_aes_value("Peak Center"), "`Peak Center`")
  expect_equal(
    code_aes_value("Peak Center", "Peak Center"),
    "factor(`Peak Center`)"
  )
})

test_that("code_metadata_filter builds per-file conditions", {
  all <- tibble::tibble(
    file_name = c("a", "a", "b", "b", "c"),
    analysis = c(1L, 2L, 1L, 2L, 1L)
  )
  # nothing selected / NULL / all selected -> no filter
  expect_null(code_metadata_filter(NULL, all))
  expect_null(code_metadata_filter(all[0, ], all))
  expect_null(code_metadata_filter(all, all))

  # whole file selected -> just file_name; partial -> name + analysis subset
  sel <- tibble::tibble(file_name = c("a", "a", "b"), analysis = c(1L, 2L, 1L))
  expect_equal(
    code_metadata_filter(sel, all),
    '(file_name == "a") |\n  (file_name == "b" & analysis %in% c(1))'
  )
  # single selected file -> no surrounding parens
  sel1 <- tibble::tibble(file_name = "c", analysis = 1L)
  expect_equal(code_metadata_filter(sel1, all), 'file_name == "c"')
})

test_that("code_assign and code_pipe compose", {
  expect_equal(code_assign("x", "f()"), "x <- f()")
  expect_equal(
    code_pipe("iso_files", "ir_filter_for_scans()"),
    "iso_files |>\n  ir_filter_for_scans()"
  )
  expect_equal(code_pipe(NULL, "a", NULL), "a")
})
