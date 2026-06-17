test_that("ie_code_server assembles registered generators depth-first with threading", {
  shiny::testServer(ie_code_server, {
    register(
      "isofiles",
      "Read data files",
      get_code = function(input_var = NULL) {
        list(code = "iso_files <- ir_read_isofiles()", output = "iso_files")
      }
    )
    register(
      "scans_select",
      "Select scans",
      depends_on = "isofiles",
      get_code = function(input_var = NULL) {
        list(
          code = paste0("scans <- ", input_var, " |> ir_filter_for_scans()"),
          output = "scans"
        )
      }
    )
    register(
      "scans_plot",
      "Plot scans",
      depends_on = "scans_select",
      get_code = function(input_var = NULL) {
        list(code = paste0(input_var, " |> ir_plot_scans()"), output = NULL)
      }
    )

    doc <- build_document(quarto = FALSE)

    # heading levels follow the dependency depth
    expect_equal(doc$headings$level, c(1L, 2L, 3L))
    expect_equal(
      doc$headings$text,
      c("Read data files", "Select scans", "Plot scans")
    )
    # output -> input variable threading
    expect_match(doc$script, "scans <- iso_files |> ir_filter_for_scans()", fixed = TRUE)
    expect_match(doc$script, "scans |> ir_plot_scans()", fixed = TRUE)
    # the recorded line really is the heading line
    lines <- strsplit(doc$script, "\n", fixed = TRUE)[[1]]
    expect_equal(lines[doc$headings$line[[1]]], "# Read data files")
    expect_equal(lines[doc$headings$line[[3]]], "### Plot scans")

    # Quarto view wraps each section in a chunk
    expect_match(build_document(quarto = TRUE)$script, "```{r}", fixed = TRUE)
  })
})

test_that("ie_code_server restricts to the active group", {
  shiny::testServer(
    ie_code_server,
    args = list(get_active_group = reactive("Scans")),
    {
      register("isofiles", "Read", get_code = function(input_var = NULL) {
        list(code = "iso_files <- ir_read_isofiles()", output = "iso_files")
      })
      register("cf_plot", "Plot CF", depends_on = "isofiles", group = "Continuous Flow",
        get_code = function(input_var = NULL) list(code = "cf()", output = NULL))
      register("scans_plot", "Plot scans", depends_on = "isofiles", group = "Scans",
        get_code = function(input_var = NULL) list(code = "scans()", output = NULL))

      doc <- build_document(quarto = FALSE)
      # the group-less root + the active "Scans" group, NOT "Continuous Flow"
      expect_match(doc$script, "scans()", fixed = TRUE)
      expect_match(doc$script, "iso_files", fixed = TRUE)
      expect_false(grepl("cf()", doc$script, fixed = TRUE))
      expect_equal(doc$headings$text, c("Read", "Plot scans"))
    }
  )
})

test_that("ie_code_server degrades a failing generator to a comment", {
  shiny::testServer(ie_code_server, {
    register(
      "isofiles",
      "Read data files",
      get_code = function(input_var = NULL) stop("boom")
    )
    doc <- build_document(quarto = FALSE)
    expect_match(doc$script, "code unavailable", fixed = TRUE)
    expect_match(doc$script, "boom", fixed = TRUE)
  })
})
