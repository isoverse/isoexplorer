# reactive tests for the shared plot module (R/app_plots.R), focused on the
# Ratios popover's staged Apply/Cancel behavior and the ir_calculate_ratios()
# argument resolution it drives.

# a minimal ir_aggregated_data-like object for the continuous-flow plot server:
# two masses for one species (so a ratio can be formed) over a couple of points
mock_cf_data <- function() {
  agg <- list(
    metadata = tibble::tibble(
      uidx = 1L,
      analysis = "a1",
      file_name = "f1"
    ),
    traces = tibble::tibble(
      uidx = 1L,
      analysis = "a1",
      species = "CO2",
      mass = rep(c("44", "45"), each = 2L),
      time.s = rep(c(0, 1), times = 2L),
      intensity.mV = c(1000, 1000, 100, 200)
    )
  )
  class(agg) <- "ir_aggregated_data"
  agg
}

# a stand-in file handle exposing just what ie_cf_plot_server() reads
mock_file <- function(units = "mV") {
  u <- reactiveVal(units)
  list(
    get_aggregated_cf_data = reactive(mock_cf_data()),
    get_units = reactive(u()),
    set_units = function(x) u(x),
    get_cf_selection = reactive(NULL),
    get_cf_metadata = reactive(mock_cf_data()$metadata)
  )
}

test_that("Ratios popover stages settings and only Apply commits them", {
  shiny::testServer(
    ie_cf_plot_server,
    args = list(file = mock_file()),
    {
      # ratios are off by default -> no ir_calculate_ratios() args
      expect_null(get_ratio_calc())

      # staging the inputs alone does NOT change the applied settings
      session$setInputs(ratios_calculate = TRUE, ratios_normalize = FALSE)
      session$setInputs(ratios_num_add = 250, ratios_denom_add = 100)
      expect_null(get_ratio_calc())

      # Apply commits -> non-default numerator offset is emitted (mV -> V family)
      session$setInputs(ratios_apply = 1)
      expect_equal(get_ratio_calc(), list(num_add.V = 250))

      # the plot data now carries the calculated ratios
      expect_true("ratio_name" %in% names(get_ratio_data()$traces))

      # changing inputs again without Apply leaves the applied settings intact
      session$setInputs(ratios_num_add = 999)
      expect_equal(get_ratio_calc(), list(num_add.V = 250))

      # Cancel discards the staged edit (and the applied value is unchanged)
      session$setInputs(ratios_cancel = 1)
      expect_equal(get_ratio_calc(), list(num_add.V = 250))
    }
  )
})

test_that("Ratios popover emits normalize and respects the unit family", {
  file <- mock_file(units = "nA")
  shiny::testServer(
    ie_cf_plot_server,
    args = list(file = file),
    {
      # nA family default offset is 0, so a 0 stays default (omitted), but
      # normalize is emitted as a marker
      session$setInputs(
        ratios_calculate = TRUE,
        ratios_normalize = TRUE,
        ratios_num_add = 0,
        ratios_denom_add = 0,
        ratios_apply = 1
      )
      expect_equal(get_ratio_calc(), list(normalize_ratios = TRUE))

      # a non-default current offset uses the nA suffix
      session$setInputs(ratios_num_add = 5, ratios_apply = 2)
      expect_equal(
        get_ratio_calc(),
        list(num_add.nA = 5, normalize_ratios = TRUE)
      )
    }
  )
})

test_that("get_code reflects facet (default NULL) and color (default trace)", {
  shiny::testServer(
    ie_cf_plot_server,
    args = list(file = mock_file()),
    {
      # facet "(none)" matches the plot function's NULL default -> omitted; color
      # "(none)" suppresses the (trace) default -> explicit `= NULL`
      session$setInputs(facet = "(none)", color = "(none)", linetype = "(none)")
      code <- get_code()$code
      expect_false(grepl("facet =", code, fixed = TRUE))
      expect_match(code, "color = NULL", fixed = TRUE)

      # a real column is emitted as a bare column for both
      session$setInputs(facet = "species", color = "mass")
      code2 <- get_code()$code
      expect_match(code2, "facet = species", fixed = TRUE)
      expect_match(code2, "color = mass", fixed = TRUE)

      # facet = file_name (the app default) is emitted now that the plot function
      # defaults to facet = NULL; color = trace (the function default) is omitted
      session$setInputs(facet = "file_name", color = "trace")
      code3 <- get_code()$code
      expect_match(code3, "facet = file_name", fixed = TRUE)
      expect_false(grepl("color =", code3, fixed = TRUE))
    }
  )
})
