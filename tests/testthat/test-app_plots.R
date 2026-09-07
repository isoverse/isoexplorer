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

test_that("get_code reflects facet (default NULL) and color (default isoreader2)", {
  shiny::testServer(
    ie_cf_plot_server,
    args = list(file = mock_file()),
    {
      # facet "(none)" matches the plot function's NULL default -> omitted; color
      # "(none)" suppresses isoreader2's colour grouping -> explicit `= NULL`
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
      # defaults to facet = NULL; color "(default)" leaves the colour aesthetic to
      # isoreader2 entirely, so nothing is emitted for it
      session$setInputs(facet = "file_name", color = "(default)")
      code3 <- get_code()$code
      expect_match(code3, "facet = file_name", fixed = TRUE)
      expect_false(grepl("color =", code3, fixed = TRUE))

      # picking `trace` is now an explicit override (one colour per trace), so it
      # IS emitted -- it no longer coincides with the plot function's default
      session$setInputs(color = "trace")
      expect_match(get_code()$code, "color = trace", fixed = TRUE)
    }
  )
})

# a comparable summary of what a plot actually draws: the traces on screen and
# the column the colour aesthetic is mapped to
plot_summary <- function(p) {
  b <- ggplot2::ggplot_build(p)
  list(
    traces = sort(unique(as.character(b$plot$data$trace))),
    color = rlang::as_label(p$mapping$colour %||% rlang::quo(NULL))
  )
}

test_that("the mass/ratio selection reaches the plot function as arguments", {
  attach_isoreader2()
  shiny::testServer(
    ie_cf_plot_server,
    args = list(file = mock_file()),
    {
      session$setInputs(
        facet = "(none)",
        color = "(default)",
        linetype = "(none)"
      )
      session$setInputs(
        ratios_calculate = TRUE,
        ratios_normalize = FALSE,
        ratios_num_add = 100,
        ratios_denom_add = 100,
        ratios_apply = 1
      )
      # untouched checkboxes mean "everything" -> no selection arguments at all
      expect_equal(get_selection_args(), list())

      # narrowing the masses names them; the ratio stays at its default
      session$setInputs(CO2 = "44")
      expect_equal(get_selection_args(), list(mass = "44"))

      # un-checking every ratio is an explicit "none" -- the whole point of the
      # c() semantics, since omitting `ratio` would show all of them again
      session$setInputs(`CO2-ratios` = character(0))
      expect_equal(
        get_selection_args(),
        list(mass = "44", ratio = character(0))
      )
      expect_match(get_code()$code, "ratio = c()", fixed = TRUE)

      # ... and un-checking every mass while keeping a ratio plots ratios only
      session$setInputs(CO2 = character(0), `CO2-ratios` = "45/44")
      expect_equal(
        get_selection_args(),
        list(mass = character(0))
      )
      expect_match(get_code()$code, "mass = c()", fixed = TRUE)
    }
  )
})

test_that("the generated code reproduces the plot that is on screen", {
  attach_isoreader2()
  shiny::testServer(
    ie_cf_plot_server,
    args = list(file = mock_file()),
    {
      session$setInputs(
        facet = "(none)",
        color = "(default)",
        linetype = "(none)"
      )
      session$setInputs(
        ratios_calculate = TRUE,
        ratios_normalize = FALSE,
        ratios_num_add = 100,
        ratios_denom_add = 100,
        ratios_apply = 1
      )

      # the generated code runs against the same (ratio-calculated) data the app
      # plots, so evaluating it must yield the same traces
      check_matches <- function(label) {
        data <- get_ratio_data()
        generated <- eval(parse(text = get_code()$code))
        expect_equal(
          plot_summary(generated),
          plot_summary(generate_plot()),
          info = label
        )
      }

      check_matches("everything selected")

      session$setInputs(CO2 = "44")
      check_matches("one mass, all ratios")

      session$setInputs(`CO2-ratios` = character(0))
      check_matches("one mass, no ratios")

      session$setInputs(CO2 = character(0), `CO2-ratios` = "45/44")
      check_matches("no masses, one ratio")

      session$setInputs(CO2 = c("44", "45"), `CO2-ratios` = "45/44")
      check_matches("all masses, one ratio")

      # an explicit colour override must survive into the code too
      session$setInputs(color = "trace")
      check_matches("colour by trace")

      session$setInputs(color = "(none)")
      check_matches("no colour aesthetic")
    }
  )
})
