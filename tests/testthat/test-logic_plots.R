# tests for the pure plot-logic helpers in R/logic_plots.R

test_that("extract_masses builds a sorted, unique mass_id table", {
  # with species: mass_id is "mass:species", duplicates collapsed, sorted by mass
  traces <- tibble::tibble(
    mass = c("44", "28", "44", "45"),
    species = c("CO2", "N2", "CO2", "CO2"),
    intensity = 1:4
  )
  out <- extract_masses(traces)
  expect_equal(names(out), c("mass_id", "mass", "species"))
  expect_equal(out$mass, c("28", "44", "45")) # numeric sort, deduped
  expect_equal(out$mass_id, c("28:N2", "44:CO2", "45:CO2"))

  # without species: mass_id falls back to the mass as character
  scans <- tibble::tibble(mass = c(10, 2, 2), signal = 1:3)
  out2 <- extract_masses(scans)
  expect_equal(out2$mass, c(2, 10))
  expect_equal(out2$mass_id, c("2", "10"))
  expect_false("species" %in% names(out2))
})

test_that("species_mass_groups groups masses by species, sorted", {
  cycles <- tibble::tibble(
    species = c("CO2", "CO2", "N2", "CO2"),
    mass = c("45", "44", "28", "44"),
    intensity = 1:4
  )
  g <- species_mass_groups(cycles)
  expect_equal(g$species, c("CO2", "N2")) # alphabetical
  expect_equal(g$masses[[1]], c("44", "45")) # numeric sort, deduped
  expect_equal(g$masses[[2]], "28")
})

test_that("species_mass_groups handles a missing species column", {
  scans <- tibble::tibble(mass = c("10", "2"), signal = 1:2)
  g <- species_mass_groups(scans)
  expect_equal(nrow(g), 1L)
  expect_true(is.na(g$species))
  expect_equal(g$masses[[1]], c("2", "10"))
  expect_equal(g$ratios, list(character(0)))
})

test_that("species_mass_groups attaches available ratio names per species", {
  traces <- tibble::tibble(
    species = c("CO2", "CO2", "CO2", "N2", "N2"),
    mass = c("44", "45", "46", "28", "29"),
    # base-mass rows carry NA; ratios live on the numerator mass row
    ratio_name = c(NA, "45/44", "46/44", NA, "29/28"),
    ratio = c(NA, 1, 2, NA, 3)
  )
  g <- species_mass_groups(traces)
  expect_equal(g$species, c("CO2", "N2"))
  expect_equal(g$masses[[1]], c("44", "45", "46"))
  expect_equal(g$ratios[[1]], c("45/44", "46/44")) # sorted by numerator mass
  expect_equal(g$ratios[[2]], "29/28")
})

test_that("species_mass_groups yields empty ratios when none are present", {
  # no ratio_name column at all
  cycles <- tibble::tibble(species = c("CO2", "N2"), mass = c("44", "28"))
  expect_equal(
    species_mass_groups(cycles)$ratios,
    list(character(0), character(0))
  )
  # a ratio_name column that is all NA (single mass per species -> no ratios)
  scans <- tibble::tibble(
    species = c("CO2", "N2"),
    mass = c("44", "28"),
    ratio_name = c(NA_character_, NA_character_)
  )
  expect_equal(
    species_mass_groups(scans)$ratios,
    list(character(0), character(0))
  )
})

test_that("extract_ratio_groups sorts ratio names by numerator mass", {
  traces <- tibble::tibble(
    species = c("CO2", "CO2", "CO2"),
    mass = c("46", "45", "44"),
    ratio_name = c("46/44", "45/44", NA)
  )
  rg <- extract_ratio_groups(traces)
  expect_equal(rg$species, "CO2")
  expect_equal(rg$ratios[[1]], c("45/44", "46/44"))
  # no ratio_name column -> empty
  expect_equal(nrow(extract_ratio_groups(tibble::tibble(mass = "44"))), 0L)
})

test_that("initial_selection_row_ids reflects a selection as table row ids", {
  # table data: one row per analysis (uidx repeats for multi-analysis files)
  td <- tibble::tibble(
    row_id = 1:5,
    uidx = c(1, 1, 2, 3, 3),
    analysis = c("a1", "a2", "b1", "c1", "c2")
  )
  # no table yet -> NULL
  expect_null(initial_selection_row_ids(NULL, NULL))
  # NULL selection ("all") -> every row
  expect_equal(initial_selection_row_ids(td, NULL), 1:5)
  # a partial-file selection -> exactly those analyses, not the whole file
  sel <- tibble::tibble(uidx = c(1, 3), analysis = c("a1", "c2"))
  expect_equal(initial_selection_row_ids(td, sel), c(1L, 5L))
  # a 0-row selection ("none") -> no rows
  expect_equal(initial_selection_row_ids(td, tibble::tibble()), integer(0))
  # falls back to uidx when there is no analysis column
  td2 <- tibble::tibble(row_id = 1:5, uidx = c(1, 1, 2, 3, 3))
  expect_equal(
    initial_selection_row_ids(td2, tibble::tibble(uidx = c(1, 3))),
    c(1L, 2L, 4L, 5L)
  )
})

test_that("resolve_selection_filter maps a filter expression to a selection", {
  md <- tibble::tibble(
    uidx = 1:3,
    file_name = c("std_1", "smp_1", "std_2"),
    analysis = c(1L, 2L, 3L)
  )
  # TRUE -> everything matches -> NULL (the "all" sentinel)
  expect_null(resolve_selection_filter(md, rlang::quo(TRUE)))
  # FALSE -> nothing matches -> 0-row tibble (the "none" case)
  none <- resolve_selection_filter(md, rlang::quo(FALSE))
  expect_equal(nrow(none), 0L)
  # an expression -> the matching rows
  sub <- resolve_selection_filter(md, rlang::quo(grepl("std", file_name)))
  expect_equal(sub$file_name, c("std_1", "std_2"))
  # an expression matching everything still collapses to NULL ("all")
  expect_null(resolve_selection_filter(md, rlang::quo(uidx > 0)))
  # NULL / empty metadata -> NULL
  expect_null(resolve_selection_filter(NULL, rlang::quo(TRUE)))
  expect_null(resolve_selection_filter(md[0, ], rlang::quo(TRUE)))
  # the quosure can reference variables from its environment
  keep <- c("smp_1")
  expect_equal(
    resolve_selection_filter(md, rlang::quo(file_name %in% keep))$file_name,
    "smp_1"
  )
})

test_that("filter_by_selected_masses keeps only selected mass/species rows", {
  df <- tibble::tibble(
    mass = c("44", "45", "28"),
    species = c("CO2", "CO2", "N2"),
    y = 1:3
  )

  # joins on mass + species
  sel <- tibble::tibble(mass = "44", species = "CO2")
  expect_equal(filter_by_selected_masses(df, sel)$y, 1L)

  # empty selection -> zero rows, same columns
  empty <- filter_by_selected_masses(df, df[0L, ])
  expect_equal(nrow(empty), 0L)
  expect_equal(names(empty), names(df))

  # joins on mass only when species is absent from df
  df2 <- tibble::tibble(mass = c("44", "28"), y = 1:2)
  sel2 <- tibble::tibble(mass = "28", species = "N2")
  expect_equal(filter_by_selected_masses(df2, sel2)$y, 2L)
})

test_that("filter_agg_data_by_metadata filters by (uidx, analysis), guards empties", {
  # tables with an analysis column -> analysis-level filtering: selecting one
  # analysis of a multi-analysis file must NOT pull in the file's other analyses
  agg <- list(
    metadata = tibble::tibble(
      uidx = c(1, 1, 2),
      analysis = c("a1", "a2", "b1"),
      file = c("x", "x", "y")
    ),
    traces = tibble::tibble(
      uidx = c(1, 1, 1, 2),
      analysis = c("a1", "a1", "a2", "b1"),
      x = 1:4
    )
  )
  sel <- tibble::tibble(uidx = c(1, 2), analysis = c("a1", "b1"))
  out <- filter_agg_data_by_metadata(agg, sel)
  expect_equal(out$metadata$analysis, c("a1", "b1")) # a2 dropped
  expect_equal(out$traces$analysis, c("a1", "a1", "b1")) # a2's rows dropped
  expect_equal(out$traces$x, c(1L, 2L, 4L))

  # tables lacking an analysis column fall back to uidx; tables with neither key
  # are left untouched
  agg2 <- list(
    metadata = tibble::tibble(uidx = c(1, 2, 3), analysis = c("a", "b", "c")),
    cycles = tibble::tibble(uidx = c(1, 1, 2, 3), y = 1:4), # no analysis -> uidx
    other = tibble::tibble(no_keys = 1:2) # neither key -> untouched
  )
  out2 <- filter_agg_data_by_metadata(
    agg2,
    tibble::tibble(uidx = c(1, 3), analysis = c("a", "c"))
  )
  expect_equal(out2$cycles$uidx, c(1, 1, 3)) # uidx fallback
  expect_equal(out2$other, agg2$other) # unchanged

  # no selection -> NULL
  expect_null(filter_agg_data_by_metadata(agg, NULL))
  expect_null(filter_agg_data_by_metadata(agg, sel[0L, ]))
})

test_that("intensity_unit_family maps units to ir_calculate_ratios offset family", {
  expect_equal(intensity_unit_family("mV"), "V")
  expect_equal(intensity_unit_family("V"), "V")
  expect_equal(intensity_unit_family("nA"), "nA")
  expect_equal(intensity_unit_family("fA"), "nA")
  expect_equal(intensity_unit_family("A"), "nA")
  expect_equal(intensity_unit_family("µA"), "nA")
  expect_equal(intensity_unit_family("cps"), "cps")
  # unknown / empty falls back to voltage
  expect_equal(intensity_unit_family(NULL), "V")
  expect_equal(intensity_unit_family("???"), "V")
})

test_that("ratio_add_defaults matches the ir_calculate_ratios defaults", {
  expect_equal(ratio_add_defaults("V"), c(num = 100, denom = 100))
  expect_equal(ratio_add_defaults("nA"), c(num = 0, denom = 0))
  expect_equal(ratio_add_defaults("cps"), c(num = 0, denom = 0))
})

test_that("ratio_calc_params resolves settings + units to non-default args", {
  settings <- function(...) {
    modifyList(
      list(
        calculate = TRUE,
        normalize = FALSE,
        num_add = list(V = 100, nA = 0, cps = 0),
        denom_add = list(V = 100, nA = 0, cps = 0)
      ),
      list(...)
    )
  }
  # not calculating -> NULL
  expect_null(ratio_calc_params(settings(calculate = FALSE), "mV"))
  # calculating, all defaults -> empty arg list (ir_calculate_ratios() bare)
  expect_equal(ratio_calc_params(settings(), "mV"), list())
  # non-default voltage offsets are emitted with the family suffix
  expect_equal(
    ratio_calc_params(settings(num_add = list(V = 200, nA = 0, cps = 0)), "V"),
    list(num_add.V = 200)
  )
  # only the current unit family's offsets matter
  expect_equal(
    ratio_calc_params(settings(num_add = list(V = 200, nA = 5, cps = 0)), "nA"),
    list(num_add.nA = 5)
  )
  # both offsets + normalize marker
  p <- ratio_calc_params(
    settings(
      normalize = TRUE,
      num_add = list(V = 100, nA = 7, cps = 0),
      denom_add = list(V = 100, nA = 3, cps = 0)
    ),
    "pA"
  )
  expect_equal(
    p,
    list(num_add.nA = 7, denom_add.nA = 3, normalize_ratios = TRUE)
  )
})

test_that("plottable_ratios keeps only selected ratios with surviving data", {
  filtered <- tibble::tibble(
    mass = c("45", "46"),
    species = c("CO2", "CO2"),
    ratio_name = c("45/44", "46/44")
  )
  expect_equal(
    plottable_ratios(filtered, c("45/44", "46/44")),
    c("45/44", "46/44")
  )
  # a ratio whose numerator-mass row was dropped is removed; selection order kept
  expect_equal(plottable_ratios(filtered[1, ], c("46/44", "45/44")), "45/44")
  # nothing selected, or no ratio_name column -> empty
  expect_equal(plottable_ratios(filtered, character(0)), character(0))
  expect_equal(
    plottable_ratios(tibble::tibble(mass = "44"), "45/44"),
    character(0)
  )
})

test_that("apply_legend_position sets or hides the legend", {
  p <- ggplot2::ggplot()
  expect_equal(
    apply_legend_position(p, "bottom")$theme$legend.position,
    "bottom"
  )
  expect_equal(apply_legend_position(p, "hide")$theme$legend.position, "none")
  expect_null(apply_legend_position(NULL, "right")) # NULL passes through
})

test_that("build_data_plot returns NULL when there is nothing to plot", {
  plot_fn <- function(dataset, ...) ggplot2::ggplot()
  masses <- tibble::tibble(mass = "44")
  agg <- list(traces = tibble::tibble(mass = c("44", "45"), x = 1:2))

  expect_null(build_data_plot(NULL, "traces", masses, plot_fn))
  expect_null(build_data_plot(list(), "traces", masses, plot_fn)) # missing key
  expect_null(build_data_plot(
    list(traces = agg$traces[0L, ]),
    "traces",
    masses,
    plot_fn
  )) # empty dataset
  expect_null(build_data_plot(agg, "traces", NULL, plot_fn)) # no selection
  expect_null(build_data_plot(
    agg,
    "traces",
    tibble::tibble(mass = "99"),
    plot_fn
  )) # selection matches nothing
})

test_that("build_data_plot filters by masses and forwards args to plot_fn", {
  captured <- NULL
  plot_fn <- function(dataset, scientific, theme, ...) {
    captured <<- list(
      dataset = dataset,
      scientific = scientific,
      extra = list(...)
    )
    ggplot2::ggplot()
  }
  agg <- list(traces = tibble::tibble(mass = c("44", "45"), x = 1:2))
  masses <- tibble::tibble(mass = "44")

  p <- build_data_plot(
    agg,
    "traces",
    masses,
    plot_fn,
    scientific = NULL, # coerced via isTRUE() -> FALSE
    legend_position = "hide",
    time_window = c(0, 10) # extra plot arg flows through ...
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$theme$legend.position, "none")
  expect_equal(captured$dataset$traces$mass, "44") # filtered to the selection
  expect_false(captured$scientific)
  expect_equal(captured$extra$time_window, c(0, 10))
})

test_that("build_data_plot forwards selected ratios via ratio=", {
  captured <- NULL
  plot_fn <- function(dataset, scientific, ...) {
    captured <<- list(...)
    ggplot2::ggplot()
  }
  agg <- list(
    traces = tibble::tibble(
      species = rep("CO2", 3),
      mass = c("44", "45", "46"),
      ratio_name = c(NA, "45/44", "46/44"),
      x = 1:3
    )
  )
  masses <- tibble::tibble(species = rep("CO2", 3), mass = c("44", "45", "46"))

  # selected ratios with data -> forwarded as ratio=
  build_data_plot(
    agg,
    "traces",
    masses,
    plot_fn,
    selected_ratios = c("45/44", "46/44")
  )
  expect_equal(captured$ratio, c("45/44", "46/44"))

  # a ratio whose numerator mass is not selected is dropped
  build_data_plot(
    agg,
    "traces",
    tibble::tibble(species = "CO2", mass = "45"),
    plot_fn,
    selected_ratios = c("45/44", "46/44")
  )
  expect_equal(captured$ratio, "45/44")

  # no ratios selected -> no ratio argument at all
  captured <- NULL
  build_data_plot(agg, "traces", masses, plot_fn)
  expect_null(captured$ratio)
})

test_that("select_species_or_mass chooses species= vs mass= vs nothing", {
  groups <- tibble::tibble(
    species = c("A", "B", "C"),
    masses = list(c("1", "2"), "1", c("1", "2"))
  )
  all_sel <- tibble::tibble(
    species = c("A", "A", "B", "C", "C"),
    mass = c("1", "2", "1", "1", "2")
  )
  # everything selected -> no argument
  expect_equal(select_species_or_mass(all_sel, groups), list())
  # whole species C dropped, A and B fully kept -> species=
  drop_c <- tibble::tibble(species = c("A", "A", "B"), mass = c("1", "2", "1"))
  expect_equal(
    select_species_or_mass(drop_c, groups),
    list(species = c("A", "B"))
  )
  # A narrowed to just mass 1 (partial) -> mass=
  partial <- tibble::tibble(species = c("A", "B"), mass = c("1", "1"))
  expect_equal(select_species_or_mass(partial, groups), list(mass = "1"))
  # nothing selected -> no argument
  expect_equal(select_species_or_mass(all_sel[0, ], groups), list())
})
