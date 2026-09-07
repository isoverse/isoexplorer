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

test_that("apply_legend_position sets or hides the legend", {
  p <- ggplot2::ggplot()
  expect_equal(
    apply_legend_position(p, "bottom")$theme$legend.position,
    "bottom"
  )
  expect_equal(apply_legend_position(p, "hide")$theme$legend.position, "none")
  expect_null(apply_legend_position(NULL, "right")) # NULL passes through
})

# species_mass_groups()-shaped fixture: two species, N2 with ratios, CO2 without
mass_groups <- function(
  species = c("CO2", "N2"),
  masses = list(c("44", "45", "46"), c("28", "29")),
  ratios = list(character(0), c("29/28"))
) {
  tibble::tibble(species = species, masses = masses, ratios = ratios)
}
mass_sel <- function(...) {
  pairs <- list(...)
  dplyr::bind_rows(lapply(pairs, function(p) {
    tibble::tibble(species = p[[1]], mass = p[[2]])
  }))
}

test_that("selection_to_plot_args omits everything that is fully selected", {
  g <- mass_groups()
  all_sel <- mass_sel(
    c("CO2", "44"),
    c("CO2", "45"),
    c("CO2", "46"),
    c("N2", "28"),
    c("N2", "29")
  )
  # everything checked -> all defaults, nothing to pass
  expect_equal(selection_to_plot_args(all_sel, g, "29/28"), list())
  # no groups (no data yet) -> nothing either
  expect_equal(selection_to_plot_args(all_sel, NULL), list())
  expect_equal(selection_to_plot_args(all_sel, g[0, ]), list())
})

test_that("selection_to_plot_args expresses a de-selected species as species=", {
  g <- mass_groups()
  # all of N2 unchecked, CO2 untouched -> species= only (masses are complete)
  keep_co2 <- mass_sel(c("CO2", "44"), c("CO2", "45"), c("CO2", "46"))
  expect_equal(selection_to_plot_args(keep_co2, g), list(species = "CO2"))

  # two species sharing the same masses: `mass=` alone could not tell them apart,
  # which is exactly why the species argument is emitted
  shared <- mass_groups(
    species = c("CO", "N2"),
    masses = list(c("28", "29", "30"), c("28", "29", "30")),
    ratios = list(character(0), character(0))
  )
  keep_n2 <- mass_sel(c("N2", "28"), c("N2", "29"), c("N2", "30"))
  expect_equal(selection_to_plot_args(keep_n2, shared), list(species = "N2"))
})

test_that("selection_to_plot_args narrows masses with mass=", {
  g <- mass_groups()
  # a strict subset within a species -> mass= (sorted, both species still shown)
  narrowed <- mass_sel(
    c("CO2", "44"),
    c("CO2", "45"),
    c("N2", "28"),
    c("N2", "29")
  )
  expect_equal(
    selection_to_plot_args(narrowed, g, "29/28"),
    list(mass = c("44", "45", "28", "29") |> sort())
  )
  # species dropped AND masses narrowed -> both arguments
  both <- mass_sel(c("CO2", "44"))
  expect_equal(
    selection_to_plot_args(both, g),
    list(species = "CO2", mass = "44")
  )
})

test_that("selection_to_plot_args marks a fully empty selection with c()", {
  g <- mass_groups()
  # nothing checked at all -> explicit "no masses, no ratios" and NO species
  # filter (an empty species filter would error rather than draw nothing)
  expect_equal(
    selection_to_plot_args(mass_sel(), g),
    list(mass = character(0), ratio = character(0))
  )
  expect_equal(
    selection_to_plot_args(NULL, g),
    list(mass = character(0), ratio = character(0))
  )
})

test_that("selection_to_plot_args ignores masses that are not in the data", {
  g <- mass_groups()
  # a stale checkbox (e.g. just after a unit / scan-type change) must not name a
  # mass isoreader2 would reject
  stale <- mass_sel(c("CO2", "44"), c("CO2", "99"))
  expect_equal(
    selection_to_plot_args(stale, g),
    list(species = "CO2", mass = "44")
  )
})

test_that("selection_to_plot_args handles ratios independently of masses", {
  g <- mass_groups()
  all_masses <- mass_sel(
    c("CO2", "44"),
    c("CO2", "45"),
    c("CO2", "46"),
    c("N2", "28"),
    c("N2", "29")
  )
  # all ratios checked -> omitted (everything() is the default)
  expect_equal(selection_to_plot_args(all_masses, g, "29/28"), list())
  # no ratio checked -> explicit none, which is what stops the plot function from
  # falling back to everything() and showing ratios the user hid
  expect_equal(
    selection_to_plot_args(all_masses, g, character(0)),
    list(ratio = character(0))
  )
  # a checked ratio keeps its species shown even when all of its masses are
  # unchecked -- "just the ratio, no intensities" has to stay expressible
  ratio_only <- selection_to_plot_args(mass_sel(c("CO2", "44")), g, "29/28")
  expect_null(ratio_only$species) # N2 is still shown (via its ratio)
  expect_equal(ratio_only$mass, "44") # ... but none of its masses

  # nothing of N2 checked at all -> N2 drops out, and its ratio is never named:
  # naming a ratio that the species filter removed is an error in isoreader2
  g2 <- mass_groups(ratios = list("45/44", "29/28"))
  scoped <- selection_to_plot_args(
    mass_sel(c("CO2", "44"), c("CO2", "45"), c("CO2", "46")),
    g2,
    "45/44"
  )
  expect_equal(scoped$species, "CO2")
  expect_null(scoped$mass) # all CO2 masses kept
  expect_null(scoped$ratio) # all of CO2's ratios kept -> default
})

test_that("selection_is_empty spots a fully hidden selection", {
  # both explicitly none -> nothing to draw
  expect_true(selection_is_empty(list(
    mass = character(0),
    ratio = character(0)
  )))
  expect_false(selection_is_empty(list()))
  expect_false(selection_is_empty(list(mass = "44")))
  # no masses but a ratio still draws something ...
  expect_false(selection_is_empty(list(mass = character(0), ratio = "45/44")))
  # ... and so does an ABSENT ratio, which means "all of them" rather than none
  expect_false(selection_is_empty(list(mass = character(0))))

  # the pairing selection_is_empty() relies on: whenever nothing can be drawn,
  # selection_to_plot_args() emits BOTH arguments as an explicit none
  g <- mass_groups()
  # every mass AND every ratio un-checked
  expect_true(selection_is_empty(selection_to_plot_args(
    mass_sel(),
    g,
    character(0)
  )))
  # every mass un-checked and there are no ratios to fall back on
  no_ratios <- mass_groups(ratios = list(character(0), character(0)))
  expect_true(selection_is_empty(selection_to_plot_args(mass_sel(), no_ratios)))
  # but a still-checked ratio keeps it non-empty (that is the "ratios only" view)
  expect_false(selection_is_empty(selection_to_plot_args(
    mass_sel(),
    g,
    "29/28"
  )))
})

test_that("selection args render as NULL for a plot call and c() for code", {
  args <- list(species = "CO2", mass = character(0), ratio = "45/44")
  plot_args <- selection_args_for_plot(args)
  expect_named(plot_args, c("species", "mass", "ratio"))
  expect_null(plot_args$mass) # the element stays, its value becomes NULL
  expect_equal(plot_args$ratio, "45/44")

  code_args <- selection_args_for_code(args)
  expect_equal(code_value(code_args$mass), "c()")
  expect_equal(code_value(code_args$ratio), '"45/44"')
})

test_that("build_data_plot returns NULL when there is nothing to plot", {
  plot_fn <- function(dataset, ...) ggplot2::ggplot()
  agg <- list(traces = tibble::tibble(mass = c("44", "45"), x = 1:2))

  expect_null(build_data_plot(NULL, "traces", plot_fn))
  expect_null(build_data_plot(list(), "traces", plot_fn)) # missing key
  expect_null(build_data_plot(
    list(traces = agg$traces[0L, ]),
    "traces",
    plot_fn
  ))
  # everything de-selected -> empty plot instead of the plot function's error
  expect_null(build_data_plot(
    agg,
    "traces",
    plot_fn,
    selection_args = list(mass = character(0), ratio = character(0))
  ))
})

test_that("build_data_plot hands the whole dataset plus the selection to plot_fn", {
  captured <- NULL
  plot_fn <- function(dataset, scientific, ...) {
    captured <<- list(
      dataset = dataset,
      scientific = scientific,
      extra = list(...)
    )
    ggplot2::ggplot()
  }
  agg <- list(traces = tibble::tibble(mass = c("44", "45"), x = 1:2))

  p <- build_data_plot(
    agg,
    "traces",
    plot_fn,
    selection_args = list(mass = "44", ratio = character(0)),
    scientific = NULL, # coerced via isTRUE() -> FALSE
    legend_position = "hide",
    time_window = c(0, 10) # extra plot arg flows through ...
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$theme$legend.position, "none")
  # the data is NOT pre-filtered any more - isoreader2 does the sub-selecting
  expect_equal(captured$dataset$traces$mass, c("44", "45"))
  expect_equal(captured$extra$mass, "44")
  expect_null(captured$extra$ratio) # an explicit "none" is passed as NULL
  expect_false(captured$scientific)
  expect_equal(captured$extra$time_window, c(0, 10))
})

test_that("build_data_plot leaves color out unless it is given", {
  captured <- NULL
  plot_fn <- function(dataset, scientific, ...) {
    captured <<- list(names = names(list(...)), args = list(...))
    ggplot2::ggplot()
  }
  agg <- list(traces = tibble::tibble(mass = "44", x = 1))

  # no color aesthetic -> the argument is absent, so isoreader2 uses its own
  build_data_plot(
    agg,
    "traces",
    plot_fn,
    aes_args = list(facet = rlang::quo(file_name))
  )
  expect_false("color" %in% captured$names)

  # an explicit color is forwarded as a quosure
  build_data_plot(
    agg,
    "traces",
    plot_fn,
    aes_args = list(color = rlang::quo(trace))
  )
  expect_true("color" %in% captured$names)
})
