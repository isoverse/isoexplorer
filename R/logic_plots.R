# plot logic (pure functions, no shiny) =====
# shared, side-effect-free helpers behind the cf/di/scans plot modules.
# kept free of shiny so they can be unit-tested directly.

# empty placeholder plot shown when there is nothing to draw
make_empty_plot <- function(font_size = 16) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0,
      y = 0,
      label = "no data",
      vjust = 0.5,
      hjust = 0.5,
      size = font_size
    ) +
    ggplot2::theme_void()
}

# filter all datasets in agg_data to only the analyses present in
# selected_metadata. A file (uidx) can hold several analyses, so we match on
# (uidx, analysis) -- matching on uidx alone would pull in unselected analyses of
# a partially-selected file. Tables lacking an analysis column fall back to uidx.
# Returns NULL if no rows are selected.
filter_agg_data_by_metadata <- function(agg_data, selected_metadata) {
  if (is.null(selected_metadata) || nrow(selected_metadata) == 0) {
    return(NULL)
  }
  key_cols <- intersect(c("uidx", "analysis"), names(selected_metadata))
  keep <- dplyr::distinct(dplyr::select(
    selected_metadata,
    dplyr::all_of(key_cols)
  ))
  for (ds in c("metadata", "traces", "cycles", "scans")) {
    tbl <- agg_data[[ds]]
    if (is.null(tbl)) {
      next
    }
    join_cols <- intersect(key_cols, names(tbl))
    if (length(join_cols) > 0) {
      agg_data[[ds]] <- dplyr::semi_join(
        tbl,
        keep,
        by = join_cols
      )
    }
  }
  agg_data
}

# the selector-table row ids (its `row_id` id column) to select so the table
# visually reflects a file-server selection. NULL table data -> NULL (no table
# yet); NULL selection -> all rows (the "all" case); a 0-row selection -> none;
# otherwise the rows matching the selection on (uidx, analysis) -- same
# granularity as the data filtering, so a partial-file selection highlights
# exactly those analyses, not the whole file.
initial_selection_row_ids <- function(table_data, selection) {
  if (is.null(table_data)) {
    return(NULL)
  }
  if (is.null(selection)) {
    return(table_data$row_id)
  }
  # 0-row "none" selection (a bare tibble has no key columns) -> select nothing
  if (nrow(selection) == 0L) {
    return(table_data$row_id[0])
  }
  key_cols <- intersect(
    c("uidx", "analysis"),
    intersect(names(table_data), names(selection))
  )
  if (length(key_cols) == 0L) {
    return(table_data$row_id[0])
  }
  keep <- dplyr::distinct(dplyr::select(selection, dplyr::all_of(key_cols)))
  dplyr::semi_join(table_data, keep, by = key_cols)$row_id
}

# resolve an initial-selection filter (`filter_quo`, a tidy-eval quosure) against
# the aggregated metadata: NULL when every row matches ("all" -- the efficient
# default path the file server treats as "no filter"), a 0-row tibble for `FALSE`
# or an empty match ("none"), and otherwise the matching rows. NULL/empty metadata
# yields NULL. The caller (a reactive) wraps this to catch a filter that errors.
resolve_selection_filter <- function(metadata, filter_quo) {
  if (is.null(metadata) || nrow(metadata) == 0) {
    return(NULL)
  }
  rows <- dplyr::filter(metadata, !!filter_quo)
  if (nrow(rows) == nrow(metadata)) NULL else rows
}

# distinct mass (+ species) rows of a dataset, sorted by mass, with a unique
# mass_id column ("mass:species" when species is present, else "mass") that the
# mass selector table keys on
extract_masses <- function(dataset) {
  dataset |>
    dplyr::select(dplyr::any_of(c("mass", "species"))) |>
    dplyr::distinct() |>
    dplyr::arrange(as.numeric(.data$mass)) |>
    dplyr::mutate(
      mass_id = if ("species" %in% names(dataset)) {
        paste(.data$mass, .data$species, sep = ":")
      } else {
        as.character(.data$mass)
      },
      .before = 1L
    )
}

# distinct ratio names (+ species) of a dataset, grouped by species and sorted by
# numerator mass then name. `ratio_name`/`ratio` are added by
# isoreader2::ir_calculate_ratios(); base-mass rows carry NA and are dropped here.
# Returns a tibble with one row per species (`species`, NA if the dataset has no
# species column) and a `ratios` list-column. An empty 0-row tibble when the
# dataset has no `ratio_name` column or no (non-NA) ratios at all.
extract_ratio_groups <- function(dataset) {
  empty <- tibble::tibble(species = character(0), ratios = list())
  if (!"ratio_name" %in% names(dataset)) {
    return(empty)
  }
  rn <- dataset |>
    dplyr::select(dplyr::any_of(c("species", "ratio_name"))) |>
    dplyr::filter(!is.na(.data$ratio_name)) |>
    dplyr::distinct()
  if (nrow(rn) == 0L) {
    return(empty)
  }
  if (!"species" %in% names(rn)) {
    rn$species <- NA_character_
  }
  rn |>
    dplyr::mutate(
      .num = suppressWarnings(as.numeric(sub("/.*$", "", .data$ratio_name)))
    ) |>
    dplyr::arrange(.data$species, .data$.num, .data$ratio_name) |>
    dplyr::summarise(
      ratios = list(as.character(.data$ratio_name)),
      .by = "species"
    )
}

# group a dataset's masses by species for the species-button selection UI.
# returns a tibble with one row per species: `species` (chr, NA if the dataset
# has no species column), `masses` (list of that species' mass values as
# character, sorted numerically) and `ratios` (list of that species' available
# ratio names as character, sorted by numerator mass; empty when ratios have not
# been calculated). Species are ordered alphabetically.
species_mass_groups <- function(dataset) {
  m <- extract_masses(dataset)
  if (!"species" %in% names(m)) {
    m$species <- NA_character_
  }
  groups <- m |>
    dplyr::summarise(
      masses = list(as.character(.data$mass)),
      .by = "species"
    ) |>
    dplyr::arrange(.data$species)
  # attach the available ratio names per species (NA-species safe lookup)
  rg <- extract_ratio_groups(dataset)
  groups$ratios <- lapply(groups$species, function(sp) {
    idx <- if (is.na(sp)) {
      which(is.na(rg$species))
    } else {
      which(!is.na(rg$species) & rg$species == sp)
    }
    if (length(idx) == 0L) character(0) else as.character(rg$ratios[[idx[1]]])
  })
  groups
}

# Resolve the species/mass/ratio checkbox selection into the `species`, `mass`,
# and `ratio` arguments of the isoreader2 plot functions (pure).
#
# isoreader2 (>= 0.7.0) owns the sub-selection itself: `mass`/`ratio` default to
# `everything()`, take a character vector to keep exactly those, and take `c()`
# to keep none. So the app never pre-filters the data - it just names what the
# user checked and lets the plot function do the rest, which is also what keeps
# the trace/colour levels (and hence the legend) in isoreader2's hands.
#
# `selected` is the selected (species, mass) rows, `groups` is
# species_mass_groups() (one row per species with `masses` and `ratios`
# list-columns), `selected_ratios` are the checked ratio names.
#
# Returns a named list holding ONLY the arguments that differ from the defaults,
# where a `character(0)` value means an explicit "none" (`c()`):
#   * `species` - when only some of the species have anything checked. Emitting it
#     (rather than relying on `mass` alone) matters when two species share a mass,
#     e.g. N2 and CO both measured at 28/29/30.
#   * `mass` - when the checked masses are a strict subset, `character(0)` when no
#     mass at all is checked (ratios may still be).
#   * `ratio` - likewise, and always scoped to the species that are being shown,
#     since naming a ratio that the species filter removed is an error.
# Everything checked -> an empty list (all defaults).
selection_to_plot_args <- function(
  selected,
  groups,
  selected_ratios = character(0)
) {
  args <- list()
  if (is.null(groups) || nrow(groups) == 0L) {
    return(args)
  }
  all_species <- as.character(groups$species)
  masses_of <- function(sp) {
    i <- match(sp, all_species)
    if (is.na(i)) character(0) else as.character(groups$masses[[i]])
  }
  ratios_of <- function(sp) {
    i <- match(sp, all_species)
    if (is.na(i)) character(0) else as.character(groups$ratios[[i]])
  }

  # the checked masses per species, restricted to what is actually in the data:
  # a stale checkbox (e.g. right after a unit / scan-type change) must never name
  # a mass that is not there, which isoreader2 reports as an error
  sel_masses <- lapply(all_species, function(sp) {
    if (is.null(selected) || nrow(selected) == 0L) {
      return(character(0))
    }
    intersect(
      as.character(selected$mass[selected$species == sp]),
      masses_of(sp)
    )
  })
  names(sel_masses) <- all_species
  # ... and the checked ratios per species, same restriction
  sel_ratios <- lapply(all_species, function(sp) {
    intersect(as.character(selected_ratios), ratios_of(sp))
  })
  names(sel_ratios) <- all_species

  # a species is shown when anything of it is checked - a mass OR a ratio, so
  # "no masses, just the ratios" stays expressible
  shown <- all_species[
    lengths(sel_masses) > 0L | lengths(sel_ratios) > 0L
  ]
  if (length(shown) == 0L) {
    # nothing checked at all: no species filter (an empty one would error), just
    # an explicit "no masses and no ratios"
    return(list(mass = character(0), ratio = character(0)))
  }
  if (!setequal(shown, all_species)) {
    args$species <- sort(shown)
  }

  # masses: omitted when every shown species keeps all of them
  chosen_masses <- unique(unlist(sel_masses[shown], use.names = FALSE))
  all_shown_masses <- unique(unlist(
    lapply(shown, masses_of),
    use.names = FALSE
  ))
  if (!setequal(chosen_masses, all_shown_masses)) {
    args$mass <- sort(chosen_masses)
  }

  # ratios: only meaningful once ir_calculate_ratios() has run, and scoped to the
  # shown species so the selection can always be resolved
  available_ratios <- unique(unlist(
    lapply(shown, ratios_of),
    use.names = FALSE
  ))
  if (length(available_ratios) > 0L) {
    chosen_ratios <- unique(unlist(sel_ratios[shown], use.names = FALSE))
    if (!setequal(chosen_ratios, available_ratios)) {
      args$ratio <- chosen_ratios
    }
  }
  args
}

# does a selection_to_plot_args() result leave anything to draw? Only an explicit
# `mass = c()` AND an explicit `ratio = c()` hide everything - the plot functions
# treat that as an error, so the caller shows the empty plot instead.
#
# An ABSENT `ratio` means "all of them" (the everything() default), which still
# draws whenever ratios exist - so `mass = c()` on its own is the "ratios only"
# case, not an empty one. selection_to_plot_args() guarantees the pairing: when
# nothing at all is checked (or there are no ratios to fall back on) it emits both.
selection_is_empty <- function(args) {
  explicitly_none <- function(x) !is.null(args[[x]]) && length(args[[x]]) == 0L
  explicitly_none("mass") && explicitly_none("ratio")
}

# a selection_to_plot_args() result as arguments for an actual plot call: an
# explicit "none" (`character(0)`) is passed as NULL, which is what the plot
# functions read as "select nothing"
selection_args_for_plot <- function(args) {
  for (nm in names(args)) {
    if (length(args[[nm]]) == 0L) {
      args[nm] <- list(NULL)
    }
  }
  args
}

# the same result as arguments for generated code, where an explicit "none" reads
# better as `c()` than as `NULL`
selection_args_for_code <- function(args) {
  for (nm in names(args)) {
    if (length(args[[nm]]) == 0L) {
      args[[nm]] <- code_raw("c()")
    }
  }
  args
}

# the intensity-unit family an additive-offset pair belongs to, matching
# isoreader2::ir_calculate_ratios()'s `num_add.{V,nA,cps}` arguments: voltage
# (V/mV) -> "V", current (A/mA/\u00b5A/nA/pA/fA) -> "nA", counts (cps) -> "cps".
# Unknown units fall back to "V".
intensity_unit_family <- function(units) {
  if (is.null(units) || length(units) == 0) {
    return("V")
  }
  if (units %in% c("V", "mV")) {
    "V"
  } else if (units %in% c("A", "mA", "\u00b5A", "uA", "nA", "pA", "fA")) {
    "nA"
  } else if (units == "cps") {
    "cps"
  } else {
    "V"
  }
}

# the default additive offsets (numerator, denominator) for an intensity-unit
# family, matching ir_calculate_ratios()'s defaults: voltage 100/100, current 0/0,
# counts 0/0.
ratio_add_defaults <- function(family) {
  num <- if (identical(family, "V")) 100 else 0
  c(num = num, denom = num)
}

# resolve the Ratios-popover settings + current intensity units into the arguments
# for isoreader2::ir_calculate_ratios() (used both to compute ratios for the plot
# and to generate the code). `settings` is a list with `calculate` (bool),
# `normalize` (bool), and `num_add`/`denom_add` named lists keyed by family
# ("V"/"nA"/"cps"). Returns NULL when ratios are not to be calculated; otherwise a
# named list of ONLY the non-default arguments: `num_add.<fam>`/`denom_add.<fam>`
# (when changed from the family default) and `normalize_ratios = TRUE` (a marker,
# resolved to the `mean` function / `mean` code by the caller) when normalizing.
ratio_calc_params <- function(settings, units) {
  if (is.null(settings) || !isTRUE(settings$calculate)) {
    return(NULL)
  }
  fam <- intensity_unit_family(units)
  defaults <- ratio_add_defaults(fam)
  params <- list()
  num <- settings$num_add[[fam]]
  if (
    !is.null(num) &&
      !is.na(num) &&
      !identical(as.numeric(num), defaults[["num"]])
  ) {
    params[[paste0("num_add.", fam)]] <- as.numeric(num)
  }
  denom <- settings$denom_add[[fam]]
  if (
    !is.null(denom) &&
      !is.na(denom) &&
      !identical(as.numeric(denom), defaults[["denom"]])
  ) {
    params[[paste0("denom_add.", fam)]] <- as.numeric(denom)
  }
  if (isTRUE(settings$normalize)) {
    params$normalize_ratios <- TRUE
  }
  params
}

# apply (or hide) the legend on a ggplot; NULL passes through unchanged. A
# bottom/top legend is laid out vertically (legend.direction = "vertical").
apply_legend_position <- function(plot, position = "right") {
  if (is.null(plot)) {
    return(plot)
  }
  if (identical(position, "hide")) {
    plot + ggplot2::theme(legend.position = "none")
  } else if (position %in% c("bottom", "top")) {
    plot +
      ggplot2::theme(legend.position = position, legend.direction = "vertical")
  } else {
    plot + ggplot2::theme(legend.position = position)
  }
}

# shared cf/di/scans plot pipeline: plot the metadata-filtered agg_data with
# `plot_fn` and set the legend. Returns NULL when there is nothing to plot (caller
# substitutes an empty plot).
#
# The data is handed over WHOLE: the species/mass/ratio sub-selection travels as
# the plot function's own `species=`/`mass=`/`ratio=` arguments (`selection_args`,
# from selection_to_plot_args()), so isoreader2 does the filtering and keeps
# ownership of the trace/colour levels and the legend.
#
# `aes_args` is a named list of QUOSURES for the tidy-eval aesthetics
# (facet/color/linetype) -- they are injected so the columns are evaluated as
# variables, not strings; leave `color` out of it to keep isoreader2's default
# colour grouping. Other plot-specific extras (time_window, scan_type, scales,
# ...) pass through via `...` as plain values.
build_data_plot <- function(
  agg_data,
  dataset_key,
  plot_fn,
  selection_args = list(),
  font_size = 16,
  scientific = FALSE,
  legend_position = "right",
  aes_args = list(),
  ...
) {
  if (is.null(agg_data)) {
    return(NULL)
  }
  dataset <- agg_data[[dataset_key]]
  if (is.null(dataset) || nrow(dataset) == 0) {
    return(NULL)
  }
  # everything de-selected -> nothing to draw (the plot functions would error)
  if (selection_is_empty(selection_args)) {
    return(NULL)
  }

  # inject the aesthetic quosures alongside the plain-value args
  call_args <- c(
    list(agg_data, scientific = isTRUE(scientific)),
    selection_args_for_plot(selection_args),
    aes_args,
    list(...)
  )
  plot <- rlang::inject(plot_fn(!!!call_args)) +
    ggplot2::theme(text = ggplot2::element_text(size = font_size))
  apply_legend_position(plot, legend_position)
}
