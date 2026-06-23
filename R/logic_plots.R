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

# filter a dataset tibble to only the mass (+ species) rows the user selected
# selected_items is the get_selected_items() result from the mass selector table
filter_by_selected_masses <- function(df, selected_items) {
  join_cols <- intersect(c("mass", "species"), names(df))
  join_cols <- intersect(join_cols, names(selected_items))
  if (length(join_cols) == 0 || nrow(selected_items) == 0) {
    return(df[0L, ])
  }
  dplyr::inner_join(
    df,
    dplyr::select(selected_items, dplyr::all_of(join_cols)),
    by = join_cols
  )
}

# decide the ir_plot_*() species/mass argument for the current selection (pure):
# `selected` is the selected (species, mass) rows, `groups` is species_mass_groups()
# (one row per species with a list-column `masses`). Returns list(species=) when
# whole species were dropped but every kept species keeps all its masses,
# list(mass=) when masses were narrowed within a species, or list() when
# everything (or nothing) is selected -- so the plot/code can splice it in.
select_species_or_mass <- function(selected, groups) {
  if (is.null(selected) || nrow(selected) == 0L || is.null(groups)) {
    return(list())
  }
  all_species <- as.character(groups$species)
  sel_species <- unique(as.character(selected$species))
  species_level <- all(vapply(
    sel_species,
    function(sp) {
      i <- match(sp, groups$species)
      setequal(
        as.character(selected$mass[selected$species == sp]),
        as.character(groups$masses[[i]])
      )
    },
    logical(1)
  ))
  all_masses <- unique(as.character(unlist(groups$masses)))
  chosen_masses <- unique(as.character(selected$mass))
  if (species_level && !setequal(sel_species, all_species)) {
    list(species = sort(sel_species))
  } else if (!setequal(chosen_masses, all_masses)) {
    list(mass = sort(chosen_masses))
  } else {
    list()
  }
}

# whether an aggregated-data object carries any (non-NA) ratios, i.e. whether
# isoreader2::ir_calculate_ratios() has produced ratio names in any of its
# traces/cycles/scans tables. Used to decide whether to offer ratio selection and
# whether to emit ir_calculate_ratios() in the generated code.
agg_has_ratios <- function(agg) {
  if (is.null(agg)) {
    return(FALSE)
  }
  any(vapply(
    c("traces", "cycles", "scans"),
    function(ds) {
      tbl <- agg[[ds]]
      !is.null(tbl) &&
        "ratio_name" %in% names(tbl) &&
        any(!is.na(tbl$ratio_name))
    },
    logical(1)
  ))
}

# the subset of `selected_ratios` (ratio names like "45/44") that can actually be
# plotted from `filtered_dataset` -- i.e. those whose numerator-mass rows survived
# the mass selection (a ratio lives on its numerator mass row). Keeps the plot/code
# `ratio=` argument from naming ratios with no data (which the plotting functions
# treat as an error). Returns selection order, de-duplicated.
plottable_ratios <- function(filtered_dataset, selected_ratios) {
  selected_ratios <- as.character(selected_ratios)
  if (
    length(selected_ratios) == 0L ||
      is.null(filtered_dataset) ||
      !"ratio_name" %in% names(filtered_dataset)
  ) {
    return(character(0))
  }
  rn <- as.character(filtered_dataset$ratio_name)
  present <- unique(rn[!is.na(rn)])
  intersect(selected_ratios, present)
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

# shared cf/di/scans plot pipeline: from metadata-filtered agg_data, restrict the
# `dataset_key` table to the selected masses, plot it with `plot_fn`, and set the
# legend. Returns NULL when there is nothing to plot (caller substitutes an empty
# plot). `aes_args` is a named list of QUOSURES for the tidy-eval aesthetics
# (facet/color/linetype) -- they are injected so the columns are evaluated as
# variables, not strings. `selected_ratios` are the ratio names (e.g. "45/44") to
# add as ratio traces via the plot function's `ratio=` argument (restricted with
# plottable_ratios() to those with surviving data). Other plot-specific extras
# (time_window, scan_type, scales, ...) pass through via `...` as plain values.
build_data_plot <- function(
  agg_data,
  dataset_key,
  selected_masses,
  plot_fn,
  font_size = 16,
  scientific = FALSE,
  legend_position = "right",
  aes_args = list(),
  selected_ratios = character(0),
  ...
) {
  if (is.null(agg_data)) {
    return(NULL)
  }
  dataset <- agg_data[[dataset_key]]
  if (
    is.null(dataset) ||
      nrow(dataset) == 0 ||
      is.null(selected_masses) ||
      nrow(selected_masses) == 0
  ) {
    return(NULL)
  }

  filtered <- filter_by_selected_masses(dataset, selected_masses)
  if (nrow(filtered) == 0) {
    return(NULL)
  }
  agg_data[[dataset_key]] <- filtered

  # ratios are added as extra traces by the plot function via `ratio=`; only
  # request those whose numerator-mass rows survived the mass selection so the
  # call never errors on a ratio with no data
  ratio_arg <- plottable_ratios(filtered, selected_ratios)

  # inject the aesthetic quosures alongside the plain-value args
  call_args <- c(
    list(agg_data, scientific = isTRUE(scientific)),
    if (length(ratio_arg) > 0) list(ratio = ratio_arg) else list(),
    aes_args,
    list(...)
  )
  plot <- rlang::inject(plot_fn(!!!call_args)) +
    ggplot2::theme(text = ggplot2::element_text(size = font_size))
  apply_legend_position(plot, legend_position)
}
