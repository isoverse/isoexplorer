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

# filter all datasets in agg_data to only the uidx values present in selected_metadata
# returns NULL if no rows are selected
filter_agg_data_by_metadata <- function(agg_data, selected_metadata) {
  if (is.null(selected_metadata) || nrow(selected_metadata) == 0) {
    return(NULL)
  }
  selected_uidx <- selected_metadata$uidx
  for (ds in c("metadata", "traces", "cycles", "scans")) {
    if (!is.null(agg_data[[ds]]) && "uidx" %in% names(agg_data[[ds]])) {
      agg_data[[ds]] <- dplyr::filter(
        agg_data[[ds]],
        .data$uidx %in% selected_uidx
      )
    }
  }
  agg_data
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

# group a dataset's masses by species for the species-button selection UI.
# returns a tibble with one row per species: `species` (chr, NA if the dataset
# has no species column) and `masses` (list of that species' mass values as
# character, sorted numerically). Species are ordered alphabetically.
species_mass_groups <- function(dataset) {
  m <- extract_masses(dataset)
  if (!"species" %in% names(m)) {
    m$species <- NA_character_
  }
  m |>
    dplyr::summarise(masses = list(as.character(.data$mass)), .by = "species") |>
    dplyr::arrange(.data$species)
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

# apply (or hide) the legend on a ggplot; NULL passes through unchanged
apply_legend_position <- function(plot, position = "right") {
  if (is.null(plot)) {
    return(plot)
  }
  if (identical(position, "hide")) {
    plot + ggplot2::theme(legend.position = "none")
  } else {
    plot + ggplot2::theme(legend.position = position)
  }
}

# shared cf/di/scans plot pipeline: from metadata-filtered agg_data, restrict the
# `dataset_key` table to the selected masses, plot it with `plot_fn`, and set the
# legend. Returns NULL when there is nothing to plot (caller substitutes an empty
# plot). Plot-specific extras (e.g. time_window, scan_type) pass through via `...`.
build_data_plot <- function(
  agg_data,
  dataset_key,
  selected_masses,
  plot_fn,
  font_size = 16,
  scientific = FALSE,
  legend_position = "right",
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

  agg_data[[dataset_key]] <- filter_by_selected_masses(dataset, selected_masses)
  if (nrow(agg_data[[dataset_key]]) == 0) {
    return(NULL)
  }

  plot <- plot_fn(
    agg_data,
    scientific = isTRUE(scientific),
    theme = isoreader2::ir_default_theme(text_size = font_size),
    ...
  )
  apply_legend_position(plot, legend_position)
}
