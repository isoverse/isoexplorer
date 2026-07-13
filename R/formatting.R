# pretty formatting ===========

# convert seconds to pretty time
secs_to_text <- function(secs) {
  stopifnot(is.numeric(secs))
  tibble(
    idx = seq_along(secs),
    d = floor(secs / 86400),
    h = floor((secs / 3600) %% 24),
    m = floor((secs / 60) %% 60),
    s = round(secs %% 60, 1),
    ms = round((secs * 1000L) %% 1000),
    small = secs < 1
  ) |>
    tidyr::pivot_longer(cols = -c("idx", "small")) |>
    dplyr::filter(
      is.na(.data$value) |
        (.data$small & .data$name == "ms") |
        (!.data$small & .data$value > 0 & .data$name != "ms")
    ) |>
    dplyr::mutate(
      label = dplyr::if_else(
        !is.na(.data$value),
        paste0(.data$value, .data$name),
        NA_character_
      )
    ) |>
    dplyr::summarise(
      .by = "idx",
      out = dplyr::if_else(
        all(is.na(.data$label)),
        NA_character_,
        paste(.data$label[!is.na(.data$label)], collapse = " ")
      )
    ) |>
    dplyr::pull(.data$out)
}
