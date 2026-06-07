# app utility functions =====

# inject CSS/JS required by the app
use_app_utils <- function() {
  tagList(
    shinyjs::useShinyjs(),
    # adopt error color of the theme and make error validation larger
    tags$style(HTML(
      ".shiny-output-error-validation {
        color: var(--bs-danger) !important;
        font-size: 1.5rem;
      }"
    )) |>
      singleton(),
    # support ansi color codes from cli
    tags$style(HTML(paste(format(cli::ansi_html_style()), collapse = "\n"))) |>
      singleton(),
    # make card headings taller
    tags$style(HTML(".card-header { padding: 1rem 1.25rem;}")) |> singleton(),
    # preserve spacing of CLI errors that might make it to the GUI
    tags$style(HTML(
      ".cli-inline-error {
        display: inline;
        white-space: pre-wrap;
      }"
    )) |>
      singleton()
  )
}

# logging =====

log_any <- function(
  msg,
  log_fun,
  ns = NULL,
  toaster = NULL,
  position = "bottom-left",
  ...
) {
  ns_prefix <- if (!is.null(ns)) paste0("[", ns(NULL), "] ") else ""
  if (!is.null(toaster)) {
    log_fun(paste0(ns_prefix, msg, " [GUI msg: '", toaster, "']", collapse = ""))
    bslib::toast(
      HTML(cli::ansi_html(toaster)),
      position = position,
      ...
    ) |>
      bslib::show_toast()
  } else {
    log_fun(paste0(ns_prefix, msg, collapse = ""))
  }
}

# calls log_warning and log_error for any encountered conditions
log_cnds <- function(
  cnds = tibble(),
  ns = NULL,
  user_msg = NULL,
  .call = caller_call()
) {
  if (!is.data.frame(cnds) && is.data.frame(cnds$conditions)) {
    cnds <- cnds$conditions
  }
  if (nrow(cnds) == 0) {
    return(invisible(NULL))
  }

  call <- as.character(.call[1]) |>
    stringr::str_remove(stringr::fixed("<reactive:")) |>
    stringr::str_remove(stringr::fixed(">"))
  if (is_empty(call)) {
    call <- "unknown"
  }

  warnings <- cnds |> filter(type == "warning")
  if (nrow(warnings) > 0) {
    for (message in warnings$message) {
      log_warning(
        ns = ns,
        user_msg = if (!is.null(user_msg)) {
          user_msg
        } else {
          format_inline("Warning in {call}()")
        },
        warning = message
      )
    }
  }

  errors <- cnds |> filter(type == "error")
  if (nrow(errors) > 0) {
    log_error(
      ns = ns,
      user_msg = if (!is.null(user_msg)) {
        user_msg
      } else {
        format_inline("{qty(nrow(errors))}Error{?s} in {call}()")
      },
      error = errors$condition |>
        purrr::map_chr(~ format(.x) |> paste(collapse = "\n"))
    )
  }
}

log_error <- function(..., ns = NULL, user_msg = NULL, error = NULL) {
  error_msg <-
    if (!is.null(error)) {
      gsub("\\n", "<br>", cli::ansi_html(error)) |> paste(collapse = "<br>")
    } else {
      ""
    }

  issue_title <- sprintf(
    "Version %s: %s",
    if (getPackageName() != ".GlobalEnv") {
      packageVersion(getPackageName())
    } else {
      "app"
    },
    user_msg
  )

  issue_body <- sprintf(
    "Please describe here what you were attempting to do in the app when this issue occurred.\n\n## Trace (do NOT delete)\n\n<pre>%s</pre>",
    error_msg
  )

  issue_url <- sprintf(
    "https://github.com/KopfLab/isoexplorer/issues/new?title=%s&body=%s",
    URLencode(issue_title, reserved = TRUE),
    URLencode(HTML(issue_body), reserved = TRUE)
  )

  error_screen <- modalDialog(
    title = span(
      style = "color: red;",
      h2(user_msg, style = "color: red;"),
      h4(
        "Please try again. If the issue persists, please",
        tags$a("report this error", href = issue_url, target = "_blank")
      )
    ),
    if (nchar(error_msg) > 0) pre(HTML(error_msg))
  )

  log_any(
    msg = paste0(
      ...,
      if (!is.null(error)) paste0("Encountered error:\n", error, "\n"),
      collapse = ""
    ),
    ns = ns,
    log_fun = rlog::log_error,
    toaster = user_msg,
    header = "Encountered error",
    type = "danger",
    duration_s = 10
  )

  if (!is.null(error)) {
    showModal(error_screen)
  }
}

log_warning <- function(..., ns = NULL, user_msg = NULL, warning = NULL) {
  msg <- paste0(..., collapse = "")
  if (!nzchar(msg) && !is.null(user_msg)) {
    msg <- user_msg
  }
  log_any(
    msg = msg,
    ns = ns,
    log_fun = rlog::log_warn,
    toaster = if (!is.null(warning)) warning else user_msg,
    header = if (!is.null(warning)) HTML(cli::ansi_html(user_msg)) else NULL,
    type = "warning",
    duration_s = 5
  )
}

log_info <- function(..., ns = NULL, user_msg = NULL) {
  msg <- paste0(..., collapse = "")
  if (!nzchar(msg) && !is.null(user_msg)) {
    msg <- user_msg
  }
  log_any(
    msg = msg,
    ns = ns,
    log_fun = rlog::log_info,
    toaster = user_msg,
    type = "info",
    duration_s = 2
  )
}

log_success <- function(..., ns = NULL, user_msg = NULL) {
  msg <- paste0(..., collapse = "")
  if (!nzchar(msg) && !is.null(user_msg)) {
    msg <- user_msg
  }
  log_any(
    msg = msg,
    ns = ns,
    log_fun = rlog::log_info,
    toaster = user_msg,
    type = "success",
    duration_s = 2
  )
}

log_debug <- function(..., ns = NULL) {
  log_any(msg = paste0(..., collapse = ""), ns = ns, log_fun = rlog::log_debug)
}

# plot helpers =====

get_empty_plot_in_app <- function(font_size = 16) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text", x = 0, y = 0, label = "no data",
      vjust = 0.5, hjust = 0.5, size = font_size
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
      agg_data[[ds]] <- dplyr::filter(agg_data[[ds]], .data$uidx %in% selected_uidx)
    }
  }
  agg_data
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

# ui helpers =====

# convenience function for adding non-breaking spaces
spaces <- function(n = 1) {
  htmltools::HTML(rep("&nbsp;", n))
}

# wrap content in an inline div
inline <- function(...) {
  htmltools::div(style = "display: inline-block;", ...)
}

# add a bslib tooltip to a widget
add_tooltip <- function(widget, ...) {
  bslib::tooltip(widget, ...)
}
