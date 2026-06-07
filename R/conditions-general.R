# general condition handling (any package) -----------

# Try/catch for executing functions that should not disrupt overall flow
# (conceptually similar to purrr::safely but has more detailed conditions summary)
# @param error_value which value to return if an error is caught
# @param catch_errors whether to catch errors (vs. throwing them)
# @param catch_warnings whether to catch warnings (vs. throwing them)
# @param truncate_call_stack whether to omit the try_catch_cnds calls from the resulting call stack in errors
# @param truncate_shiny_call_stack whether to omit everything but the last shiny::..stacktraceon.. from the call stack?
# @param call caller env - only relevant if re-throwing the error (catch_errors = FALSE)
# @return list with result and conditions, use show_cnds(out$conditions) to show conditions if any were caught
try_catch_cnds <- function(
  expr,
  error_value = NULL,
  catch_errors = TRUE,
  catch_warnings = TRUE,
  truncate_call_stack = TRUE,
  truncate_shiny_call_stack = TRUE,
  augment_errors_to_rlang = TRUE,
  call = caller_call()
) {
  conds <- tibble::tibble(type = character(0), condition = list())

  handle_warning <- function(cnd) {
    conds <<- conds |>
      dplyr::bind_rows(tibble::tibble(type = "warning", condition = list(cnd)))
    cnd_muffle(cnd)
  }

  handle_error <- function(cnd) {
    if (!catch_errors) {
      abort(message = "", parent = cnd, call = call, trace = cnd$trace)
    }

    if (truncate_call_stack) {
      cnd <- cnd |> truncate_call_stack()
    }

    if (truncate_shiny_call_stack) {
      cnd <- cnd |> truncate_shiny_call_stack()
    }

    conds <<- conds |>
      dplyr::bind_rows(tibble::tibble(type = "error", condition = list(cnd)))
    return(error_value)
  }

  augment_non_rlang_error <- function(cnd) {
    if (is(cnd, "rlang_error")) {
      cnd_signal(cnd)
    }

    if (!augment_errors_to_rlang) {
      stop(cnd)
    }

    abort(
      message = conditionMessage(cnd),
      call = conditionCall(cnd),
      trace = trace_back()
    )
  }

  if (catch_warnings) {
    result <- tryCatch(
      error = handle_error,
      withCallingHandlers(
        expr,
        warning = handle_warning,
        error = augment_non_rlang_error
      )
    )
  } else {
    result <- tryCatch(
      error = handle_error,
      withCallingHandlers(expr, error = augment_non_rlang_error)
    )
  }

  conds <- conds |>
    dplyr::mutate(
      call = .data$condition |>
        purrr::map_chr(~ as.character(conditionCall(.x))[1]),
      message = .data$condition |> purrr::map_chr(condition_cnd_message),
      .before = "condition"
    )

  return(list(result = result, conditions = conds))
}

# helper function to truncate call stacks (removing the try_catch_cnds wrapper)
truncate_call_stack <- function(cnd, recursive = TRUE) {
  if (!is.null(cnd$parent)) {
    cnd$parent <- cnd$parent |> truncate_call_stack()
  }
  if (is.null(cnd$trace)) {
    return(cnd)
  }
  is_helper_start <- cnd$trace$call |>
    sapply(function(x) as.character(x)[1] == "try_catch_cnds")
  is_helper_end <- cnd$trace$call |>
    sapply(function(x) as.character(x)[1] == "withCallingHandlers")
  if (any(is_helper_start) && any(is_helper_end[cumsum(is_helper_start) > 0])) {
    total_shift <- 0
    for (first_call in which(is_helper_start)) {
      last_call <- first_call +
        which(is_helper_end[seq_along(is_helper_end) > first_call])[1]
      if (last_call > first_call) {
        shift <- last_call - first_call + 1L
        cnd$trace <-
          cnd$trace |>
          dplyr::mutate(
            parent = ifelse(
              .data$parent > last_call - total_shift,
              .data$parent - shift,
              .data$parent
            )
          ) |>
          dplyr::filter(
            dplyr::row_number() < first_call - total_shift |
              dplyr::row_number() > last_call - total_shift
          )

        total_shift <- total_shift + shift
      }
    }
  }
  return(cnd)
}

# helper function to truncate shiny call stack
truncate_shiny_call_stack <- function(cnd, recursive = TRUE) {
  if (!is.null(cnd$parent)) {
    cnd$parent <- cnd$parent |> truncate_shiny_call_stack()
  }
  if (is.null(cnd$trace)) {
    return(cnd)
  }
  is_shiny_stacktrace_start <- cnd$trace$call |>
    sapply(function(x) as.character(x)[1] == "..stacktraceon..")
  if (any(is_shiny_stacktrace_start)) {
    last_call <- max(which(is_shiny_stacktrace_start))
    cnd$trace <-
      cnd$trace |>
      dplyr::mutate(
        parent = case_when(
          dplyr::row_number() == last_call + 1L ~ 0L,
          .data$parent > last_call ~ .data$parent - last_call,
          TRUE ~ .data$parent
        )
      ) |>
      dplyr::filter(dplyr::row_number() > last_call)
  }
  return(cnd)
}

# condition cnd message to preserve intended linebreaks and avoid introduced linebreaks
condition_cnd_message <- function(cnd) {
  lines <- strsplit(cnd$message, "\n", fixed = TRUE)[[1]] |>
    purrr::map_chr(
      ~ {
        cnd$message <- .x
        conditionMessage(cnd) |>
          gsub(pattern = "\n", replacement = " ", fixed = TRUE)
      }
    )
  lines |> paste(collapse = "\n")
}

# summarize cnds, i.e. how many issues/errors in cli format
summarize_cnds <- function(
  conditions,
  message = NULL,
  include_symbol = TRUE,
  include_call = TRUE,
  call_format = "in {.strong {call}()}: ",
  summary_format = "{issues} {message}",
  indent = 0,
  .call = caller_call()
) {
  call <- as.character(.call[1])
  if (is_empty(call)) {
    include_call <- FALSE
  }

  issues <- c()
  if (nrow(conditions) == 0) {
    issues <- format_inline("{cli::col_green('no issues')}")
  }
  if ((n <- sum(conditions$type == 'warning')) > 0) {
    issues <- format_inline(
      "{cli::col_yellow(format_inline('{n} warning{?s}'))}"
    )
  }
  if ((n <- sum(conditions$type == 'error')) > 0) {
    issues <- c(
      issues,
      format_inline("{cli::col_red(format_inline('{n} error{?s}'))}")
    )
  }

  summary <- format_inline(summary_format)
  if (include_call) {
    summary <- paste0(format_inline(call_format), summary)
  }
  if (include_symbol) {
    symbol <-
      if (nrow(conditions) == 0L) {
        "v"
      } else if (any(conditions$type == "error")) {
        "x"
      } else {
        "!"
      }
    summary <- set_names(summary, symbol)
  }

  summary <-
    withr::with_options(
      list(cli.width = console_width() - indent),
      summary |>
        gsub(pattern = "<", replacement = "<<", fixed = TRUE) |>
        gsub(pattern = ">", replacement = ">>", fixed = TRUE) |>
        format_bullets_raw()
    )
  return(summary)
}

# helper to cli format conditions into a bullet list
format_cnds <- function(
  conditions,
  include_symbol = TRUE,
  include_call = TRUE,
  prefix = "",
  call_format = "in {.strong {call}()}: ",
  indent = 0
) {
  if (nrow(conditions) == 0L) {
    return(c())
  }
  out <- conditions |>
    dplyr::mutate(
      symbol = ifelse(
        .data$type == "error",
        format_inline("{col_red(cli::symbol$cross)} "),
        format_inline("{col_yellow('!')} ")
      ),
      call_label = .data$call |>
        purrr::map_chr(
          ~ {
            if (!is.na(.x)) {
              call <- .x
              format_inline(call_format)
            } else {
              ""
            }
          }
        ),
      message_w_type = strsplit(.data$message, "\n", fixed = TRUE) |>
        list(.data$symbol, .data$call_label) |>
        purrr::pmap(
          function(msg, symbol, call) {
            c(
              paste0(
                !!prefix,
                if (!!include_symbol) symbol,
                if (!!include_call) call,
                utils::head(msg, 1)
              ),
              utils::tail(msg, -1)
            )
          }
        )
    ) |>
    dplyr::pull(.data$message_w_type) |>
    unlist()

  out <-
    withr::with_options(
      list(cli.width = console_width() - indent * 2),
      out |>
        gsub(pattern = "<", replacement = "<<", fixed = TRUE) |>
        gsub(pattern = ">", replacement = ">>", fixed = TRUE) |>
        format_bullets_raw()
    )

  if (indent > 0) {
    out <-
      paste0(rep(" ", (indent - 1) * 2) |> paste(collapse = ""), out) |>
      set_names(" ")
  }

  return(out)
}

# Summarizes and formats cnds
summarize_and_format_cnds <- function(
  conditions,
  include_symbol = TRUE,
  include_summary = TRUE,
  include_call = include_summary,
  summary_format = "{message} encountered {issues}",
  summary_indent = 0,
  message = NULL,
  include_cnds = TRUE,
  include_cnd_calls = TRUE,
  indent_cnds = include_summary,
  collapse_single_line_cnd = TRUE,
  .call = caller_call()
) {
  force(.call)
  if (missing(conditions) || !is.data.frame(conditions)) {
    cli_abort("{.var conditions} must be provided as a data frame")
  }

  summary_line <- NULL
  if (include_summary) {
    summary_line <- summarize_cnds(
      conditions,
      message = message,
      include_symbol = include_symbol,
      include_call = include_call,
      summary_format = summary_format,
      indent = summary_indent,
      .call = .call,
    )
  }

  formatted_cnds <- c()
  if (include_cnds) {
    formatted_cnds <- conditions |>
      format_cnds(
        include_call = include_cnd_calls,
        indent = indent_cnds,
        prefix = if (indent_cnds) {
          format_inline("{cli::symbol$arrow_right} ")
        } else {
          ""
        }
      )
    if (
      collapse_single_line_cnd && include_summary && length(formatted_cnds) == 1
    ) {
      formatted_cnds <- c()
      summary_line <-
        c(
          utils::head(summary_line, -1),
          paste(
            utils::tail(summary_line, 1),
            format_inline("{cli::symbol$arrow_right}"),
            format_cnds(
              conditions,
              include_call = include_cnd_calls,
              include_symbol = FALSE,
              call_format = "{.strong {call}()}: ",
            ) |>
              paste(collapse = " ")
          )
        )
    }
  }
  return(c(summary_line, formatted_cnds))
}

# Prints out the caught conditions in a cli_bullets list
show_cnds <- function(
  conditions,
  include_symbol = TRUE,
  include_summary = TRUE,
  include_call = include_summary,
  summary_format = "{message} encountered {issues}",
  message = NULL,
  include_cnds = TRUE,
  include_cnd_calls = TRUE,
  indent_cnds = include_summary,
  collapse_single_line_cnd = FALSE,
  .call = caller_call()
) {
  if (!is.data.frame(conditions) && is.data.frame(conditions$conditions)) {
    conditions <- conditions$conditions
  }

  if (nrow(conditions) > 0) {
    output <-
      summarize_and_format_cnds(
        conditions,
        include_symbol = include_symbol,
        include_summary = include_summary,
        include_call = include_call,
        summary_format = summary_format,
        message = message,
        include_cnds = include_cnds,
        include_cnd_calls = include_cnd_calls,
        collapse_single_line_cnd = collapse_single_line_cnd,
        .call = .call
      )
    if (is_interactive()) {
      cli_bullets(output)
    } else {
      cli(cli_bullets(output))
    }
  }
}

# Aborts if there are any conditions (for both warnings and errors)
abort_cnds <- function(
  conditions,
  include_symbol = FALSE,
  include_summary = TRUE,
  include_call = FALSE,
  summary_format = "{message} encountered {issues}",
  summary_indent = 5,
  message = NULL,
  include_cnds = TRUE,
  include_cnd_calls = TRUE,
  indent_cnds = include_summary,
  collapse_single_line_cnd = FALSE,
  .call = caller_call(),
  .env = caller_env()
) {
  if (!is.data.frame(conditions) && is.data.frame(conditions$conditions)) {
    conditions <- conditions$conditions
  }

  if (nrow(conditions) > 0) {
    summarize_and_format_cnds(
      conditions,
      include_symbol = include_symbol,
      include_summary = include_summary,
      include_call = include_call,
      summary_format = summary_format,
      summary_indent = summary_indent,
      message = message,
      include_cnds = include_cnds,
      include_cnd_calls = include_cnd_calls,
      collapse_single_line_cnd = collapse_single_line_cnd,
      .call = .call
    ) |>
      cli_abort(
        call = .call,
        trace = trace_back(bottom = .env)
      )
  }
}

warn_cnds <- function(
  conditions,
  include_cnd_symbols = TRUE,
  include_cnd_calls = TRUE
) {
  if (!is.data.frame(conditions) && is.data.frame(conditions$conditions)) {
    conditions <- conditions$conditions
  }

  if (nrow(conditions) > 0L) {
    1:nrow(conditions) |>
      purrr::walk(
        ~ format_cnds(
          conditions[.x, ],
          include_symbol = include_cnd_symbols,
          include_call = include_cnd_calls
        ) |>
          cli_warn()
      )
  }
}
