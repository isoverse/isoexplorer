# Pure helpers for the code server (no shiny / reactivity) =====
# These assemble the "idealized example code" document from the per-module code
# snippets. The code server (app_module_code.R) supplies the snippets (by calling
# each module's get_code()) and renders the result; everything here is pure and
# unit-tested in test-logic_code.R.

# Build the dependency tree of registered code generators.
#
# `specs` is a named list (names = code ids) of `list(heading=, depends_on=)`.
# Returns the nodes in depth-first order, each a `list(id, heading, depends_on,
# depth)`. A node is a root (depth 1) when its `depends_on` is NULL or names an
# id that was never registered; a child's depth is its parent's depth + 1. Roots
# and siblings keep their registration order. (Single-parent tree; a cycle would
# be a registration error -- guarded against infinite recursion via `seen`.)
build_code_tree <- function(specs) {
  ids <- names(specs)
  if (length(ids) == 0) {
    return(list())
  }
  dep <- vapply(
    ids,
    function(i) specs[[i]]$depends_on %||% NA_character_,
    character(1)
  )
  is_root <- is.na(dep) | !(dep %in% ids)

  nodes <- list()
  seen <- character(0)
  visit <- function(id, depth) {
    if (id %in% seen) {
      return(invisible(NULL)) # cycle guard
    }
    seen <<- c(seen, id)
    nodes[[length(nodes) + 1L]] <<- list(
      id = id,
      heading = specs[[id]]$heading,
      depends_on = if (is.na(dep[[id]])) NULL else dep[[id]],
      depth = depth
    )
    # children in registration order
    for (child in ids[!is_root & dep == id]) {
      visit(child, depth + 1L)
    }
  }
  for (root in ids[is_root]) {
    visit(root, 1L)
  }
  nodes
}

# Render the assembled document from ordered sections.
#
# `sections` is a list of `list(depth, heading, code)` (depth-first, as produced
# by walking build_code_tree() and calling each get_code()). Returns
# `list(script = <single string>, headings = <tibble id/level/text/line>)` where
# `line` is the 1-based line number of each heading in `script` (for the viewer's
# jump-to-heading nav). Headings have a `#` count equal to the tree depth (`#`,
# `##`, `###`, ...) -- R comments in plain-code mode, markdown headers in Quarto
# mode. With `quarto = TRUE` each code block is wrapped in a ```{r} chunk;
# `front_matter` (defaults to `quarto`) additionally prepends the YAML header --
# the viewer omits it, the download keeps it. Heading line numbers account for both.
render_code_document <- function(sections, quarto = FALSE, front_matter = quarto) {
  empty_headings <- tibble::tibble(
    level = integer(0),
    text = character(0),
    line = integer(0)
  )
  if (length(sections) == 0) {
    front <- if (front_matter) paste(quarto_front_matter(), collapse = "\n") else ""
    return(list(script = front, headings = empty_headings))
  }
  # one block of lines per section (heading, blank, code [in a chunk if quarto])
  blocks <- lapply(sections, function(s) {
    heading_line <- paste0(strrep("#", s$depth), " ", s$heading)
    code_lines <- strsplit(s$code %||% "", "\n", fixed = TRUE)[[1]]
    if (quarto) {
      c(heading_line, "", "```{r}", code_lines, "```")
    } else {
      c(heading_line, "", code_lines)
    }
  })

  # Quarto documents open with a YAML front matter (so the heading line numbers,
  # tracked below, are offset by it)
  lines <- if (front_matter) c(quarto_front_matter(), "") else character(0)
  headings <- vector("list", length(blocks))
  for (i in seq_along(blocks)) {
    if (i > 1L) {
      lines <- c(lines, "") # one blank line between sections
    }
    headings[[i]] <- tibble::tibble(
      level = sections[[i]]$depth,
      text = sections[[i]]$heading,
      line = length(lines) + 1L # heading is the first line of the block
    )
    lines <- c(lines, blocks[[i]])
  }
  list(
    script = paste(lines, collapse = "\n"),
    headings = dplyr::bind_rows(headings)
  )
}

# YAML front matter for the Quarto (.qmd) document
quarto_front_matter <- function() {
  c("---", 'title: "isoexplorer generated code"', "format: html", "---")
}

# code-generation mini-DSL (adapted/trimmed from isoviewer's code.R) =====

# mark a string as raw code (a symbol or expression) so code_value() emits it
# verbatim instead of quoting it -- e.g. code_raw("ir_default_theme(text_size = 14)")
code_raw <- function(x) {
  structure(x, class = "code_raw")
}

# format a single R value as code: strings quoted, vectors wrapped in c(), etc.
code_value <- function(value) {
  if (inherits(value, "code_raw")) {
    return(unclass(value))
  }
  fmt <- function(x) {
    if (is.character(x)) {
      sprintf('"%s"', x)
    } else if (is.logical(x)) {
      ifelse(x, "TRUE", "FALSE")
    } else {
      # plain decimal text (no width padding, no scientific notation) for the
      # numeric values the app produces (units exponents, font sizes, zoom bounds)
      as.character(x)
    }
  }
  if (length(value) == 1L) {
    fmt(value)
  } else {
    sprintf("c(%s)", paste(fmt(value), collapse = ", "))
  }
}

# format one argument: `value` or `name = value` (unnamed when name is NULL/"")
code_arg <- function(name, value) {
  v <- code_value(value)
  if (is.null(name) || !nzchar(name)) v else paste0(name, " = ", v)
}

# a function call `fn(a, name = b, ...)` from a (optionally named) list of args;
# breaks onto multiple lines (one arg per line, indented) when it gets long
code_call <- function(fn, args = list()) {
  if (length(args) == 0L) {
    return(paste0(fn, "()"))
  }
  nms <- names(args) %||% rep("", length(args))
  parts <- vapply(
    seq_along(args),
    function(i) code_arg(nms[[i]], args[[i]]),
    character(1)
  )
  one_line <- paste0(fn, "(", paste(parts, collapse = ", "), ")")
  if (nchar(one_line) <= 72L && !any(grepl("\n", parts, fixed = TRUE))) {
    one_line
  } else {
    paste0(fn, "(\n  ", paste(parts, collapse = ",\n  "), "\n)")
  }
}

# `var <- rhs`
code_assign <- function(var, rhs) {
  paste0(var, " <- ", rhs)
}

# the bare-column code for a plot aesthetic (facet/color/linetype): the column
# name (backticked when not a syntactic name), wrapped in factor() when it is one
# of `factor_cols` (numeric metadata columns used as a discrete aesthetic).
code_aes_value <- function(col, factor_cols = character(0)) {
  bare <- if (grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", col)) {
    col
  } else {
    paste0("`", col, "`")
  }
  if (col %in% factor_cols) paste0("factor(", bare, ")") else bare
}

# Build an ir_filter_metadata() condition for the currently selected analyses.
#
# `selected` is the selected metadata rows (file_name + analysis columns), `all`
# is every available row. Returns a condition string like
# `(file_name == "a") | (file_name == "b" & analysis %in% c(1, 2))` -- one term
# per selected file, naming the analyses only when a strict subset of that file's
# analyses is chosen. Returns NULL when nothing should be filtered (selection is
# NULL/empty or every row is selected), so the caller can drop the filter step.
code_metadata_filter <- function(selected, all) {
  cols_ok <- function(x) {
    !is.null(x) && all(c("file_name", "analysis") %in% names(x))
  }
  if (!cols_ok(selected) || !cols_ok(all) || nrow(all) == 0L) {
    return(NULL)
  }
  if (nrow(selected) == 0L || nrow(selected) >= nrow(all)) {
    return(NULL)
  }
  terms <- vapply(
    unique(selected$file_name),
    function(fn) {
      sel_analyses <- selected$analysis[selected$file_name == fn]
      all_analyses <- all$analysis[all$file_name == fn]
      name_term <- paste0("file_name == ", code_value(fn))
      if (length(all_analyses) > 0 && setequal(sel_analyses, all_analyses)) {
        name_term
      } else {
        # always c(...) for the analyses, even a single one (matches `%in% c(...)`)
        vals <- sort(unique(sel_analyses))
        vals_code <- paste0(
          "c(",
          paste(vapply(vals, code_value, character(1)), collapse = ", "),
          ")"
        )
        paste0(name_term, " & analysis %in% ", vals_code)
      }
    },
    character(1)
  )
  if (length(terms) == 1L) {
    terms[[1]]
  } else {
    # one parenthesized condition per line (joined by `|`)
    paste0("(", terms, ")", collapse = " |\n  ")
  }
}

# join non-NULL parts into a `|>` pipe (continuation lines indented by 2). The
# inner lines of each continuation part are indented too, so multi-line steps (a
# wrapped call, a multi-condition ir_filter_metadata) stay aligned under the pipe.
code_pipe <- function(...) {
  parts <- Filter(Negate(is.null), list(...))
  if (length(parts) == 0L) {
    return("")
  }
  rest <- vapply(
    parts[-1],
    function(p) gsub("\n", "\n  ", p, fixed = TRUE),
    character(1)
  )
  paste(c(parts[[1]], rest), collapse = " |>\n  ")
}
