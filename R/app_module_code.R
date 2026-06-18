#' Code-generation server
#'
#' A central module that assembles "idealized example code" for an isoexplorer
#' app and shows it in a viewer. It is instantiated once at the top level; every
#' other module that can contribute code returns a `get_code()` generator, and the
#' top-level wiring **registers** each one here with `register()` under an id, a
#' heading, and (optionally) the id it `depends_on`.
#'
#' The registrations form a dependency tree. When the user clicks the navbar
#' **Show code** button ([ie_code_ui()]), the registered generators are walked
#' depth-first and assembled into one document: the tree depth sets the heading
#' level (`#` for a root such as a read step, `##` for a step depending on a root,
#' `###` for one depending on that, ...), and each step's `output` variable is
#' threaded in as the `input_var` of the steps that depend on it.
#' The document is shown in a read-only [shinyAce::aceEditor()] with a clickable
#' headings tree (jump-to-section), a toggle between plain code and a Quarto view,
#' and a `.qmd` download.
#'
#' Each registration may carry a `group`; when `get_active_group()` returns a
#' non-empty value only registrations in that group (plus group-less ones) are
#' shown -- the full multi-tab app uses this to show just the active measurement
#' type's code.
#'
#' @section The `get_code()` contract: a registered generator is
#'   `function(input_var = NULL)` returning `list(code = <string>, output =
#'   <string or NULL>)`. It is called during assembly inside a reactive context,
#'   so it reflects the module's *current* state; `input_var` is the output
#'   variable of the module it depends on (`NULL` for a root), and `output` is the
#'   variable this snippet binds (or `NULL` for a terminal node such as a plot).
#'
#' @param id the module id (namespace); pair with [ie_code_ui()] on the same id
#' @param get_active_group a reactive returning the active group id; only
#'   registrations in this group (plus group-less ones) are shown. The default
#'   `reactive(NULL)` shows everything.
#' @return the code handle: a list with `register(code_id, heading, get_code,
#'   depends_on = NULL, group = NULL)` (each `depends_on` is a single id -- the
#'   tree is single-parent) and `build_document(quarto = FALSE)` (assemble +
#'   return `list(script, headings)`; exposed mainly for testing).
#' @export
ie_code_server <- function(id, get_active_group = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # registry of code generators:
    # code_id -> list(heading, get_code, depends_on, group). Populated once at app
    # setup via register() (synchronous, before any click), read on assembly.
    registry <- list()
    register <- function(
      code_id,
      heading,
      get_code,
      depends_on = NULL,
      group = NULL
    ) {
      registry[[code_id]] <<- list(
        heading = heading,
        get_code = get_code,
        depends_on = depends_on,
        group = group
      )
      invisible(NULL)
    }

    # plain code vs Quarto view (toggled in the modal); plain code is the default
    quarto_view <- reactiveVal(FALSE)
    # the most recently assembled document (drives the headings nav)
    current_doc <- reactiveVal(NULL)

    # walk the dependency tree depth-first, calling each get_code() with its
    # parent's output variable, and render the assembled document. Restricted to
    # the active group (group-less roots always show; group is consistent down a
    # branch, so dropping a branch never orphans a kept node). Called inside the
    # click / toggle observers (reactive context) so each get_code() reflects the
    # current state; a failing generator degrades to a comment, not an abort.
    build_document <- function(quarto = quarto_view(), front_matter = FALSE) {
      specs <- lapply(registry, function(r) {
        list(heading = r$heading, depends_on = r$depends_on)
      })
      nodes <- build_code_tree(specs)
      active <- tryCatch(get_active_group(), error = function(e) NULL)
      in_active_group <- function(node) {
        g <- registry[[node$id]]$group
        is.null(g) || is.null(active) || !nzchar(active) || identical(g, active)
      }
      nodes <- Filter(in_active_group, nodes)

      outputs <- list() # code_id -> output variable name
      sections <- list()
      for (node in nodes) {
        parent_output <- if (!is.null(node$depends_on)) {
          outputs[[node$depends_on]]
        } else {
          NULL
        }
        res <- tryCatch(
          registry[[node$id]]$get_code(input_var = parent_output),
          error = function(e) {
            list(
              code = paste0("# (code unavailable: ", conditionMessage(e), ")")
            )
          }
        )
        outputs[[node$id]] <- res$output
        sections[[length(sections) + 1L]] <- list(
          depth = node$depth,
          heading = node$heading,
          code = res$code %||% ""
        )
      }
      # always open with a setup chunk loading the required libraries
      if (length(sections) > 0L) {
        sections <- c(
          list(list(
            depth = 1L,
            heading = "Setup",
            code = "library(isoreader2)\nlibrary(ggplot2)"
          )),
          sections
        )
      }
      render_code_document(
        sections,
        quarto = quarto,
        front_matter = front_matter
      )
    }

    # (re)open the viewer with the freshly assembled document (no YAML front
    # matter in the viewer; the download adds it). Both views render in the R Ace
    # mode + ambiance theme (see code_modal()).
    open_modal <- function() {
      doc <- build_document(front_matter = FALSE)
      current_doc(doc)
      showModal(code_modal(ns, doc$script, quarto_view()))
    }

    observeEvent(input$show_code, open_modal())
    observeEvent(input$toggle_format, {
      quarto_view(!quarto_view())
      open_modal() # re-show: refreshes editor mode/value, toggle label, and nav
    })

    # clickable headings tree (indented by level); each entry jumps the editor
    output$headings_nav <- renderUI({
      doc <- current_doc()
      if (is.null(doc) || nrow(doc$headings) == 0L) {
        return(em("No code to show yet."))
      }
      tagList(lapply(seq_len(nrow(doc$headings)), function(i) {
        h <- doc$headings[i, ]
        tags$a(
          href = "#",
          class = "d-block text-truncate",
          style = sprintf("padding-left:%dpx;", (h$level - 1L) * 14L),
          onclick = sprintf(
            "Shiny.setInputValue('%s', %d, {priority:'event'}); return false;",
            ns("goto_line"),
            h$line
          ),
          h$text
        )
      }))
    })

    # jump the Ace editor to a heading's line (a plain Shiny custom message, so it
    # does not depend on a shinyjs extension being registered)
    observeEvent(input$goto_line, {
      session$sendCustomMessage(
        "isoexplorer_goto_line",
        list(id = ns("code_editor"), line = input$goto_line)
      )
    })

    output$download_qmd <- downloadHandler(
      filename = function() {
        paste0(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "_isoexplorer_code.qmd")
      },
      content = function(file) {
        # the downloaded .qmd is a complete document -- with the YAML front matter
        writeLines(
          build_document(quarto = TRUE, front_matter = TRUE)$script,
          file
        )
      }
    )

    list(register = register, build_document = build_document)
  })
}

#' @describeIn ie_code_server the navbar **Show code** button (place in the navbar,
#'   e.g. `bslib::nav_item(ie_code_ui("code"))`) plus the one-time JS used to jump
#'   the editor to a heading. Pair with [ie_code_server()] on the same `id`.
#' @export
ie_code_ui <- function(id) {
  ns <- NS(id)
  tagList(
    singleton(code_preview_js()),
    actionButton(
      ns("show_code"),
      "Show code",
      icon = icon("code"),
      class = "btn-sm"
    )
  )
}

# the modal viewer: read-only Ace editor + headings nav, with a code/Quarto toggle
# and a .qmd download in the title row
code_modal <- function(ns, script, quarto) {
  modalDialog(
    title = div(
      class = "d-flex justify-content-between align-items-center gap-5",
      span("Example code"),
      div(
        class = "d-flex gap-2 me-3",
        # copy the editor's current contents (plain or Quarto) to the clipboard,
        # with brief "Copied!" feedback -- pure client-side, no server round-trip
        tags$button(
          type = "button",
          class = "btn btn-default btn-sm",
          onclick = sprintf(
            "navigator.clipboard.writeText(ace.edit('%s').getValue()); var b=this, h=b.innerHTML; b.innerHTML='Copied!'; setTimeout(function(){b.innerHTML=h;}, 1200);",
            ns("code_editor")
          ),
          icon("copy"),
          " Copy code"
        ),
        actionButton(
          ns("toggle_format"),
          if (quarto) "Show plain code" else "Show as Quarto",
          icon = icon(if (quarto) "code" else "file-lines"),
          class = "btn-sm"
        ),
        downloadButton(
          ns("download_qmd"),
          "Download .qmd",
          class = "btn-sm"
        )
      )
    ),
    size = "xl",
    easyClose = TRUE,
    footer = modalButton("Close"),
    bslib::layout_sidebar(
      height = "72vh",
      sidebar = bslib::sidebar(
        position = "right",
        width = 240,
        title = "Sections",
        uiOutput(ns("headings_nav"))
      ),
      shinyAce::aceEditor(
        ns("code_editor"),
        value = script,
        # both views use R highlighting + the ambiance theme, like isoviewer (in
        # the Quarto view the `#` headings simply read as R comments)
        mode = "r",
        theme = "ambiance",
        readOnly = TRUE,
        height = "100%",
        fontSize = 14,
        showLineNumbers = TRUE,
        highlightActiveLine = FALSE
      )
    )
  )
}

# get_code generator for the "Read data files" step: a type-specific find
# (e.g. ir_find_continuous_flow) read into `iso_files`. The folders searched
# depend on where the loaded files came from: bundled examples -> `examples_folder`
# (prepended with ir_copy_examples()), uploaded files -> `upload_folder`. These are
# the actual folders the app was given (NOT hard-coded). `uses_examples` /
# `uses_uploads` are optional reactives/functions returning whether each source has
# any loaded files; when both are present the finds combine into one
# ir_find_<type>(c(...)). Defaults to the upload folder.
code_read_step <- function(
  find_fn,
  uses_examples = NULL,
  uses_uploads = NULL,
  examples_folder = "examples",
  upload_folder = "data"
) {
  force(find_fn)
  force(uses_examples)
  force(uses_uploads)
  ex_dir <- examples_folder %||% "examples"
  up_dir <- upload_folder %||% "data"
  function(input_var = NULL) {
    ex <- !is.null(uses_examples) && isTRUE(uses_examples())
    up <- !is.null(uses_uploads) && isTRUE(uses_uploads())
    folders <- c(if (ex) ex_dir, if (up) up_dir)
    if (length(folders) == 0L) {
      folders <- up_dir
    }
    read <- code_assign(
      "iso_files",
      code_pipe(
        code_call(find_fn, list(folders)),
        code_call("ir_read_isofiles")
      )
    )
    comment <- if (ex && up) {
      sprintf(
        "# copy the bundled examples into '%s'; your own files go in '%s'\n",
        ex_dir,
        up_dir
      )
    } else if (ex) {
      sprintf(
        "# copy the bundled isoreader2 example files into '%s', then read them\n",
        ex_dir
      )
    } else {
      sprintf(
        "# assumes all data files are in the '%s' folder in the working directory\n",
        up_dir
      )
    }
    copy <- if (ex) {
      paste0(code_call("ir_copy_examples", list(ex_dir)), "\n")
    } else {
      ""
    }
    list(code = paste0(comment, copy, read), output = "iso_files")
  }
}

# get_code generator for the "Aggregate data files" step: aggregate ALL read files
# with the current intensity units into `output_var` (the plot step then subsets).
code_aggregate_step <- function(get_units, output_var) {
  force(get_units)
  force(output_var)
  function(input_var = NULL) {
    list(
      code = code_assign(
        output_var,
        code_pipe(
          input_var %||% "iso_files",
          code_call(
            "ir_aggregate_isofiles",
            list(intensity_units = get_units())
          )
        )
      ),
      output = output_var
    )
  }
}

# get_code generator for a focused app's root step: there is no folder read --
# the user passed an in-memory ir_isofiles object (`obj_name` is what they called
# it), so aggregate it directly into `output_var`. `filter_fn` is the
# ir_filter_for_<type> name to insert first, or NULL to omit it (the caller passes
# NULL when the object already holds only the focused type -- no filter needed).
code_object_aggregate_step <- function(
  obj_name,
  filter_fn,
  get_units,
  output_var
) {
  force(obj_name)
  force(filter_fn)
  force(get_units)
  force(output_var)
  function(input_var = NULL) {
    list(
      code = code_assign(
        output_var,
        code_pipe(
          obj_name,
          if (!is.null(filter_fn)) code_call(filter_fn) else NULL,
          code_call(
            "ir_aggregate_isofiles",
            list(intensity_units = get_units())
          )
        )
      ),
      output = output_var
    )
  }
}

# one-time JS: a plain Shiny custom-message handler that scrolls the Ace editor to
# a 1-based line, used by the headings nav to jump around the document. (A plain
# handler -- not a shinyjs extension -- so it is always available once on the page.)
code_preview_js <- function() {
  tags$script(HTML(
    "Shiny.addCustomMessageHandler('isoexplorer_goto_line', function(msg) {
      if (typeof ace === 'undefined') return;
      var editor = ace.edit(msg.id);
      if (editor) {
        editor.gotoLine(msg.line, 0, true);
        editor.scrollToLine(msg.line - 1, true, true);
      }
    });"
  ))
}
