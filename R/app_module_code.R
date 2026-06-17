#' Code-generation server
#'
#' A central module that assembles "idealized example code" for an isoexplorer
#' app and shows it in a viewer. It is instantiated once at the top level; every
#' other module that can contribute code returns a `get_code()` generator, and the
#' top-level wiring **registers** each one here with [register()] under an id, a
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
    register <- function(code_id, heading, get_code, depends_on = NULL, group = NULL) {
      registry[[code_id]] <<- list(
        heading = heading,
        get_code = get_code,
        depends_on = depends_on,
        group = group
      )
      invisible(NULL)
    }

    # Quarto vs plain-code view (toggled in the modal); Quarto is the default
    quarto_view <- reactiveVal(TRUE)
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
            list(code = paste0("# (code unavailable: ", conditionMessage(e), ")"))
          }
        )
        outputs[[node$id]] <- res$output
        sections[[length(sections) + 1L]] <- list(
          depth = node$depth,
          heading = node$heading,
          code = res$code %||% ""
        )
      }
      render_code_document(sections, quarto = quarto, front_matter = front_matter)
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
        writeLines(build_document(quarto = TRUE, front_matter = TRUE)$script, file)
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
        class = "d-flex gap-3 me-3",
        actionLink(
          ns("toggle_format"),
          if (quarto) "Show plain code" else "Show as Quarto",
          icon = icon(if (quarto) "code" else "file-lines")
        ),
        downloadLink(
          ns("download_qmd"),
          "Download .qmd",
          icon = icon("download")
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

# get_code generator for the "Read data files" step: a type-specific find on the
# placeholder "data" folder (e.g. ir_find_continuous_flow), read into `iso_files`.
# Type-specific find means no separate filter-for-type step is needed downstream.
code_read_step <- function(find_fn) {
  force(find_fn)
  function(input_var = NULL) {
    list(
      code = paste0(
        "# assumes all data files are in a data folder in the current working directory\n",
        code_assign(
          "iso_files",
          code_pipe(code_call(find_fn, list("data")), code_call("ir_read_isofiles"))
        )
      ),
      output = "iso_files"
    )
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
          code_call("ir_aggregate_isofiles", list(intensity_units = get_units()))
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
