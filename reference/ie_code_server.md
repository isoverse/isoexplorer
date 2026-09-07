# Code-generation server

A central module that assembles "idealized example code" for an
isoexplorer app and shows it in a viewer. It is instantiated once at the
top level; every other module that can contribute code returns a
`get_code()` generator, and the top-level wiring **registers** each one
here with `register()` under an id, a heading, and (optionally) the id
it `depends_on`.

## Usage

``` r
ie_code_server(id, get_active_group = reactive(NULL))

ie_code_ui(id)
```

## Arguments

- id:

  the module id (namespace); pair with `ie_code_ui()` on the same id

- get_active_group:

  a reactive returning the active group id; only registrations in this
  group (plus group-less ones) are shown. The default `reactive(NULL)`
  shows everything.

## Value

the code handle: a list with
`register(code_id, heading, get_code, depends_on = NULL, group = NULL)`
(each `depends_on` is a single id – the tree is single-parent) and
`build_document(quarto = FALSE)` (assemble + return
`list(script, headings)`; exposed mainly for testing).

## Details

The registrations form a dependency tree. When the user clicks the
navbar **Show code** button (`ie_code_ui()`), the registered generators
are walked depth-first and assembled into one document: the tree depth
sets the heading level (`#` for a root such as a read step, `##` for a
step depending on a root, `###` for one depending on that, ...), and
each step's `output` variable is threaded in as the `input_var` of the
steps that depend on it. The document is shown in a read-only
[`shinyAce::aceEditor()`](https://rdrr.io/pkg/shinyAce/man/aceEditor.html)
with a clickable headings tree (jump-to-section), a toggle between plain
code and a Quarto view, and a `.qmd` download.

Each registration may carry a `group`; when `get_active_group()` returns
a non-empty value only registrations in that group (plus group-less
ones) are shown – the full multi-tab app uses this to show just the
active measurement type's code.

## Functions

- `ie_code_ui()`: the navbar **Show code** button (place in the navbar,
  e.g. `bslib::nav_item(ie_code_ui("code"))`) plus the one-time JS used
  to jump the editor to a heading. Pair with `ie_code_server()` on the
  same `id`.

## The `get_code()` contract

a registered generator is `function(input_var = NULL)` returning
`list(code = <string>, output = <string or NULL>)`. It is called during
assembly inside a reactive context, so it reflects the module's
*current* state; `input_var` is the output variable of the module it
depends on (`NULL` for a root), and `output` is the variable this
snippet binds (or `NULL` for a terminal node such as a plot).

## Examples

``` r
if (interactive()) {
  library(shiny)
  # ie_run_app() instantiates the code server for you; here it is wired
  # directly. Register each module's get_code generator into the dependency
  # tree; the "Show code" button (ie_code_ui()) assembles and displays it.
  ui <- bslib::page_fillable(ie_code_ui("code"))
  server <- function(input, output, session) {
    code <- ie_code_server("code")
    code$register("read", "Read data files", get_code = function(input_var = NULL) {
      list(
        code = 'iso <- ir_find_scans("data") |> ir_read_isofiles()',
        output = "iso"
      )
    })
    code$register("agg", "Aggregate data files", depends_on = "read",
      get_code = function(input_var = NULL) {
        list(
          code = sprintf("scans <- %s |> ir_aggregate_isofiles()", input_var),
          output = "scans"
        )
      })
  }
  shinyApp(ui, server)
}
```
