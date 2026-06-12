# shared plot GUI (shiny components) =====
# generalized layout + server pieces used by the cf/di/scans plot modules.
# the per-plot modules compose these and supply what differs (dataset, plot
# function, extra options/controls).

# UI ----

# the right-hand "Plot Options" sidebar; `extra_top` is an optional slot rendered
# above the standard inputs (e.g. the scans scan-type selector)
plot_options_sidebar <- function(ns, extra_top = NULL) {
  bslib::sidebar(
    position = "right",
    width = "160",
    title = "Plot Options",
    extra_top,
    selectInput(
      ns("legend_position"),
      "Legend:",
      choices = c("right", "bottom", "top", "left", "hide"),
      selected = "right"
    ),
    numericInput(ns("font_size"), "Font size:", value = 16, min = 6, step = 1),
    checkboxInput(ns("scientific"), "Scientific notation", value = FALSE)
  )
}

# the standard plot-view layout: left mass-selector card, right plot-options
# sidebar, center (Re)plot controls + plot output.
# - options_extra: extra UI for the top of the plot-options sidebar
# - controls: extra left-aligned controls in the button row (e.g. cf zoom buttons)
# - brush: whether the plot output supports brushing + double-click (cf zoom)
data_plot_ui <- function(
  ns,
  options_extra = NULL,
  controls = NULL,
  brush = FALSE
) {
  replot_button <-
    actionButton(ns("plot_refresh"), "(Re)plot", icon = icon("sync")) |>
    add_tooltip("Refresh the plot with the current selection and options.")

  controls_row <- if (is.null(controls)) {
    fluidRow(column(width = 12, align = "right", replot_button))
  } else {
    fluidRow(
      column(width = 6, align = "left", controls),
      column(width = 6, align = "right", replot_button)
    )
  }

  data_plot_output <- if (brush) {
    plotOutput(
      ns("data_plot"),
      dblclick = ns("data_plot_dblclick"),
      brush = brushOpts(
        id = ns("data_plot_brush"),
        delayType = "debounce",
        direction = "x",
        resetOnNew = TRUE
      )
    )
  } else {
    plotOutput(ns("data_plot"))
  }

  bslib::layout_sidebar(
    padding = 0,
    # LEFT: mass selector
    sidebar = bslib::sidebar(
      position = "left",
      width = "200",
      fillable = TRUE,
      bslib::card(
        full_screen = TRUE,
        min_height = 300,
        padding = 0,
        module_selector_table_ui(ns("masses"))
      )
    ),
    # CENTER + RIGHT
    bslib::layout_sidebar(
      fill = TRUE,
      sidebar = plot_options_sidebar(ns, extra_top = options_extra),
      bslib::card_body(
        min_height = 400,
        controls_row,
        data_plot_output |>
          shinycssloaders::withSpinner() |>
          bslib::as_fill_carrier()
      )
    )
  )
}

# server ----

# wire up the "masses" mass-selector table for a plot module. Call inside the
# module server. `get_dataset` is a reactive returning the dataset to pull masses
# from (e.g. reactive(get_aggregated_data()$traces)). Returns the selector table
# handle (get_selected_items(), select_all(), is_table_reloaded(), ...).
setup_mass_selector <- function(get_dataset, no_data_message, ns) {
  get_masses <- reactive({
    dataset <- get_dataset()
    validate(need(!is.null(dataset) && nrow(dataset) > 0, no_data_message))
    out <- extract_masses(dataset) |> try_catch_cnds()
    out |> log_cnds(ns = ns)
    out$result
  })

  masses <- module_selector_table_server(
    "masses",
    get_data = get_masses,
    id_column = "mass_id",
    columnDefs = list(list(visible = FALSE, targets = 0)),
    paging = FALSE,
    dom = "ft"
  )

  # select everything by default once the table (re-)loads with no selection
  observeEvent(masses$is_table_reloaded(), {
    if (is_empty(masses$get_selected_ids())) {
      masses$select_all()
    }
  })

  masses
}
