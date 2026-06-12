# Dual Inlet plot UI + server =====
# shared logic lives in logic_plots.R; plot options sidebar in app_plots.R.
# this module takes a reactive get_isofiles(), aggregates it itself with the
# intensity units picked in the plot controls, and lets the user pick masses per
# species via popover checkboxes that replot immediately.

di_plot_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    fill = TRUE,
    # right: plot options (legend / font / scientific)
    sidebar = plot_options_sidebar(ns),
    bslib::card_body(
      min_height = 400,
      # top of the plot: units picker + per-species mass buttons + (re)plot
      div(
        class = "d-flex flex-wrap align-items-center gap-2 mb-2",
        selectInput(
          ns("units"),
          label = NULL,
          choices = ie_units(),
          selected = "mV",
          width = "72px"
        ) |>
          add_tooltip("Intensity units to aggregate the data with"),
        uiOutput(ns("species_buttons")),
        div(
          class = "ms-auto",
          actionButton(ns("plot_refresh"), "(Re)plot", icon = icon("sync")) |>
            add_tooltip("Refresh the plot with the current options.")
        )
      ),
      plotOutput(ns("data_plot")) |>
        shinycssloaders::withSpinner() |>
        bslib::as_fill_carrier()
    )
  )
}

di_plot_server <- function(id, get_isofiles) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    values <- reactiveValues(refresh_trigger = 0)

    # aggregate the isofiles with the selected intensity units
    get_aggregated_data <- reactive({
      req(get_isofiles())
      req(input$units)
      log_info(
        ns = ns,
        user_msg = paste("Aggregating with intensity units", input$units)
      )
      out <- isoreader2::ir_aggregate_isofiles(
        get_isofiles(),
        intensity_units = input$units
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      out$result
    })

    # species -> available masses (drives the buttons + the selection state)
    species_masses <- reactive({
      req(get_aggregated_data())
      cycles <- get_aggregated_data()$cycles
      validate(need(
        !is.null(cycles) && nrow(cycles) > 0,
        "No dual inlet cycle data available."
      ))
      out <- species_mass_groups(cycles) |> try_catch_cnds()
      out |> log_cnds(ns = ns)
      out$result
    })

    # one button per species; clicking opens a popover with a mass checkbox each
    # (all on by default). IDs are mass_<species-index>_<mass-index>.
    output$species_buttons <- renderUI({
      g <- species_masses()
      req(!is.null(g) && nrow(g) > 0)
      buttons <- lapply(seq_len(nrow(g)), function(i) {
        species <- g$species[i]
        species_masses_i <- g$masses[[i]]
        checks <- lapply(seq_along(species_masses_i), function(j) {
          checkboxInput(
            ns(paste0("mass_", i, "_", j)),
            label = species_masses_i[j],
            value = TRUE
          )
        })
        bslib::popover(
          actionButton(
            ns(paste0(species, "-trigger")),
            species,
            icon = icon("caret-down")
          ),
          title = species,
          div(checks),
          options = list(trigger = "focus")
        )
      })
      tagList(buttons)
      #div(class = "d-flex flex-wrap gap-1 align-items-center", buttons)
    })

    # selected (species, mass) pairs across all popovers. A checkbox that hasn't
    # reported yet (popover never opened) counts as selected, so the plot shows
    # everything at startup without the user opening each popover.
    get_selected_masses <- reactive({
      g <- species_masses()
      req(!is.null(g) && nrow(g) > 0)
      rows <- list()
      for (i in seq_len(nrow(g))) {
        species_masses_i <- g$masses[[i]]
        for (j in seq_along(species_masses_i)) {
          checked <- input[[paste0("mass_", i, "_", j)]] %||% TRUE
          if (isTRUE(checked)) {
            rows[[length(rows) + 1L]] <- tibble::tibble(
              species = g$species[i],
              mass = species_masses_i[j]
            )
          }
        }
      }
      dplyr::bind_rows(rows)
    })

    # plot
    refresh_plot <- function() {
      values$refresh_trigger <- values$refresh_trigger + 1L
    }
    observeEvent(input$plot_refresh, refresh_plot())

    # (re)plot on manual refresh, when the data (units) change, or when the mass
    # selection changes - so toggling masses replots immediately and the plot
    # renders at startup
    generate_plot <- eventReactive(
      {
        values$refresh_trigger
        get_aggregated_data()
        get_selected_masses()
      },
      {
        isolate({
          out <- build_data_plot(
            get_aggregated_data(),
            dataset_key = "cycles",
            selected_masses = get_selected_masses(),
            plot_fn = isoreader2::ir_plot_dual_inlet,
            font_size = input$font_size,
            scientific = input$scientific,
            legend_position = input$legend_position
          ) |>
            try_catch_cnds()
          out |> log_cnds(ns = ns)
          out$result %||% make_empty_plot(input$font_size)
        })
      },
      ignoreNULL = FALSE
    )

    output$data_plot <- renderPlot(generate_plot(), res = 96)

    invisible(NULL)
  })
}
