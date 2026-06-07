# Continuous Flow plot UI + server =====

cf_plot_ui <- function(id) {
  ns <- NS(id)

  bslib::layout_sidebar(
    padding = 0,
    # LEFT: mass selector ----
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
    # CENTER + RIGHT ----
    bslib::layout_sidebar(
      fill = TRUE,
      # RIGHT: plot options ----
      sidebar = bslib::sidebar(
        position = "right",
        width = "160",
        title = "Plot Options",
        selectInput(
          ns("legend_position"),
          "Legend:",
          choices = c("right", "bottom", "top", "left", "hide"),
          selected = "right"
        ),
        numericInput(ns("font_size"), "Font size:", value = 16, min = 6, step = 1),
        checkboxInput(ns("scientific"), "Scientific notation", value = FALSE)
      ),
      # CENTER: controls + plot ----
      bslib::card_body(
        min_height = 400,
        fluidRow(
          column(
            width = 6,
            align = "left",
            actionButton(ns("zoom_all"), "", icon = icon("resize-full", lib = "glyphicon")) |>
              add_tooltip("Show all data") |>
              shinyjs::disabled(),
            actionButton(ns("zoom_move_left"), "", icon = icon("arrow-left")) |>
              add_tooltip("Move left") |>
              shinyjs::disabled(),
            actionButton(ns("zoom_move_right"), "", icon = icon("arrow-right")) |>
              add_tooltip("Move right") |>
              shinyjs::disabled(),
            actionButton(ns("zoom_back"), "", icon = icon("rotate-left", verify_fa = FALSE)) |>
              add_tooltip("Revert to previous view") |>
              shinyjs::disabled()
          ),
          column(
            width = 6,
            align = "right",
            actionButton(ns("plot_refresh"), "(Re)plot", icon = icon("sync")) |>
              add_tooltip("Refresh the plot with the current selection and options.")
          )
        ),
        plotOutput(
          ns("data_plot"),
          dblclick = ns("data_plot_dblclick"),
          brush = brushOpts(
            id = ns("data_plot_brush"),
            delayType = "debounce",
            direction = "x",
            resetOnNew = TRUE
          )
        ) |>
          shinycssloaders::withSpinner() |>
          bslib::as_fill_carrier()
      )
    )
  )
}

cf_plot_server <- function(id, get_aggregated_data, get_selected_metadata) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    zoom_move <- 0.5
    values <- reactiveValues(
      valid_plot = FALSE,
      refresh_trigger = 0,
      zoom_stack = list(list(x_min = NULL, x_max = NULL))
    )

    # MASS SELECTOR ----

    get_masses_for_table <- reactive({
      req(get_aggregated_data())
      traces <- get_aggregated_data()$traces
      validate(need(
        !is.null(traces) && nrow(traces) > 0,
        "No continuous flow trace data available."
      ))
      out <- traces |>
        dplyr::select(dplyr::any_of(c("mass", "species"))) |>
        dplyr::distinct() |>
        dplyr::arrange(as.numeric(.data$mass)) |>
        dplyr::mutate(
          mass_id = if ("species" %in% names(traces)) {
            paste(.data$mass, .data$species, sep = ":")
          } else {
            as.character(.data$mass)
          },
          .before = 1L
        ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      out$result
    })

    masses <- module_selector_table_server(
      "masses",
      get_data = get_masses_for_table,
      id_column = "mass_id",
      columnDefs = list(list(visible = FALSE, targets = 0)),
      paging = FALSE,
      dom = "ft"
    )

    observeEvent(masses$is_table_reloaded(), {
      if (is_empty(masses$get_selected_ids())) {
        masses$select_all()
      }
    })

    # ZOOM ----

    get_last_zoom <- function() values$zoom_stack[[length(values$zoom_stack)]]

    add_to_zoom_stack <- function(x_min, x_max) {
      new_zoom <- list(x_min = x_min, x_max = x_max)
      if (!identical(get_last_zoom(), new_zoom)) {
        values$zoom_stack <- c(values$zoom_stack, list(new_zoom))
        refresh_plot()
      }
    }

    load_last_zoom <- function() {
      n <- length(values$zoom_stack)
      if (n > 1L) values$zoom_stack[[n]] <- NULL
      refresh_plot()
    }

    observeEvent(input$zoom_back, load_last_zoom())
    observeEvent(input$data_plot_dblclick, load_last_zoom())
    observeEvent(input$zoom_all, add_to_zoom_stack(x_min = NULL, x_max = NULL))

    observeEvent(input$data_plot_brush, {
      brush <- input$data_plot_brush
      if (!is.null(brush$xmin) && !is.null(brush$xmax)) {
        add_to_zoom_stack(brush$xmin, brush$xmax)
      }
    })

    move_zoom <- function(direction) {
      z <- get_last_zoom()
      if (!is.null(z$x_min) && !is.null(z$x_max)) {
        span <- (z$x_max - z$x_min) * zoom_move
        add_to_zoom_stack(z$x_min + direction * span, z$x_max + direction * span)
      }
    }
    observeEvent(input$zoom_move_left, move_zoom(-1))
    observeEvent(input$zoom_move_right, move_zoom(+1))

    # reset zoom when data changes
    observe({
      get_aggregated_data()
      isolate(values$zoom_stack <- list(list(x_min = NULL, x_max = NULL)))
    })

    # PLOT ----

    refresh_plot <- function() {
      values$refresh_trigger <- values$refresh_trigger + 1L
    }
    observeEvent(input$plot_refresh, refresh_plot())

    observe({
      shinyjs::toggleState("zoom_all", condition = values$valid_plot)
      shinyjs::toggleState("zoom_move_left", condition = values$valid_plot)
      shinyjs::toggleState("zoom_move_right", condition = values$valid_plot)
      shinyjs::toggleState("zoom_back", condition = values$valid_plot)
    })

    generate_plot <- eventReactive(values$refresh_trigger, {
      isolate({
        agg_data <- filter_agg_data_by_metadata(get_aggregated_data(), get_selected_metadata())
        traces <- agg_data$traces
        selected <- masses$get_selected_items()

        if (is.null(agg_data) || is.null(traces) || nrow(traces) == 0 || is.null(selected) || nrow(selected) == 0) {
          values$valid_plot <- FALSE
          return(get_empty_plot_in_app(input$font_size))
        }

        agg_data$traces <- filter_by_selected_masses(traces, selected)

        if (nrow(agg_data$traces) == 0) {
          values$valid_plot <- FALSE
          return(get_empty_plot_in_app(input$font_size))
        }

        z <- get_last_zoom()
        time_window <- if (!is.null(z$x_min)) c(z$x_min, z$x_max) else NULL

        out <- isoreader2::ir_plot_continuous_flow(
          agg_data,
          time_window = time_window,
          scientific = isTRUE(input$scientific),
          theme = isoreader2::ir_default_theme(text_size = input$font_size)
        ) |>
          try_catch_cnds()
        out |> log_cnds(ns = ns)

        p <- out$result
        if (!is.null(p)) {
          p <- p + if (input$legend_position == "hide") {
            theme(legend.position = "none")
          } else {
            theme(legend.position = input$legend_position)
          }
        }
        values$valid_plot <- !is.null(p)
        p
      })
    })

    output$data_plot <- renderPlot(generate_plot(), res = 96)

    invisible(NULL)
  })
}
