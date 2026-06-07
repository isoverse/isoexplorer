# Scans plot UI + server =====

scans_plot_ui <- function(id) {
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
        uiOutput(ns("scan_type_input")),
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
            width = 12,
            align = "right",
            actionButton(ns("plot_refresh"), "(Re)plot", icon = icon("sync")) |>
              add_tooltip("Refresh the plot with the current selection and options.")
          )
        ),
        plotOutput(ns("data_plot")) |>
          shinycssloaders::withSpinner() |>
          bslib::as_fill_carrier()
      )
    )
  )
}

scans_plot_server <- function(id, get_aggregated_data, get_selected_metadata) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    values <- reactiveValues(refresh_trigger = 0)

    # SCAN TYPE SELECTOR ----

    output$scan_type_input <- renderUI({
      req(get_aggregated_data())
      scans <- get_aggregated_data()$scans
      req(!is.null(scans) && nrow(scans) > 0 && "scan_type" %in% names(scans))
      types <- unique(scans$scan_type)
      if (length(types) > 1L) {
        selectInput(
          ns("scan_type"),
          "Scan type:",
          choices = types,
          selected = types[1]
        )
      }
    })

    # MASS SELECTOR ----

    get_masses_for_table <- reactive({
      req(get_aggregated_data())
      scans <- get_aggregated_data()$scans
      validate(need(
        !is.null(scans) && nrow(scans) > 0,
        "No scan data available."
      ))
      out <- scans |>
        dplyr::select(dplyr::any_of(c("mass", "species"))) |>
        dplyr::distinct() |>
        dplyr::arrange(as.numeric(.data$mass)) |>
        dplyr::mutate(
          mass_id = if ("species" %in% names(scans)) {
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

    # PLOT ----

    refresh_plot <- function() {
      values$refresh_trigger <- values$refresh_trigger + 1L
    }
    observeEvent(input$plot_refresh, refresh_plot())

    generate_plot <- eventReactive(values$refresh_trigger, {
      isolate({
        agg_data <- filter_agg_data_by_metadata(get_aggregated_data(), get_selected_metadata())
        scans <- agg_data$scans
        selected <- masses$get_selected_items()

        if (is.null(agg_data) || is.null(scans) || nrow(scans) == 0 || is.null(selected) || nrow(selected) == 0) {
          return(get_empty_plot_in_app(input$font_size))
        }

        agg_data$scans <- filter_by_selected_masses(scans, selected)

        if (nrow(agg_data$scans) == 0) {
          return(get_empty_plot_in_app(input$font_size))
        }

        # use selected scan_type if the input exists, otherwise let the function decide
        scan_type <- if (!is.null(input$scan_type) && nzchar(input$scan_type)) {
          input$scan_type
        } else {
          NULL
        }

        out <- isoreader2::ir_plot_scans(
          agg_data,
          scan_type = scan_type,
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
        p
      })
    })

    output$data_plot <- renderPlot(generate_plot(), res = 96)

    invisible(NULL)
  })
}
