# Dual Inlet plot UI + server =====
# shared logic lives in logic_plots.R; plot options sidebar in app_plots.R.
# this module takes a reactive get_isofiles(), aggregates it itself with the
# intensity units picked in the plot controls, and lets the user pick masses per
# species via popover checkbox groups (with Show/Hide) that replot immediately.

di_plot_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    fill = TRUE,
    # right: plot options (legend / font / scientific)
    sidebar = plot_options_sidebar(ns),
    bslib::card_body(
      min_height = 400,
      # one row above the plot: units + species (left), zoom navigation centered,
      # download on the right
      div(
        class = "d-flex align-items-center gap-2 mb-2",
        div(
          class = "d-flex flex-wrap align-items-center gap-2",
          style = "flex: 1 1 0;",
          bslib::popover(
            actionButton(
              ns("units-trigger"),
              textOutput(ns("units_label"), inline = TRUE),
              icon = icon("caret-down")
            ),
            title = "Units",
            div(
              style = "width: 5rem;",
              radioButtons(
                ns("units"),
                label = NULL,
                choices = ie_units(),
                selected = "mV"
              )
            ),
            options = list(trigger = "focus")
          ),
          uiOutput(ns("species_buttons"))
        ),
        # zoom navigation, centered (same as the continuous flow module)
        div(
          class = "d-flex gap-1",
          actionButton(
            ns("zoom_all"),
            "",
            icon = icon("resize-full", lib = "glyphicon")
          ) |>
            add_tooltip("Show all data") |>
            shinyjs::disabled(),
          actionButton(ns("zoom_move_left"), "", icon = icon("arrow-left")) |>
            add_tooltip("Move left") |>
            shinyjs::disabled(),
          actionButton(ns("zoom_move_right"), "", icon = icon("arrow-right")) |>
            add_tooltip("Move right") |>
            shinyjs::disabled(),
          actionButton(
            ns("zoom_back"),
            "",
            icon = icon("rotate-left", verify_fa = FALSE)
          ) |>
            add_tooltip("Revert to previous view") |>
            shinyjs::disabled()
        ),
        # right: plot download (balances the left zone to keep zoom centered)
        div(
          class = "d-flex justify-content-end",
          style = "flex: 1 1 0;",
          plot_download_link(ns("plot_download"), label = NULL) |>
            shinyjs::disabled()
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
}

di_plot_server <- function(id, get_isofiles) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # species the user explicitly hid via "Hide". A hidden species stays hidden
    # (its empty checkbox group reading as NULL would otherwise revert to all)
    # until "Show" is clicked or any of its masses is (re)selected.
    hidden <- reactiveValues()

    # current intensity units (the picker lives in a popover, so its input is
    # NULL until the popover is first opened -> default to "mV")
    get_units <- reactive(input$units %||% "mV")
    output$units_label <- renderText(get_units())

    # aggregate the isofiles with the selected intensity units
    get_aggregated_data <- reactive({
      req(get_isofiles())
      log_info(
        ns = ns,
        user_msg = paste("Aggregating with intensity units", get_units())
      )
      out <- isoreader2::ir_aggregate_isofiles(
        get_isofiles(),
        intensity_units = get_units()
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      out$result
    })

    # species -> available masses (drives the buttons + the selection state).
    # de-duplicated so switching units - which re-aggregates but keeps the same
    # species & masses - doesn't rebuild the popovers (a rebuild briefly left
    # duplicate copies of their inputs). Only emits when the structure changes.
    species_masses_raw <- reactive({
      cycles <- get_aggregated_data()$cycles
      if (is.null(cycles) || nrow(cycles) == 0) {
        return(NULL)
      }
      out <- species_mass_groups(cycles) |> try_catch_cnds()
      out |> log_cnds(ns = ns)
      out$result
    })
    species_masses_val <- reactiveVal(NULL)
    observe({
      new_value <- species_masses_raw()
      if (!identical(new_value, isolate(species_masses_val()))) {
        species_masses_val(new_value)
      }
    })
    species_masses <- reactive(species_masses_val())

    # register Hide / Show / deflag handlers once per species (inputs are keyed
    # by species name). Re-runs as new species appear; existing ones are skipped.
    handlers_for <- reactiveVal(character(0))
    observe({
      g <- species_masses()
      req(g)
      done <- isolate(handlers_for())
      for (new_species in setdiff(g$species, done)) {
        local({
          species <- new_species
          # Hide: clear the group and flag the species as hidden
          observeEvent(
            input[[paste0(species, "-hide")]],
            {
              updateCheckboxGroupInput(
                session,
                species,
                selected = character(0)
              )
              hidden[[species]] <- TRUE
            },
            ignoreInit = TRUE
          )
          # Show: select all masses and un-hide
          observeEvent(
            input[[paste0(species, "-show")]],
            {
              g_now <- species_masses()
              updateCheckboxGroupInput(
                session,
                species,
                selected = g_now$masses[[which(g_now$species == species)]]
              )
              hidden[[species]] <- NULL
            },
            ignoreInit = TRUE
          )
          # react to the group changing: an empty value (NULL) means the user
          # just deselected everything (the change was triggered by a checkbox,
          # so it isn't the never-opened case) -> hide; any selection -> un-hide
          observeEvent(
            input[[species]],
            {
              if (is.null(input[[species]])) {
                hidden[[species]] <- TRUE
              } else {
                hidden[[species]] <- NULL
              }
            },
            ignoreNULL = FALSE,
            ignoreInit = TRUE
          )
        })
      }
      handlers_for(union(done, g$species))
    })

    # one button per species; clicking opens a popover with Show/Hide and a
    # checkbox group of the species' masses (all on by default). The group input
    # is named by the species, so input[[species]] / hidden[[species]] align.
    output$species_buttons <- renderUI({
      g <- species_masses()
      validate(need(
        !is.null(g) && nrow(g) > 0,
        "No dual inlet cycle data available."
      ))
      buttons <- lapply(seq_len(nrow(g)), function(i) {
        species <- g$species[i]
        species_masses_i <- g$masses[[i]]
        bslib::popover(
          actionButton(
            ns(paste0(species, "-trigger")),
            species,
            icon = icon("caret-down")
          ),
          title = species,
          div(
            style = "width: 8rem;",
            div(
              class = "d-flex gap-1 mb-2",
              actionButton(
                ns(paste0(species, "-show")),
                "Show",
                class = "btn-sm btn-outline-success flex-fill"
              ),
              actionButton(
                ns(paste0(species, "-hide")),
                "Hide",
                class = "btn-sm btn-outline-danger flex-fill"
              )
            ),
            checkboxGroupInput(
              ns(species),
              label = NULL,
              choices = species_masses_i,
              selected = if (isTRUE(isolate(hidden[[species]]))) {
                character(0)
              } else {
                species_masses_i
              }
            )
          ),
          options = list(trigger = "focus")
        )
      })
      tagList(buttons)
    })

    # selected (species, mass) pairs across all popovers. A hidden species
    # contributes nothing; otherwise a group that hasn't reported yet (popover
    # never opened) reads as NULL and counts as all selected, so the plot shows
    # everything at startup (an opened-then-emptied group is flagged hidden by
    # the observer above, so it doesn't fall through to "all").
    get_selected_masses <- reactive({
      g <- species_masses()
      req(!is.null(g) && nrow(g) > 0)
      rows <- list()
      for (i in seq_len(nrow(g))) {
        species <- g$species[i]
        chosen <- if (isTRUE(hidden[[species]])) {
          character(0)
        } else {
          input[[species]] %||% g$masses[[i]]
        }
        for (mass in chosen) {
          rows[[length(rows) + 1L]] <- tibble::tibble(
            species = species,
            mass = mass
          )
        }
      }
      dplyr::bind_rows(rows)
    })

    # ZOOM (x-axis / cycle) - same interaction as the continuous flow module ----

    zoom_move <- 0.5
    values <- reactiveValues(
      valid_plot = FALSE,
      zoom_stack = list(list(x_min = NULL, x_max = NULL))
    )

    get_last_zoom <- function() values$zoom_stack[[length(values$zoom_stack)]]

    add_to_zoom_stack <- function(x_min, x_max) {
      new_zoom <- list(x_min = x_min, x_max = x_max)
      if (!identical(get_last_zoom(), new_zoom)) {
        # generate_plot depends on the zoom stack, so this replots on its own
        values$zoom_stack <- c(values$zoom_stack, list(new_zoom))
      }
    }

    load_last_zoom <- function() {
      n <- length(values$zoom_stack)
      if (n > 1L) values$zoom_stack[[n]] <- NULL
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
        add_to_zoom_stack(
          z$x_min + direction * span,
          z$x_max + direction * span
        )
      }
    }
    observeEvent(input$zoom_move_left, move_zoom(-1))
    observeEvent(input$zoom_move_right, move_zoom(+1))

    # reset zoom when the data changes
    observe({
      get_aggregated_data()
      isolate(values$zoom_stack <- list(list(x_min = NULL, x_max = NULL)))
    })

    # enable the zoom + download buttons only when there is a valid plot
    observe({
      shinyjs::toggleState("zoom_all", condition = values$valid_plot)
      shinyjs::toggleState("zoom_move_left", condition = values$valid_plot)
      shinyjs::toggleState("zoom_move_right", condition = values$valid_plot)
      shinyjs::toggleState("zoom_back", condition = values$valid_plot)
      shinyjs::toggleState(
        "plot_download-download_dialog",
        condition = values$valid_plot
      )
    })

    # PLOT ----

    # a plain reactive, so it renders at startup and re-renders immediately on any
    # change to the data (units), the mass selection, the plot options (legend /
    # font size / scientific), or the zoom window
    generate_plot <- reactive({
      z <- get_last_zoom()
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

      p <- out$result
      values$valid_plot <- !is.null(p)
      if (!is.null(p) && !is.null(z$x_min)) {
        # cycle x-axis has no plot-function window arg, so zoom the view here
        p <- p + ggplot2::coord_cartesian(xlim = c(z$x_min, z$x_max))
      }
      p %||% make_empty_plot(input$font_size)
    })

    output$data_plot <- renderPlot(generate_plot(), res = 96)

    # plot download (PDF) via the shared module - saves the current plot
    plot_download_server(
      "plot_download",
      plot_func = generate_plot,
      filename_func = reactive({
        paste0(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "_dual_inlet.pdf")
      })
    )

    invisible(NULL)
  })
}
