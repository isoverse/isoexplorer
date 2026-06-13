# shared plot framework (shiny UI + server) =====
# the "isofiles plot" view used by the cf/di/scans modules: units + species
# popovers (upper left), x-axis zoom (center), PDF download (upper right), and a
# plot-options sidebar. Each module supplies what differs: the aggregated dataset
# key, the ir_plot_* function, and (scans only) an extra scan-type popover.

# UI ----

# the right-hand "Plot Options" sidebar
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

# the intensity-units popover button (its label shows the current unit)
units_popover <- function(ns) {
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
  )
}

# the centered x-axis zoom controls (show all / move left / move right / back)
zoom_controls <- function(ns) {
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
  )
}

# the shared plot view: units (+ optional `extra_left`, e.g. the scans scan-type
# popover) and species popovers on the left, zoom centered, download on the
# right, plot output below, and the plot-options sidebar on the right.
data_plot_view_ui <- function(id, extra_left = NULL) {
  ns <- NS(id)
  bslib::layout_sidebar(
    fill = TRUE,
    sidebar = plot_options_sidebar(ns),
    bslib::card_body(
      min_height = 400,
      div(
        class = "d-flex align-items-center gap-2 mb-2",
        # left: units + optional extra + species
        div(
          class = "d-flex flex-wrap align-items-center gap-2",
          style = "flex: 1 1 0;",
          units_popover(ns),
          extra_left,
          uiOutput(ns("species_buttons"))
        ),
        # center: zoom navigation
        zoom_controls(ns),
        # right: download (balances the left zone to keep zoom centered)
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

# server ----

# wire up the shared "isofiles plot" server. `dataset_key` selects the aggregated
# table ("traces"/"cycles"/"scans"); `plot_fn` is the ir_plot_* function;
# `no_data_message` is shown when the dataset is empty; `download_basename` is the
# PDF filename stem. `setup_extra(get_aggregated_data, input, output, session)` is
# optional and returns `list(plot_args = <reactive list spliced into plot_fn>,
# filter_dataset = <optional function(dataset) restricting the species/mass
# options>)`; it may also set up extra outputs (e.g. the scans scan-type UI).
# `zoom_arg` is the name of `plot_fn`'s x-window argument (cf "time_window", scans
# "x_window"); when set, the zoom window is passed there (filters + rescales y).
# When NULL (dual inlet), zoom is applied as a post-hoc `coord_cartesian(xlim=)`.
setup_data_plot <- function(
  id,
  get_isofiles,
  dataset_key,
  plot_fn,
  no_data_message,
  download_basename = "plot",
  setup_extra = NULL,
  zoom_arg = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # species the user explicitly hid via "Hide" (stays hidden until "Show" is
    # clicked or any of its masses is (re)selected)
    hidden <- reactiveValues()

    # intensity units (popover; input is NULL until first opened -> default mV)
    get_units <- reactive(input$units %||% "mV")
    output$units_label <- renderText(get_units())

    # aggregate the isofiles with the selected units
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

    # module-specific extras: `setup_extra` returns
    # list(plot_args = <reactive list spliced into plot_fn>, filter_dataset =
    # <optional function(dataset) restricting the species/mass options, e.g.
    # scans to the selected scan_type>); it may also set up extra outputs.
    extra <- if (!is.null(setup_extra)) {
      setup_extra(get_aggregated_data, input, output, session)
    } else {
      list()
    }
    get_extra_plot_args <- extra$plot_args %||% reactive(list())
    filter_dataset <- extra$filter_dataset

    # species -> available masses, de-duplicated so a units change (which
    # re-aggregates but keeps the same species/masses) doesn't rebuild the
    # popovers (a rebuild briefly leaves duplicate bound inputs). `filter_dataset`
    # (if provided) restricts the options, e.g. scans to the selected scan_type.
    species_masses_raw <- reactive({
      dataset <- get_aggregated_data()[[dataset_key]]
      if (!is.null(filter_dataset)) {
        dataset <- filter_dataset(dataset)
      }
      if (is.null(dataset) || nrow(dataset) == 0) {
        return(NULL)
      }
      out <- species_mass_groups(dataset) |> try_catch_cnds()
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

    # register Hide / Show / deflag handlers once per species (keyed by species)
    handlers_for <- reactiveVal(character(0))
    observe({
      g <- species_masses()
      req(g)
      done <- isolate(handlers_for())
      for (new_species in setdiff(g$species, done)) {
        local({
          species <- new_species
          observeEvent(
            input[[paste0(species, "-hide")]],
            {
              updateCheckboxGroupInput(session, species, selected = character(0))
              hidden[[species]] <- TRUE
            },
            ignoreInit = TRUE
          )
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

    # one popover button per species (Show/Hide + mass checkbox group, named by
    # the species so input[[species]] / hidden[[species]] align)
    output$species_buttons <- renderUI({
      g <- species_masses()
      validate(need(!is.null(g) && nrow(g) > 0, no_data_message))
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

    # selected (species, mass) pairs. A hidden species contributes nothing;
    # otherwise an unreported group (popover never opened) counts as all-selected
    # so the plot shows everything at startup.
    get_selected_masses <- reactive({
      g <- species_masses()
      if (is.null(g) || nrow(g) == 0) {
        # no data (e.g. no scan files) -> nothing selected, yields an empty plot
        return(tibble::tibble(species = character(0), mass = character(0)))
      }
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

    # ZOOM (x-axis) ----
    zoom_move <- 0.5
    values <- reactiveValues(
      valid_plot = FALSE,
      zoom_stack = list(list(x_min = NULL, x_max = NULL))
    )
    get_last_zoom <- function() values$zoom_stack[[length(values$zoom_stack)]]
    add_to_zoom_stack <- function(x_min, x_max) {
      new_zoom <- list(x_min = x_min, x_max = x_max)
      if (!identical(get_last_zoom(), new_zoom)) {
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
        add_to_zoom_stack(z$x_min + direction * span, z$x_max + direction * span)
      }
    }
    observeEvent(input$zoom_move_left, move_zoom(-1))
    observeEvent(input$zoom_move_right, move_zoom(+1))
    # reset zoom when the data changes (units) or the extra args change (e.g. the
    # scans scan_type, whose x-axis range differs entirely between scan types)
    observe({
      get_aggregated_data()
      get_extra_plot_args()
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
    # plain reactive: renders at startup and re-renders on any change to the data
    # (units), the mass selection, the plot options, the extras, or the zoom
    generate_plot <- reactive({
      z <- get_last_zoom()
      window <- if (!is.null(z$x_min)) c(z$x_min, z$x_max) else NULL
      # extra plot args (e.g. scans scan_type) plus the zoom window for plot
      # functions that take one (cf `time_window`, scans `x_window`); these filter
      # the data and rescale y. di has no such arg -> coord_cartesian below.
      extra <- get_extra_plot_args()
      if (!is.null(zoom_arg) && !is.null(window)) {
        extra[[zoom_arg]] <- window
      }
      out <- do.call(
        build_data_plot,
        c(
          list(
            get_aggregated_data(),
            dataset_key = dataset_key,
            selected_masses = get_selected_masses(),
            plot_fn = plot_fn,
            font_size = input$font_size,
            scientific = input$scientific,
            legend_position = input$legend_position
          ),
          extra
        )
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)

      p <- out$result
      values$valid_plot <- !is.null(p)
      if (is.null(zoom_arg) && !is.null(p) && !is.null(window)) {
        # no plot-function window arg (dual inlet): zoom the view post-hoc
        p <- p + ggplot2::coord_cartesian(xlim = window)
      }
      p %||% make_empty_plot(input$font_size)
    })
    output$data_plot <- renderPlot(generate_plot(), res = 96)

    # PDF download of the current plot
    plot_download_server(
      "plot_download",
      plot_func = generate_plot,
      filename_func = reactive({
        paste0(
          format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
          "_",
          download_basename,
          ".pdf"
        )
      })
    )

    invisible(NULL)
  })
}
