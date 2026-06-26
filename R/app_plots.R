# shared plot framework (shiny UI + server) =====
# the "isofiles plot" view used by the cf/di/scans modules: units + species
# popovers (upper left), x-axis zoom (center), PDF download (upper right), and a
# plot-options sidebar. Each module supplies what differs: the aggregated dataset
# key, the ir_plot_* function, and (scans only) an extra scan-type popover.

# UI ----

# the right-hand "Plot Options" sidebar. `extra_options` is module-specific extra
# UI (e.g. the continuous-flow `short_time_labels` checkbox). The facet/color/
# linetype dropdowns are rendered server-side (their choices include the data's
# metadata columns) via the `aes_options` placeholder.
plot_options_sidebar <- function(ns, extra_top = NULL, extra_options = NULL) {
  bslib::sidebar(
    position = "right",
    width = "190",
    title = "Plot Options",
    extra_top,
    uiOutput(ns("aes_options")),
    selectInput(
      ns("scales"),
      "Scales:",
      choices = c("free", "fixed", "free_x", "free_y"),
      selected = "free"
    ),
    checkboxInput(ns("scientific"), "Scientific notation", value = FALSE),
    checkboxInput(
      ns("drop_unused_levels"),
      "Drop unused levels",
      value = FALSE
    ),
    extra_options,
    # styling options last
    selectInput(
      ns("legend_position"),
      "Legend:",
      choices = c("right", "bottom", "top", "left", "hide"),
      selected = "right"
    ),
    numericInput(ns("font_size"), "Font size:", value = 16, min = 6, step = 1)
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
        choices = app_units(),
        selected = "mV"
      )
    ),
    options = list(trigger = "focus")
  )
}

# the ratios popover: a "Calculate ratios" toggle that, when on, reveals the
# isoreader2::ir_calculate_ratios() options for the current intensity unit family
# (the num/denom additive offsets, rendered server-side, plus a Normalize toggle).
# Edits are staged: nothing takes effect until "Apply" (green) is clicked; "Cancel"
# (gray) discards them. Sits between the units and species popovers.
ratios_popover <- function(ns) {
  bslib::popover(
    actionButton(
      ns("ratios-trigger"),
      "Ratios",
      icon = icon("caret-down")
    ),
    title = "Ratios",
    id = ns("ratios_popover"),
    div(
      style = "width: 12rem;",
      checkboxInput(ns("ratios_calculate"), "Calculate ratios", value = FALSE),
      # the additive-offset inputs (unit-family dependent) + normalize toggle are
      # only relevant once ratios are being calculated
      conditionalPanel(
        condition = "input.ratios_calculate == true",
        ns = ns,
        uiOutput(ns("ratios_params")),
        checkboxInput(ns("ratios_normalize"), "Normalize", value = FALSE)
      ),
      div(
        class = "d-flex gap-1 mt-2",
        actionButton(
          ns("ratios_apply"),
          "Apply",
          class = "btn-sm btn-success flex-fill"
        ),
        actionButton(
          ns("ratios_cancel"),
          "Cancel",
          class = "btn-sm btn-secondary flex-fill"
        )
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
data_plot_view_ui <- function(id, extra_left = NULL, extra_options = NULL) {
  ns <- NS(id)
  # the whole card (controls + options sidebar + plot) can go fullscreen
  bslib::card(
    full_screen = TRUE,
    min_height = 400,
    bslib::layout_sidebar(
      fill = TRUE,
      sidebar = plot_options_sidebar(ns, extra_options = extra_options),
      # controls row (units/species left, zoom centered, download right)
      div(
        class = "d-flex align-items-center gap-2 mb-2",
        # left: units + optional extra + species
        div(
          class = "d-flex flex-wrap align-items-center gap-2",
          style = "flex: 1 1 0;",
          units_popover(ns),
          ratios_popover(ns),
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
      # plot fills the remaining space below the controls
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

# wire up the shared "isofiles plot" server. The data and units come from the
# central file server: `get_data` is its selection-filtered aggregated data
# reactive (e.g. file$get_aggregated_scans_data) and `get_units` / `set_units`
# are its shared intensity-units accessors (the popover reads + drives them).
# `dataset_key` selects the aggregated table ("traces"/"cycles"/"scans");
# `plot_fn` is the ir_plot_* function; `no_data_message` is shown when the
# dataset is empty; `download_basename` is the PDF filename stem.
# `setup_extra(get_data, input, output, session)` is optional and returns
# `list(plot_args = <reactive list spliced into plot_fn>, filter_dataset =
# <optional function(dataset) restricting the species/mass options>)`; it may
# also set up extra outputs (e.g. the scans scan-type UI). `zoom_arg` is the name
# of `plot_fn`'s x-window argument (cf "time_window.s", di "cycle_window", scans
# "x_window"); when set, the zoom window is passed there (filters + rescales y).
# When NULL, zoom falls back to a post-hoc `coord_cartesian(xlim=)`, which zooms
# the view but does NOT rescale y (so it keeps the full y-range, incl. 0).
setup_data_plot <- function(
  id,
  get_data,
  get_units,
  set_units,
  dataset_key,
  plot_fn,
  plot_fn_name,
  no_data_message,
  download_basename = "plot",
  setup_extra = NULL,
  zoom_arg = NULL,
  get_selection = NULL,
  get_all_metadata = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # species whose masses / ratios the user explicitly hid via "Hide" or by
    # un-checking everything (stays hidden until "Show" is clicked or something is
    # (re)selected). Masses and ratios are tracked independently so e.g. the ratios
    # can stay shown while the raw-intensity masses are hidden.
    hidden <- reactiveValues()
    hidden_ratios <- reactiveValues()

    # intensity units live in the file server (shared across all plots). Show its
    # current value in the popover label, push popover changes back into it, and
    # keep the radio in sync if the units get changed from elsewhere. (Setting a
    # reactiveVal to its current value is a no-op, so this can't loop.)
    output$units_label <- renderText(get_units())
    observeEvent(input$units, set_units(input$units), ignoreInit = TRUE)
    observe(updateRadioButtons(session, "units", selected = get_units()))

    # module-specific extras: `setup_extra` returns
    # list(plot_args = <reactive list spliced into plot_fn>, filter_dataset =
    # <optional function(dataset) restricting the species/mass options, e.g.
    # scans to the selected scan_type>); it may also set up extra outputs.
    extra <- if (!is.null(setup_extra)) {
      setup_extra(get_data, input, output, session)
    } else {
      list()
    }
    get_extra_plot_args <- extra$plot_args %||% reactive(list())
    filter_dataset <- extra$filter_dataset

    # RATIOS popover ----
    # the APPLIED ratio settings (only these drive the plot + code; the popover's
    # live inputs are a staged draft committed on "Apply"). Additive offsets are
    # kept per intensity-unit family so switching units preserves each family's
    # configuration. Defaults match isoreader2::ir_calculate_ratios().
    ratio_settings <- reactiveVal(list(
      calculate = FALSE,
      normalize = FALSE,
      num_add = list(V = 100, nA = 0, cps = 0),
      denom_add = list(V = 100, nA = 0, cps = 0)
    ))

    # the additive-offset inputs for the current intensity unit family (V/nA/cps),
    # pre-filled from the applied settings (isolated; the reset-on-open observer
    # below keeps the draft in sync without rebuilding the inputs)
    output$ratios_params <- renderUI({
      fam <- intensity_unit_family(get_units())
      s <- isolate(ratio_settings())
      defaults <- ratio_add_defaults(fam)
      tagList(
        numericInput(
          ns("ratios_num_add"),
          sprintf("num_add.%s", fam),
          value = s$num_add[[fam]] %||% defaults[["num"]],
          step = 1
        ),
        numericInput(
          ns("ratios_denom_add"),
          sprintf("denom_add.%s", fam),
          value = s$denom_add[[fam]] %||% defaults[["denom"]],
          step = 1
        )
      )
    })

    # reset the popover's draft inputs back to the applied settings (run when the
    # popover (re)opens and on "Cancel", so un-applied edits never linger)
    reset_ratio_inputs <- function() {
      s <- ratio_settings()
      fam <- intensity_unit_family(get_units())
      updateCheckboxInput(
        session,
        "ratios_calculate",
        value = isTRUE(s$calculate)
      )
      updateCheckboxInput(
        session,
        "ratios_normalize",
        value = isTRUE(s$normalize)
      )
      updateNumericInput(session, "ratios_num_add", value = s$num_add[[fam]])
      updateNumericInput(
        session,
        "ratios_denom_add",
        value = s$denom_add[[fam]]
      )
    }
    observeEvent(input[["ratios-trigger"]], reset_ratio_inputs())
    observeEvent(input$ratios_cancel, {
      reset_ratio_inputs()
      bslib::toggle_popover("ratios_popover", show = FALSE)
    })
    # Apply: commit the draft inputs into the applied settings (only the current
    # unit family's offsets are touched; the others keep their values)
    observeEvent(input$ratios_apply, {
      s <- ratio_settings()
      fam <- intensity_unit_family(get_units())
      s$calculate <- isTRUE(input$ratios_calculate)
      s$normalize <- isTRUE(input$ratios_normalize)
      if (!is.null(input$ratios_num_add) && !is.na(input$ratios_num_add)) {
        s$num_add[[fam]] <- input$ratios_num_add
      }
      if (!is.null(input$ratios_denom_add) && !is.na(input$ratios_denom_add)) {
        s$denom_add[[fam]] <- input$ratios_denom_add
      }
      ratio_settings(s)
      bslib::toggle_popover("ratios_popover", show = FALSE)
    })

    # the ir_calculate_ratios() arguments for the applied settings + current units
    # (NULL when ratios are off); used to compute ratios for the plot and to drive
    # the generated aggregate step
    get_ratio_calc <- reactive(ratio_calc_params(ratio_settings(), get_units()))

    # the aggregated data the plot works on: the file server's data plus, when the
    # Ratios popover requests it, the ir_calculate_ratios() `ratio_name`/`ratio`
    # columns (computed with the applied offsets/normalization for the current units)
    get_ratio_data <- reactive({
      agg <- get_data()
      if (is.null(agg)) {
        return(NULL)
      }
      params <- get_ratio_calc()
      if (is.null(params)) {
        return(agg)
      }
      if (isTRUE(params$normalize_ratios)) {
        params$normalize_ratios <- stats::median
      }
      # call as a normal isoreader2:: call (not do.call on the function value, which
      # makes isoreader2's progress message deparse the whole function definition);
      # sit at 50% so the user sees the (potentially slow) calculation is running
      out <- withProgress(
        message = "Calculating ratios",
        detail = "This may take a moment...",
        value = 0.5,
        rlang::inject(isoreader2::ir_calculate_ratios(agg, !!!params))
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)
      out$result %||% agg
    })

    # facet / color / linetype aesthetic dropdowns. Choices are species/mass/trace
    # plus the data's metadata columns; rendered server-side (data-dependent) and
    # only once data is loaded so the function defaults (facet=file_name,
    # color=trace, linetype=none) are valid choices. Numeric and date/time metadata
    # columns are factor()-wrapped when used as a discrete aesthetic (see
    # aes_factor_cols + the plot/code below).
    # the selected data's metadata, restricted to columns that actually carry a
    # value for the current selection: the app's selection filter keeps every
    # column (NA where it doesn't apply), so we drop the all-NA ones here -- the
    # same columns ir_filter_metadata() keeps -- so the aes dropdowns only offer
    # metadata present in the selected files/analyses.
    aes_metadata <- reactive({
      md <- get_data()$metadata
      if (is.null(md)) {
        return(NULL)
      }
      md[, vapply(md, function(x) !all(is.na(x)), logical(1)), drop = FALSE]
    })
    aes_factor_cols <- reactive({
      md <- aes_metadata()
      if (is.null(md)) {
        return(character(0))
      }
      names(md)[vapply(
        md,
        function(x) is.numeric(x) || inherits(x, c("POSIXt", "Date")),
        logical(1)
      )]
    })
    # "data_type" (intensity vs. ratios, added by the plot function) is offered
    # alongside species/mass/trace; faceting defaults to file_name (the plot
    # functions split intensities/ratios into rows automatically via their
    # data_type_as_facet = auto() option, regardless of the facet column)
    output$aes_options <- renderUI({
      md <- aes_metadata()
      req(md)
      ch <- c(
        "species",
        "mass",
        "trace",
        "data_type",
        setdiff(names(md), "file_path")
      )
      tagList(
        selectInput(
          ns("facet"),
          "Facet by:",
          choices = c("(none)", ch),
          selected = isolate(input$facet) %||% "file_name"
        ),
        selectInput(
          ns("color"),
          "Color by:",
          choices = c("(none)", ch),
          selected = isolate(input$color) %||% "trace"
        ),
        selectInput(
          ns("linetype"),
          "Linetype by:",
          choices = c("(none)", ch),
          selected = isolate(input$linetype) %||% "(none)"
        )
      )
    })

    # species -> available masses, de-duplicated so a units change (which
    # re-aggregates but keeps the same species/masses) doesn't rebuild the
    # popovers (a rebuild briefly leaves duplicate bound inputs). `filter_dataset`
    # (if provided) restricts the options, e.g. scans to the selected scan_type.
    species_masses_raw <- reactive({
      dataset <- get_ratio_data()[[dataset_key]]
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
          ratios_input <- paste0(species, "-ratios")
          # Hide: clear both the mass and ratio checkbox groups
          observeEvent(
            input[[paste0(species, "-hide")]],
            {
              updateCheckboxGroupInput(
                session,
                species,
                selected = character(0)
              )
              updateCheckboxGroupInput(
                session,
                ratios_input,
                selected = character(0)
              )
              hidden[[species]] <- TRUE
              hidden_ratios[[species]] <- TRUE
            },
            ignoreInit = TRUE
          )
          # Show: re-select every mass and every ratio for the species
          observeEvent(
            input[[paste0(species, "-show")]],
            {
              g_now <- species_masses()
              i <- which(g_now$species == species)
              updateCheckboxGroupInput(
                session,
                species,
                selected = g_now$masses[[i]]
              )
              updateCheckboxGroupInput(
                session,
                ratios_input,
                selected = g_now$ratios[[i]]
              )
              hidden[[species]] <- NULL
              hidden_ratios[[species]] <- NULL
            },
            ignoreInit = TRUE
          )
          # track explicit emptying of the mass / ratio groups (NULL = nothing
          # checked) so a rebuild remembers it; the two are independent
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
          observeEvent(
            input[[ratios_input]],
            {
              if (is.null(input[[ratios_input]])) {
                hidden_ratios[[species]] <- TRUE
              } else {
                hidden_ratios[[species]] <- NULL
              }
            },
            ignoreNULL = FALSE,
            ignoreInit = TRUE
          )
        })
      }
      handlers_for(union(done, g$species))
    })

    # one popover button per species (Show/Hide + a mass checkbox group, plus a
    # ratio checkbox group when ratios are available, named by the species so
    # input[[species]] / hidden[[species]] -- and the "-ratios" variants -- align)
    output$species_buttons <- renderUI({
      g <- species_masses()
      validate(need(!is.null(g) && nrow(g) > 0, no_data_message))
      buttons <- lapply(seq_len(nrow(g)), function(i) {
        species <- g$species[i]
        species_masses_i <- g$masses[[i]]
        species_ratios_i <- g$ratios[[i]]
        has_ratios <- length(species_ratios_i) > 0
        mass_group <- tagList(
          if (has_ratios) {
            tags$small(class = "text-muted d-block", "Masses")
          },
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
        )
        ratio_group <- if (has_ratios) {
          tagList(
            tags$small(class = "text-muted d-block", "Ratios"),
            checkboxGroupInput(
              ns(paste0(species, "-ratios")),
              label = NULL,
              choices = species_ratios_i,
              selected = if (isTRUE(isolate(hidden_ratios[[species]]))) {
                character(0)
              } else {
                species_ratios_i
              }
            )
          )
        }
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
            mass_group,
            ratio_group
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

    # the selected ratio names (across species), same logic as the masses: a
    # hidden ratio group contributes nothing, an untouched one counts as all of
    # its ratios. Species without ratios are skipped.
    get_selected_ratios <- reactive({
      g <- species_masses()
      if (is.null(g) || nrow(g) == 0) {
        return(character(0))
      }
      out <- character(0)
      for (i in seq_len(nrow(g))) {
        ratios_i <- g$ratios[[i]]
        if (length(ratios_i) == 0) {
          next
        }
        species <- g$species[i]
        chosen <- if (isTRUE(hidden_ratios[[species]])) {
          character(0)
        } else {
          input[[paste0(species, "-ratios")]] %||% ratios_i
        }
        out <- c(out, chosen)
      }
      unique(out)
    })

    # the ratio names actually passed to the plot function / generated code: the
    # selection restricted to ratios whose numerator-mass rows survive the current
    # mass selection (and, for scans, the scan-type filter), so the plot's `ratio=`
    # argument never names a ratio with no data
    get_plot_ratios <- reactive({
      sel <- get_selected_ratios()
      if (length(sel) == 0) {
        return(character(0))
      }
      dataset <- get_ratio_data()[[dataset_key]]
      if (is.null(dataset) || nrow(dataset) == 0) {
        return(character(0))
      }
      if (!is.null(filter_dataset)) {
        dataset <- filter_dataset(dataset)
      }
      filtered <- filter_by_selected_masses(dataset, get_selected_masses())
      plottable_ratios(filtered, sel)
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
        add_to_zoom_stack(
          z$x_min + direction * span,
          z$x_max + direction * span
        )
      }
    }
    observeEvent(input$zoom_move_left, move_zoom(-1))
    observeEvent(input$zoom_move_right, move_zoom(+1))
    # reset zoom when the plotted data changes (units or file selection) or the
    # extra args change (e.g. the scans scan_type, whose x-axis range differs
    # entirely between scan types)
    observe({
      get_data()
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
      # functions that take one (cf `time_window.s`, di `cycle_window`, scans
      # `x_window`); these filter the data and rescale y.
      extra <- get_extra_plot_args()
      if (!is.null(zoom_arg) && !is.null(window)) {
        extra[[zoom_arg]] <- window
      }
      # plot-function passthrough option (forwarded to plot_fn via build_data_plot)
      if (isTRUE(input$drop_unused_levels)) {
        extra$drop_unused_levels <- TRUE
      }
      # scales / short_time_labels are plain-value args (pass via extra/...)
      extra$scales <- input$scales %||% "free"
      if (isTRUE(input$short_time_labels)) {
        extra$short_time_labels <- TRUE
      }
      # facet / color / linetype are tidy-eval aesthetics -> build them as quosures
      # (symbols, factor()-wrapped for numeric columns) so build_data_plot can
      # inject them and the columns are evaluated as variables, not strings
      fcols <- aes_factor_cols()
      aes_quo <- function(sel, default) {
        v <- sel %||% default
        if (is.null(v) || identical(v, "(none)")) {
          return(NULL)
        }
        s <- rlang::sym(v)
        if (v %in% fcols) rlang::quo(factor(!!s)) else rlang::quo(!!s)
      }
      aes_args <- list(
        facet = aes_quo(input$facet, "file_name"),
        color = aes_quo(input$color, "trace")
      )
      linetype_quo <- aes_quo(input$linetype, NULL)
      if (!is.null(linetype_quo)) {
        aes_args$linetype <- linetype_quo
      }
      out <- do.call(
        build_data_plot,
        c(
          list(
            get_ratio_data(),
            dataset_key = dataset_key,
            selected_masses = get_selected_masses(),
            plot_fn = plot_fn,
            font_size = input$font_size %||% 16,
            scientific = input$scientific,
            legend_position = input$legend_position %||% "right",
            aes_args = aes_args,
            selected_ratios = get_plot_ratios()
          ),
          extra
        )
      ) |>
        try_catch_cnds()
      out |> log_cnds(ns = ns)

      p <- out$result
      values$valid_plot <- !is.null(p)
      if (is.null(zoom_arg) && !is.null(p) && !is.null(window)) {
        # no plot-function window arg: zoom the view post-hoc (does not rescale y)
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

    # CODE GENERATION =====
    # the plot step for the code server: subset the (already-aggregated) input
    # variable to the selected files/analyses with ir_filter_metadata(), then plot,
    # reflecting the current controls -- scan_type and other extras, the zoom
    # window, a strict mass subset, scientific notation, and the font size / legend
    # position (folded into the plot's `theme` argument, the way build_data_plot()
    # applies them). A terminal node (output = NULL).
    get_code <- function(input_var = NULL) {
      data_var <- input_var %||% "data"

      # file/analysis selection -> ir_filter_metadata() (dropped when all selected)
      filter_part <- NULL
      if (!is.null(get_selection) && !is.null(get_all_metadata)) {
        expr <- code_metadata_filter(get_selection(), get_all_metadata())
        if (!is.null(expr)) {
          filter_part <- code_call("ir_filter_metadata", list(code_raw(expr)))
        }
      }

      plot_args <- list()
      # module extras (e.g. scans scan_type); the zoom window is added separately
      extra <- get_extra_plot_args()
      for (nm in names(extra)) {
        if (!identical(nm, zoom_arg) && !is.null(extra[[nm]])) {
          plot_args[[nm]] <- extra[[nm]]
        }
      }
      # zoom window -> the plot function's window argument (cf/scans)
      z <- get_last_zoom()
      if (!is.null(zoom_arg) && !is.null(z$x_min) && !is.null(z$x_max)) {
        plot_args[[zoom_arg]] <- round(c(z$x_min, z$x_max), 1)
      }
      # species / mass selection (see select_species_or_mass): `species=` when
      # whole species were de-selected, `mass=` when narrowed within a species
      plot_args <- c(
        plot_args,
        select_species_or_mass(get_selected_masses(), species_masses())
      )
      # ratio traces (added by ir_calculate_ratios()): plotted via `ratio=`, which
      # defaults to none in the plot functions, so it is emitted whenever any ratio
      # is selected (all of them by default)
      ratios <- get_plot_ratios()
      if (length(ratios) > 0) {
        plot_args$ratio <- ratios
      }
      # facet / color / linetype aesthetics; numeric metadata columns are
      # factor()-wrapped. The plot functions default to facet = NULL (they split
      # intensities/ratios automatically), so facet is emitted for every choice
      # except "(none)", which matches that NULL default. color (default trace) and
      # linetype (default none) emit only when changed; "(none)" color -> `= NULL`.
      fcols <- aes_factor_cols()
      facet <- input$facet %||% "file_name"
      if (!identical(facet, "(none)")) {
        plot_args$facet <- code_raw(code_aes_value(facet, fcols))
      }
      color <- input$color %||% "trace"
      if (identical(color, "(none)")) {
        plot_args$color <- code_raw("NULL")
      } else if (!identical(color, "trace")) {
        plot_args$color <- code_raw(code_aes_value(color, fcols))
      }
      linetype <- input$linetype %||% "(none)"
      if (!identical(linetype, "(none)")) {
        plot_args$linetype <- code_raw(code_aes_value(linetype, fcols))
      }
      # facet scales (default "free")
      scales <- input$scales %||% "free"
      if (!identical(scales, "free")) {
        plot_args$scales <- scales
      }
      # scientific notation (default off)
      if (isTRUE(input$scientific)) {
        plot_args$scientific <- TRUE
      }
      # drop unused factor levels (default off)
      if (isTRUE(input$drop_unused_levels)) {
        plot_args$drop_unused_levels <- TRUE
      }
      # short_time_labels: continuous flow only (dataset_key == "traces")
      if (identical(dataset_key, "traces") && isTRUE(input$short_time_labels)) {
        plot_args$short_time_labels <- TRUE
      }
      plot_code <- code_pipe(
        data_var,
        filter_part,
        code_call(plot_fn_name, plot_args)
      )

      # font size + legend position as a proper ggplot `+ theme(...)` addition
      # (NOT the plot function's theme= arg); one line per option when several
      theme_args <- list()
      fs <- input$font_size %||% 16
      lp <- input$legend_position %||% "right"
      if (!identical(as.numeric(fs), 16)) {
        theme_args$text <- code_raw(paste0("element_text(size = ", fs, ")"))
      }
      if (!identical(lp, "right")) {
        theme_args[["legend.position"]] <- if (identical(lp, "hide")) {
          "none"
        } else {
          lp
        }
        # a bottom/top legend is laid out vertically (matches build_data_plot)
        if (lp %in% c("bottom", "top")) {
          theme_args[["legend.direction"]] <- "vertical"
        }
      }
      if (length(theme_args) > 0) {
        parts <- vapply(
          names(theme_args),
          function(nm) code_arg(nm, theme_args[[nm]]),
          character(1)
        )
        theme_call <- if (length(parts) == 1L) {
          paste0("theme(", parts, ")")
        } else {
          paste0("theme(\n  ", paste(parts, collapse = ",\n  "), "\n)")
        }
        plot_code <- paste0(
          plot_code,
          " +\n  ",
          gsub("\n", "\n  ", theme_call, fixed = TRUE)
        )
      }

      list(code = plot_code, output = NULL)
    }

    # `get_ratio_calc` lets the (separately registered) aggregate code step emit a
    # matching ir_calculate_ratios() call (NULL when ratios are off)
    list(get_code = get_code, get_ratio_calc = get_ratio_calc)
  })
}
