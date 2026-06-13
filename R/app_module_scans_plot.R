# Scans plot UI + server =====
# thin wrapper over the shared data-plot framework (app_plots.R): plots the
# aggregated `scans` with ir_plot_scans, plus a scan-type popover that lists the
# scan types present in the data (ir_plot_scans plots one type at a time).

scans_plot_ui <- function(id) {
  ns <- NS(id)
  data_plot_view_ui(
    id,
    extra_left = bslib::popover(
      actionButton(
        ns("scan_type-trigger"),
        "Scan type",
        icon = icon("caret-down")
      ),
      title = "Scan type",
      div(style = "width: 10rem;", uiOutput(ns("scan_type_input"))),
      options = list(trigger = "focus")
    )
  )
}

scans_plot_server <- function(id, get_isofiles) {
  setup_data_plot(
    id,
    get_isofiles = get_isofiles,
    dataset_key = "scans",
    plot_fn = isoreader2::ir_plot_scans,
    no_data_message = "No scan data available.",
    download_basename = "scans",
    zoom_arg = "x_window",
    setup_extra = function(get_aggregated_data, input, output, session) {
      ns <- session$ns

      # scan types live in metadata (ir_plot_scans joins them into the scans);
      # drop NA, which marks the non-scan files in the same collection
      get_scan_types <- reactive({
        metadata <- get_aggregated_data()$metadata
        if (is.null(metadata) || !"scan_type" %in% names(metadata)) {
          return(character(0))
        }
        types <- unique(metadata$scan_type)
        types[!is.na(types)]
      })

      # effective scan type: input is NULL until the popover opens -> default to
      # the first (ir_plot_scans also errors if scan_type is NULL with >1 type)
      get_scan_type <- reactive({
        types <- get_scan_types()
        input$scan_type %||% (if (length(types) > 0) types[1] else NULL)
      })

      output$scan_type_input <- renderUI({
        types <- get_scan_types()
        validate(need(length(types) > 0, "No scan types in this data."))
        radioButtons(
          ns("scan_type"),
          label = NULL,
          choices = types,
          selected = types[1]
        )
      })

      list(
        plot_args = reactive(list(scan_type = get_scan_type())),
        # restrict the species/mass options to the files of the selected scan
        # type (scan_type lives in metadata; semi-join the scans to those files)
        filter_dataset = function(dataset) {
          st <- get_scan_type()
          metadata <- get_aggregated_data()$metadata
          if (
            is.null(st) ||
              is.null(metadata) ||
              !"scan_type" %in% names(metadata)
          ) {
            return(dataset)
          }
          keep <- dplyr::filter(
            metadata,
            !is.na(.data$scan_type) & .data$scan_type == st
          )
          dplyr::semi_join(dataset, keep, by = c("uidx", "analysis"))
        }
      )
    }
  )
}
