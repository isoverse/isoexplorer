#' Scans plot module
#'
#' A plot view for scan data, wired to a [ie_file_server()]: it plots the
#' selection-filtered aggregated `scans` with [isoreader2::ir_plot_scans()], with
#' unit, species/mass, legend, zoom and PDF-download controls plus a scan-type
#' popover (scans are plotted one type at a time). Pair `ie_scans_plot_ui()` and
#' `ie_scans_plot_server()` on one `id`.
#'
#' @inheritParams ie_metadata_server
#' @return `ie_scans_plot_ui()` returns a UI element; `ie_scans_plot_server()`
#'   returns a list with a `get_code` generator for the code server (see
#'   [ie_code_server()])
#' @seealso [ie_file_server()], [ie_scans_metadata_server()]
#' @name ie_scans_plot
#' @export
ie_scans_plot_ui <- function(id) {
  ns <- NS(id)
  data_plot_view_ui(
    id,
    extra_left = bslib::popover(
      actionButton(
        ns("scan_type-trigger"),
        textOutput(ns("scan_type_label"), inline = TRUE),
        icon = icon("caret-down")
      ),
      title = "Scan type",
      div(style = "width: 10rem;", uiOutput(ns("scan_type_input"))),
      options = list(trigger = "focus")
    )
  )
}

#' @rdname ie_scans_plot
#' @export
ie_scans_plot_server <- function(id, file) {
  setup_data_plot(
    id,
    get_data = file$get_aggregated_scans_data,
    get_units = file$get_units,
    set_units = file$set_units,
    dataset_key = "scans",
    plot_fn = isoreader2::ir_plot_scans,
    plot_fn_name = "ir_plot_scans",
    no_data_message = "No scan data selected/available.",
    download_basename = "scans",
    zoom_arg = "x_window",
    get_selection = file$get_scans_selection,
    get_all_metadata = file$get_scans_metadata,
    setup_extra = function(get_data, input, output, session) {
      ns <- session$ns

      # scan types live in metadata (ir_plot_scans joins them into the scans);
      # drop NA, which marks the non-scan files in the same collection
      get_scan_types <- reactive({
        metadata <- get_data()$metadata
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

      # the button label shows the current scan type in title case (the radio
      # choices stay as the raw scan_type values)
      output$scan_type_label <- renderText({
        st <- get_scan_type()
        if (is.null(st)) "Scan type" else tools::toTitleCase(st)
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
          metadata <- get_data()$metadata
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
