ie_server <- function(isofiles, allow_upload, path, timezone) {
  function(input, output, session) {
    # aggregate isofiles
    get_aggregated_data <- reactive({
      req(!is.null(isofiles))
      req(input$intensity_units)
      log_info(user_msg = paste("Aggregating isofiles with intensity units", input$intensity_units))
      out <- isoreader2::ir_aggregate_isofiles(
        isofiles,
        intensity_units = input$intensity_units
      ) |>
        try_catch_cnds()
      log_cnds(out)
      out$result
    })

    # metadata module
    metadata <- metadata_server("metadata", get_aggregated_data = get_aggregated_data)

    # plots module
    plots <- plots_server(
      "plots",
      get_aggregated_data = get_aggregated_data,
      get_selected_metadata = metadata$get_selected_metadata
    )

    # theme switching
    current_theme <- reactiveVal(NA_character_)
    observe({
      req(input$theme)
      isolate({
        if (!identical(input$theme, isolate(current_theme()))) {
          current_theme(input$theme)
          log_info(user_msg = paste("Loading theme", input$theme))
          session$setCurrentTheme(bslib::bs_theme(
            preset = input$theme,
            version = 5,
            "navbar-brand-font-size" = "1.5rem"
          ))
        }
      })
    })
  }
}
