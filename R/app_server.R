# app server: instantiates the central file-management module and wires the
# app-specific modules via `setup_modules(file)`, and handles theme switching.
# `initial_selection` ("all" / "none" / function(metadata)) sets what is
# selected on load (passed through to the file server).
app_server <- function(isofiles, setup_modules, initial_selection = "all") {
  function(input, output, session) {
    # central file management: splits the isofiles by measurement type, owns the
    # shared units + per-type selection, and provides metadata + aggregated data
    # to the other modules. Everything goes through this single instance.
    file <- ie_file_server(
      "files",
      get_isofiles = reactive(isofiles),
      initial_selection = initial_selection
    )

    # app-specific module wiring
    setup_modules(file)

    # theme switching
    current_theme <- reactiveVal(NA_character_)
    observe({
      req(input$theme)
      isolate({
        if (!identical(input$theme, isolate(current_theme()))) {
          current_theme(input$theme)
          log_info(user_msg = paste("Loading theme", input$theme))
          session$setCurrentTheme(app_theme(input$theme))
        }
      })
    })
  }
}
