# app server: instantiates the central file-management and code-generation
# modules and wires the app-specific modules via `setup_modules(file, code)`, and
# handles theme switching.
# `initial_selection` ("all" / "none" / function(metadata)) sets what is selected
# on load; `upload_folder` controls the navbar upload button; `monitoring_folders`
# lists folders to watch (all passed through to the file server).
app_server <- function(
  isofiles,
  setup_modules,
  initial_selection = "all",
  upload_folder = NULL,
  monitoring_folders = NULL,
  examples_folder = NULL
) {
  function(input, output, session) {
    # central file management: splits the isofiles by measurement type, owns the
    # shared units + per-type selection, and provides metadata + aggregated data
    # to the other modules. Everything goes through this single instance.
    file <- ie_file_server(
      "files",
      get_isofiles = reactive(isofiles),
      initial_selection = initial_selection,
      upload_folder = upload_folder,
      monitoring_folders = monitoring_folders,
      examples_folder = examples_folder
    )

    # central code generation: each measurement type registers its own read ->
    # aggregate -> plot chain in setup_modules() below. The active navbar tab (its
    # title doubles as the registration `group`) restricts the document to that
    # type; focused apps have no real tabs, so input$ie_navbar is empty and
    # everything registered is shown.
    code <- ie_code_server("code", get_active_group = reactive(input$ie_navbar))

    # app-specific module wiring (older setup_modules(file) closures still work)
    if (length(formals(setup_modules)) >= 2L) {
      setup_modules(file, code)
    } else {
      setup_modules(file)
    }

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
