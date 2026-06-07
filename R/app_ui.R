ie_ui <- function(path, timezone, default_theme) {
  app_title <- "isoexplorer"

  # return ui function (request param required by shiny for bookmarking)
  function(request) {
    bslib::page_navbar(
      title = app_title,
      theme = bslib::bs_theme(
        preset = default_theme,
        version = 5,
        "navbar-brand-font-size" = "1.5rem"
      ),
      fillable = TRUE,
      header = tagList(
        use_app_utils(),
        tags$style(HTML(
          ".centered-pills .nav.nav-pills {justify-content: center;}"
        ))
      ),
      bslib::nav_spacer(),
      bslib::nav_item(bslib::input_dark_mode(id = "color_mode", mode = NULL)),

      bslib::nav_panel(
        title = NULL,
        padding = 0,
        bslib::page_sidebar(
          sidebar = bslib::sidebar(
            open = FALSE,
            h5(
              a(
                "isoexplorer",
                href = "https://github.com/KopfLab/isoexplorer",
                target = "_blank"
              ),
              as.character(packageVersion("isoexplorer")),
              align = "center"
            ),
            selectInput(
              "timezone",
              label = "Timezone",
              choices = OlsonNames(),
              selected = timezone
            ),
            selectInput(
              "theme",
              label = "Theme",
              choices = c(
                "flatly",
                "cosmo",
                "lumen",
                "minty",
                "sandstone",
                "darkly",
                "cyborg",
                "slate",
                "superhero",
                "solar"
              ),
              selected = default_theme
            ),
            selectInput(
              "intensity_units",
              label = "Intensity units",
              choices = c("mV", "V", "fA", "pA", "nA", "µA", "mA", "A", "cps"),
              selected = "mV"
            )
          ),

          metadata_ui("metadata"),
          plots_ui("plots")
        )
      )
    )
  }
}
