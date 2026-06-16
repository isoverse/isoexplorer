# bslib theme presets offered in the app (first one is the default)
app_themes <- function() {
  c(
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
  )
}

# intensity unit options offered for aggregation (first one is the default)
app_units <- function() {
  c("mV", "V", "fA", "pA", "nA", "µA", "mA", "A", "cps")
}

# the app's bslib theme for a given preset
app_theme <- function(preset) {
  bslib::bs_theme(
    preset = preset,
    version = 5,
    "navbar-brand-font-size" = "1.5rem"
  )
}

# app UI shell: a navbar with the app content in the body. Supply EITHER `main`
# (a single content area, shown in one untitled panel) OR `nav_panels` (a list of
# titled `bslib::nav_panel`s shown as a centered tabset in the navbar, e.g. the
# full explorer's per-type tabs). The theme picker, dark-mode toggle and about
# popover always sit at the right of the navbar.
app_ui <- function(
  main = NULL,
  nav_panels = NULL,
  timezone,
  default_theme,
  selected = NULL
) {
  app_title <- "isoexplorer"

  # the right-aligned navbar controls (theme / dark mode / about)
  controls <- list(
    bslib::nav_item(
      selectInput(
        "theme",
        label = NULL,
        choices = app_themes(),
        selected = default_theme,
        width = "120px"
      )
    ),
    bslib::nav_item(bslib::input_dark_mode(id = "color_mode", mode = NULL)),
    # version / about info popup (upper right of the navbar)
    bslib::nav_item(
      bslib::popover(
        actionButton(
          "about",
          label = NULL,
          icon = icon("circle-info"),
          class = "btn-sm"
        ),
        title = "Versions",
        div(
          div(
            a(
              "isoextractor",
              href = "https://github.com/isoverse/IsofileExtractor",
              target = "_blank"
            ),
            " ",
            as.character(isoreader2:::get_isoextract_version())
          ),
          div(
            a(
              "isoreader2",
              href = "https://github.com/isoverse/isoreader2",
              target = "_blank"
            ),
            " ",
            as.character(packageVersion("isoreader2"))
          ),
          div(
            a(
              "isoexplorer",
              href = "https://github.com/isoverse/isoexplorer",
              target = "_blank"
            ),
            " ",
            as.character(packageVersion("isoexplorer"))
          )
        ),
        placement = "bottom",
        options = list(trigger = "focus")
      )
    )
  )

  # navbar body: either a centered tabset of `nav_panels`, or a single untitled
  # panel holding `main`. The spacers center the tabs and push controls right.
  # The file-upload button sits on the left, next to the title (it only shows
  # when the file server was given an upload_folder).
  upload <- list(bslib::nav_item(ie_file_ui("files")))
  body <- if (!is.null(nav_panels)) {
    c(
      upload,
      list(bslib::nav_spacer()),
      nav_panels,
      list(bslib::nav_spacer()),
      controls
    )
  } else {
    c(
      upload,
      list(bslib::nav_spacer()),
      controls,
      list(bslib::nav_panel(title = NULL, padding = 0, main))
    )
  }

  # return ui function (request param required by shiny for bookmarking)
  function(request) {
    do.call(
      bslib::page_navbar,
      c(
        list(
          id = "ie_navbar",
          title = app_title,
          theme = app_theme(default_theme),
          fillable = TRUE,
          selected = selected, # initial tab (NULL -> the first one)
          header = tagList(
            use_app_utils(),
            tags$style(HTML(
              ".centered-pills .nav.nav-pills {justify-content: center;}
              .navbar .form-group {margin-bottom: 0;}"
            ))
          )
        ),
        body
      )
    )
  }
}
