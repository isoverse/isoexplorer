#' Central file-management module
#'
#' The single source of truth for the isofiles in an isoexplorer app, and the hub
#' every other module talks to. It maintains a *running* set of read isofiles
#' (seeded from `get_isofiles()`, grown by uploads and watched folders), splits it
#' into the three measurement types (scans / continuous flow / dual inlet) with
#' [isoreader2::ir_filter_for_scans()] and friends, owns the shared intensity-units
#' selection and the per-type file selection, and exposes the per-type metadata
#' (for the [ie_metadata_server()] selector tables) and the selection-filtered
#' aggregated data (for the `*_plot_server()` modules).
#'
#' Selector tables read `get_<type>_metadata()` and push their selection via
#' `set_selected_<type>()`; plot modules read `get_aggregated_<type>_data()` and
#' drive the shared units via `get_units()` / `set_units()`.
#'
#' **Dynamic files.** New files (uploaded, or appearing in `monitoring_folders`)
#' are read with [isoreader2::ir_read_isofiles()] and appended; aggregation is
#' incremental (only new files are read/aggregated, then combined with `c()`), so
#' already-read files are never re-read.
#'
#' **Upload.** When `upload_folder` is set the module owns a navbar upload button
#' (the [ie_file_ui()] placeholder) that opens a modal to upload multiple files or
#' whole folders bundled as `.zip` archives (the picker allows `.zip`, the file
#' types isoreader2 reads, and `.json` -- which covers their `.<type>.json`
#' serializations such as `foo.cf.json`). Uploaded files are stored in
#' `upload_folder` (archives unpacked), and **only the just-uploaded files are
#' read** -- files already present in `upload_folder` when the app started are
#' left untouched. A "Select the uploaded files" checkbox (on by default)
#' exclusively selects the new files in the relevant type's table and, in the full
#' multi-tab app, switches to that type's tab.
#'
#' **Monitoring.** `monitoring_folders` are polled; any isofiles found there with
#' [isoreader2::ir_find_isofiles()] (including files already present at startup)
#' are read and added.
#'
#' **Getting started.** When `examples_folder` and/or `upload_folder` is set but the
#' app is launched without data (empty `get_isofiles()`), a prompt is shown once
#' inviting the user to load the examples and/or upload their own files; an app
#' launched with data never sees it.
#'
#' @param id the module id (namespace)
#' @param get_isofiles a reactive returning an `ir_isofiles` to seed the app with
#'   (already read); `reactive(NULL)` is fine when files only arrive via upload /
#'   monitoring
#' @param initial_selection what is selected, per type, before any selector pushes
#'   a selection. One of `"all"` (default), `"none"`, or a `function(metadata)`
#'   called with a type's metadata tibble that returns the subset of rows to
#'   select (e.g. `\(m) dplyr::filter(m, grepl("std", file_name))`).
#' @param upload_folder directory where uploaded files are stored (created on
#'   demand); `NULL` (the default) means no upload button -- set a directory path
#'   to enable uploads.
#' @param monitoring_folders character vector of folders to watch; isofiles found
#'   there with [isoreader2::ir_find_isofiles()] are read and added automatically.
#'   `NULL` (default) disables monitoring.
#' @param examples_folder directory the "Load examples" navbar button copies the
#'   isoreader2 bundled example files into (and then loads). `NULL` (the default)
#'   means no examples button.
#' @return The "file handle": a list of reactive accessors / setters. For each
#'   `<type>` in `scans` / `cf` / `di`: `get_units()`/`set_units(units)` (shared
#'   intensity units, default "mV"); `get_<type>_metadata()`;
#'   `set_selected_<type>(rows)`; `get_<type>_selection()`;
#'   `get_aggregated_<type>_data()`; plus `get_<type>_select_signal()` (file paths
#'   a selector should select, fired by upload auto-select) and `get_active_type()`
#'   (the type whose tab to activate after an auto-select).
#' @export
ie_file_server <- function(
  id,
  get_isofiles,
  initial_selection = "all",
  upload_folder = NULL,
  monitoring_folders = NULL,
  examples_folder = NULL
) {
  if (!is.function(initial_selection)) {
    initial_selection <- arg_match(initial_selection, c("all", "none"))
  }
  upload_folder |>
    check_arg(
      is.null(upload_folder) || is_scalar_character(upload_folder),
      "must be a directory path or NULL"
    )
  examples_folder |>
    check_arg(
      is.null(examples_folder) || is_scalar_character(examples_folder),
      "must be a directory path or NULL"
    )
  monitoring_folders |>
    check_arg(
      is.null(monitoring_folders) || is.character(monitoring_folders),
      "must be a character vector of folder paths or NULL"
    )
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # shared intensity units (default mV) -- plot modules get/set this
    units <- reactiveVal("mV")
    get_units <- reactive(units())
    set_units <- function(new_units) units(new_units)

    # surface any isoreader2 `$problems` recorded in a read/aggregate result.
    # These are collected internally, NOT thrown as R conditions, so
    # try_catch_cnds() never sees them. We display exactly what ir_show_problems()
    # prints -- its concise, cli-formatted summary -- captured with cli::cli_fmt()
    # (no backtraces) in a modal dialog (line breaks kept via \n -> <br>, like
    # log_error). `last_problems` dedups: a read and its follow-up aggregation, and
    # every units-driven re-aggregation, all re-surface the SAME problems, so we
    # only pop the dialog when a genuinely new problem appears.
    last_problems <- ""
    log_problems <- function(result, user_msg) {
      if (is.null(result) || nrow(isoreader2::ir_get_problems(result)) == 0) {
        return(invisible(NULL))
      }
      summary <- paste(
        cli::cli_fmt(isoreader2::ir_show_problems(result)),
        collapse = "\n"
      )
      if (identical(summary, last_problems)) {
        return(invisible(NULL))
      }
      last_problems <<- summary
      rlog::log_warn(paste0("[", ns(NULL), "] ", user_msg, "\n", summary))
      showModal(modalDialog(
        title = user_msg,
        pre(HTML(gsub("\\n", "<br>", cli::ansi_html(summary)))),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }

    # RUNNING set of read isofiles =====
    # grows as files are added; already-read files (by file_path) are never
    # re-read -- new files are read, then combined onto the running set with c().
    managed <- reactiveVal(NULL)
    current_paths <- function() {
      m <- isolate(managed())
      if (is.null(m)) character(0) else m$file_path
    }
    # append already-read isofiles, keeping only rows whose file_path is new
    merge_isofiles <- function(iso) {
      if (is.null(iso) || nrow(iso) == 0) {
        return(invisible(NULL))
      }
      keep <- iso[!(iso$file_path %in% current_paths()), , drop = FALSE]
      if (nrow(keep) == 0) {
        return(invisible(NULL))
      }
      class(keep) <- class(iso)
      cur <- isolate(managed())
      managed(if (is.null(cur)) keep else c(cur, keep))
      invisible(keep)
    }
    # read the paths that aren't already read and append; returns the new isofiles
    add_paths <- function(paths) {
      new <- setdiff(unique(paths), current_paths())
      if (length(new) == 0) {
        return(invisible(NULL))
      }
      # reading can take a while for many/large files; show a progress indicator
      # so the user knows the upload / load is being processed (the file picker's
      # own bar only covers the browser->server transfer, not the read)
      out <- withProgress(
        message = format_inline("Loading {length(new)} data file{?s}"),
        value = 0.5,
        isoreader2::ir_read_isofiles(new, show_progress = FALSE) |>
          try_catch_cnds()
      )
      out |> log_cnds(ns = ns)
      log_problems(out$result, "Problem(s) reading data files")
      merge_isofiles(out$result)
    }

    # seed from / merge in the externally provided isofiles (already read)
    observeEvent(
      get_isofiles(),
      merge_isofiles(get_isofiles()),
      ignoreNULL = TRUE
    )

    # per-type isofiles: filter the running set; NULL when a type has no files
    # (aggregating a 0-row isofiles errors)
    split_for <- function(filter_fn) {
      reactive({
        m <- managed()
        if (is.null(m) || nrow(m) == 0) {
          return(NULL)
        }
        out <- filter_fn(m) |> try_catch_cnds()
        out |> log_cnds(ns = ns)
        res <- out$result
        if (is.null(res) || nrow(res) == 0) NULL else res
      })
    }
    scans_isofiles <- split_for(isoreader2::ir_filter_for_scans)
    cf_isofiles <- split_for(isoreader2::ir_filter_for_continuous_flow)
    di_isofiles <- split_for(isoreader2::ir_filter_for_dual_inlet)

    # INCREMENTAL aggregation =====
    # a running ir_aggregated_data for get_subset(): only files whose file_path is
    # new since the last run are aggregated (then c()-ed on); a change in
    # reset_key() (e.g. units) forces a full re-aggregation from scratch.
    incremental_agg <- function(
      get_subset,
      aggregate,
      reset_key = reactive(NULL)
    ) {
      out <- reactiveVal(NULL)
      state <- list(paths = character(0), key = NULL)
      observe({
        iso <- get_subset()
        key <- reset_key()
        isolate({
          if (is.null(iso) || nrow(iso) == 0) {
            out(NULL)
            state <<- list(paths = character(0), key = key)
            return()
          }
          if (!identical(state$key, key)) {
            res <- aggregate(iso) |> try_catch_cnds()
            res |> log_cnds(ns = ns)
            log_problems(res$result, "Problem(s) aggregating data")
            out(res$result)
            state <<- list(paths = iso$file_path, key = key)
            return()
          }
          new <- setdiff(iso$file_path, state$paths)
          if (length(new) == 0) {
            return()
          }
          new_iso <- iso[iso$file_path %in% new, , drop = FALSE]
          class(new_iso) <- class(iso)
          res <- aggregate(new_iso) |> try_catch_cnds()
          res |> log_cnds(ns = ns)
          log_problems(res$result, "Problem(s) aggregating data")
          cur <- out()
          out(if (is.null(cur)) res$result else c(cur, res$result))
          state <<- list(paths = iso$file_path, key = key)
        })
      })
      out
    }
    meta_agg_for <- function(get_subset) {
      incremental_agg(get_subset, function(iso) {
        isoreader2::ir_aggregate_isofiles(iso, aggregator = "metadata")
      })
    }
    data_agg_for <- function(get_subset, type_label) {
      incremental_agg(
        get_subset,
        function(iso) {
          log_info(
            ns = ns,
            user_msg = paste0(
              "Aggregating ",
              type_label,
              " data with intensity units ",
              get_units()
            )
          )
          isoreader2::ir_aggregate_isofiles(iso, intensity_units = get_units())
        },
        reset_key = get_units
      )
    }
    scans_meta_agg <- meta_agg_for(scans_isofiles)
    cf_meta_agg <- meta_agg_for(cf_isofiles)
    di_meta_agg <- meta_agg_for(di_isofiles)
    scans_data_agg <- data_agg_for(scans_isofiles, "scans")
    cf_data_agg <- data_agg_for(cf_isofiles, "continuous flow")
    di_data_agg <- data_agg_for(di_isofiles, "dual inlet")

    metadata_of <- function(get_agg) {
      reactive({
        a <- get_agg()
        if (is.null(a)) NULL else a$metadata
      })
    }
    get_scans_metadata <- metadata_of(scans_meta_agg)
    get_cf_metadata <- metadata_of(cf_meta_agg)
    get_di_metadata <- metadata_of(di_meta_agg)

    # resolve the configured `initial_selection` for a type, given its metadata
    resolve_initial_selection <- function(metadata) {
      if (is.function(initial_selection)) {
        if (is.null(metadata)) {
          return(NULL)
        }
        return(initial_selection(metadata))
      }
      switch(initial_selection, all = NULL, none = tibble::tibble())
    }

    # per-type selection: a user override (NULL until a selector pushes something)
    # on top of `initial_selection` (NULL = all, 0-row = none, else filter by uidx)
    make_selection <- function(get_type_metadata) {
      override <- reactiveVal(NULL)
      list(
        get = reactive({
          if (!is.null(override())) {
            override()
          } else {
            resolve_initial_selection(get_type_metadata())
          }
        }),
        set = function(x) override(x)
      )
    }
    scans_selection <- make_selection(get_scans_metadata)
    cf_selection <- make_selection(get_cf_metadata)
    di_selection <- make_selection(get_di_metadata)

    # the selection-filtered aggregated data the plot modules consume
    aggregated_for <- function(get_data_agg, get_selected) {
      reactive({
        agg <- get_data_agg()
        if (is.null(agg)) {
          return(NULL)
        }
        selected <- get_selected()
        if (is.null(selected)) {
          return(agg) # nothing pushed yet -> all files
        }
        filter_agg_data_by_metadata(agg, selected) # NULL when 0 rows -> empty
      })
    }

    # FOLDER MONITORING (optional) =====
    if (length(monitoring_folders) > 0) {
      find_all <- function() {
        unlist(lapply(monitoring_folders, function(f) {
          if (dir.exists(f)) isoreader2::ir_find_isofiles(f) else character(0)
        }))
      }
      monitored <- reactivePoll(
        intervalMillis = 2000,
        session = session,
        checkFunc = function() paste(sort(find_all()), collapse = "|"),
        valueFunc = find_all
      )
      observe(add_paths(monitored()))
    }

    # AUTO-SELECT signals (upload) =====
    # `active_type` carries list(type, n) so re-uploads of the same type re-fire;
    # each `<type>_select_signal` carries the file_paths a selector should select.
    active_type <- reactiveVal(NULL)
    active_n <- 0L
    set_active_type <- function(type) {
      active_n <<- active_n + 1L
      active_type(list(type = type, n = active_n))
    }
    select_signals <- list(
      scans = reactiveVal(NULL),
      cf = reactiveVal(NULL),
      di = reactiveVal(NULL)
    )
    type_filters <- list(
      scans = isoreader2::ir_filter_for_scans,
      cf = isoreader2::ir_filter_for_continuous_flow,
      di = isoreader2::ir_filter_for_dual_inlet
    )

    # LOAD EXAMPLES (optional) =====
    # a navbar button that copies the isoreader2 bundled example files into
    # `examples_folder` and loads everything in it; rendered only when set
    output$examples_button <- renderUI({
      if (is.null(examples_folder)) {
        return(NULL)
      }
      actionButton(
        ns("load_examples"),
        "Load examples",
        icon = icon("flask"),
        class = "btn-sm"
      )
    })

    do_load_examples <- function() {
      # copy the isoreader2 bundled examples into examples_folder, then read them
      isoreader2::ir_copy_examples(examples_folder)
      new_iso <- add_paths(isoreader2::ir_find_isofiles(examples_folder))
      n <- if (is.null(new_iso)) 0L else nrow(new_iso)
      log_info(
        ns = ns,
        user_msg = sprintf(
          "Loaded %d example file%s",
          n,
          if (n == 1L) "" else "s"
        )
      )
    }
    observeEvent(input$load_examples, do_load_examples())

    # FILE UPLOAD (optional) =====
    output$upload_button <- renderUI({
      if (is.null(upload_folder)) {
        return(NULL)
      }
      actionButton(
        ns("upload"),
        "Upload",
        icon = icon("upload"),
        class = "btn-sm"
      )
    })

    show_upload_modal <- function() {
      showModal(modalDialog(
        title = "Upload data files",
        p(
          "Upload multiple files at once, or whole folders bundled as ",
          tags$code(".zip"),
          " archives (the archive contents are unpacked)."
        ),
        fileInput(
          ns("upload_files"),
          label = NULL,
          multiple = TRUE,
          accept = app_upload_accept(),
          width = "100%",
          buttonLabel = "Browse...",
          placeholder = "No files selected"
        ),
        checkboxInput(
          ns("upload_autoselect"),
          "Select the uploaded files",
          value = TRUE
        ),
        footer = modalButton("Close"),
        easyClose = TRUE
      ))
    }
    observeEvent(input$upload, show_upload_modal())

    observeEvent(input$upload_files, {
      uploads <- input$upload_files
      req(uploads)
      if (!dir.exists(upload_folder)) {
        dir.create(upload_folder, recursive = TRUE)
      }
      # remember exactly the files written by THIS upload, so files that already
      # existed in upload_folder are not loaded
      written <- character(0)
      for (i in seq_len(nrow(uploads))) {
        nm <- uploads$name[i]
        src <- uploads$datapath[i]
        if (grepl("\\.zip$", nm, ignore.case = TRUE)) {
          written <- c(written, utils::unzip(src, exdir = upload_folder))
        } else {
          dest <- file.path(upload_folder, nm)
          file.copy(src, dest, overwrite = TRUE)
          written <- c(written, dest)
        }
      }
      removeModal()
      # read the just-uploaded isofiles: ir_find_isofiles detects/canonicalizes
      # everything in the folder (incl. `.json` variants -- foo.cf.json canonicalizes
      # to foo.cf, which ir_read_isofiles resolves); intersecting with the files we
      # just wrote (canonicalized the same way by dropping a trailing .json) loads
      # ONLY the uploads, leaving any pre-existing folder contents untouched.
      new_iso <- add_paths(intersect(
        isoreader2::ir_find_isofiles(upload_folder),
        sub("\\.json$", "", written)
      ))
      n_new <- if (is.null(new_iso)) 0L else nrow(new_iso)
      log_info(
        ns = ns,
        user_msg = sprintf(
          "Uploaded %d file%s",
          n_new,
          if (n_new == 1L) "" else "s"
        )
      )
      # auto-select the uploaded files (per type) + switch to their tab
      if (isTRUE(input$upload_autoselect) && n_new > 0) {
        first_type <- NULL
        for (type in names(type_filters)) {
          sub <- type_filters[[type]](new_iso)
          if (!is.null(sub) && nrow(sub) > 0) {
            select_signals[[type]](sub$file_path)
            if (is.null(first_type)) first_type <- type
          }
        }
        if (!is.null(first_type)) set_active_type(first_type)
      }
    })

    # STARTUP PROMPT (optional) =====
    # if examples and/or uploads are enabled but the app was launched without data,
    # prompt the user (once) to get started. We key off get_isofiles() -- the only
    # startup seed -- so the check is race-free; an app launched with data skips it.
    # The buttons reuse the same load / upload actions as the navbar buttons.
    if (!is.null(examples_folder) || !is.null(upload_folder)) {
      observeEvent(
        get_isofiles(),
        {
          seed <- get_isofiles()
          if (is.null(seed) || nrow(seed) == 0) {
            actions <- list()
            if (!is.null(examples_folder)) {
              actions <- c(
                actions,
                list(actionButton(
                  ns("prompt_load_examples"),
                  "Load examples",
                  icon = icon("flask")
                ))
              )
            }
            if (!is.null(upload_folder)) {
              actions <- c(
                actions,
                list(actionButton(
                  ns("prompt_upload"),
                  "Upload files",
                  icon = icon("upload")
                ))
              )
            }
            intro <- if (!is.null(examples_folder) && !is.null(upload_folder)) {
              "To get started, load the bundled example files, or upload your own data files."
            } else if (!is.null(examples_folder)) {
              "Load the bundled example files to get started."
            } else {
              "Upload your own data files to get started."
            }
            showModal(modalDialog(
              title = "No data loaded yet",
              p(intro),
              footer = do.call(tagList, c(actions, list(modalButton("Close")))),
              easyClose = TRUE
            ))
          }
        },
        ignoreNULL = FALSE,
        once = TRUE
      )
    }
    observeEvent(input$prompt_load_examples, {
      removeModal()
      do_load_examples()
    })
    observeEvent(input$prompt_upload, show_upload_modal())

    # whether any loaded file came from the examples / upload folder -- the code
    # server uses these to choose the read folders ("examples" + ir_copy_examples,
    # and/or "data") in the generated read step
    get_examples_loaded <- reactive({
      m <- managed()
      if (is.null(m) || nrow(m) == 0) {
        return(FALSE)
      }
      any(app_under_folder(m$file_path, examples_folder))
    })
    get_uploads_loaded <- reactive({
      m <- managed()
      if (is.null(m) || nrow(m) == 0) {
        return(FALSE)
      }
      any(app_under_folder(m$file_path, upload_folder))
    })

    list(
      # whether any loaded file came from the examples / upload folder
      get_examples_loaded = get_examples_loaded,
      get_uploads_loaded = get_uploads_loaded,
      # shared intensity units (default mV)
      get_units = get_units,
      set_units = set_units,
      # per-type metadata for the selector tables
      get_scans_metadata = get_scans_metadata,
      get_cf_metadata = get_cf_metadata,
      get_di_metadata = get_di_metadata,
      # per-type selection setters (selector tables push their selection here)
      set_selected_scans = scans_selection$set,
      set_selected_cf = cf_selection$set,
      set_selected_di = di_selection$set,
      # per-type current (resolved) selection getters
      get_scans_selection = scans_selection$get,
      get_cf_selection = cf_selection$get,
      get_di_selection = di_selection$get,
      # per-type "select these file paths" signal (fired by upload auto-select)
      get_scans_select_signal = reactive(select_signals$scans()),
      get_cf_select_signal = reactive(select_signals$cf()),
      get_di_select_signal = reactive(select_signals$di()),
      # the type whose tab to activate after an auto-select (list(type, n))
      get_active_type = reactive(active_type()),
      # per-type selection-filtered aggregated data for the plot modules
      get_aggregated_scans_data = aggregated_for(
        scans_data_agg,
        scans_selection$get
      ),
      get_aggregated_cf_data = aggregated_for(cf_data_agg, cf_selection$get),
      get_aggregated_di_data = aggregated_for(di_data_agg, di_selection$get)
    )
  })
}

#' @describeIn ie_file_server the navbar placeholders for the "Load examples" and
#'   "Upload" buttons (each rendered only when the server's `examples_folder` /
#'   `upload_folder` is set). Pair with [ie_file_server()] on the same `id`.
#' @export
ie_file_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("examples_button"), inline = TRUE),
    uiOutput(ns("upload_button"), inline = TRUE)
  )
}

# the isofile types isoreader2 can read (the `ir_find_isofiles` defaults)
app_isofile_types <- function() {
  tryCatch(
    eval(formals(isoreader2::ir_find_isofiles)$types),
    error = function(e) character(0)
  )
}

# which of `paths` live inside `folder` (a logical vector). Used to tell where a
# loaded file came from (examples vs upload folder) for the generated read code.
app_under_folder <- function(paths, folder) {
  if (is.null(folder) || length(paths) == 0 || !dir.exists(folder)) {
    return(logical(length(paths)))
  }
  nf <- normalizePath(folder, winslash = "/", mustWork = FALSE)
  np <- normalizePath(paths, winslash = "/", mustWork = FALSE)
  startsWith(np, paste0(nf, "/"))
}

# accepted upload extensions for the file picker: .zip, every type isoreader2 can
# read, and a bare ".json" for the `.<type>.json` serializations -- browsers do
# NOT reliably match multi-part extensions like ".cf.json" (they key off the last
# extension), so ".json" is what actually makes those selectable. This is only a
# picker hint; ir_find_isofiles still decides what is ingested server-side.
app_upload_accept <- function() {
  c(".zip", paste0(".", app_isofile_types()), ".json")
}
