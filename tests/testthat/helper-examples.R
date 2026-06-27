# shared test helpers for reading the isoreader2 example files

# isoreader2 must be attached so its .onAttach registers the aggregators
attach_isoreader2 <- function() {
  if (!"isoreader2" %in% .packages()) {
    suppressMessages(library(isoreader2))
  }
}

# read the bundled continuous-flow example files, or skip the calling test (e.g.
# when the external isoextract helper is not available)
read_cf_examples <- function() {
  attach_isoreader2()
  tmp <- tempfile("ie_examples")
  dir.create(tmp)
  iso <- tryCatch(
    {
      isoreader2::ir_copy_examples(tmp)
      isoreader2::ir_read_isofiles(
        isoreader2::ir_find_continuous_flow(tmp),
        show_progress = FALSE
      )
    },
    error = function(e) NULL
  )
  testthat::skip_if(
    is.null(iso) || nrow(iso) == 0,
    "no readable isoreader2 example files (isoextract not available?)"
  )
  iso
}
