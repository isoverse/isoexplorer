# DO NOT CHANGE #
# file used for autoreload during app development
# to use: run devtools::load_all("..") then call ie_explore_isofiles()
devtools::load_all("..")

## files single app
# library(isoreader2)
# example_files <-
#   ir_examples_folder() |>
#   ir_find_isofiles() |>
#   ir_read_isofiles()

# # for testing purposes, always use launch = FALSE land detach = FALSE, use log_level trace to get all info
# example_files |>
#   ie_explore_continuous_flow(
#     options = list(port = 5558),
#     launch = FALSE,
#     detach = FALSE,
#     log_level = "TRACE"
#   )

# files server
ie_create_isofiles_server(
  options = list(port = 5558),
  upload_folder = "data",
  temporary_storage = TRUE,
  max_upload_size = 500 * 1024^2 # allow up to 500 MB files
)
