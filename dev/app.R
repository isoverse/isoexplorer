# DO NOT CHANGE #
# file used for autoreload during app development
# to use: run devtools::load_all("..") then call ie_explore_isofiles()
devtools::load_all("..")

library(isoreader2)

ie_start_isofiles_server(options = list(port = 5558), upload_folder = "data")
