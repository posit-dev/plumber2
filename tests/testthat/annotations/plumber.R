#* @plumber
function(api) {
  api <- api() # Overwrite input
  api$set_data("first", TRUE)
  api
}

#* @plumber
function(api) {
  api$set_data("second", TRUE)
  api
}
