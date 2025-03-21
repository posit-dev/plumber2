#* @get /
function(query) {
  paste0("The q parameter is '", query$q %||% "", "'. ",
         "The pretty parameter is '", query$pretty %||% 0, "'.")
}
