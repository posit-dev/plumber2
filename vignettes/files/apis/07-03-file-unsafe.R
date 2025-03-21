#* This is an example of an UNSAFE endpoint which
#* does not sanitize user input
#* @get /
#* @query file:string*
function(query) {

  # An example of an UNSAFE endpoint.
  path <- file.path("./datasets", query$file)
  readLines(path)
}
