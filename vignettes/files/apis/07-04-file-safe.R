#* This is an example of an endpoint which
#* checks user input.
#* @get /
#* @query file:string*
function(query) {
  # Strip all "non-word" characters from user input
  sanitizedFile <- gsub("\\W", "", query$file)

  path <- file.path("./datasets", sanitizedFile)
  readLines(path)
}
