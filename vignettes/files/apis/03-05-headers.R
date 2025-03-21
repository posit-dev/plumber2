#* Return the value of a custom header and the total number of headers
#* @get /
function(request) {
  list(
    val = request$get_header("Custom-Header"),
    n_headers = length(request$headers)
  )
}
