#* This is an example of an UNSAFE endpoint which
#* is vulnerable to a DOS attack.
#* @get /
#* @query pts:integer(10)
#* @serializer png
function(query) {
  # An example of an UNSAFE endpoint.
  plot(1:query$pts)
}
