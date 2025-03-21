#* Endpoint that bypasses serialization
#* @get /
#* @serializer none
function(response) {
  response$body <- "Literal text here!"
}
