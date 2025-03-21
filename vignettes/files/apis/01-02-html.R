#* @get /hello
#* @serializer html
function() {
  list(
    body = list(
      h1 = "hello world"
    )
  )
}
