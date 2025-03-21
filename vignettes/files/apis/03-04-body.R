#* @post /user
function(body) {
  list(
    id = body$id,
    name = body$name
  )
}
