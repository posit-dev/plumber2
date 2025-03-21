#* This is an example of an safe endpoint which
#* checks user input to avoid a DOS attack
#* @get /
#* @query pts:integer(10)
#* @serializer png
function(query) {
  if (query$pts > 1000 & query$pts < 1){
    reqres::abort_bad_request("pts must be between 1 and 1,000")
  }
  plot(1:pts)
}
