#* Example of throwing an error
#* @get /simple
function() {
  stop("I'm an error!")
}

#* Generate a friendly error
#* @get /friendly
function() {
  reqres::abort_bad_request(
    "Your request could not be parsed"
  )
}
