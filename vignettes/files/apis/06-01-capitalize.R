#* @put /preferences
#* @body capital:integer*
function(response, body) {
  response$set_cookie("capitalize", body$capital)
}

#* @get /letter
function(request) {
  capitalize <- request$cookies$capitalize

  # Default to lower-case unless user preference is capitalized
  alphabet <- letters

  # The capitalize cookie will initially be empty (NULL)
  if (!is.null(capitalize) && capitalize == "1"){
    alphabet <- LETTERS
  }

  list(
    letter = sample(alphabet, 1)
  )
}
