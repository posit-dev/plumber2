#* Get letters after a given letter
#* @serializer json
#* @get /boxed
#* @query letter:string("A")
function(query) {
  LETTERS[LETTERS > query$letter]
}

#* Get letters after a given letter
#* @serializer unboxedJSON
#* @get /unboxed
#* @query letter:string("A")
function(query) {
  LETTERS[LETTERS > query$letter]
}
