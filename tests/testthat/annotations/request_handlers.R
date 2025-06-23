#* A get endpoint
#*
#* description
#*
#* more description
#*
#* @param test:string test
#* @query test2:[integer] test2 and more
#* @query test3:number* test3
#*
#* @response 200:{mpg:[number], cyl:[integer], disp:[number]} test4
#* @response 404:string test5
#*
#* @get /hello/<test>/
#*
#* @download testfile
#*
#* @tag my_tag
#*
#* @serializer csv{col_names = FALSE}
#* @serializer ...
#*
#* @serializerStrict
#*
function(test) {
  mtcars[, 1:3]
}

#* A post endpoint
#*
#* @body test:integer an integer
#* @body test2:[string] an array of strings
#*
#* @post /hello/
#*
#* @parser json
#* @parser yaml
#*
function(body) {
  body
}

#* An async endpoint
#*
#* @get /plot/
#*
#* @serializer svg
#* @serializer ...
#*
#* @async
#*
function() {
  plot.new()
  points(seq(0, 1, by = 0.1), seq(0, 1, by = 0.1))
}

#* A header endpoint
#*
#* @any /header/
#*
#* @header
#*
function() {
  abort_status(406)
}

#* A nodoc endpoint
#*
#* Lorem ipsum
#*
#* @query test:date
#* @get /invisible
#*
#* @noDoc
function() {
  Next
}

#* An endpoint to test type casting
#*
#* @param param:enum|a, b, c|
#* @query required:string*
#* @query range:integer|2, 9|
#* @query default:string("test")
#* @query array:[string]
#* @query regex:pattern|\d-\d{2}|
#* @body str:string
#* @body upper:number|,10|(5)
#* @body today:date
#* @body now:date-time
#* @body data:byte
#* @body flag:boolean
#*
#* @post /type/<param>/
#*
#* @serializer rds
#* @parser rds
function(param, query, body) {
  list(
    param = param,
    query = query,
    body = body
  )
}
