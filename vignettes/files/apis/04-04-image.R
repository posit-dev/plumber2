#* Example of customizing graphical output
#* @serializer png{width = 400, height = 500}
#* @get /
function() {
  plot(1:10)
}
