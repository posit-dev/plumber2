#* @message
#*
#* @async
function(message) {
  message
}
#* @then
function(result, server) {
  server$set_data("async_message", result)
}
