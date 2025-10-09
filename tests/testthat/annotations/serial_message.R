#* @message
#*
function(message, server) {
  server$set_data("message", message)
}
