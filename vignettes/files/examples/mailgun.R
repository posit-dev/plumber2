emails <- data.frame(
  from = character(0),
  time = character(0),
  subject = character(0)
)

#* @post /mail
function(body) {
  emails <<- rbind(emails, data.frame(
    from = body$from,
    time = date(),
    subject = body$subject
  ))
  Next
}

#* @get /tail
function() {
  tail(emails[,-1], n=5)
}
