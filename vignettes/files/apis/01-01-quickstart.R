#* Echo the path parameter
#*
#* @get /echo/<msg>
#*
#* @param msg:string The message to echo back.
#*
function(msg) {
  list(
    msg = paste0("The message is: '", msg, "'")
  )
}

#* Plot the palmer penguins dataset
#*
#* @get /plot
#*
#* @query spec:string If provided, filter the data to only this species
#* (e.g. 'Adelie')
#*
#* @serializer png
#*
function(query) {
  my_data <- palmerpenguins::penguins
  title <- "All Species"

  # Filter if the species was specified
  if (!is.null(query$spec)) {
    title <- paste0("Only the '", query$spec, "' Species")
    my_data <- subset(my_data, species == query$spec)
  }

  plot(
    my_data$flipper_length_mm,
    my_data$bill_length_mm,
    main = title,
    xlab = "Flipper Length (mm)",
    ylab = "Bill Length (mm)"
  )
}
