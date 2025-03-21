#* Echo the parameter that was sent in
#*
#* @get /echo/<msg>
#*
#* @param msg:string* The message to echo back.
#*
function(msg) {
  list(
    msg = paste0("The message is: '", msg, "'")
  )
}

#* Plot out data from the iris dataset
#*
#* @get /plot
#*
#* @query spec:string If provided, filter the data to only this species
#* (e.g. 'setosa')
#*
#* @serializer png
#*
function(query) {
  myData <- iris
  title <- "All Species"

  # Filter if the species was specified
  if (!is.null(query$spec)){
    title <- paste0("Only the '", query$spec, "' Species")
    myData <- subset(iris, Species == query$spec)
  }

  plot(
    myData$Sepal.Length,
    myData$Petal.Length,
    main=title,
    xlab="Sepal Length",
    ylab="Petal Length"
  )
}
