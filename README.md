
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plumber2

<!-- badges: start -->

[![R-CMD-check](https://github.com/posit-dev/plumber2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/posit-dev/plumber2/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/posit-dev/plumber2/graph/badge.svg)](https://app.codecov.io/gh/posit-dev/plumber2)
<!-- badges: end -->

This is a complete rewrite of [plumber](https://www.rplumber.io). The
purpose of the rewrite is to take everything we’ve learned from plumber,
shed the bad decision that you inevitably make over the course of
development, and start from scratch.

You’ll find that plumber2 is very similar to plumber in a lot of ways,
but diverts in key areas, resulting in API incompatibility between the
two packages. Because of this you may need to update your plumber APIs
if switching to plumber2.

## Installation

plumber2 is still a work in progress, and it is recommended to only use
it to experiment and get familiarity with the future direction of
plumber APIs. If you wish to try it out you can install the development
version from GitHub using [pak](https://pak.r-lib.org):

``` r
pak::pak("posit-dev/plumber2")
```

## Feedback

At this point in the development feedback is crucial. If you do decide
to try out plumber2, please share your experience, both good and bad,
and ask questions as it informs us about where to spend more time with
documentation.

## Hello World

Below is a simply “hello world” API written for plumber2 that
illustrates some of the differences from plumber:

``` r
#* Echo the parameter that was sent in
#*
#* @get /echo/<msg>
#*
#* @param msg:string The message to echo back.
#*
#* @response 200:{msg:string} A string containing the input message
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
#* @query spec:enum|setosa, versicolor, virginica| If provided, filter the
#* data to only this species
#*
#* @serializer png{width = 700, height = 500}
#* @serializer jpeg{width = 700, height = 500}
#*
#* @async
function(query) {
  myData <- iris
  title <- "All Species"

  # Filter if the species was specified
  if (!is.null(query$spec)){
    title <- paste0("Only the '", query$spec, "' Species")
    myData <- subset(iris, Species == query$spec)
    if (nrow(myData) == 0) {
      abort_internal_error("Missing data for {query$spec}")
    }
  }

  plot(
    myData$Sepal.Length,
    myData$Petal.Length,
    main=title,
    xlab="Sepal Length",
    ylab="Petal Length"
  )
}
```

Above you can both see some breaking changes and some new features in
action. The biggest breaking change is that parameters coming from the
path, the query string, and the body are now clearly separated. Only
parameters from the path are provided as direct arguments to the handler
function. Query and body parameters are accessible through the `query`
and `body` argument respectively. As can be seen above they also use
different tags in the documentation.

Speaking of documentation, the parsing of plumber blocks have been
greatly improved. It is now built upon roxygen2, so it follows that
convention, allowing multiline tags and defaulting to the first line as
title and proceeding untagged lines as description. The ability to
define input and output types has also been greatly expanded, adding the
ability to define nested objects, adding default values and (as seen
above) define enum (factors) to name a few. All input will get type
checked and default value imputed if missing.

For the `/plot` handler you can also see that it specifies multiple
serializers. Doing so will allow the client to request its preferred
response format using the `Accept` header. Plumber2 will then perform
content negotiation to figure out the best response format based on what
it supports and what the client prefers.

Lastly, you can see a new tag (one of many) in the `/plot` handler.
`@async` allows you to convert your handler into an async handler
automatically. It is still possible to create an async handler manually
by returning a promise, but the new tag significantly simplifies this
for the most classic cases. There is still overhead involved in handling
requests asynchronously so this is mainly a good idea for longer running
handlers, but it is shown here as an example.
