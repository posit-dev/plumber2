# Introduction

The [R Programming Language](https://www.R-project.org/) is one of the
most dominant programming languages for data analysis and visualization.
Still, the world is multilingual, and often R is just a piece of the
puzzle. Web services are a common way to allow various systems to
interact with one another and the plumber2 R package allows users to
expose existing R code as a service available to others on the Web.
Plumber2 is best illustrated with an example:

``` r
#* Echo the parameter that was sent in
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

#* Plot out data from the palmer penguins dataset
#*
#* @get /plot
#*
#* @query spec:string If provided, filter the data to only this species
#* (e.g. 'Adelie')
#*
#* @serializer png
#*
function(query) {
  myData <- penguins
  title <- "All Species"

  # Filter if the species was specified
  if (!is.null(query$spec)){
    title <- paste0("Only the '", query$spec, "' Species")
    myData <- subset(myData, species == query$spec)
  }

  plot(
    myData$flipper_len,
    myData$bill_len,
    main=title,
    xlab="Flipper Length (mm)",
    ylab="Bill Length (mm)"
  )
}
```

Even without knowing R, you can probably get a rough idea for what the
above Plumber API will do. The first function above defines the
`/echo/<msg>` handler which simply echoes back the text that makes up
the second portion of the path. The second function generates a plot
based on the Palmer Penguins dataset; it includes a query filter that
allows the caller to subset the dataset to a particular species.

Plumber2 makes use of these comment “annotations” above your functions
to define the web service. When you feed the above file into
[`plumber2::api()`](https://plumber2.posit.co/reference/api.md), you’ll
get a runnable web service that other systems can interact with over a
network.

## Web APIs

The Hypertext Transfer Protocol (HTTP) is the dominant medium by which
information is exchanged on the Internet. An Application Programming
Interface (API) is a broad term that defines the rules that guide your
interaction with some software. In the case of HTTP APIs, you have a
defined set of endpoints that accept particular inputs. Plumber2
translates the annotations you place on your functions into an HTTP API
that can be called from other machines on your network. If you execute
your Plumber API on a public server, you can even make your API
available to the public Internet.

HTTP APIs have become the predominant language by which software
communicates. By creating an HTTP API, you’ll empower your R code to be
leveraged by other services – whether they’re housed inside your
organization or hosted on the other side of the world. Here are just a
few ideas of the doors that are opened to you when you wrap your R code
in a Plumber API:

- Software written in other languages in your organization can run your
  R code. Your company’s Java application could now pull in a custom
  ggplot2 graph that you generate on-demand, or a Python client could
  query a predictive model defined in R.
- You can have [some third-party](https://www.mailgun.com/) receive
  emails on your behalf and then notify your Plumber service when new
  messages arrive.
- You could register a “[Slash
  Command](https://api.slack.com/slash-commands)” on Slack, enabling you
  to execute your R function in response to a command being entered in
  Slack.
- You can write JavaScript code that queries your Plumber API from a
  visitor’s web browser. Even further, you could use Plumber exclusively
  as the back-end of an interactive web application.
