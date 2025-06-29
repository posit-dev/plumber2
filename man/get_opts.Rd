% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{get_opts}
\alias{get_opts}
\alias{all_opts}
\title{Retrieve options for creating a plumber2 api}
\usage{
get_opts(x, default = NULL)

all_opts()
}
\arguments{
\item{x}{The name of the option}

\item{default}{The default value, if \code{x} is not set}
}
\value{
For \code{get_opts} The value of \code{x}, if any, or \code{default}. For
\code{all_opts()} a named list of all the options that are set
}
\description{
You can provide options for your plumber2 api which will be picked up when
you create the API with \code{\link[=api]{api()}}. Options can be set either through the
internal \code{\link[=options]{options()}} functionality, or by setting environment variables. In
the former case, the name of the option must be prefixed with \code{"plumber2."},
in the latter case the variable name must be in upper case and prefixed with
\code{"PLUMBER2_"}. If the option is stored as an environment variable then the
value is cast to the type giving in \code{default}. See the docs for \code{\link[=api]{api()}} for
the default values of the different options.
}
\examples{
# Using `options()`
old_opts <- options(plumber2.port = 9889L)
get_opts("port")
options(old_opts)

# Using environment variables
old_env <- Sys.getenv("PLUMBER2_PORT")
Sys.setenv(PLUMBER2_PORT = 9889)

## If no default is provided the return value is a string
get_opts("port")

## Provide a default to hint at the options type
get_opts("port", 8080L)

Sys.setenv(PLUMBER2_PORT = old_env)

}
