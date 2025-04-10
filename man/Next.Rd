% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/controls.R
\docType{data}
\name{Next}
\alias{Next}
\alias{Break}
\alias{should_break}
\title{Router control flow}
\usage{
Next

Break

should_break(x)
}
\arguments{
\item{x}{An object to test}
}
\description{
In plumber2 your API can have multiple middleware that a request passes
through. At any point can you short-circuit the remaining middleware by
returning \code{Break}, which instructs plumber2 to return the response as is.
Returning \code{Next} indicates the opposite, ie that the request should be
allowed to pass on to the next middleware in the chain. A handler function
that doesn't return either of these are assumed to return a value that should
be set to the response body and implicitely continue to the next middleware.
}
\keyword{datasets}
