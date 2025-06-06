% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/serializers.R
\name{register_serializer}
\alias{register_serializer}
\alias{show_registered_serializers}
\alias{get_serializers}
\title{Register or fetch a serializer}
\usage{
register_serializer(name, fun, mime_type, default = TRUE)

show_registered_serializers()

get_serializers(serializers = NULL)
}
\arguments{
\item{name}{The name to register the serializer function to. If already
present the current serializer will be overwritten by the one provided by you}

\item{fun}{A function that, when called, returns a unary function that can
serialize a response body to the mime type defined in \code{mime_type}}

\item{mime_type}{The format this serializer creates. You should take care to
ensure that the value provided is a standard mime type for the format}

\item{default}{Should this serializer be part of the default set of
serializers}

\item{serializers}{Serializers to collect. This can either be a character
vector of names of registered serializers or a list. If it is a list then the
following expectations apply:
\itemize{
\item Any unnamed elements containing a character vector will be considered as
names of registered serializers constructed with default values. The
special value \code{"..."} will fetch all the serializers that are otherwise not
specified in the call.
\item Any element containing a function are considered as a provided serializer
and the element must be named by the mime type the serializer understands
\item Any remaining named elements will be considered names of registered
serializers that should be constructed with the arguments given in the
element
}}
}
\value{
For \code{get_serializers} a named list of serializer functions named by
their mime type. The order given in \code{serializers} is preserved.
}
\description{
plumber2 comes with many serializers that should cover almost all standard
use cases. Still you might want to provide some of your own, which this
function facilitates.
}
\details{
If you want to register your own serializer, then the function you register
must be a factory function, i.e. a function returning a function. The returned
function must accept a single argument which is the response body. All
arguments to the factory function should be optional.
}
\note{
Using the \code{...} will provide remaining graphics serializers if a
graphics serializer is explicitely requested elsewhere. Otherwise it will
provide the remaining non-graphics serializers. A warning is thrown if a mix
of graphics and non-graphics serializers are requested.
}
\seealso{
\link{serializers}

\code{\link[=register_serializer]{register_serializer()}}
}
