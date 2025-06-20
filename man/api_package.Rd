% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_package.R
\name{api_package}
\alias{api_package}
\title{Load up an API distributed with a package}
\usage{
api_package(package = NULL, name = NULL, ...)
}
\arguments{
\item{package}{The name of the package that provides the api. If \code{NULL} then
a list of available apis across all installed packages is returned}

\item{name}{The name of the api. If \code{NULL} then a list of available apis in
the given package is returned}

\item{...}{
  Arguments passed on to \code{\link[=api]{api}}
  \describe{
    \item{\code{host}}{A string that is a valid IPv4 address that is owned by this
server}
    \item{\code{port}}{A number or integer that indicates the server port that should be
listened on. Note that on most Unix-like systems including Linux and macOS,
port numbers smaller than 1024 require root privileges.}
    \item{\code{doc_type}}{The type of API documentation to generate. Can be either
\code{"rapidoc"} (the default), \code{"redoc"}, \code{"swagger"}, or \code{NULL} (equating to not
generating API docs)}
    \item{\code{doc_path}}{The URL path to serve the api documentation from}
    \item{\code{reject_missing_methods}}{Should requests to paths that doesn't
have a handler for the specific method automatically be rejected with a
405 Method Not Allowed response with the correct Allow header informing
the client of the implemented methods. Assigning a handler to \code{"any"} for
the same path at a later point will overwrite this functionality. Be
aware that setting this to \code{TRUE} will prevent the request from falling
through to other routes that might have a matching method and path. This
setting anly affects handlers on the request router.}
    \item{\code{ignore_trailing_slash}}{Logical. Should the trailing slash of a path
be ignored when adding handlers and handling requests. Setting this will
not change the request or the path associated with but just ensure that
both \code{path/to/resource} and \verb{path/to/resource/} ends up in the same
handler.}
    \item{\code{max_request_size}}{Sets a maximum size of request bodies. Setting this
will add a handler to the header router that automatically rejects requests
based on their \code{Content-Length} header}
    \item{\code{shared_secret}}{Assigns a shared secret to the api. Setting this will
add a handler to the header router that automatically rejects requests if
their \code{Plumber-Shared-Secret} header doesn't contain the same value. Be aware
that this type of authentication is very weak. Never put the shared secret in
plain text but rely on e.g. the keyring package for storage. Even so, if
requests are send over HTTP (not HTTPS) then anyone can read the secret and
use it}
    \item{\code{compression_limit}}{The size threshold in bytes for trying to
compress the response body (it is still dependant on content negotiation)}
    \item{\code{default_async}}{The default evaluator to use for async request handling}
    \item{\code{env}}{The parent environment to the environment the files should be
evaluated in. Each file will be evaluated in it's own environment so they
don't interfere with each other}
  }}
}
\value{
If \code{package} or \code{name} is \code{NULL} then a data frame providing
available apis filtered on either package or name (if any is provided) is
returned. Otherwise a \link{Plumber2} object representing the api is returned
}
\description{
Packages can included one or more api specification(s) by storing the
annotated route files and/or \verb{_server.yml} file in subfolders of
\code{./inst/plumber2}. The name of the subfolder will be the name of the api
}
\examples{
# Load one of the plumber2 examples
api_package("plumber2", "quickstart")

# List all available apis
api_package()

}
