% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_handlers.R
\name{api_add_route}
\alias{api_add_route}
\title{Add a new route to either the request or header router}
\usage{
api_add_route(api, name, route = NULL, header = FALSE, after = NULL)
}
\arguments{
\item{api}{A plumber2 api object to add the route to}

\item{name}{The name of the route to add. If a route is already present
with this name then the provided route (if any) is merged into it}

\item{route}{The route to add. If \code{NULL} a new empty route will be
created}

\item{header}{Logical. Should the route be added to the header router?}

\item{after}{The location to place the new route on the stack. \code{NULL}
will place it at the end. Will not have an effect if a route with the
given name already exists.}
}
\value{
This functions return the \code{api} object allowing for easy chaining
with the pipe
}
\description{
This function allows explicit creation of routes or addition/merging of a
predefined \link[routr:Route-class]{routr::Route} into the router of the api. A new route can also be
created with the \code{route} argument when \link[=api_get]{adding a handler}. However,
that way will always add new routes to the end of the stack, whereas using
\code{api_add_route()} allows you full control of the placement.
}
