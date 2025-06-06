% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_on.R
\name{api_on}
\alias{api_on}
\alias{api_off}
\title{Add a handler to an event}
\usage{
api_on(api, event, handler, id = NULL)

api_off(api, id)
}
\arguments{
\item{api}{A plumber2 api object to launch or stop}

\item{event}{A string naming the event to listen for}

\item{handler}{A function to call when \code{event} fires}

\item{id}{A string uniquely identifying the handler. If \code{NULL} a random id
will be generated making it impossible to remove the handler again}
}
\value{
These functions return the \code{api} object allowing for easy chaining
with the pipe
}
\description{
During the life cycle of a plumber API various events will be fired, either
automatically or manually. See the \href{https://fiery.data-imaginist.com/articles/events.html}{article on events in fiery}
for a full overview. \code{api_on()} allows you to add handlers that are called
when specific events fire. \code{api_off()} can be used to remove the handler if
necessary
}
