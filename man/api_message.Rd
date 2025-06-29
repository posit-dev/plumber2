% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_handlers.R
\name{api_message}
\alias{api_message}
\title{Add a handler to a WebSocket message}
\usage{
api_message(api, handler, async = NULL, then = NULL)
}
\arguments{
\item{api}{A plumber2 api object to add the handler to}

\item{handler}{A function conforming to the specifications laid out in
Details}

\item{async}{If \code{FALSE} create a regular handler. If \code{TRUE}, use the default
async evaluator to create an async handler. If a string, the async evaluator
registered to that name is used. If a function is provided then this is used
as the async evaluator. See the \emph{Async} section for more detail}

\item{then}{A list of function to be called once the async handler is done.
The functions will be chained using \code{\link[promises:then]{promises::then()}}. See the \emph{Async}
section for more detail}
}
\value{
This functions return the \code{api} object allowing for easy chaining
with the pipe
}
\description{
WebSockets is a bidirectional communication channel that can be established
at the request of the client. While websocket communication is not really
part of a standard REST api, it has many uses and can easily be used together
with one.
}
\details{
A handler for a websocket message is much simpler than for requests in
general since it doesn't have to concern itself with methods, paths, and
responses. Any message handler registered will get called in sequence when a
websocket message is recieved from a client. Still, a few expectations apply
\subsection{Handler Arguments}{

The handler can take any of the following arguments:
\itemize{
\item \code{message}: Either a raw vector if the message recieved is in binary form or
a single string, giving the message sent from the client
\item \code{server}: The \link{Plumber2} object representing your server implementation
\item \code{client_id}: A string uniquely identifying the session the request comes
from
\item \code{request}: The request that was initially used to establish the websocket
connection with the client as a \link[reqres:Request]{reqres::Request} object
}
}

\subsection{Handler Return Value}{

It is not expected that a websocket message sends a response and thus the
handler is not required to do anything like that. However, if the handler
returns either a raw vector or a single string it is taken as a signal to
send this back to the client. Any other return value is silently ignored.
}
}
\section{Async}{
You can handle websocket messages asynchronously if needed. Like with
\link[=api_get]{request handlers} you can either do it manually by creating and
returning a promise inside the handler, or by letting plumber2 convert your
handler to an async handler using the \code{async} argument. Due to the nature of
promises a handler being converted to a promise can't take \code{request} and
\code{server} arguments, so if you need to manipulate these you need to use \code{then}
(more on this shortly). The same conventions about return value holds for
async message handlers as for regular ones.
\subsection{Async chaining}{

Because you can't manipulate \code{request} or \code{server} in the async handler it
may be needed to add operations to perform once the async handler has
finished. This can be done through the \code{then} argument. This takes a list of
functions to chain to the promise using \code{\link[promises:then]{promises::then()}}. Before the \code{then}
chain is executed the return value of the async handler will be send back to
the client if it is a string or a raw vector. Each \code{then} call will receive
the same arguments as a standard message handler as well as \code{result} which
will hold the return value of the previous handler in the chain. For the
first \code{then} call \code{result} will be whatever the main async handler returned.
The return value of the last call in the chain will be silently ignored.
}
}

\section{Using annotation}{
A websocket message handler can be added to an API in an annotated route file
by using the \verb{@message} tag

\if{html}{\out{<div class="sourceCode">}}\preformatted{#* @message
function(message) \{
  if (message == "Hello") \{
    return("Hello, you...")
  \}
\}
}\if{html}{\out{</div>}}

You can create async handlers with \code{then} chaining using annotation, through
the \verb{@async} and \verb{@then} tags

\if{html}{\out{<div class="sourceCode">}}\preformatted{#* @message
#* @async
function(message) \{
  if (message == "Hello") \{
    return("Hello, you...")
  \}
\}
#* @then
function(server) \{
  server$log("message", "websocket message received")
\}
}\if{html}{\out{</div>}}
}

\examples{
api() |>
  api_message(
    function(message) {
      if (message == "Hello") {
        return("Hello, you...")
      }
    }
  )

}
