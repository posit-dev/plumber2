# Instrumentation with OpenTelemetry

Monitoring your server is crucial once it is up and running. This
monitoring can come in many forms, ranging from classic logging and all
the way up to collecting metrics such as number of concurrent requests
being handled at any given point in time.
[OpenTelemetry](https://opentelemetry.io) is a standard for collecting
such instrumentation which in R is available in the
[otel](https://otel.r-lib.org) package. It includes support for traces
(hierarchical spans of operations), logs, and metrics and it comes with
a huge and detailed set of semantics for how these should be formatted
and which information to include.

Plumber2 provides expansive support for OpenTelemetry, though all the
actual support is implemented in the underlying packages reqres, fiery,
and routr (this also means that all we discuss here will translated to
barebone fiery servers). This document will detail the various ways in
which instrumentation is supported and how you may add to it in your own
servers, should you choose to.

Understanding how to turn on OpenTelemetry instrumentation is not part
of this document. You should consult the otel package for learning about
this.

## Traces

OpenTelemetry comes with an expansive [semantic
convention](https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-server)
for how servers should record their handling of HTTP requests, which has
been fully implemented in reqres.

Once a request is received an OpenTelemetry span is created and attached
to the request. It is available in the `otel` field
(e.g. `request$otel`) and will be `NULL` if instrumentation is not
turned on. The span is automatically closed when the response is send
back.

To the extent possible, the span will capture all the attributes
mentioned in the conventions and should be used as the parent span for
all subspans that are part of handling this request (this will happen
automatically for user-initiated spans). If the request comes with a
span context from the client then this will be used as parent for the
reqres span.

Apart from the attributes mentioned in the semantics, fiery will add
three attributes to this span:

- `server.id` will hold the unique id of the server handling the
  request. If you have multiple servers running in parallel each will
  have their own unique id.
- `server.framework.name` will hold the name of the framework used to
  create the server (`"plumber2"` for a plumber2 api, `"fiery"` for a
  barebone fiery server, or something else if others choose to build a
  framework on top of fiery)
- `server.framework.version` will hold the version of the framework (the
  package version)

### Subtraces

You are often interested in what goes on during handling of the request.
For plumber2, this usually means the different routes the request is
passed through. routr, which handles the routes, will automatically
create a subspan for each of the routes a request goes through and
activate that span (meaning that any spans created within the route
logic will get this span as a parent). Since all the attributes
described in the semantic conventions are already recorded in the parent
span it will not be repeated in the subspan. Instead, the subspan
created by routr will contain the following attributes:

- `routr.route` holds the path of the route the request was matched to.
  Be aware that routr has a different path naming convention than
  plumber2 and the attribute will use the routr naming. Here, path
  parameters are prefixed with `:` rather than being enclosed in angle
  braces. So, if you have a plumber2 path like
  `/users/<username>/settings` it will appear in the subspan as
  `/users/:username/settings`
- `routr.path.param.<name>` will hold the actual parameter of the path
  param. Be aware that there can be multiple of these. Continuing the
  example from a above, a url path of `/users/thomasp85/settings` will
  give rise to an `routr.path.param.username` attribute with a value of
  `"thomasp85"`.

Apart from creating this subspan, routr also takes responsibility of
setting the `http.route` attribute on the parent span (the one created
by reqres). This attribute is part of the semantic convention but is not
known by reqres so must be set by the code handling the request. The
attribute will be set if the route path does not contain any wildcards.
This means that it can in theory be overwritten multiple times during
handling and the last route that satisfies the specificity requirement
will win.

If you wish to add additional instrumentation to your server logic
you’ll likely do this within your endpoint handlers. If so, you should
probably use
[`otel::start_local_active_span()`](https://otel.r-lib.org/reference/start_local_active_span.html)
which will automatically close itself at the end of your code and pick
up the active routr subspan as the parent. However, don’t go overboard
and only add instrumentation for things your are specifically interested
in collecting. The default instrumentation is already quite detailed. An
alternative way to add additional information is to add your own
attributes or events to the spans already in place. As mentioned above,
the parent span is available in `request$otel`, while the routr span can
be accessed using
[`otel::get_active_span()`](https://otel.r-lib.org/reference/get_active_span.html).

## Metrics

The HTTP semantic convention also covers metrics. reqres takes care of
collecting those from the convention which are:

- `http.server.request.duration` collects the duration of the request
  handling by the server
- `http.server.active_requests` collects the number of active requests
  at any point in time
- `http.server.request.body.size` collects the size of the body of
  requests received
- `http.server.response.body.size` collects the size of the body of
  responses send back

You can define your own metrics if you wish and collect them during your
endpoint logic. If you do so, make sure to use the request span
(`request$otel`) as the context for the metric so it gets correctly
linked (assuming the metric is related to request handling)

## Logs

fiery, and hence plumber2, has build in facility for logging and comes
with a plethora of different loggers. You choose the logger with
[`api_logger()`](https://plumber2.posit.co/reference/api_logger.md) and
then log information using either `server$log()` (`server` being the
plumber2 api object), or by throwing errors, warnings, or messages using
whatever means you prefer (base, rlang, cli, etc).

fiery and plumber2 comes with a `logger_otel` which you can use to
collect the logs with OpenTelemetry.

``` r
pa <- api() |>
  api_logger(logger_otel())
```

If you use this logger then all logs will appear together with your
spans and metrics. If a log occur during request handling then the log
will automatically be associated with the request span. Further, all
logs will get the attribute `server.id` which identifies the server
instance from which the log originated.
