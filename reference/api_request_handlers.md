# Add a handler for a request

This family of functions facilitates adding a request handler for a
specific HTTP method and path.

## Usage

``` r
api_get(
  api,
  path,
  handler,
  serializers = get_serializers(),
  parsers = get_parsers(),
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  auth_flow = NULL,
  auth_scope = NULL,
  then = NULL,
  doc = NULL,
  route = NULL
)

api_head(
  api,
  path,
  handler,
  serializers = get_serializers(),
  parsers = get_parsers(),
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  auth_flow = NULL,
  auth_scope = NULL,
  then = NULL,
  doc = NULL,
  route = NULL
)

api_post(
  api,
  path,
  handler,
  serializers = get_serializers(),
  parsers = get_parsers(),
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  auth_flow = NULL,
  auth_scope = NULL,
  then = NULL,
  doc = NULL,
  route = NULL
)

api_put(
  api,
  path,
  handler,
  serializers = get_serializers(),
  parsers = get_parsers(),
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  auth_flow = NULL,
  auth_scope = NULL,
  then = NULL,
  doc = NULL,
  route = NULL
)

api_delete(
  api,
  path,
  handler,
  serializers = get_serializers(),
  parsers = get_parsers(),
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  auth_flow = NULL,
  auth_scope = NULL,
  then = NULL,
  doc = NULL,
  route = NULL
)

api_connect(
  api,
  path,
  handler,
  serializers = get_serializers(),
  parsers = get_parsers(),
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  auth_flow = NULL,
  auth_scope = NULL,
  then = NULL,
  doc = NULL,
  route = NULL
)

api_options(
  api,
  path,
  handler,
  serializers = get_serializers(),
  parsers = get_parsers(),
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  auth_flow = NULL,
  auth_scope = NULL,
  then = NULL,
  doc = NULL,
  route = NULL
)

api_trace(
  api,
  path,
  handler,
  serializers = get_serializers(),
  parsers = get_parsers(),
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  auth_flow = NULL,
  auth_scope = NULL,
  then = NULL,
  doc = NULL,
  route = NULL
)

api_patch(
  api,
  path,
  handler,
  serializers = get_serializers(),
  parsers = get_parsers(),
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  auth_flow = NULL,
  auth_scope = NULL,
  then = NULL,
  doc = NULL,
  route = NULL
)

api_any(
  api,
  path,
  handler,
  serializers = get_serializers(),
  parsers = get_parsers(),
  use_strict_serializer = FALSE,
  download = FALSE,
  async = FALSE,
  auth_flow = NULL,
  auth_scope = NULL,
  then = NULL,
  doc = NULL,
  route = NULL
)
```

## Arguments

- api:

  A plumber2 api object to add the handler to

- path:

  A string giving the path the handler responds to. See Details

- handler:

  A handler function to call when a request is matched to the path

- serializers:

  A named list of serializers that can be used to format the response
  before sending it back to the client. Which one is selected is based
  on the request `Accept` header. See
  [`get_serializers()`](https://plumber2.posit.co/reference/register_serializer.md)
  for a helper to construct this

- parsers:

  A named list of parsers that can be used to parse the request body
  before passing it in as the `body` argument. Which one is selected is
  based on the request `Content-Type` header. See
  [`get_parsers()`](https://plumber2.posit.co/reference/register_parser.md)
  for a helper to construct this

- use_strict_serializer:

  By default, if a serializer that respects the requests `Accept` header
  cannot be found, then the first of the provided ones are used. Setting
  this to `TRUE` will instead send back a `406 Not Acceptable` response

- download:

  Should the response mark itself for download instead of being shown
  inline? Setting this to `TRUE` will set the `Content-Disposition`
  header in the response to `attachment`. Setting it to a string is
  equivalent to setting it to `TRUE` but will in addition also set the
  default filename of the download to the string value

- async:

  If `FALSE` create a regular handler. If `TRUE`, use the default async
  evaluator to create an async handler. If a string, the async evaluator
  registered to that name is used. If a function is provided then this
  is used as the async evaluator. See the *Async* section for more
  detail

- auth_flow:

  A logical expression giving the authentication flow the client must
  pass to get access to the resource.

- auth_scope:

  The scope requirements of the resource

- then:

  A list of function to be called once the async handler is done. The
  functions will be chained using
  [`promises::then()`](https://rstudio.github.io/promises/reference/then.html).
  See the *Async* section for more detail

- doc:

  A list with the OpenAPI spec for the endpoint

- route:

  The route this handler should be added to. Defaults to the last route
  in the stack. If the route does not exist it will be created as the
  last route in the stack

## Value

These functions return the `api` object allowing for easy chaining with
the pipe

## Using annotation

Handlers can be specified in an annotated route file using one of the
method tags followed by the path it pertains to. You can use various
tags to describe the handler and these will automatically be converted
to OpenAPI documentation. Further, additional tags allow you to modify
the behavior of the handler, reflecting the arguments available in the
functional approach.

    #* A handler for /user/<username>
    #*
    #* @param username:string The name of the user to provide information on
    #*
    #* @get /user/<username>
    #*
    #* @response 200:{name:string, age:integer, hobbies:[string]} Important
    #* information about the user such as their name, age, and hobbies
    #*
    function(username) {
      find_user_in_db(username)
    }

Handlers can be specified in an annotated route file using one of the
method tags followed by the path it pertains to. You can use various
tags to describe the handler and these will automatically be converted
to OpenAPI documentation. Further, additional tags allow you to modify
the behavior of the handler, reflecting the arguments available in the
functional approach.

    #* A handler for /user/<username>
    #*
    #* @param username:string The name of the user to provide information on
    #*
    #* @get /user/<username>
    #*
    #* @response 200:{name:string, age:integer, hobbies:[string]} Important
    #* information about the user such as their name, age, and hobbies
    #*
    function(username) {
      find_user_in_db(username)
    }

You can create async handlers with `then` chaining using annotation,
through the `@async` and `@then` tags

    #* A handler for /user/<username>
    #*
    #* @param username:string The name of the user to provide information on
    #*
    #* @get /user/<username>
    #*
    #* @response 200:{name:string, age:integer, hobbies:[string]} Important
    #* information about the user such as their name, age, and hobbies
    #*
    #* @async
    function(username) {
      find_user_in_db(username)
    }
    #* @then
    function(server, response) {
      server$log("message", "async operation completed")
      response$set_header("etag", "abcdef")
      Next
    }

You can add authentication using the `@auth` and `@authScope` tags

    #* A handler for /user/<username>
    #*
    #* @param username:string The name of the user to provide information on
    #*
    #* @get /user/<username>
    #*
    #* @response 200:{name:string, age:integer, hobbies:[string]} Important
    #* information about the user such as their name, age, and hobbies
    #*
    #* @auth auth1 || auth2
    #* @authScope read, write
    #*
    function(username) {
      find_user_in_db(username)
    }

## HTTP Methods

The HTTP specs provide a selection of specific methods that clients can
send to the server (your plumber api). While there is no enforcement
that the server follows any conventions you should strive to create a
server API that adheres to common expectations. It is not required that
a server understands all methods, most often the opposite is true. The
HTTP methods are described below, but consider consulting
[MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods) to get
acquainted with the HTTP spec in general

- `GET`: This method is used to request specific content and is perhaps
  the most ubiquitous method in use. `GET` requests should only retrieve
  data and should not contain any body content

- `HEAD`: This method is identical to `GET`, except the response should
  only contain headers, no body. Apart from this it is expected that a
  `HEAD` request is identical to a `GET` request for the same resource

- `POST`: This method delivers content, in the form of a request body,
  to the server, potentially causing a change in the server. In the
  context of plumber2 it is often used to call functions that require
  input larger than what can be put in the URL

- `PUT`: This method is used to update a specific resource on the
  server. In the context of a standard plumber2 server this is rarely
  relevant, though usage can come up. `PUT` is considered by clients to
  be idempotent meaning that sending the same `PUT` request multiple
  times have no effect

- `DELETE`: This method deletes a resource and is the opposite to `PUT`.
  As with `PUT` this method has limited use in most standard plumber2
  servers

- `CONNECT`: This method request the establishment of a proxy tunnel. It
  is considered advanced use and is very unlikely to have a use case for
  your plumber2 api

- `OPTIONS`: This method is used by clients to query a server about what
  methods and other settings are supported on a server

- `TRACE`: This method is a form of ping that should send a response
  containing the request (stripped of any sensitive information). Many
  servers disallow this method due to security concerns

- `PATCH`: This method is like `PUT` but allows partial modification of
  a resource

Apart from the above, plumber2 also understands the `ANY` method which
responds to requests to any of the above methods, assuming that a
specific handler for the method is not found. As the semantics of the
various methods are quite different an `ANY` handler should mainly be
used for rejections or for setting specific broad headers on the
response, not as the main handler for the request

## The Path

The path defines the URL the request is being made to with the root
removed. If your plumber2 server runs from `http://example.com/api/` and
a request is made to `http://example.com/api/user/thomas/`, then the
path would be `user/thomas/`. Paths can be static like the prior
example, or dynamic as described below:

### Path arguments

Consider you have a bunch of users. It would be impractical to register
a handler for each one of them. Instead you can use a dynamic path like
with the following syntax: `user/<username>/`. This path would be
matched to any requests made to `user/..something../`. The actual value
of `..something..` (e.g. `thomas`) would be made available to the
handler (see below). A path can contain multiple arguments if needed,
such as `user/<username>/settings/<setting>/`

### Path wildcards

Apart from path arguments it is also possible to be even less specific
by adding a wildcard to the path. The path `user/*` will match both
`user/thomas/`, `user/thomas/settings/interests/`, and anything other
path that begins with `user/`. As with arguments a path can contain
multiple wildcards but the use of these have very diminishing returns.
Contrary to path arguments the value(s) corresponding to `*` is not made
available to the handler.

### Path Priority

With the existence of path arguments and wildcards it is possible that
multiple handlers in a route can be matched to a single request. Since
only one can be selected we need to determine which one wins. The
priority is based on the specificity of the path. Consider a server
containing the following handler paths: `user/thomas/`,
`user/<username>/`, `user/<username>/settings/<setting>/`, `user/*`.
These paths will have the following priority:

1.  `user/<username>/settings/<setting>/`

2.  `user/thomas/`

3.  `user/<username>/`

4.  `user/*`

The first spot is due to the fact that it is the path with the most
elements so it is deemed most specific. For the remaining 3 they all
have the same number of elements, but static paths are considered more
specific than dynamic paths, and path arguments are considered more
specific than wildcards.

A request made to `user/carl` will thus end up in the third handler,
while a request made to `user/thomas` will end up in the second. This
ordering makes it possible to both provide default handlers as well as
specializations for specific paths.

## The Handler

The handler is a standard R function that is called when a request is
made that matches the handlers path (unless a more specific handler path
exists — see above). A handler function can perform any operation a
normal R function can do, though you should consider strongly the
security implications of your handler functions. However, there are
certain expectations in plumber around the arguments a handler function
takes and the return value it provides

### Handler Arguments

The handler function can take one or more of the following arguments.

- **Path arguments**: Any path arguments are passed on to the handler.
  If a handler is registered for the following path
  `user/<username>/settings/<setting>/` and it handles a request to
  `user/thomas/settings/interests/` then it will be called with
  `username = "thomas", setting = "interest"`

- `request`: The request the handler is responding to as a
  [reqres::Request](https://reqres.data-imaginist.com/reference/Request.html)
  object

- `response`: The response being returned to the client as a
  [reqres::Response](https://reqres.data-imaginist.com/reference/Response.html)
  object

- `server`: The
  [Plumber2](https://plumber2.posit.co/reference/Plumber2.md) object
  representing your server implementation

- `client_id`: A string uniquely identifying the session the request
  comes from

- `query`: A list giving any additional arguments passed into the
  handler as part of the url query string

- `body`: The request body, parsed as specified by the provided parsers

### Handler Return Value

Handlers can return a range of different value types, which will inform
plumber2 what to do next:

#### Returning `Next` or `Break`

These two control objects informs plumber2 to either proceed handling
the request (`Next`) or return the response as is, circumventing any
remaining routes (`Break`)

#### Returning `NULL` or the `response` object

This is the same as returning `Next`, i.e. it signals that handling can
proceed

#### Returning a ggplot2 object

If you return a ggplot2 object it will get plotted for you (and added to
the response assuming a graphics serializer is provided) before handling
continues

#### Returning any other value

Any kind of value returned that is not captured by the above description
will be set to the response body (overwriting what was already there)
and handling is then allowed to continue

### Handler conditions

Like any function in R, a handler may need to signal that something
happened, either by throwing an error or warning or by emitting a
message. You can use [`stop()`](https://rdrr.io/r/base/stop.html),
[`warning()`](https://rdrr.io/r/base/warning.html), and
[`message()`](https://rdrr.io/r/base/message.html) as you are used to.
For all of them, the condition message will end up in the log. Further,
for [`stop()`](https://rdrr.io/r/base/stop.html) any further handling of
the request will end and a `500 Internal Error` response is returned. To
take more control over problems you can use the
[`abort_*()`](https://reqres.data-imaginist.com/reference/abort_http_problem.html)
family of conditions from reqres. Like
[`stop()`](https://rdrr.io/r/base/stop.html) they will halt any further
processing, but they also allow control over what kind of response is
sent back, what kind of information about the issue is communicated to
the client, and what kind of information is logged internally. The
response they send back (except for
[`abort_status()`](https://reqres.data-imaginist.com/reference/abort_http_problem.html))
all adhere to the HTTP Problem spec defined in [RFC
9457](https://datatracker.ietf.org/doc/html/rfc9457).

While it may feel like a good idea to send a detailed error message back
to the client it is often better to only inform the client of what they
need to change to solve the issue. Too much information about internal
implementation details can be a security risk and forwarding internal
errors to a client can help inform the client about how the server has
been implemented.

## Async handling

plumber2 supports async handling of requests in one of two ways:

1.  The handler you provide returns a promise object

2.  You set `async = TRUE` (or the name of a registered async evaluator)
    when adding the handler

For 1), there is no more to do. You have full custody over the created
promise and any `then()`-chaining that might be added to it. For 2) it
is a bit different. In that case you provide a regular function and
plumber2 takes care of converting it to a promise. Due to the nature of
promises a handler being converted to a promise can't take `request`,
`response`, and `server` arguments, so if you need to manipulate these
you need to use `then` (more on this shortly). The async handler should
yield the value that the response should ultimately get assigned to the
body or have plotting side effects (in which case the plot will get
added to the response).

### Async chaining

Because you can't manipulate `request` `response`, or `server` in the
async handler it may be needed to add operations to perform once the
async handler has finished. This can be done through the `then` argument
(or using the `@then` tag in annotated route files). This takes a list
of functions to chain to the promise using
[`promises::then()`](https://rstudio.github.io/promises/reference/then.html).
Before the `then` chain is executed the response will get the return
value of the main handler assigned to the body. Each `then` call will
receive the same arguments as a standard request handler as well as
`result` which will hold the return value of the previous handler in the
chain. For the first `then` call `result` will be a boolean signalling
if the async handler wants request handling to proceed to the next route
or terminate early. The last call in the chain must return `Next` or
`Break` to signal if processing should be allowed to continue to the
next route.

## Authentication

plumber2 supports various authentication schemas which can be added with
[`api_auth_guard()`](https://plumber2.posit.co/reference/api_auth_guard.md).
An authentication flow for the handler can then be specified with the
`auth_flow` argument and optional scopes can be set with the
`auth_scope` argument. The flow is defined by a logical expression
referencing the names of the authenticators part of the flow. Assuming
two authenticators are available, `auth1` and `auth2`, then a flow could
be `auth1 && auth2` to require the request passes both authenticators.
Alternatively it could be `auth1 || auth2` to require the request
passing either. Flows can be arbitrarily complex with nesting etc, but
he OpenAPI spec has limits to what it can describe so if you want to
have an OpenAPI compliant api you must limit yourself to at most two
levels of nesting with the outer level being `||` (ie.
`(auth1 && auth2) || (auth3 && auth4)` is ok, but
`(auth1 || auth2) && (auth3 || auth4)` is not due to the outer level
being `&&`, and `(auth1 && auth2) || (auth3 && (auth4 || auth5))` is not
allowed because it has 3 nesting levels). This is only a limitation of
OpenAPI and plumber2 itself can handle all of the above.

plumber2 also supports requiring specific scopes to access resources. If
you require these you must make sure the authenticator provides scopes
upon a successful authentication, otherwise the request will be denied.

## See also

Other Request Handlers:
[`api_request_header_handlers`](https://plumber2.posit.co/reference/api_request_header_handlers.md)

## Examples

``` r
# Standard use
api() |>
  api_get("/hello/<name:string>", function(name) {
    list(
      msg = paste0("Hello ", name, "!")
    )
  })
#> Creating default route in request router
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running

# Specify serializers
api() |>
  api_get(
    "/hello/<name:string>",
    function(name) {
      list(
        msg = paste0("Hello ", name, "!")
      )
    },
    serializers = get_serializers(c("json", "xml"))
  )
#> Creating default route in request router
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running

# Request a download and make it async
api() |>
  api_get(
    "/the_plot",
    function() {
      plot(1:10, 1:10)
    },
    serializers = get_serializers(c("png", "jpeg")),
    download = TRUE,
    async = TRUE
  )
#> Creating default route in request router
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running
```
