handle_constructor <- function(method, header = FALSE) {
  force(method)
  force(header)
  doc <- NULL
  fun <- function(
    api,
    path,
    handler,
    serializers = NULL,
    parsers = NULL,
    use_strict_serializer = FALSE,
    download = FALSE,
    async = FALSE,
    then = NULL,
    doc = NULL,
    route = NULL
  ) {
    api$request_handler(
      method = method,
      path = path,
      handler = handler,
      serializers = serializers,
      parsers = parsers,
      use_strict_serializer = use_strict_serializer,
      download = download,
      async = async,
      then = then,
      doc = doc,
      route = route,
      header = header
    )
    api
  }
  if (header) {
    fn_fmls(fun) <- fn_fmls(fun)[fn_fmls_names(fun) != "doc"]
  }
  fun
}

#' Add a handler for a request
#'
#' This family of functions facilitates adding a request handler for a specific
#' HTTP method and path.
#'
#' # Using annotation
#' Handlers can be specified in an annotated route file using one of the method
#' tags followed by the path it pertains to. You can use various tags to
#' descripe the handler and these will automatically be converted to OpenAPI
#' documentation. Further, additional tags allow you to modify the behaviour of
#' the handler, reflecting the arguments available in the functional approach.
#'
#' ```
#' #* A handler for /user/<username>
#' #*
#' #* @param username:string The name of the user to provide information on
#' #*
#' #* @get /user/<username>
#' #*
#' #* @response 200:{name:string, age:integer, hobbies:[string]} Important
#' #* information about the user such as their name, age, and hobbies
#' #*
#' function(username) {
#'   find_user_in_db(username)
#' }
#' ```
#'
#' # HTTP Methods
#' The HTTP specs provide a selection of specific methods that clients can send
#' to the server (your plumber api). While there is no enforcement that the
#' server follows any conventions you should strive to create a server API that
#' adheres to common expectations. It is not required that a server understands
#' all methods, most often the opposite is true. The HTTP methods are described
#' below, but consider consulting [MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
#' to get acquainted with the HTTP spec in general
#'
#' * `GET`: This method is used to request specific content and is perhaps the
#'   most ubiquitous method in use. `GET` requests should only retrieve data and
#'   should not contain any body content
#' * `HEAD`: This method is identical to `GET`, except the response should only
#'   contain headers, no body. Apart from this it is expected that a `HEAD`
#'   request is identical to a `GET` request for the same resource
#' * `POST`: This method delivers content, in the form of a request body, to the
#'   server, potentially causing a change in the server. In the context of
#'   plumber2 it is often used to call functions that require input larger than
#'   what can be put in the URL
#' * `PUT`: This method is used to update a specific resource on the server. In
#'   the context of a standard plumber2 server this is rarely relevant, though
#'   usage can come up. `PUT` is considered by clients to be indemptotent meaning
#'   that sending the same `PUT` request multiple times have no effect
#' * `DELETE`: This method deletes a resource and is the opposite to `PUT`. As
#'   with `PUT` this method has limited use in most standard plumber2 servers
#' * `CONNECT`: This method request the establishment of a proxy tunnel. It is
#'   considered advanced use and is very unlikely to have a usecase for your
#'   plumber2 api
#' * `OPTIONS`: This method is used by clients to query a server about what
#'   methods and other settings are supported on a server
#' * `TRACE`: This method is a form of ping that should send a response
#'   containing the request (stripped of any sensitive information). Many
#'   servers disallow this method due to security concerns
#' * `PATCH`: This method is like `PUT` but allows partial modification of a
#'   resource
#'
#' Apart from the above, plumber2 also understands the `ANY` method which
#' responds to requests to any of the above methods, assuming that a specific
#' handler for the method is not found. As the semantics of the various methods
#' are quite different an `ANY` handler should mainly be used for rejections or
#' for setting specific broad headers on the response, not as the main handler
#' for the request
#'
#' # The Path
#' The path defines the URL the request is being made to with the root removed.
#' If your plumber2 server runs from `http://example.com/api/` and a request is
#' made to `http://example.com/api/user/thomas/`, then the path would be
#' `user/thomas/`. Paths can be static like the prior example, or dynamic as
#' described below:
#'
#' ## Path arguments
#' Consider you have a bunch of users. It would be impractical to register a
#' handler for each one of them. Instead you can use a dynamic path like with
#' the following syntax: `user/<username>/`. This path would be matched to any
#' requests made to `user/..something../`. The actual value of `..something..`
#' (e.g. `thomas`) would be made available to the handler (see below). A path
#' can contain multiple arguments if needed, such as
#' `user/<username>/settings/<setting>/`
#'
#' ## Path wildcards
#' Apart from path arguments it is also possible to be even less specific by
#' adding a wildcard to the path. The path `user/*` will match both
#' `user/thomas/`, `user/thomas/settings/interests/`, and anything other path
#' that begins with `user/`. As with arguments a path can contain multiple
#' wildcards but the use of these have very diminishing returns. Contrary to
#' path arguments the value(s) corresponding to `*` is not made available to the
#' handler.
#'
#' ## Path Priority
#' With the existence of path arguments and wildcards it is possible that
#' multiple handlers in a route can be matched to a single request. Since only
#' one can be selected we need to determine which one wins. The priority is
#' based on the specificity of the path. Consider a server containing the
#' following handler paths: `user/thomas/`, `user/<username>/`,
#' `user/<username>/settings/<setting>/`, `user/*`. These paths will have the
#' following priority:
#'
#' 1. `user/<username>/settings/<setting>/`
#' 2. `user/thomas/`
#' 3. `user/<username>/`
#' 4. `user/*`
#'
#' The first spot is due to the fact that it is the path with the most elements
#' so it is deemed most specific. For the remaining 3 they all have the same
#' number of elements, but static paths are considered more specific than
#' dynamic paths, and path arguments are considered more specific than
#' wildcards.
#'
#' A request made to `user/carl` will thus end up in the third handler, while a
#' request made to `user/thomas` will end up in the second. This ordering makes
#' it possible to both provide default handlers as well as specialisations for
#' specific paths.
#'
#' # The Handler
#' The handler is a standard R function that is called when a request is made
#' that matches the handlers path (unless a more specific handler path exists —
#' see above). A handler function can perform any operation a normal R function
#' can do, though you should consider strongly the security implications of your
#' handler functions. However, there are certain expectations in plumber around
#' the arguments a handler function takes and the return value it provides
#'
#' ## Handler Arguments
#' The handler function can take one or more of the following arguments.
#' * **Path arguments**: Any path arguments are passed on to the handler. If a
#'   handler is registered for the following path
#'   `user/<username>/settings/<setting>/` and it handles a request to
#'   `user/thomas/settings/interests/` then it will be called with
#'   `username = "thomas", setting = "interest"`
#' * `request`: The request the handler is responding to as a [reqres::Request]
#'   object
#' * `response`: The response being returned to the client as a
#'   [reqres::Response] object
#' * `server`: The [Plumber2] object representing your server implementation
#' * `client_id`: A string uniquely identifying the session the request comes
#'   from
#' * `query`: A list giving any additional arguments passed into the handler as
#'   part of the url query string
#' * `body`: The request body, parsed as specified by the provided parsers
#'
#' ## Handler Return Value
#' Handlers can return a range of different value types, which will inform
#' plumber2 what to do next:
#'
#' ### Returning `Next` or `Break`
#' These two control objects informs plumber2 to either proceed handling the
#' request (`Next`) or return the response as is, circumventing any remaining
#' routes (`Break`)
#'
#' ### Returning `NULL` or the `response` object
#' This is the same as returning `Next`, i.e. it signals that handling can
#' proceed
#'
#' ### Returning a ggplot2 object
#' If you return a ggplot2 object it will get plotted for you (and added to the
#' response assuming a graphics serializer is provided) before handling
#' continues
#'
#' ### Returning any other value
#' Any kind of value returned that is not captured by the above description will
#' be set to the response body (overwriting what was already there) and
#' handling is then allowed to continue
#'
#' ## Handler conditions
#' Like any function in R, a handler may need to signal that something happened,
#' either by throwing an error or warning or by emitting a message. You can use
#' [stop()], [warning()], and [message()] as you are used to. For all of them,
#' the condition message will end up in the log. Further, for `stop()` any
#' further handling of the request will end and a `500 Internal Error` response
#' is returned. To take more control over problems you can use the
#' [`abort_*()`][abort_status] family of conditions from reqres. Like `stop()`
#' they will halt any further processing, but they also allow control over what
#' kind of response is sent back, what kind of information about the issue is
#' communicated to the client, and what kind of information is logged
#' internally. The response they send back (except for `abort_status()`) all
#' adhere to the HTTP Problem spec defined in
#' [RFC 9457](https://datatracker.ietf.org/doc/html/rfc9457).
#'
#' While it may feel like a good idea to send a detailed error message back to
#' the client it is often better to only inform the client of what they need to
#' change to solve the issue. Too much information about internal implementation
#' details can be a security risk and forwarding internal errors to a client can
#' help inform the client about how the server has been implemented.
#'
#' # Async handling
#' plumber2 supports async handling of requests in one of two ways:
#'
#' 1. The handler you provide returns a promise object
#' 2. You set `async = TRUE` (or the name of a registered async evaluator) when
#'    adding the handler
#'
#' For 1), there is no more to do. You have full custody over the created
#' promise and any `then()`-chaining that might be added to it. For 2) it is a
#' bit different. In that case you provide a regular function and plumber2 takes
#' care of converting it to a promise. Due to the nature of promises a handler
#' being converted to a promise can't take `request`, `response`, and `server`
#' arguments, so if you need to manipulate these you need to use `then` (more on
#' this shortly). The async handler should yield the value that the response
#' should ultimately get assigned to the body or have plotting side effects (in
#' which case the plot will get added to the response).
#'
#' ## Async chaining
#' Because you can't manipulate `request` `response`, or `server` in the async
#' handler it may be needed to add operations to perform once the async handler
#' has finished. This can be done through the `then` argument (or using the
#' `@then` tag in annotated route files). This takes a list of functions to
#' chain to the promise using [promises::then()]. Before the `then` chain is
#' executed the response will get the return value of the main handler asigned
#' to the body. Each `then` call will receive the same arguments as a standard
#' request handler as well as `result` which will hold the return value of the
#' previous handler in the chain. For the first `then` call `result` will be a
#' boolean signalling if the async handler wants request handling to proceed to
#' the next route or terminate early. The last call in the chain must return
#' `Next` or `Break` to signal if processing should be allowed to continue to
#' the next route.
#'
#' # Using annotation
#' Handlers can be specified in an annotated route file using one of the method
#' tags followed by the path it pertains to. You can use various tags to
#' descripe the handler and these will automatically be converted to OpenAPI
#' documentation. Further, additional tags allow you to modify the behaviour of
#' the handler, reflecting the arguments available in the functional approach.
#'
#' ```
#' #* A handler for /user/<username>
#' #*
#' #* @param username:string The name of the user to provide information on
#' #*
#' #* @get /user/<username>
#' #*
#' #* @response 200:{name:string, age:integer, hobbies:[string]} Important
#' #* information about the user such as their name, age, and hobbies
#' #*
#' function(username) {
#'   find_user_in_db(username)
#' }
#' ```
#'
#' You can create async handlers with `then` chaining using annotation, through
#' the `@async` and `@then` tags
#'
#' ```
#' #* A handler for /user/<username>
#' #*
#' #* @param username:string The name of the user to provide information on
#' #*
#' #* @get /user/<username>
#' #*
#' #* @response 200:{name:string, age:integer, hobbies:[string]} Important
#' #* information about the user such as their name, age, and hobbies
#' #*
#' #* @async
#' function(username) {
#'   find_user_in_db(username)
#' }
#' #* @then
#' function(server, response) {
#'   server$log("message", "async operation completed")
#'   response$set_header("etag", "abcdef")
#'   Next
#' }
#' ```
#'
#' @param api A plumber2 api object to add the handler to
#' @param path A string giving the path the handler responds to. See Details
#' @param handler A handler function to call when a request is matched to the
#' path
#' @param serializers A named list of serializers that can be used to format the
#' response before sending it back to the client. Which one is selected is based
#' on the request `Accept` header. See [get_serializers()] for a helper to
#' construct this
#' @param parsers A named list of parsers that can be used to parse the
#' request body before passing it in as the `body` argument. Which one is
#' selected is based on the request `Content-Type` header. See [get_parsers()]
#' for a helper to construct this
#' @param use_strict_serializer By default, if a serializer that respects the
#' requests `Accept` header cannot be found, then the first of the provided ones
#' are used. Setting this to `TRUE` will instead send back a
#' `406 Not Acceptable` response
#' @param download Should the response mark itself for download instead of being
#' shown inline? Setting this to `TRUE` will set the `Content-Disposition`
#' header in the response to `attachment`. Setting it to a string is equivalent
#' to setting it to `TRUE` but will in addition also set the default filename of
#' the download to the string value
#' @param async If `FALSE` create a regular handler. If `TRUE`, use the default
#' async evaluator to create an async handler. If a string, the async evaluator
#' registered to that name is used. If a function is provided then this is used
#' as the async evaluator. See the *Async* section for more detail
#' @param then A list of function to be called once the async handler is done.
#' The functions will be chained using [promises::then()]. See the *Async*
#' section for more detail
#' @param doc A list with the OpenAPI spec for the endpoint
#' @param route The route this handler should be added to. Defaults to the last
#' route in the stack. If the route does not exist it will be created as the
#' last route in the stack
#'
#' @return These functions return the `api` object allowing for easy chaining
#' with the pipe
#'
#' @rdname api_request_handlers
#' @name api_request_handlers
#'
#' @family Request Handlers
#'
#' @examples
#' # Standard use
#' api() |>
#'   api_get("/hello/<name:string>", function(name) {
#'     list(
#'       msg = paste0("Hello ", name, "!")
#'     )
#'   })
#'
#' # Specify serializers
#' api() |>
#'   api_get(
#'     "/hello/<name:string>",
#'     function(name) {
#'       list(
#'         msg = paste0("Hello ", name, "!")
#'       )
#'     },
#'     serializers = get_serializers(c("json", "xml"))
#'   )
#'
#' # Request a download and make it async
#' api() |>
#'   api_get(
#'     "/the_plot",
#'     function() {
#'       plot(1:10, 1:10)
#'     },
#'     serializers = get_serializers(c("png", "jpeg")),
#'     download = TRUE,
#'     async = TRUE
#'   )
#'
NULL

#' @rdname api_request_handlers
#' @export
#'
api_get <- handle_constructor("get")
#' @rdname api_request_handlers
#' @export
#'
api_head <- handle_constructor("head")
#' @rdname api_request_handlers
#' @export
#'
api_post <- handle_constructor("post")
#' @rdname api_request_handlers
#' @export
#'
api_put <- handle_constructor("put")
#' @rdname api_request_handlers
#' @export
#'
api_delete <- handle_constructor("delete")
#' @rdname api_request_handlers
#' @export
#'
api_connect <- handle_constructor("connect")
#' @rdname api_request_handlers
#' @export
#'
api_options <- handle_constructor("options")
#' @rdname api_request_handlers
#' @export
#'
api_trace <- handle_constructor("trace")
#' @rdname api_request_handlers
#' @export
#'
api_patch <- handle_constructor("patch")
#' @rdname api_request_handlers
#' @export
#'
api_any <- handle_constructor("any")

#' Add a handler for a request header
#'
#' These handlers are called before the request body has been recieved and lets
#' you preemptively reject requests before recieving their full content. If the
#' handler does not return [Next] then the request will be returned at once.
#' Most of your logic, however, will be in the main handlers and you are asked to
#' consult the [api_request_handlers] docs for in-depth details on how to use
#' request handlers in general.
#'
#' # Using annotation
#' Adding request header handler is done in the same way as for [standard
#' request handlers][api_request_handlers]. The only difference is that you
#' include a `@header` tag as well. It is not normal to document header requests
#' as they usually exist as internal controls. You can add `@noDoc` to avoid
#' generating OpenAPI docs for the handler
#'
#' ```
#' #* A header handler authorizing users
#' #*
#' #* @get /*
#' #*
#' #* @header
#' #* @noDoc
#' function(client_id, response) {
#'   if (user_is_allowed(username)) {
#'     Next
#'   } else {
#'     response$status <- 404L
#'     Break
#'   }
#' }
#' ```
#'
#' @inheritParams api_request_handlers
#'
#' @return These functions return the `api` object allowing for easy chaining
#' with the pipe
#'
#' @rdname api_request_header_handlers
#' @name api_request_header_handlers
#'
#' @family Request Handlers
#'
#' @examples
#' # Simple size limit (better to use build-in functionality)
#' api() |>
#'   api_post_header(
#'     "/*",
#'     function(request, response) {
#'       if (request$get_header("content-type") > 1024) {
#'         response$status <- 413L
#'         Break
#'       } else {
#'         Next
#'       }
#'     }
#'   )
#'
#'
NULL

#' @rdname api_request_header_handlers
#' @export
#'
api_get_header <- handle_constructor("get", header = TRUE)
#' @rdname api_request_header_handlers
#' @export
#'
api_head_header <- handle_constructor("head", header = TRUE)
#' @rdname api_request_header_handlers
#' @export
#'
api_post_header <- handle_constructor("post", header = TRUE)
#' @rdname api_request_header_handlers
#' @export
#'
api_put_header <- handle_constructor("put", header = TRUE)
#' @rdname api_request_header_handlers
#' @export
#'
api_delete_header <- handle_constructor("delete", header = TRUE)
#' @rdname api_request_header_handlers
#' @export
#'
api_connect_header <- handle_constructor("connect", header = TRUE)
#' @rdname api_request_header_handlers
#' @export
#'
api_options_header <- handle_constructor("options", header = TRUE)
#' @rdname api_request_header_handlers
#' @export
#'
api_trace_header <- handle_constructor("trace", header = TRUE)
#' @rdname api_request_header_handlers
#' @export
#'
api_patch_header <- handle_constructor("patch", header = TRUE)
#' @rdname api_request_header_handlers
#' @export
#'
api_any_header <- handle_constructor("any", header = TRUE)

#' Add a new route to either the request or header router
#'
#' This function allows explicit creation of routes or addition/merging of a
#' predefined [routr::Route] into the router of the api. A new route can also be
#' created with the `route` argument when [adding a handler][api_get]. However,
#' that way will always add new routes to the end of the stack, whereas using
#' `api_add_route()` allows you full control of the placement.
#'
#' # Using annotation
#' There is no direct equivalent to this when using annotated route files.
#' However you can name your route in a file by adding `@routeName <name>` to
#' the first block of the file like so.
#'
#' ```
#' #* @routeName my_route
#' NULL
#' ```
#'
#' All relevant blocks in the file will then be added to this route, even if the
#' route already exist. In that way you can split the definition of a single
#' route out among multiple files if needed.
#'
#' @param api A plumber2 api object to add the route to
#' @param name The name of the route to add. If a route is already present
#' with this name then the provided route (if any) is merged into it
#' @param route The route to add. If `NULL` a new empty route will be
#' created
#' @param header Logical. Should the route be added to the header router?
#' @param after The location to place the new route on the stack. `NULL`
#' will place it at the end. Will not have an effect if a route with the
#' given name already exists.
#' @param root The root path to serve this route from.
#'
#' @return This functions return the `api` object allowing for easy chaining
#' with the pipe
#'
#' @export
#'
#' @examples
#' # Add a new route and use it for a handler
#' api() |>
#'   api_add_route("logger_route") |>
#'   api_any(
#'     "/*",
#'     function() {
#'       cat("I just handled a request!")
#'     },
#'     route = "logger_route"
#'   )
#'
#'
api_add_route <- function(
  api,
  name,
  route = NULL,
  header = FALSE,
  after = NULL,
  root = ""
) {
  api$add_route(name = name, route = route, header = header, after = after, root = root)
  api
}

#' Add a handler to a WebSocket message
#'
#' WebSockets is a bidirectional communication channel that can be established
#' at the request of the client. While websocket communication is not really
#' part of a standard REST api, it has many uses and can easily be used together
#' with one.
#'
#' A handler for a websocket message is much simpler than for requests in
#' general since it doesn't have to concern itself with methods, paths, and
#' responses. Any message handler registered will get called in sequence when a
#' websocket message is recieved from a client. Still, a few expectations apply
#'
#' ## Handler Arguments
#' The handler can take any of the following arguments:
#' * `message`: Either a raw vector if the message recieved is in binary form or
#'   a single string, giving the message sent from the client
#' * `server`: The [Plumber2] object representing your server implementation
#' * `client_id`: A string uniquely identifying the session the request comes
#'   from
#' * `request`: The request that was initially used to establish the websocket
#'   connection with the client as a [reqres::Request] object
#'
#' ## Handler Return Value
#' It is not expected that a websocket message sends a response and thus the
#' handler is not required to do anything like that. However, if the handler
#' returns either a raw vector or a single string it is taken as a signal to
#' send this back to the client. Any other return value is silently ignored.
#'
#' # Async
#' You can handle websocket messages asynchronously if needed. Like with
#' [request handlers][api_get] you can either do it manually by creating and
#' returning a promise inside the handler, or by letting plumber2 convert your
#' handler to an async handler using the `async` argument. Due to the nature of
#' promises a handler being converted to a promise can't take `request` and
#' `server` arguments, so if you need to manipulate these you need to use `then`
#' (more on this shortly). The same conventions about return value holds for
#' async message handlers as for regular ones.
#'
#' ## Async chaining
#' Because you can't manipulate `request` or `server` in the async handler it
#' may be needed to add operations to perform once the async handler has
#' finished. This can be done through the `then` argument. This takes a list of
#' functions to chain to the promise using [promises::then()]. Before the `then`
#' chain is executed the return value of the async handler will be send back to
#' the client if it is a string or a raw vector. Each `then` call will receive
#' the same arguments as a standard message handler as well as `result` which
#' will hold the return value of the previous handler in the chain. For the
#' first `then` call `result` will be whatever the main async handler returned.
#' The return value of the last call in the chain will be silently ignored.
#'
#' # Using annotation
#' A websocket message handler can be added to an API in an annotated route file
#' by using the `@message` tag
#'
#' ```
#' #* @message
#' function(message) {
#'   if (message == "Hello") {
#'     return("Hello, you...")
#'   }
#' }
#' ```
#'
#' You can create async handlers with `then` chaining using annotation, through
#' the `@async` and `@then` tags
#'
#' ```
#' #* @message
#' #* @async
#' function(message) {
#'   if (message == "Hello") {
#'     return("Hello, you...")
#'   }
#' }
#' #* @then
#' function(server) {
#'   server$log("message", "websocket message received")
#' }
#' ```
#'
#' @param api A plumber2 api object to add the handler to
#' @param handler A function conforming to the specifications laid out in
#' Details
#' @param async If `FALSE` create a regular handler. If `TRUE`, use the default
#' async evaluator to create an async handler. If a string, the async evaluator
#' registered to that name is used. If a function is provided then this is used
#' as the async evaluator. See the *Async* section for more detail
#' @param then A list of function to be called once the async handler is done.
#' The functions will be chained using [promises::then()]. See the *Async*
#' section for more detail
#'
#' @return This functions return the `api` object allowing for easy chaining
#' with the pipe
#'
#' @export
#'
#' @examples
#' api() |>
#'   api_message(
#'     function(message) {
#'       if (message == "Hello") {
#'         return("Hello, you...")
#'       }
#'     }
#'   )
#'
api_message <- function(api, handler, async = NULL, then = NULL) {
  api$message_handler(handler, async, then)
  api
}

#' Redirect request to another resource
#'
#' While it is optimal that an API remains stable over its lifetime it is often
#' not fully attainable. In order to direct requests for resources that has
#' been moved to the new location you can add a redirect that ensures a smooth
#' transition for clients still using the old path. Depending on the value
#' of `permanent` the redirect will respond with a `307 Temporary Redirect` or
#' `308 Permanent Redirect`. `from` and `to` can contain path parameters and
#' wildcards which will be matched between the two to construct the correct
#' redirect path. Further, `to` can either be a path to the same server or a
#' fully qualified URL to redirect requests to another server alltogether.
#'
#' # Using annotation
#' You can specify redirects in an annotated plumber file using the `@redirect`
#' tag. Preceed the method with a `!` to mark the redirect as permanent
#'
#' ```
#' #* @redirect !get /old/data/* /new/data/*
#' #* @redirect any /unstable/endpoint /stable/endpoint
#' NULL
#' ```
#'
#' @param api A plumber2 api object to add the redirect to
#' @param method The HTTP method the redirect should respond to
#' @param from The path the redirect should respond to
#' @param to The path/URL to redirect the incoming request towards. After
#' resolving any path parameters and wildcards it will be used in the
#' `Location` header
#' @param permanent Logical. Is the redirect considered permanent or
#' temporary? Determines the type of redirct status code to use
#'
#' @return This functions return the `api` object allowing for easy chaining
#' with the pipe
#'
#' @export
#'
#' @examples
#' api() |>
#'   api_redirect("get", "/old/data/*", "/new/data/*")
#'
api_redirect <- function(api, method, from, to, permanent = TRUE) {
  api$redirect(method, from, to, permanent)
  api
}

#' Serve a Shiny app from a plumber2 api
#'
#' You can serve one or more shiny apps as part of a plumber2 api. The shiny app
#' launches in a background process and the api will work as a reverse proxy to
#' forward requests to `path` to the process and relay the response to the
#' client. The shiny app is started along with the api and shut down once the
#' api is stopped. This functionality requires the shiny and callr packages to
#' be installed. Be aware that all requests to subpaths of `path` will be
#' forwarded to the shiny process, and thus not end up in your normal route
#'
#' # Using annotation
#' A shiny app can be served using an annotated route file by using the `@shiny`
#' tag and proceeding the annotation block with the shiny app object
#'
#' ```
#' #* @shiny /my_app/
#' shiny::shinyAppDir("./shiny")
#' ```
#'
#' @param api A plumber2 api to add the shiny app to
#' @param path The path to serve the shiny app from
#' @param app A shiny app object
#'
#' @return This functions return the `api` object allowing for easy chaining
#' with the pipe
#'
#' @export
#'
#' @examplesIf requireNamespace("shiny", quietly = TRUE)
#' blank_shiny <- shiny::shinyApp(
#'   ui = shiny::fluidPage(),
#'   server = shiny::shinyServer(function(...) {})
#' )
#'
#' api() |>
#'   api_shiny("my_app/", blank_shiny)
#'
api_shiny <- function(api, path, app) {
  api$add_shiny(path, app)
  api
}

#' Set up a plumber2 api to act as a reverse proxy
#'
#' You can set up your plumber2 api to act as reverse proxy and forward all
#' requests to a specific path (and it's subpaths) to a different URL. In
#' contrast to [api_shiny()], `api_forward()` is not responsible for launching
#' whatever service is being proxied so this should be handled elsewhere. The
#' `path` will be stripped from the request before being forwarded to the url,
#' meaning that if you set up a proxy on `my/proxy/` to `http://example.com`,
#' then a request for `my/proxy/user/thomas` will end at
#' `http://example.com/user/thomas`. Proxying is most useful when forwarding to
#' internal servers though you are free to forward to public URLs as well.
#' However, for the later you'd usually use a redirect instead (via
#' [api_redirect()])
#'
#' # Using annotation
#' You can set up a reverse proxy in your annotated route file using the
#' `@forward` tag
#'
#' ```
#' #* @forward /proxy http://127.0.0.1:56789
#' NULL
#' ```
#'
#' @param api A plumber2 api to add the shiny app to
#' @param path The path to serve the shiny app from
#' @param url The url to forward to
#'
#' @return This functions return the `api` object allowing for easy chaining
#' with the pipe
#'
#' @export
#'
#' @examples
#' # Serve wikipedia directly from your app
#' api() |>
#'   api_forward("my_wiki/", "https://www.wikipedia.org")
#'
api_forward <- function(api, path, url) {
  api$forward(path, url)
  api
}
