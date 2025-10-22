#' Add an authentication mechanism to your API
#'
#' This function adds an authentication scheme to your API. Notably, this does
#' not turn on authentication for any of your handlers but makes it available
#' for reference in an authentication flow. To use it, reference it in the
#' `auth_flow` argument of functions supporting it. Authentication schemes are
#' defined using the various `auth_*()` constructors in the fireproof package.
#' Refer to these for further documentation
#'
#' # Using annotation
#' To add an authenticator to your api defined in an annotated file use the
#' `@authenticator` tag:
#'
#' ```
#' #* @authenticator BasicAuth
#' fireproof::auth_basic(...)
#' ```
#'
#' The tag parameter (`BasicAuth`) provides the name for the authenticator
#'
#' @param api A plumber2 api object to add the authenticator to
#' @param auth An [Auth][fireproof::Auth] subclass object defining an
#' authentication scheme
#' @param name The name to use for referencing the scheme in an authentication
#' flow
#'
#' @return This functions return the `api` object allowing for easy chaining
#' with the pipe
#'
#' @export
#'
#' @examples
#' auth <- fireproof::auth_key("plumber2-key", "MY_VERY_SECRET_KEY")
#'
#' api() |>
#'   api_authenticator(auth, "cookie_key")
#'
api_authenticator <- function(api, auth, name = NULL) {
  api$add_authenticator(auth = auth, name = name)
  api
}

#' Add authentication to an endpoint
#'
#' This function adds authentication to a specific method + path. It does so by
#' defining an authentication flow the request must pass in order to proceed, as
#' well as an optional vector of scopes required. The flow is given as a logical
#' expression of [authenticators][api_authenticator] it must satisfy. If you
#' have registered two authenticators `auth1` and `auth2`, then a flow could be
#' `auth1 && auth2` to require that both authenticators must be passed to gain
#' access. Alternatively you could use `auth1 || auth2` to require that just one
#' of them are passed. Flows can be arbitrarily complex with nesting etc, but
#' the OpenAPI spec has limits to what it can describe so if you want to have an
#' OpenAPI compliant api you must limit yourself to at most two levels of
#' nesting with the outer level being `||` (ie.
#' `(auth1 && auth2) || (auth3 && auth4)` is ok, but
#' `(auth1 || auth2) && (auth3 || auth4)` is not due to the outer level being
#' `&&`, and `(auth1 && auth2) || (auth3 && (auth4 || auth5))` is not allowed
#' because it has 3 nesting levels). This is only a limitation of OpenAPI and
#' plumber2 itself can handle all of the above. If scope is given the scope of
#' the user after successful authentication must contain *all* of the provided
#' scopes. While this function allows you to add authentication to a path
#' directly, it is often more convenient to add it along with the resource or
#' functionality you want to protect. To that end, many functions such as
#' [api_get()] and [api_report()] also takes `auth_flow` and `auth_scope` as
#' input and if given will add authentication to the relevant endpoint.
#'
#' @param api A plumber2 api object to add authentication to
#' @param method The HTTP method to add authentication to
#' @param path A string giving the path to be authenticated
#' @param auth_flow A logical expression giving the authentication flow the
#' client must pass to get access to the resource.
#' @param auth_scope The scope requirements of the resource
#' @param add_doc Should OpenAPI documentation be added for the
#' authentication
#'
#' @return This functions return the `api` object allowing for easy chaining
#' with the pipe
#'
#' @export
#'
#' @examples
#' # We are not adding the authenticators here - only the authentication flow
#' # We assume the authenticators `oauth`, `basic`, and `key` will be added
#' # later
#' api() |>
#'   api_authentication(
#'     method = "get",
#'     path = "/user/<username>",
#'     auth_flow = oauth || (basic && key)
#'   )
#'
api_authentication <- function(
  api,
  method,
  path,
  auth_flow,
  auth_scope = NULL,
  add_doc = TRUE
) {
  api$add_authentication(
    method = method,
    path = path,
    auth_flow = {{ auth_flow }},
    auth_scope = auth_scope,
    add_doc = TRUE
  )
  api
}
