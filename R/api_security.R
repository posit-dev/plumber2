#' Add various security related headers to your plumber2 API
#'
#' This function adds the [SecurityHeaders][firesafety::SecurityHeaders] plugin
#' to your plumber2 API. Please consult the documentation for the plugin for
#' up-to-date information on its behaviour.
#'
#' # Using annotation
#' Security headers doesn't have a dedicated annotation tag, but you can set
#' it up in a `@plumber` block
#'
#' ```
#' #* @plumber
#' function(api) {
#'   api |>
#'     api_security_headers()
#' }
#' ```
#'
#' @param api A plumber2 api object to add the plugin to
#' @param content_security_policy Set the value of the `Content-Security-Policy`
#' header. See [firesafety::csp()] for documentation of its values
#' @param content_security_policy_report_only Set the value of the
#' `Content-Security-Policy-Report-Only` header. See [firesafety::csp()] for
#' documentation of its values
#' @param cross_origin_embedder_policy Set the value of the
#' `Cross-Origin-Embedder-Policy`. Possible values are `"unsafe-none"`,
#' `"require-corp"`, and `"credentialless"`
#' @param cross_origin_opener_policy Set the value of the
#' `Cross-Origin-Opener-Policy`. Possible values are `"unsafe-none"`,
#' `"same-origin-allow-popups"`, `"same-origin"`, and
#' `"noopener-allow-popups"`
#' @param cross_origin_resource_policy Set the value of the
#' `Cross-Origin-Resource-Policy`. Possible values are `"same-site"`,
#' `"same-origin"`, and `"cross-origin"`
#' @param origin_agent_cluster Set the value of the
#' `Origin-Agent-Cluster`. Possible values are `TRUE` and `FALSE`
#' @param referrer_policy Set the value of the
#' `Referrer-Policy`. Possible values are `"no-referrer"`,
#' `"no-referrer-when-downgrade"`, `"origin"`, `"origin-when-cross-origin"`,
#' `"same-origin"`, `"strict-origin"`, `"strict-origin-when-cross-origin"`,
#' and `"unsafe-url"`
#' @param strict_transport_security Set the value of the
#' `Strict-Transport-Security` header. See [firesafety::sts()] for documentation
#' of its values
#' @param x_content_type_options Set the value of the
#' `X-Content-Type-Options`. Possible values are `TRUE` and `FALSE`
#' @param x_dns_prefetch_control Set the value of the
#' `X-DNS-Prefetch-Control`. Possible values are `TRUE` and `FALSE`
#' @param x_download_options Set the value of the
#' `X-Download-Options`. Possible values are `TRUE` and `FALSE`
#' @param x_frame_options Set the value of the
#' `X-Frame-Options`. Possible values are `"DENY"` and `"SAMEORIGIN"`
#' @param x_permitted_cross_domain_policies Set the value of the
#' `X-Permitted-Cross-Domain-Policies`. Possible values are `"none"`,
#' `"master-only"`, `"by-content-type"`, `"by-ftp-filename"`, `"all"`, and
#' `"none-this-response"`
#' @param x_xss_protection Set the value of the
#' `X-XSS-Protection`. Possible values are `TRUE` and `FALSE`
#'
#' @return This functions return the `api` object allowing for easy chaining
#' with the pipe
#'
#' @export
#' @family security features
#'
#' @examples
#' # Add default security headers to an API
#' api() |>
#'   api_security_headers()
#'
api_security_headers <- function(
  api,
  content_security_policy = csp(
    default_src = "self",
    script_src = "self",
    script_src_attr = "none",
    style_src = c("self", "https:", "unsafe-inline"),
    img_src = c("self", "data:"),
    font_src = c("self", "https:", "data:"),
    object_src = "none",
    base_uri = "self",
    form_action = "self",
    frame_ancestors = "self",
    upgrade_insecure_requests = TRUE
  ),
  content_security_policy_report_only = NULL,
  cross_origin_embedder_policy = NULL,
  cross_origin_opener_policy = "same-origin",
  cross_origin_resource_policy = "same-origin",
  origin_agent_cluster = TRUE,
  referrer_policy = "no-referrer",
  strict_transport_security = sts(
    max_age = 63072000,
    include_sub_domains = TRUE
  ),
  x_content_type_options = TRUE,
  x_dns_prefetch_control = FALSE,
  x_download_options = TRUE,
  x_frame_options = "SAMEORIGIN",
  x_permitted_cross_domain_policies = "none",
  x_xss_protection = FALSE
) {
  headers <- firesafety::SecurityHeaders$new(
    content_security_policy = content_security_policy,
    content_security_policy_report_only = content_security_policy_report_only,
    cross_origin_embedder_policy = cross_origin_embedder_policy,
    cross_origin_opener_policy = cross_origin_opener_policy,
    cross_origin_resource_policy = cross_origin_resource_policy,
    origin_agent_cluster = origin_agent_cluster,
    referrer_policy = referrer_policy,
    strict_transport_security = strict_transport_security,
    x_content_type_options = x_content_type_options,
    x_dns_prefetch_control = x_dns_prefetch_control,
    x_download_options = x_download_options,
    x_frame_options = x_frame_options,
    x_permitted_cross_domain_policies = x_permitted_cross_domain_policies,
    x_xss_protection = x_xss_protection
  )
  api$attach(headers)
  api
}

#' @importFrom firesafety csp
#' @export
firesafety::csp
#' @importFrom firesafety sts
#' @export
firesafety::sts

#' Set up CORS for a path in your plumber2 API
#'
#' This function adds Cross-Origin Resource Sharing (CORS) to a path in your
#' API. The function can be called multiple times to set up CORS for multiple
#' paths, potentially with different settings for each path. CORS is a complex
#' specification and more can be read about it at the [CORS][firesafety::CORS]
#' plugin documentation.
#'
#' # Using annotation
#' To add CORS to a path you can add `@cors <origin>` to a
#' handler annotation. `<origin>` must be one or more URLs or `*`, separated by
#' comma (meaning it is not possible to provide a function using the annotation).
#' This will add CORS to all endpoints described in the block. The annotation
#' doesn't allow setting `allowed_headers`, `exposed_headers`,
#' `allow_credentials` or `max_age` and the default values will be used.
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
#' #* @cors https://example.com, https://another-site.com
#' #*
#' function(username) {
#'   find_user_in_db(username)
#' }
#' ```
#'
#' @param api A plumber2 api object to add the plugin to
#' @param path The path that the policy should apply to. routr path syntax
#' applies, meaning that wilcards and path parameters are allowed.
#' @param origin The origin allowed for the path. Can be one of:
#' * A boolean. If `TRUE` then all origins are permitted and the preflight
#'   response will have the `Access-Control-Allow-Origin` header reflect
#'   the origin of the request. If `FALSE` then all origins are denied
#' * The string `"*"` which will allow all origins and set
#'   `Access-Control-Allow-Origin` to `*`. This is different than setting it
#'   to `TRUE` because `*` instructs browsers that any origin is allowed and
#'   it may use this information when searching the cache
#' * A character vector giving allowed origins. If the request origin
#'   matches any of these then the `Access-Control-Allow-Origin` header in
#'   the response will reflect the origin of the request
#' * A function taking the request and returning `TRUE` if the origin is
#'   permitted and `FALSE` if it is not. If permitted the
#'   `Access-Control-Allow-Origin` header will reflect the request origin
#' @param methods The HTTP methods allowed for the `path`
#' @param allowed_headers A character vector of request headers allowed when
#' making the request. If the request contains headers not permitted, then
#' the response will be blocked by the browser. `NULL` will allow any header
#' by reflecting the `Access-Control-Request-Headers` header value from the
#' request into the `Access-Control-Allow-Headers` header in the response.
#' @param exposed_headers A character vector of response headers that should
#' be made available to the client upon a succesful request
#' @param allow_credentials A boolean indicating whether credentials are
#' allowed in the request. Credentials are cookies or HTTP authentication
#' headers, which are normally stripped from `fetch()` requests by the
#' browser. If this is `TRUE` then `origin` cannot be `*` according to the
#' spec
#' @param max_age The duration browsers are allowed to keep the preflight
#' response in the cache
#'
#' @return This functions return the `api` object allowing for easy chaining
#' with the pipe
#'
#' @export
#' @family security features
#'
#' @examples
#' # Set up cors for your asset/ path for the https://examples.com origin
#'
#' api() |>
#'   api_security_cors(
#'     path = "asset/*",
#'     origin = "https://examples.com"
#'   )
#'
api_security_cors <- function(
  api,
  path = "/*",
  origin = "*",
  methods = c("get", "head", "put", "patch", "post", "delete"),
  allowed_headers = NULL,
  exposed_headers = NULL,
  allow_credentials = FALSE,
  max_age = NULL
) {
  cors <- api$plugins$cors
  if (is.null(cors)) {
    cors <- firesafety::CORS$new(
      path = as_routr_path(path),
      origin = origin,
      methods = methods,
      allowed_headers = allowed_headers,
      exposed_headers = exposed_headers,
      allow_credentials = allow_credentials,
      max_age = max_age
    )
    api$attach(cors)
  } else {
    cors$add_path(
      path = as_routr_path(path),
      origin = origin,
      methods = methods,
      allowed_headers = allowed_headers,
      exposed_headers = exposed_headers,
      allow_credentials = allow_credentials,
      max_age = max_age
    )
  }
  api
}

#' Set up resource isolation for a path
#'
#' This function adds resource isolation to a path in your API. The function can
#' be called multiple times to set up resource isolation for multiple
#' paths, potentially with different settings for each path. You can read in
#' depth about resource isolation at the
#' [ResourceIsolation][firesafety::ResourceIsolation] plugin documentation.
#'
#' # Using annotation
#' To add resource isolation to a path you can add `@rip <allowed_site>` to a
#' handler annotation. This will add resource isolation to all endpoints
#' described in the block. The annotation doesn't allow setting
#' `forbidden_navigation` or `allow_cors` and the default values will be used.
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
#' #* @rip same-origin
#' #*
#' function(username) {
#'   find_user_in_db(username)
#' }
#' ```
#'
#' @param api A plumber2 api object to add the plugin to
#' @param path The path that the policy should apply to. routr path syntax
#' applies, meaning that wilcards and path parameters are allowed.
#' @param allowed_site The allowance level to permit. Either `cross-site`,
#' `same-site`, or `same-origin`.
#' @param forbidden_navigation A vector of destinations not allowed for
#' navigational requests. See the [`Sec-Fetch-Dest` documentation](https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Sec-Fetch-Dest)
#' for a description of possible values. The special value `"all"` is also
#' permitted which is the equivalent of passing all values.
#' @param allow_cors Should `Sec-Fetch-Mode: cors` requests be allowed
#'
#' @return This functions return the `api` object allowing for easy chaining
#' with the pipe
#'
#' @export
#' @family security features
#'
#' @examples
#' # Set up resource isolation for everything inside a user path
#' api() |>
#'   api_security_resource_isolation(
#'     path = "<user>/*"
#'   )
#'
#'
api_security_resource_isolation <- function(
  api,
  path = "/*",
  allowed_site = "same-site",
  forbidden_navigation = c("object", "embed"),
  allow_cors = TRUE
) {
  ri <- api$plugins$resource_isolation
  if (is.null(ri)) {
    ri <- firesafety::ResourceIsolation$new(
      path = as_routr_path(path),
      allowed_site = allowed_site,
      forbidden_navigation = forbidden_navigation,
      allow_cors = allow_cors
    )
    api$attach(ri)
  } else {
    ri$add_path(
      path = as_routr_path(path),
      allowed_site = allowed_site,
      forbidden_navigation = forbidden_navigation,
      allow_cors = allow_cors
    )
  }
  api
}
