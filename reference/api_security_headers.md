# Add various security related headers to your plumber2 API

This function adds the
[SecurityHeaders](https://rdrr.io/pkg/firesafety/man/SecurityHeaders.html)
plugin to your plumber2 API. Please consult the documentation for the
plugin for up-to-date information on its behaviour.

## Usage

``` r
api_security_headers(
  api,
  content_security_policy = csp(default_src = "self", script_src = "self",
    script_src_attr = "none", style_src = c("self", "https:", "unsafe-inline"), img_src =
    c("self", "data:"), font_src = c("self", "https:", "data:"), object_src = "none",
    base_uri = "self", form_action = "self", frame_ancestors = "self",
    upgrade_insecure_requests = TRUE),
  content_security_policy_report_only = NULL,
  cross_origin_embedder_policy = NULL,
  cross_origin_opener_policy = "same-origin",
  cross_origin_resource_policy = "same-origin",
  origin_agent_cluster = TRUE,
  referrer_policy = "no-referrer",
  strict_transport_security = sts(max_age = 63072000, include_sub_domains = TRUE),
  x_content_type_options = TRUE,
  x_dns_prefetch_control = FALSE,
  x_download_options = TRUE,
  x_frame_options = "SAMEORIGIN",
  x_permitted_cross_domain_policies = "none",
  x_xss_protection = FALSE
)
```

## Arguments

- api:

  A plumber2 api object to add the plugin to

- content_security_policy:

  Set the value of the `Content-Security-Policy` header. See
  [`firesafety::csp()`](https://rdrr.io/pkg/firesafety/man/csp.html) for
  documentation of its values

- content_security_policy_report_only:

  Set the value of the `Content-Security-Policy-Report-Only` header. See
  [`firesafety::csp()`](https://rdrr.io/pkg/firesafety/man/csp.html) for
  documentation of its values

- cross_origin_embedder_policy:

  Set the value of the `Cross-Origin-Embedder-Policy`. Possible values
  are `"unsafe-none"`, `"require-corp"`, and `"credentialless"`

- cross_origin_opener_policy:

  Set the value of the `Cross-Origin-Opener-Policy`. Possible values are
  `"unsafe-none"`, `"same-origin-allow-popups"`, `"same-origin"`, and
  `"noopener-allow-popups"`

- cross_origin_resource_policy:

  Set the value of the `Cross-Origin-Resource-Policy`. Possible values
  are `"same-site"`, `"same-origin"`, and `"cross-origin"`

- origin_agent_cluster:

  Set the value of the `Origin-Agent-Cluster`. Possible values are
  `TRUE` and `FALSE`

- referrer_policy:

  Set the value of the `Referrer-Policy`. Possible values are
  `"no-referrer"`, `"no-referrer-when-downgrade"`, `"origin"`,
  `"origin-when-cross-origin"`, `"same-origin"`, `"strict-origin"`,
  `"strict-origin-when-cross-origin"`, and `"unsafe-url"`

- strict_transport_security:

  Set the value of the `Strict-Transport-Security` header. See
  [`firesafety::sts()`](https://rdrr.io/pkg/firesafety/man/sts.html) for
  documentation of its values

- x_content_type_options:

  Set the value of the `X-Content-Type-Options`. Possible values are
  `TRUE` and `FALSE`

- x_dns_prefetch_control:

  Set the value of the `X-DNS-Prefetch-Control`. Possible values are
  `TRUE` and `FALSE`

- x_download_options:

  Set the value of the `X-Download-Options`. Possible values are `TRUE`
  and `FALSE`

- x_frame_options:

  Set the value of the `X-Frame-Options`. Possible values are `"DENY"`
  and `"SAMEORIGIN"`

- x_permitted_cross_domain_policies:

  Set the value of the `X-Permitted-Cross-Domain-Policies`. Possible
  values are `"none"`, `"master-only"`, `"by-content-type"`,
  `"by-ftp-filename"`, `"all"`, and `"none-this-response"`

- x_xss_protection:

  Set the value of the `X-XSS-Protection`. Possible values are `TRUE`
  and `FALSE`

## Value

This functions return the `api` object allowing for easy chaining with
the pipe

## Using annotation

Security headers doesn't have a dedicated annotation tag, but you can
set it up in a `@plumber` block

    #* @plumber
    function(api) {
      api |>
        api_security_headers()
    }

## See also

Other security features:
[`api_security_cors()`](https://plumber2.posit.co/reference/api_security_cors.md),
[`api_security_resource_isolation()`](https://plumber2.posit.co/reference/api_security_resource_isolation.md)

## Examples

``` r
# Add default security headers to an API
api() |>
  api_security_headers()
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running
```
