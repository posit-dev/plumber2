# Serve Quarto and Rmarkdown documents from a plumber2 api

You can serve Quarto and Rmarkdown documents from a plumber2 api and
have it automatically render the report when requested. Reports are
automatically cached to reduce overhead. Parameterized reports are
supported and parameters can be provided either with the query string
for GET requests or in the request body for POST request. It is also
possible to delete the cached render using a DELETE request. See
**Details** for more information

## Usage

``` r
api_report(
  api,
  path,
  report,
  ...,
  doc = NULL,
  max_age = Inf,
  async = TRUE,
  finalize = NULL,
  continue = FALSE,
  cache_dir = tempfile(pattern = "plumber2_report"),
  cache_by_id = FALSE,
  auth_flow = NULL,
  auth_scope = NULL,
  route = NULL
)
```

## Arguments

- api:

  A plumber2 api to serve the report with.

- path:

  The base path to serve the report from. Additional endpoints will be
  created in addition to this.

- report:

  The path to the report to serve

- ...:

  Further arguments to
  [`quarto::quarto_render()`](https://quarto-dev.github.io/quarto-r/reference/quarto_render.html)
  or
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)

- doc:

  An
  [`openapi_operation()`](https://plumber2.posit.co/reference/openapi.md)
  documentation for the report. Only `query` parameters will be used and
  a request body will be generated from this for the POST methods.

- max_age:

  The maximum age in seconds to keep a rendered report before initiating
  a re-render

- async:

  Should rendering happen asynchronously (using mirai)

- finalize:

  An optional function to run before sending the response back. The
  function will receive the request as the first argument, the response
  as the second, and the server as the third.

- continue:

  A logical that defines whether the response is returned directly after
  rendering or should be made available to subsequent routes

- cache_dir:

  The location of the render cache. By default a temporary folder is
  created for it.

- cache_by_id:

  Should caching be scoped by the user id. If the rendering is dependent
  on user-level access to different data this is necessary to avoid data
  leakage.

- auth_flow:

  A logical expression giving the authentication flow the client must
  pass to get access to the resource.

- auth_scope:

  The scope requirements of the resource

- route:

  The route this handler should be added to. Defaults to the last route
  in the stack. If the route does not exist it will be created as the
  last route in the stack.

## Value

This functions return the `api` object allowing for easy chaining with
the pipe

## Details

### Parameterized reports

Parameters provided to parameterized reports are automatically type
checked and casted based on the default values in the report and the
schema provided in the `doc`. Only the `query` parameters will be used
as the request body is inferred from that. It is important to understand
that for Quarto documents, the parameters are passed through as a yaml
file and thus any type not supported by yaml will not arrive unchanged
to the document. Python reports are supported, but the type of
parameters cannot be inferred from the document so if you want type
checking you will have to provided schemas for them in the `doc`. For
POST request where the parameters are provided in the body, you must use
JSON format.

### Multiple outputs

If a report has multiple different output formats then each format is
accessible through a subpath with the name of the format. The path
provided in `path` will use content negotiation through the
`Content-Type` header to select a format. In addition, a path with the
file extension added to `path` can also be used to select the specific
format. For the last two, if multiple formats share the same mime
type/file extension then only the first one can be selected.

### Caching

Reports are cached, by default in a temporary folder. You can chose a
different folder with the `cache_dir` argument. Cached versions can be
deleted, thus forcing a rerender upon next request, by sending a DELETE
request. A DELETE request to the main path will delete all versions of
the report, whereas a DELETE request to one of the subpaths (see above)
will only delete versions of the specific output format. All different
parameterized versions will always be deleted together. Instead of
sending DELETE requests you can also set a `max_age` which will force a
rerender if the render is older than the given argument.

If the content of the report is dependent on different credentials given
by the user you can cache the reports by session id so that every user
will have it rendered uniquely for them. This is important to prevent
leakage of confidential data, but also ensures that the report looks as
expected for each user.

## Using annotation

A report can be served using an annotated route file by using the
`@report` tag and proceeding the annotation block with the path to the
report

    #* @report /quarterly
    "my/awesome/report.qmd"

## Examples

``` r
if (FALSE) { # file.exists("my/awesome/report.qmd")
api() |>
  api_report("/quarterly", "my/awesome/report.qmd")
}
```
