# Annotations reference

## Annotations

Annotations are specially-structured comments used in your plumber file
to create an API. A full annotation line starts with `#*`, then the
annotation keyword `@...`, any number of space characters followed by
the content. If you wish to use the annotation to document your API file
but don’t want any OpenAPI documentation to be generated you can use
`@noDoc` tag which works much like roxygens `@noRd`.

## Annotation settings

A few annotations modify how the file is parsed and interpreted. These
must all appear in the first block of the file (in the case of
`@roxygenPrefix` the first line).

### Use roxygen prefix

By default, plumber2 uses the `#*` prefix for its blocks to avoid
clashing with roxygen comments. plumber (the old one) allowed either and
while it is recommended to differentiate between roxygen and plumber2
annotations you can opt in to also understanding roxygen prefix.

> This tag **must** appear on the first line of the file to have an
> effect.

| Annotation       | Argument | Description/References               |
|------------------|----------|--------------------------------------|
| `@roxygenPrefix` | None     | Turns on roxygen prefix for the file |

If you wish to always allow roxygen prefixes you can set the
`plumber2.roxygenPrefix` option to `TRUE`

##### Annotations example

``` r
#* @roxygenPrefix
NULL
```

##### Equivalent programmatic usage

There is no direct equivalent in programmatic usage as this pertains to
the parsing of the plumber file.

### Specifying route name

By default, the name of the file (without extension) is used as the name
for the route any handlers etc are attached to. However, you can change
this by having a first block in your file with a `@routeName` tag
specifying the name to use for all route specific blocks in the file.
This also makes it possible to split out the specification of a single
route among multiple files if it begins to grow unwieldy

| Annotation   | Argument | Description/References                                       |
|--------------|----------|--------------------------------------------------------------|
| `@routeName` | `name`   | Sets the name of the route to use for all blocks in the file |

##### Annotations example

``` r
#* @routeName main_route
NULL
```

##### Equivalent programmatic usage

There is no direct equivalent in programmatic usage as this pertains to
the parsing of the plumber file. Any relevant function has a `route`
argument where you can specify which route to add the given
functionality to.

### Specifying route order

Route files are processed in alphanumerical order which influence the
order in which their route is attached. You can modify the route order
(but not the file processing order) using the `@routeOrder` tag. If
multiple files have the same `@routeName` then the lowest `@routeOrder`
value will be used. Any file without a `@routeOrder` tag will be added
after those that do, using the default alphanumeric order.

| Annotation    | Argument | Description/References                                                                   |
|---------------|----------|------------------------------------------------------------------------------------------|
| `@routeOrder` | `order`  | Sets the order in which routes are added to the api. Lower values means earlier position |

##### Annotations example

``` r
#* @routeOrder 3
NULL
```

##### Equivalent programmatic usage

You can programmatically control the location of a route you add with
[`api_add_route()`](https://plumber2.posit.co/reference/api_add_route.md)
using the `after` argument.

### Specifying root url for the whole file

Many annotations somehow references URL paths, either as the endpoint of
a handler, or where to serve a shiny app from. You can set a root for
the whole file which will be prepended to any relevant URL. In this way
it is easy to move all functionality from a single file to a different
location if necessary.

| Annotation | Argument | Description/References                      |
|------------|----------|---------------------------------------------|
| `@root`    | `path`   | Sets the root of every URL path in the file |

##### Annotations example

``` r
#* @root /path/to/these/apis
NULL
```

##### Equivalent programmatic usage

You can programmatically set the root of a new route you add with
[`api_add_route()`](https://plumber2.posit.co/reference/api_add_route.md)
using the `root` argument.

## Global annotations

Global annotations are not related to any handler and should be placed
in their own block. The block should be terminated by a `"_API"`
expression. Instead of `@title` and `@description` you can also use the
convention that the first line gives the title and any proceeding lines
until the first tag gives the description.

| Annotation     | Argument                 | Description/References                                                                                                                                                  |
|----------------|--------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `@title`       | `Title`                  | [Info Object](https://spec.openapis.org/oas/v3.0.3#info-object)                                                                                                         |
| `@description` | `Description`            | [Info Object](https://spec.openapis.org/oas/v3.0.3#info-object)                                                                                                         |
| `@tos`         | `TOS link`               | [Info Object](https://spec.openapis.org/oas/v3.0.3#info-object)                                                                                                         |
| `@contact`     | `Name` \[`URL` `Email`\] | [Contact Object](https://spec.openapis.org/oas/v3.0.3#contact-object)                                                                                                   |
| `@license`     | `License` \[`URL`\]      | [License Object](https://spec.openapis.org/oas/v3.0.3#license-object)                                                                                                   |
| `@version`     | `Version`                | [Info Object](https://spec.openapis.org/oas/v3.0.3#info-object)                                                                                                         |
| `@tag`         | `Tag` `Description`      | Can be repeated to add multiple tags. Quote with ” or ’ to use non word character (like spaces) in `Tag`. [Tag Object](https://spec.openapis.org/oas/v3.0.3#tag-object) |
| `@noDoc`       | None                     | Don’t generate OpenAPI documentation from this block                                                                                                                    |

##### Annotations example

``` r
#* Sample Pet Store App
#*
#* This is a sample server for a pet store.
#*
#* @tos http://example.com/terms/
#* @contact API Support http://www.example.com/support support@example.com
#* @license Apache 2.0 https://www.apache.org/licenses/LICENSE-2.0.html
#* @version 1.0.1
#* @tag pet Pets operations
#* @tag toy Toys operations
#* @tag "toy space" Toys operations
"_API"
```

##### Equivalent programmatic usage

``` r
api() |>
  api_doc_add(
    openapi(
    info = openapi_info(
      title = "Sample Pet Store App",
      description = "This is a sample server for a pet store.",
      terms_of_service = "http://example.com/terms/",
      contact = openapi_contact(name = "API Support", url = "http://www.example.com/support", email = "support@example.com"),
      license = openapi_license(name = "Apache 2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html"),
      version = "1.0.1"
    ),
    tags = list(
      openapi_tag(name = "pet", description = "Pets operations"),
      openapi_tag(name = "toy", description = "Toys operations"),
      openapi_tag(name = "toy space", description = "Toys operations")
    )
  ))
```

## Handler annotations

Handler annotation describe all aspects of a request handler and always
proceeds a function which is considered the handler function. The
following tags can be used in a handler block. The first line, unless it
has a tag is considered the title of the handler and any proceeding
lines until the first tag is considered a long-form description.

### Endpoint

[TABLE]

#### More details on `Type`

Types are used to define API inputs and outputs. For path parameters
they can be given both in `@param` and inside the handler path. If they
are given both places they **must** be in agreement. For query and body
parameters they are given in their respective `@query` and `@body` tags.

Some types can have a nested structure which is also supported, but the
type spec can quickly become difficult to read the further you recurse
so use with care

| Type                                   | OpenAPI                                                                      |
|----------------------------------------|------------------------------------------------------------------------------|
| `boolean`                              | `boolean`                                                                    |
| `number`                               | `number`                                                                     |
| `integer`                              | `integer`                                                                    |
| `string`                               | `string`                                                                     |
| `date`                                 | `string` `format:date`                                                       |
| `date-time`                            | `string` `format:date-time`                                                  |
| `byte`                                 | `string` `format:byte`                                                       |
| `binary`                               | `string` `format:binary`                                                     |
| `enum`                                 | `string` `enum:|...|`                                                        |
| `pattern`                              | `string` `pattern:|...|`                                                     |
| `[Type]`                               | `array` `items:type:Type`                                                    |
| `{prop_name: Type, prop_name2: Type2}` | `object` `properties:prop_name:type:Type` `properties:prop_name2:type:Type2` |

Types can have a default value, which is given in parentheses after the
type specification, e.g. `integer(10)`. For objects and arrays you
should use JSON notation to describe the default value
(e.g. `[integer]([2, 6, 1])`).

For the `integer` and `number` types it is also possible to specify the
lower and/or upper bounds of the value. This is done by putting these
between `|` like so: `integer|3, 7|`. Omitting one of these will remove
that bound requirement (e.g. `integer|,7|` only requires the input to be
below or equal to 7). If combining this with a default value the range
comes first (`integer|3,7|(5)`).

The `enum` type is a factor type and you must provide the valid values
of the type in between `|`, separated by comma and an optional
whitespace. The `pattern` type is a string type that must match the
regular expression given between `|`.

The `date` and `date-time` types require the use of a specific format
for the date or time described in [RFC 3339, section
5.6](https://datatracker.ietf.org/doc/html/rfc3339#section-5.6). The
date must be given in full-date notation, e.g. *2017-07-21* while the
date-time must be given in the date-time notation,
e.g. *2017-07-21T17:32:28Z*

parameters can be specified as optional or required in their type
notation. Path parameters are always required so any setting is ignored
for this. A parameter can be marked as required by adding a `*` suffix
to the type description, e.g. `arg1:integer*` to indicate that `arg1` is
required and an integer. A parameter cannot both be required and have a
default value (for reasons of logic).

Apart from being used in the documentation of your API, the type
information you provide for parameters will also be used to cast the
incoming values to the correct type and add defaults if missing.
Further, missing required parameters will result in an error. The
response is not type checked so it is up to you to conform with the
specification you provide.

##### Annotations example

``` r
#* @get /query/parameters
#* @serializer text
#* @query name:string*
#* @query age:integer*
function(query) {
  sprintf("%s is %i years old", query$name, max(query$age))
}

#* @get /dyn/<name:string>/<age:integer>/route
#* @serializer text
#* @parser none
#* @response 200:string A sentence
function(name, age) {
  sprintf("%s is %i years old", name, age)
}

#* Upload a file and return the object as an rds
#*
#* @post /upload_file
#* @serializer rds
#* @parser multi
#* @body file:binary A file
#* @download
function(body) {
  body$file
}

#* @message
function(message, client_id, server) {
  if (is.character(message)) {
    server$log("message", paste0(client_id, " says ", message))
  }
  NULL
}
```

##### Equivalent programmatic usage

``` r
text_handler <- function(name, age) {
  sprintf("%s is %i years old", name, max(age))
}
qp_handler <- function(query) {
  text_handler(query$name, query$age)
}
file_handler <- function(body) {
  body$f
}
msg_handler <- function(message, client_id, server) {
  if (is.character(message)) {
    server$log("message", paste0(client_id, " says ", message))
  }
  NULL
}

api() |>
  api_get(
    path = "/query/parameters",
    handler = qp_handler,
    serializers = get_serializers("text"),
    parsers = get_parsers(),
    doc = openapi_operation(
      parameters = list(
        openapi_parameter(
          name = "name",
          location = "query",
          required = TRUE,
          schema = openapi_schema(character())
        ),
        openapi_parameter(
          name = "age",
          location = "query",
          required = TRUE,
          schema = openapi_schema(integer())
        )
      )
    )
  ) |>
  api_get(
    path = "/dyn/<name:string>/<age:integer>/route",
    handler = text_handler,
    serializers = get_serializers("text"),
    doc = openapi_operation(
      responses = list(
         "200" = openapi_response(
          description = "A sentence",
          content = openapi_content(
            "text/plain" = openapi_schema(character())
          )
        )
      )
    )
  ) |>
  api_post(
    path = "/upload_file",
    handler = file_handler,
    serializers = get_serializers("rds"),
    parsers = get_parsers("multi"),
    doc = openapi_operation(
      description = "Upload an rds file and return the object",
      request_body = openapi_request_body(
        content = openapi_content(
          "multipart/form-data" = openapi_schema(list(file = raw()))
        )
      )
    )
  ) |>
  api_message(msg_handler)
```

## Asset annotation

There are two ways to serve static content in plumber2 and they differ
in subtle ways. The `@assets` tag instruct plumber to create a regular
handler that matches the mount path (defaults to `/`) and will serve
files from `Path`. The `@statics` tag works the same, but rather than
create a handler it instructs httpuv (which is the low-level package
powering plumber2) to serve the files before the request even reaches
the R process. This makes it much faster but also limited in flexibility
since the request never reaches your code and you are unable to modify
it. In general, you should use `@statics` unless you need to provide any
additional handling of the request, such as authentication or logging.

| Annotation | Arguments               | Description/References                                                                               |
|------------|-------------------------|------------------------------------------------------------------------------------------------------|
| `@assets`  | `Path` \[`Mount path`\] | [Static files](https://plumber2.posit.co/articles/routing-and-input.html#static-file-handler)        |
| `@statics` | `Path` \[`Mount path`\] |                                                                                                      |
| `@except`  | `Path`                  | Can be used together with `@statics` to exclude subpaths of the `@statics` `Path` from being served. |

##### Annotations example

``` r
#* @assets ./assets/files
NULL

#* @assets ./assets/files /assets
NULL

#* @statics ./assets/static_files
#* @except /secret_files
NULL
```

##### Equivalent programmatic usage (note that argument order is reversed)

``` r
api() %>%
  api_assets("/", "./assets/files")

api() %>%
  api_assets("/assets", "./assets/files")

api() %>%
  api_statics("/", "./assets/static_files", except = "/secret_files")
```

## Auth guard annotation

| Annotation   | Arguments | Description/References                                                                                                                                                           |
|--------------|-----------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `@authGuard` | `name`    | Adds a new guard with the given name. The guard is given below the annotation as a [`fireproof::Guard`](https://fireproof.data-imaginist.com/reference/Guard.html) specification |

##### Annotations example

``` r
#* @authGuard auth1
fireproof::guard_key(
  key_name = "plumber2_key",
  validate = "MY_VERY_SECRET_SECRET"
)
```

##### Equivalent programmatic usage

``` r
api() |>
  api_datastore(storr::driver_environment()) |> 
  api_auth_guard(
    fireproof::guard_key(
      key_name = "plumber2_key",
      validate = "MY_VERY_SECRET_SECRET"
    ),
    "auth1"
  )
```

## Datastore annotation

| Annotation   | Arguments | Description/References                                                                                                         |
|--------------|-----------|--------------------------------------------------------------------------------------------------------------------------------|
| `@datastore` | `[name]`  | Adds a new datastore backend optionally setting a different store name. The driver for the backend is provided below the block |

##### Annotations example

``` r
#* @datastore persistent_data
storr::driver_environment()
```

##### Equivalent programmatic usage

``` r
api() |>
  api_datastore(
    driver = storr::driver_environment(),
    store_name = "persistent_data"
  )
```

## plumber2 annotation

| Annotation | Arguments | Description/References                                                                                                                                                                                                                                                                                                                                                              |
|------------|-----------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `@plumber` | None      | Modify plumber router from plumber file. If the function returns a plumber2 api object then this object will be used going forward, otherwise the return value is ignored. In most cases, anonymous functions are used following the `#* @plumber` annotation. However, named functions can also be used. When a named function is used, it must be referenced without parentheses. |

##### Annotations example

``` r
#* @plumber
function(api) {
  api %>%
    api_doc_setting("swagger")
}

# Named function
use_swagger <- function(api) {
  api %>%
    api_doc_setting("swagger")
}

#* @plumber
use_swagger
```

##### Equivalent programmatic usage

``` r
api() |>
  api_doc_setting("swagger")
```

## Forwards and redirects

plumber2 allows you to orchestrate requests that are ultimately handled
elsewhere. There are two approaches to this. Either return a redirect
response and let the client follow that to the new location, or handle
it directly by forwarding the request to another origin and passing
along the response to the client (this is called a reverse proxy). Both
of these are considered somewhat advanced use.

| Annotation  | Arguments                 | Description/References                                                                                                                                                                                                                                                                                                            |
|-------------|---------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `@redirect` | \[!\]`Method` `From` `To` | Will add a redirect response for the method given as `Method` from the path given in `From` to the path given in `To`. Path parameters are supported and will be matched between `Trom` and `To`. If the method is preceded by a `!` then the redirect is considered permanent (308) — otherwise it is considered temporary (307) |
| `@forward`  | `Path` `URL`              | Will forward requests from `Path` to `URL`, acting as a reverse proxy. The proxy will also forward any WebSocket messaging that is established with `Path`. All forwarding is performed asynchronously so the api will not block if the URL takes a long time to respond.                                                         |

##### Annotation example

``` r
#* @redirect !any old/<path> new/<path>
#* @redirect get main/<path> temp/main/<path>
NULL

#* @forward proxy/server http://127.0.0.1:56789
NULL
```

##### Equivalent programmatic usage

``` r
api() |>
  api_redirect("any", "old/<path>", "new/<path>", permanent = TRUE) |>
  api_redirect("get", "main/*", "temp/main/*") |>
  api_forward("proxy/server", "http://127.0.0.1:56789")
```

## Shiny

plumber2 can serve one or more shiny apps at specified paths. It works
by launching the shiny app defined below the block in a separate process
and then forward requests to the defined path to the process. The shiny
app is automatically launched and stopped along with the main plumber2
api.

| Annotation | Arguments | Description/References                                                                |
|------------|-----------|---------------------------------------------------------------------------------------|
| `@shiny`   | `Path`    | Launc the app defined below the annotation block and forward requests to `Path` to it |

##### Annotation example

``` r
#* @shiny app/
shinyAppDir("path/to/shiny/app")
```

##### Equivalent programmatic usage

``` r
api() |>
  api_shiny("app/", shinyAppDir("path/to/shiny/app"))
```

## Reports

plumber2 can automatically serve Rmarkdown and Quarto reports. The
report will be rendered upon request but cached for the future.
Different output formats can be selected through the `Content-Type`
request header as long as a matching format is specified in the reports
yaml header. Query parameters are automatically passed in as parameters
to the report to support parameterized reports natively. Reports can be
annotated just like standard request handlers to have OpenAPI
documentation generated for it. Rendering is performed asynchronously so
the api is not blocked by someone requesting a report that takes a while
to render

| Annotation | Arguments | Description/References                                                                                                                                |
|------------|-----------|-------------------------------------------------------------------------------------------------------------------------------------------------------|
| `@report`  | `Path`    | Serve a report given by the path specified below the annotation block from `Path`. The report location is relative to the path of the annotation file |

##### Annotation example

``` r
#* Access the quarterly report
#*
#* @query quarter:enum|spring, summer, autumn, winter| The quarter to generate the report for
#*
#* @report quarterly/
"my_amazing_report.qmd"
```

##### Equivalent programmatic usage

``` r
api() |>
  api_add_route(routr::report_route("quarterly/", "my_amazing_report.qmd"))
```

Note that the programmatic usage doesn’t add any documentation by itself
