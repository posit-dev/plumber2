# Create a \_server.yml file to describe your API

While you can manually create a plumber2 API by calling
[`api()`](https://plumber2.posit.co/reference/api.md), you will often
need to deploy the api somewhere else. To facilitate this you can create
a `_server.yml` that encapsulates all of your settings and plumber
files. If you call [`api()`](https://plumber2.posit.co/reference/api.md)
with a path to such a file the API will be constructed according to its
content.

## Usage

``` r
create_server_yml(..., path = ".", constructor = NULL, freeze_opt = TRUE)
```

## Arguments

- ...:

  path to files and/or directories that contain annotated plumber files
  to be used by your API

- path:

  The folder to place the generated `_server.yml` file in

- constructor:

  The path to a file that creates a plumber2 API object. Can be omitted
  in which case an API object will be created for you

- freeze_opt:

  Logical specifying whether any options you currently have locally
  (either as environment variables or R options) should be written to
  the `_server.yml` file. Shared secret will never be written to the
  file and you must find a different way to move that to your deployment
  server.

## Examples

``` r
if (FALSE) { # file.exists("path/to/a/plumber/file.R")
create_server_yml(
  "path/to/a/plumber/file.R"
)
}
```
