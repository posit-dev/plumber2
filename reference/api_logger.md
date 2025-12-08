# Set logging function and access log format for the API

plumber2 has a build-in logging facility that takes care of logging any
conditions that are caught, as well as access logs. Further it is
possible to log custom messages using the
[`log()`](https://rdrr.io/r/base/Log.html) method on the api object.
However, the actual logging is handled by a customizable function that
can be set. You can read more about the logging infrastructure in the
[fiery
documentation](https://fiery.data-imaginist.com/reference/loggers.html).
plumber2 reexports the loggers provided by fiery so they are immediately
available to the user.

## Usage

``` r
api_logger(api, logger = NULL, access_log_format = NULL)

logger_null()

logger_console(format = "{time} - {event}: {message}")

logger_file(file, format = "{time} - {event}: {message}")

logger_logger(default_level = "INFO")

logger_otel(format = "{message}")

logger_switch(..., default = logger_null())

common_log_format

combined_log_format
```

## Arguments

- api:

  A plumber2 api object to set the logger on

- logger:

  A logger function. If `NULL` then the current logger is kept

- access_log_format:

  A glue string giving the format for the access logs. plumber2 (through
  fiery) provides the predefined `common_log_format` and
  `combined_log_format`, but you can easily create your own. See
  [fiery::loggers](https://fiery.data-imaginist.com/reference/loggers.html)
  for which variables the glue string has access to.

- format:

  A [glue](https://glue.tidyverse.org/reference/glue.html) string
  specifying the format of the log entry

- file:

  A file or connection to write to

- default_level:

  The log level to use for events that are not `request`, `websocket`,
  `message`, `warning`, or `error`

- ...:

  A named list of loggers to use for different events. The same
  semantics as [switch](https://rdrr.io/r/base/switch.html) is used so
  it is possible to let events *fall through* e.g.
  `logger_switch(error =, warning = logger_file('errors.log'))`.

- default:

  A catch-all logger for use with events not defined in `...`

## Using annotation

Logger setup doesn't have a dedicated annotation tag, but you can set it
up in a `@plumber` block

    #* @plumber
    function(api) {
      api |>
        api_logger(logger = logger_null())
    }

## Examples

``` r
# Use a different access log format
api() |>
  api_logger(access_log_format = combined_log_format)
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running

# Turn off logging
api() |>
  api_logger(logger_null())
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running

```
