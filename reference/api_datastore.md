# Persistent server-side data storage

While using a [session
cookie](https://plumber2.posit.co/reference/api_session_cookie.md) is a
convenient solution to persistent data storage between requests it has
the downside of requiring the data to be passed back and forth between
server and client at every exchange. This makes it impractical for all
but the smallest snippets of data. An alternative strategy is to use
server-side storage which this function facilitates. It uses the
firesale plugin under the hood to provide a list-like interface to a
storr-backed key-value store. storr in turn provides interfaces to a
range of backends such as redis, LMDB, and databases supported by DBI.
Further it provides simpler (but setup-free) solutions such as using an
environment (obviously less persistent) or a folder of rds files.

## Usage

``` r
api_datastore(
  api,
  driver,
  store_name = "datastore",
  gc_interval = 3600,
  max_age = gc_interval
)
```

## Arguments

- api:

  A plumber2 api object to add the datastore setup to

- driver:

  A storr compatible driver that defines the backend of the datastore

- store_name:

  The argument name under which the datastore will be available to the
  request handlers

- gc_interval:

  The interval between running garbage collection on the backend

- max_age:

  The time since last request to pass before a session store is cleared

## Value

These functions return the `api` object allowing for easy chaining with
the pipe

## Details

Once you turn the datastore on with this function your request handlers
will gain access to a new argument (defaults to `datastore` but this can
be changed with the `store_name` argument). The `datastore` argument
will contain a list holding two elements: `global` and `session` which
in turn will be list-like interfaces to the underlying key-value store.
The `global` element access a store shared by all sessions whereas the
`session` element is scoped to the current session. Depending on the
value of `max_age` the session specific data is purged once a certain
amount of time has passed since the last request from that session.

## Using annotation

You can define a datastore backend using the `@datastore` tag and
provide the driver specification below the block

    #* @datastore
    storr::driver_dbi(...)

## Examples

``` r
api() |>
  api_datastore(storr::driver_environment()) |>
  api_get("hello", function(datastore) {
    if (length(datastore$session) == 0) {
      datastore$global$count <- (datastore$global$count %||% 0) + 1
      datastore$session$not_first_visit <- TRUE
      paste0("Welcome. You are visitor #", datastore$global$count)
    } else {
      "Welcome back"
    }
  })
#> Creating default route in request router
#> ── A plumber server ────────────────────────────────────────────────────────────
#> Serving on http://127.0.0.1:8080
#> Currently not running
```
