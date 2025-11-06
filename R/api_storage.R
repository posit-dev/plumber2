#' Turn on session cookie data storage for your API
#'
#' If you need to keep data between requests, but don't want to store it
#' server-side (see [api_datastore()]) you can instead pass it back and forth as
#' an encrypted session cookie. This function sets it up on your api and after
#' it's use you can now access and set session data in the request and response
#' `$session` field. Be aware that session data is send back and forth with all
#' requests and should thus be kept minimal to avoid congestion on your server.
#'
#' # Using annotation
#' Session cookie setup doesn't have a dedicated annotation tag, but you can set
#' it up in a `@plumber` block
#'
#' ```
#' #* @plumber
#' function(api) {
#'   api |>
#'     api_session_cookie(keyring::key_get("my_secret_plumber_key"))
#' }
#' ```
#'
#' @param api A plumber2 api object to add the session cookie setup to
#' @param key A 32-bit secret key as a hex encoded string or a raw vector to
#' use for encrypting the session cookie. A valid key can be generated using
#' [reqres::random_key()]. NEVER STORE THE KEY IN PLAIN TEXT. Optimally use the
#' keyring package to store it
#' @inheritParams reqres::session_cookie
#'
#' @return These functions return the `api` object allowing for easy chaining
#' with the pipe
#'
#' @export
#'
#' @examples
#' key <- reqres::random_key()
#'
#' api() |>
#'   api_session_cookie(key, secure = TRUE) |>
#'   api_get("/", function(request) {
#'     if (isTRUE(request$session$foo)) {
#'       msg <- "You've been here before"
#'     } else {
#'       msg <- "You must be new here"
#'       request$session$foo <- TRUE
#'     }
#'     list(
#'       msg = msg
#'     )
#'   })
#'
api_session_cookie <- function(
  api,
  key,
  name = "reqres",
  expires = NULL,
  max_age = NULL,
  path = NULL,
  secure = NULL,
  same_site = NULL
) {
  api$key <- key
  api$session_cookie_settings <- reqres::session_cookie(
    name = name,
    expires = expires,
    max_age = max_age,
    path = path,
    secure = secure,
    same_site = same_site
  )
  api
}

#' Persistent server-side data storage
#'
#' While using a [session cookie][api_session_cookie] is a convenient solution
#' to persistent data storage between requests it has the downside of requiring
#' the data to be passed back and forth between server and client at every
#' exchange. This makes it impractical for all but the smallest snippets of
#' data. An alternative strategy is to use server-side storage which this
#' function facilitates. It uses the firesale plugin under the hood to provide
#' a list-like interface to a storr-backed key-value store. storr in turn
#' provides interfaces to a range of backends such as redis, LMDB, and databases
#' supported by DBI. Further it provides simpler (but setup-free) solutions such
#' as using an environment (obviously less persistent) or a folder of rds files.
#'
#' Once you turn the datastore on with this function your request handlers will
#' gain access to a new argument (defaults to `datastore` but this can be
#' changed with the `store_name` argument). The `datastore` argument will
#' contain a list holding two elements: `global` and `session` which in turn
#' will be list-like interfaces to the underlying key-value store. The `global`
#' element access a store shared by all sessions whereas the `session` element
#' is scoped to the current session. Depending on the value of `max_age` the
#' session specific data is purged once a certain amount of time has passed
#' since the last request from that session.
#'
#' # Using annotation
#' You can define a datastore backend using the `@datastore` tag and provide the
#' driver specification below the block
#'
#' ```
#' #* @datastore
#' storr::driver_dbi(...)
#' ```
#'
#' @param api A plumber2 api object to add the datastore setup to
#' @param driver A storr compatible driver that defines the backend of the
#' datastore
#' @param store_name The argument name under which the datastore will be
#' available to the request handlers
#' @param gc_interval The interval between running garbage collection on the
#' backend
#' @param max_age The time since last request to pass before a session store is
#' cleared
#'
#' @return These functions return the `api` object allowing for easy chaining
#' with the pipe
#'
#' @export
#'
#' @examples
#' api() |>
#'   api_datastore(storr::driver_environment()) |>
#'   api_get("hello", function(datastore) {
#'     if (length(datastore$session) == 0) {
#'       datastore$global$count <- (datastore$global$count %||% 0) + 1
#'       datastore$session$not_first_visit <- TRUE
#'       paste0("Welcome. You are visitor #", datastore$global$count)
#'     } else {
#'       "Welcome back"
#'     }
#'   })
#'
api_datastore <- function(
  api,
  driver,
  store_name = "datastore",
  gc_interval = 3600,
  max_age = gc_interval
) {
  if ("firesale" %in% api$plugins) {
    cli::cli_warn("A datastore has already been added")
  } else {
    fs <- firesale::FireSale$new(
      driver,
      arg_name = store_name,
      gc_interval = gc_interval,
      max_age = max_age
    )
    api$attach(fs)
  }
  api
}

#' @export
#' @importFrom reqres random_key
reqres::random_key
