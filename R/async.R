registry$async <- list()

#' Register an async evaluater
#'
#' plumber supports async request handling in two ways. Either manual by
#' returning a promise from the handler, or automatic through the `@async` tag /
#' `async` argument in [the handler functions][api_request_handlers]. The
#' default evaluater is controlled by the `plumber2.async` option or the
#' `PLUMBER2_ASYNC` environment variable.
#'
#' @param name The name of the evaluater
#' @param fun A function that, upon calling it returns an evaluater taking an
#' `expr` and `envir` argument. See the [async evaluater][async_evaluaters]
#' functions for examples
#' @param dependency Package dependencies for the evaluater.
#'
#' @export
#'
register_async <- function(name, fun, dependency = NULL) {
  check_string(name)
  check_character(dependency, allow_null = TRUE)
  check_function(fun)
  registry$async[[name]] <- list(
    fun = fun,
    dependency = dependency
  )
}
#' @rdname register_async
#' @export
show_registered_async <- function() {
  res <- data.frame(
    name = names(registry$async),
    dependency = I(lapply(registry$async, `[[`, "dependency"))
  )
  attr(res, "row.names") <- .set_row_names(nrow(res))
  res
}
#' @rdname register_async
#' @param ... Arguments passed on to the async function creator
#' @export
get_async <- function(name = NULL, ...) {
  if (is.null(name)) {
    return(NULL)
  }
  if (is_string(name)) {
    async <- registry$async[[name]]
    if (is.null(async)) {
      cli::cli_abort("No async evaluater registered as {.val {name}}")
    }
    if (length(async$dependency) > 0) {
      check_installed(async$dependency)
    }
    fun <- async$fun(...)
    if (!is_function(fun) || !all(c("expr", "envir") %in% fn_fmls_names(fun))) {
      cli::cli_abort(
        "The async evaluater must be a function with the arguments {.arg expr} and {.arg envir}"
      )
    }
  } else if (is_function(name)) {
    fun <- name
  } else {
    cli::cli_abort("{.arg name} must be a string or a function")
  }

  fun
}

#' Async evaluaters provided by plumber
#'
#' These functions support async request handling. You can register your own as
#' well using [register_async()].
#'
#' # Provided evaluaters
#' * `future_async()` uses [promises::future_promise()]. It is registered as
#'   `"future"`. Be aware that for this evaluater to execute asynchronously you
#'   need to set a different planner than the default. See [future::plan()].
#' * `mirai_async()` uses [mirai::mirai()]. It is registered as
#'   `"mirai"`. Be aware that for this evaluater to be performant you should
#'   start up multiple persistent background processes. See [mirai::daemons()].
#'
#' @param ... Further argument passed on to the internal async function.
#' See Details for information on which function handles the formatting
#' internally in each async evaluater
#'
#' @return A function taking `expr` and `envir`. The former is the expression to
#' evaluate and the latter is an environment with additional variables that
#' should be made available during evaluation
#'
#' @rdname async_evaluaters
#' @name async_evaluaters
#'
NULL

#' @rdname async_evaluaters
#' @export
future_async <- function(substitute = FALSE, ...) {
  function(expr, envir) {
    if (substitute) {
      expr <- substitute(expr)
    }
    promises::future_promise(
      expr = expr,
      envir = envir,
      substitute = FALSE,
      ...
    )
  }
}
#' @rdname async_evaluaters
#' @export
mirai_async <- function(...) {
  function(expr, envir) {
    mirai::mirai(.expr = expr, envir, ...)
  }
}

on_load({
  register_async("future", future_async, "future")
  register_async("mirai", mirai_async, "mirai")
})
