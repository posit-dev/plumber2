create_message_handler <- function(handler, async = NULL) {
  # Input checks
  check_function(handler)
  ## Add ... to formals so we never error on unknown arguments
  if (!"..." %in% fn_fmls_names(handler)) {
    fn_fmls(handler) <- c(fn_fmls(handler), "..." = missing_arg())
  }
  check_function(async, allow_null = TRUE)

  if (is.null(async)) {
    function(server, id, binary, message, request, ...) {
      # Call the handler
      response <- handler(
        message = message,
        server = server,
        client_id = id,
        request = request
      )
      # If a valid response is returned send it back
      if (is.raw(response) || is_string(response)) {
        server$send(response, id)
      }
      if (promises::is.promising(response)) {
        # If a promise is returned then wait for it before potentially sending
        # back message
        promises::then(response, function(response) {
          if (is.raw(response) || is_string(response)) {
            server$send(response, id)
          }
        })
      }
    }
  } else {
    envir <- list2env(list(
      handler = handler
    ))
    if (any(c("request", "server") %in% fn_fmls_names(handler))) {
      cli::cli_abort(c(
        "async handlers cannot access {.arg request} and {.arg server}",
        i = "remove these arguments from the handler definition"
      ))
    }
    function(server, id, binary, message, request, ...) {
      envir$message <- message
      envir$id <- id
      response <- async(async_message_call, envir = envir)
      promises::then(response, function(response) {
        if (is.raw(response) || is_string(response)) {
          server$send(response, id)
        }
      })
    }
  }
}

async_message_call <- quote({
  handler(
    message = message,
    client_id = id
  )
})
