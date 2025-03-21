#' Add a handler to an event
#'
#' During the life cycle of a plumber API various events will be fired, either
#' automatically or manually. See the [article on events in fiery](https://fiery.data-imaginist.com/articles/events.html)
#' for a full overview. `api_on()` allows you to add handlers that are called
#' when specific events fire. `api_off()` can be used to remove the handler if
#' necessary
#'
#' @param api A plumber2 api object to launch or stop
#' @param event A string naming the event to listen for
#' @param handler A function to call when `event` fires
#' @param id A string uniquely identifying the handler. If `NULL` a random id
#' will be generated making it impossible to remove the handler again
#'
#' @return These functions return the `api` object allowing for easy chaining
#' with the pipe
#'
#' @export
#'
api_on <- function(api, event, handler, id = NULL) {
  api$on(event, handler, id = id)
  api
}
#' @rdname api_on
#' @export
api_off <- function(api, id) {
  api$off(id)
  api
}
