#' Set logging function and access log format for the API
#'
#' Plumber has a build-in logging facility that takes care of logging any
#' conditions that are caught, as well as access logs. Further it is possible to
#' log custom messages using the `log()` method on the api object. However, the
#' actual logging is handled by a customizable function that can be set. You can
#' read more about the logging infrastructure in the
#' [fiery documentation][fiery::loggers]. plumber2 reexports the loggers
#' provided by fiery so they are immediately available to the user.
#'
#' @param api A plumber2 api object to set the logger on
#' @param logger A logger function. If `NULL` then the current logger is kept
#' @param access_log_format A glue string giving the format for the access logs.
#' plumber2 (through fiery) provides the predefined `common_log_format` and
#' `combined_log_format`, but you can easily create your own. See
#' [fiery::loggers] for which variables the glue string has access to.
#' @inheritParams fiery::loggers
#'
#' @export
#'
api_logger <- function(api, logger = NULL, access_log_format = NULL) {
  if (!is.null(logger)) {
    check_function(logger)
    api$set_logger(logger)
  }
  if (!is.null(access_log_format)) {
    check_string(access_log_format)
    api$access_log_format <- access_log_format
  }
  api
}

#' @rdname api_logger
#' @export
#' @importFrom fiery logger_null
fiery::logger_null
#' @rdname api_logger
#' @export
#' @importFrom fiery logger_console
fiery::logger_console
#' @rdname api_logger
#' @export
#' @importFrom fiery logger_file
fiery::logger_file
#' @rdname api_logger
#' @export
#' @importFrom fiery logger_logger
fiery::logger_logger
#' @rdname api_logger
#' @export
#' @importFrom fiery logger_switch
fiery::logger_switch
#' @rdname api_logger
#' @export
#' @importFrom fiery common_log_format
#' @format NULL
fiery::common_log_format
#' @rdname api_logger
#' @export
#' @importFrom fiery combined_log_format
#' @format NULL
fiery::combined_log_format
