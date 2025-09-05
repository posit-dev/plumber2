tag_extensions <- new.env(parent = emptyenv())

tag_extensions$tag <- list()

#' Add a tag extension to plumber2
#'
#' Package authors can extend plumber2 with their own functionalities. If they
#' wish to add a new tag to be used when writing annotated plumber2 routes they
#' can use this function. If so, it should be called when the package is loaded.
#'
#' The `handler` argument must be a function with the arguments `block`, `call`,
#' `tags`, `values`, and `env`. `block` is a list with the currently parsed
#' information from the block. You can add or modify the values within to suit
#' your need as well as subclass it. You should not remove any values as others
#' might need them. `call` is the parsed value of whatever expression was
#' beneath the plumber2 block. `tags` is a character vector of all the tags in
#' the block, and `values` is a list of all the values associated with the tags
#' (that is, whatever comes after the tag in the block). The values are
#' unparsed. You should assume that all tags not relevant for your extension has
#' already been handled and incorporated into `block`. The function must return
#' a modified version of `block`. If you add a subclass to `block` you should
#' make sure that a method for [apply_plumber2_block()] for the subclass exists.
#'
#' @param tag The name of the tag
#' @param handler A handler function for the tag. See *Details*
#'
#' @return This function is called for its side effects
#'
#' @export
#'
#' @seealso [apply_plumber2_block()]
#'
#' @examplesIf FALSE
#' # Add a tag that says hello when used
#' add_plumber2_tag("hello", function(block, call, tags, values, env) {
#'   message("Hello")
#'   class(block) <- c("hello_block", class(block))
#'   block
#' })
#'
#'
add_plumber2_tag <- function(tag, handler) {
  check_string(tag)
  check_function(handler)
  if (
    !identical(
      c("block", "call", "tags", "values", "env"),
      fn_fmls_names(handler)
    )
  ) {
    cli::cli_abort(
      "{.arg handler} must be a function with the following arguments: {.and {.arg {c('block', 'call', 'tags', 'values', 'env')}}}"
    )
  }

  tag_extensions$tag[[tag]] <- c(tag_extensions$tag[[tag]], list(handler))
  registerS3method(
    genname = "roxy_tag_parse",
    class = paste0("roxy_tag_", tag),
    method = function(x) x,
    envir = asNamespace("plumber2")
  )
}

is_extension_tag <- function(tag) {
  tag %in% names(tag_extensions$tag)
}

parse_extension <- function(tag, block, call, tags, values, env) {
  handlers <- tag_extensions$tag[[tag]]
  for (handler in handlers) {
    old_class <- class(block)
    old_names <- names(block)
    block <- handler(
      block = block,
      call = call,
      tags = tags,
      values = values,
      env = env
    )
    if (
      any(inherits(block, old_class, TRUE) == 0) ||
        any(!old_names %in% names(block))
    ) {
      cli::cli_abort(
        "Parsing extension tag {.val {tag}} failed. {.arg handler} must only add to or modify the values of {.arg block} and subclass it"
      )
    }
  }
  block
}

on_load({
  add_plumber2_tag("cors", function(block, call, tags, values, env) {
    class(block) <- c("plumber2_cors_block", class(block))
    block$cors <- trimws(strsplit(values[[which(tags == "cors")[1]]], ",")[[1]])
    if (block$cors == "") block$cors <- "*"
    block
  })
  add_plumber2_tag("rip", function(block, call, tags, values, env) {
    class(block) <- c("plumber2_rip_block", class(block))
    block$rip <- trimws(values[[which(tags == "rip")[1]]])
    if (block$rip == "") block$rip <- "same-site"
    block
  })
})
