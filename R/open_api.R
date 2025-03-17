parse_global_api <- function(tags, values, env = caller_env()) {
  if (any(tags == "noDoc")) return(NULL)
  values <- set_names(values, tags)
  api <- list(
    info = compact(list(
      title = trimws(values$apiTitle),
      description = trimws(values$apiDescription),
      termsOfService = trimws(values$apiTOS),
      contact = eval(parse(text = values$apiContact %||% "NULL"), env),
      license = eval(parse(text = values$apiLicense %||% "NULL"), env),
      version = trimws(values$apiVersion)
    )),
    tag = unname(lapply(values[tags == "apiTag"], function(tag) {
      tag <- stringi::stri_match_all_regex(tag, "^((\".+?\")|(\\S+))(.*)")[[1]]
      tag <- c(name = gsub('^"|"$', "", tag[2]), description = trimws(tag[5]))
      if (is.na(tag[2])) {
        tag[1]
      } else {
        tag
      }
    }))
  )
  compact(api)
}

parse_path <- function(path) {
  args <- stringi::stri_match_all_regex(path, "<(.+?)>")[[1]][, 2]
  if (isTRUE(is.na(args))) {
    args <- matrix(character())
  } else {
    args <- stringi::stri_match_first_regex(
      args,
      "^(.+?)(:(.+?))?(\\((.*?)\\))?(\\*)?$"
    )
  }
  params <- list()
  for (i in seq_len(nrow(args))) {
    params[[args[i, 2]]] <- list(
      `in` = "path",
      name = args[i, 2],
      required = TRUE,
      style = "simple", # TODO: Should this be user definable
      schema = parse_openapi_type(args[i, 4], NA)
    )
  }
  list(
    path = as_openapi_path(path),
    params = params
  )
}

combine_parameters <- function(path, doc, from_block = TRUE) {
  common_names <- intersect(names(path), names(doc))
  for (param in common_names) {
    if (
      length(path[[param]]$schema) != 0 &&
        length(doc[[param]]$schema) != 0 &&
        !identical(path[[param]]$schema, doc[[param]]$schema)
    ) {
      cli::cli_abort(
        "The type information for {.arg {param}} provided in the path doens't match the information provided in {if (from_block) '@param' else '`doc`'}"
      )
    }
  }
  utils::modifyList(doc, path)
}

parse_responses <- function(tags, values, serializers) {
  responses <- unlist(values[tags == "response"])
  responses <- stringi::stri_split_fixed(responses, " ", n = 2)
  response_codes <- vapply(responses, `[[`, character(1), 1)
  responses <- set_names(
    lapply(responses, function(response) {
      compact(list(
        description = if (length(response) == 2) response[2]
      ))
    }),
    response_codes
  )
  responses <- utils::modifyList(default_responses, responses)
  response_format <- which(tags == "responseFormat")
  if (length(response_format) != 0) {
    if (length(response_format) > 1) {
      cli::cli_warn(
        "Multiple {.field @responseFormat} tags. Only using the first"
      )
    }
    schema <- parse_openapi_type(values[[response_format[1]]])
  } else {
    schema <- list()
  }
  responses[["200"]]$content <- rep_named(
    serializers,
    list(list(schema = schema))
  )
}

parse_block_api <- function(tags, values, parsers, serializers) {
  if (any(tags == "noDoc")) return(NULL)
  api <- list()
  summary <- if ("title" %in% tags) values[[which(tags == "title")]]
  description <- paste0(
    unlist(values[tags %in% c("description", "details")]),
    collapse = "\n\n"
  )
  if (description == "") description <- NULL
  path_params <- parse_params(tags, values, "path")
  query_params <- parse_params(tags, values, "query")
  body_params <- parse_params(tags, values, "body")

  request_body <- parse_body_params(body_params, parsers)

  responses <- parse_responses(tags, values, serializers)

  tag <- unlist(values[tags == "tag"])

  endpoint_template <- compact(list(
    summary = summary,
    description = description,
    tags = tag,
    responses = responses
  ))

  methods <- which(
    tags %in%
      c(
        "get",
        "head",
        "post",
        "put",
        "delete",
        "connect",
        "options",
        "trace",
        "patch"
      )
  )
  paths <- trimws(unlist(values[methods]))
  methods <- split(tags[methods], paths)

  for (path in names(methods)) {
    path_info <- parse_path(path)
    local_params <- combine_parameters(path_info$params, path_params)
    local_params <- unname(c(local_params, query_params))
    endpoint <- list()
    for (method in methods[[path]]) {
      operation_id <- paste0(path_info$path, "-", method)
      endpoint[[method]] <- endpoint_template
      endpoint[[method]]$parameters <- lapply(local_params, function(par) {
        par$operationId <- operation_id
        par
      })
      if (!is.null(request_body) && method %in% c("put", "post", "patch")) {
        endpoint[[method]]$requestBody <- request_body
      }
    }
    api[[path_info$path]] <- endpoint
  }
  list(paths = api)
}

parse_openapi_type <- function(string, default = NA) {
  check_string(string, allow_na = TRUE, allow_null = TRUE)
  check_string(default, allow_na = TRUE)
  if (is.na(string) || is.null(string)) return(list())
  string <- trimws(string)
  if (grepl("^\\{", string)) {
    content <- gsub("^\\{|\\}$", "", string)
    last <- 0
    seps <- stringi::stri_locate_all_fixed(content, ",")[[1]][, 1]
    curly_open <- stringi::stri_locate_all_fixed(content, "{")[[1]][, 1]
    curly_close <- stringi::stri_locate_all_fixed(content, "}")[[1]][, 1]
    brack_open <- stringi::stri_locate_all_fixed(content, "[")[[1]][, 1]
    brack_close <- stringi::stri_locate_all_fixed(content, "]")[[1]][, 1]
    if (
      length(curly_open) != length(curly_close) ||
        length(brack_open) != length(brack_close)
    ) {
      cli::cli_abort(
        "Syntax error in {.val {content}}. Opening and closing brackets doesn't match"
      )
    }
    for (i in seq_along(seps)) {
      if (
        isFALSE(
          sum(curly_open > last & curly_open < seps[i]) ==
            sum(curly_close > last & curly_close < seps[i])
        ) ||
          isFALSE(
            sum(brack_open > last & brack_open < seps[i]) ==
              sum(brack_close > last & brack_close < seps[i])
          )
      ) {
        seps[i] <- NA
        next
      }
      last <- seps[i]
    }
    seps <- seps[!is.na(seps)]
    from <- c(0, seps + 1)
    to <- c(seps - 1, stringi::stri_length(content))
    content <- trimws(stringi::stri_sub_all(content, from, to)[[1]])
    content_split <- stringi::stri_match_first_regex(content, "^(.+?):(.+?)$")

    type <- list(
      type = "object",
      properties = set_names(
        lapply(content_split[, 3], parse_openapi_type),
        content_split[, 2] %|% content
      )
    )
  } else if (grepl("^\\[", string)) {
    type <- list(
      type = "array",
      items = parse_openapi_type(gsub("^\\[|\\]$", "", string))
    )
  } else {
    if (string %in% c("date", "date-time", "byte", "binary")) {
      type <- list(
        type = "string",
        format = string
      )
    } else {
      type <- list(
        type = string
      )
    }
  }
  if (!is.na(default)) {
    type$default <- default
  }
  type
}

default_responses <- list(
  "200" = list(
    description = "OK"
  ),
  "400" = list(
    description = "The provided request body could not be parsed"
  ),
  "406" = list(
    description = "The requested content type for the response cannot be provided"
  ),
  "415" = list(
    description = "The provided request body is in an unsupported format"
  ),
  "5XX" = list(
    description = "Internal server error"
  )
)

parse_params <- function(tags, values, type = "path") {
  tag_name <- switch(type, path = "param", type)
  params <- stringi::stri_split_fixed(
    unlist(values[tags == tag_name]),
    " ",
    n = 2
  )
  params <- lapply(params, function(param) {
    arg_parsed <- stringi::stri_match_first_regex(
      param[1],
      "^(.+?)(:(.+?))?(\\((.*?)\\))?(\\*)?$"
    )[1, ]
    arg_name <- arg_parsed[2]
    arg_description <- param[2]
    arg_type <- arg_parsed[4]
    arg_default <- arg_parsed[6]
    arg_required <- type == "path" || !is.na(arg_parsed[7])
    list(
      `in` = type, # Will get overwritten if present in path
      name = arg_name,
      description = if (!is.na(arg_description)) arg_description else "",
      required = arg_required,
      style = "form", # TODO: Should this be user definable
      schema = parse_openapi_type(
        arg_type,
        if (arg_required) NA else arg_default
      )
    )
  })
  set_names(params, vapply(params, `[[`, character(1), "name"))
}

parse_body_params <- function(params, parsers) {
  if (length(params) == 1) {
    request_body <- compact(list(
      description = params[[1]]$description,
      required = params[[1]]$required,
      content = rep_named(parsers, list(list(schema = params[[1]]$schema)))
    ))
  } else if (length(params) > 1) {
    schema <- list(
      type = "object",
      properties = lapply(params, `[[`, "schema"),
      required = names(params)[vapply(
        params,
        `[[`,
        logical(1),
        "required"
      )]
    )
    body_description <- vapply(params, `[[`, character(1), "description")
    body_description <- paste0(
      "*",
      names(params)[body_description != ""],
      "*: ",
      body_description[body_description != ""],
      collapse = "; "
    )
    parsers <- sub("multipart/*", "multipart/form-data", parsers, fixed = TRUE)
    request_body <- list(
      description = body_description,
      content = rep_named(parsers, list(list(schema = schema)))
    )
  } else {
    request_body <- NULL
  }
  request_body
}
