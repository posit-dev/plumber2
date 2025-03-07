parse_global_api <- function(tags, values, env = caller_env()) {
  if (any(tags == "noDoc")) return(NULL)
  values <- set_names(values, tags)
  api <- list(
    info = compact(list(
      title = values$apiTitle,
      description = values$apiDescription,
      termsOfService = values$apiTOS,
      contact = eval(parse(text = values$apiContact %||% "NULL"), env),
      license = eval(parse(text = values$apiLicense %||% "NULL"), env),
      version = values$apiVersion
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

parse_block_api <- function(tags, values, parsers, serializers) {
  if (any(tags == "noDoc")) return(NULL)
  api <- list()
  summary <- if ("title" %in% tags) values[[which(tags == "title")]]
  description <- paste0(
    unlist(values[tags %in% c("description", "details")]),
    collapse = "\n\n"
  )
  if (description == "") description <- NULL
  params <- stringi::stri_split_fixed(
    unlist(values[tags == "param"]),
    " ",
    n = 2
  )
  params <- lapply(params, function(param) {
    arg_parsed <- stringi::stri_match_first_regex(
      param[1],
      "^(.+?)(:(.+?))?(\\*)?$"
    )[1, ]
    arg_name <- arg_parsed[2]
    arg_description <- param[2]
    arg_type <- arg_parsed[4]
    arg_is_required <- !is.na(arg_parsed[5])
    param <- list(
      `in` = "query", # Will get overwritten if present in path
      name = arg_name,
      description = if (!is.na(arg_description)) arg_description,
      required = arg_is_required,
      style = "form", # TODO: Should this be user definable
      schema = parse_openapi_type(arg_type)
    )
    compact(param)
  })
  body_params <- vapply(
    params,
    function(param)
      !is.null(param$schema) &&
        (param$schema$type == "object" ||
          isTRUE(param$schema$format == "binary")),
    logical(1)
  )
  names(params) <- vapply(params, `[[`, character(1), "name")
  params_body <- params[body_params]
  params <- params[!body_params]

  if (length(params_body) == 1) {
    request_body <- compact(list(
      description = params_body[[1]]$description,
      required = params_body[[1]]$required,
      content = rep_named(parsers, list(list(schema = params_body[[1]]$schema)))
    ))
  } else if (length(params_body) > 1) {
    if (
      !all(
        parsers %in%
          c("application/x-www-form-urlencoded", "multipart/form-data")
      )
    ) {
      cli::cli_abort(
        "Multiple {.field @params} tags for request body, but parser is not {.val application/x-www-form-urlencoded} or {.val multipart/form-data}"
      )
    }
    schema <- list(
      type = "object",
      properties = lapply(params_body, `[[`, "schema")
    )
    request_body <- list(
      content = rep_named(parsers, list(schema))
    )
  } else {
    request_body <- NULL
  }

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

  tag <- unlist(values[tags == "tag"])

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
    args <- stringi::stri_match_all_regex(path, "<(.+?)>")[[1]][, 2]
    if (isTRUE(is.na(args))) {
      args <- matrix(character())
    } else {
      args <- stringi::stri_match_first_regex(args, "^(.+?)(:(.+?))?(\\*)?$")
    }
    clean_path <- stringi::stri_replace_all_regex(
      path,
      "<(.+?)(:.+?)?>",
      "{$1}"
    )
    endpoint <- list()
    for (method in methods[[path]]) {
      local_params <- params
      for (i in seq_len(nrow(args))) {
        param <- list(
          `in` = "path",
          name = args[i, 2],
          operationId = paste0(clean_path, "-", method),
          required = TRUE,
          style = "simple", # TODO: Should this be user definable
          schema = parse_openapi_type(args[i, 4])
        )
        local_params[[args[i, 2]]] <- utils::modifyList(
          local_params[[args[i, 2]]] %||% list(),
          compact(param)
        )
      }
      endpoint[[method]] <- compact(list(
        summary = summary,
        description = description,
        tags = tag,
        parameters = unname(local_params),
        responses = responses
      ))
      if (!is.null(request_body) && method %in% c("put", "post", "patch")) {
        endpoint[[method]]$requestBody <- request_body
      }
    }
    api[[clean_path]] <- endpoint
  }
  list(paths = api)
}

parse_openapi_type <- function(string) {
  check_string(string, allow_na = TRUE, allow_null = TRUE)
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

    list(
      type = "object",
      properties = set_names(
        lapply(content_split[, 3], parse_openapi_type),
        content_split[, 2] %|% content
      )
    )
  } else if (grepl("^\\[", string)) {
    list(
      type = "array",
      items = parse_openapi_type(gsub("^\\[|\\]$", "", string))
    )
  } else {
    if (string %in% c("date", "date-time", "byte", "binary")) {
      list(
        type = "string",
        format = string
      )
    } else {
      list(
        type = string
      )
    }
  }
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
