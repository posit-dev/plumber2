parse_global_api <- function(tags, values, env = caller_env()) {
  if (any(tags == "noDoc")) return(NULL)
  values <- set_names(values, tags)
  api <- list(
    info = compact(list(
      title = trimws(values$apiTitle %||% values$title),
      description = trimws(
        values$apiDescription %||%
          paste0(values$description, "\n\n", values$details)
      ),
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
  args <- stringi::stri_match_all_regex(
    path,
    "<(.+?)>",
    omit_no_match = TRUE
  )[[1]][, 2]
  params <- list()
  for (arg in args) {
    arg <- split_param_spec(arg)
    params[[arg$name]] <- list(
      `in` = "path",
      name = arg$name,
      required = TRUE,
      style = "simple", # TODO: Should this be user definable
      schema = parse_openapi_type(arg$type, NULL, min = arg$min, max = arg$max)
    )
  }
  list(
    path = as_openapi_path(path),
    params = params
  )
}

combine_parameters <- function(path, doc, from_block = TRUE) {
  path <- path %||% list()
  doc <- doc %||% list()
  names(path) <- vapply(path, `[[`, character(1), "name")
  names(doc) <- vapply(doc, `[[`, character(1), "name")
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
  unname(utils::modifyList(doc, path))
}

parse_responses <- function(tags, values, serializers) {
  responses <- lapply(unlist(values[tags == "response"]), function(response) {
    response <- split_param_spec(response)
    list2(
      !!response$name := compact(list(
        description = response$description,
        content = rep_named(
          if (response$name == "200") serializers else "*/*",
          list(list(schema = parse_openapi_type(response$type)))
        )
      ))
    )
  })
  responses <- unlist(responses, recursive = FALSE) %||% list()

  utils::modifyList(default_responses, responses)
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

parse_openapi_type <- function(string, default = NULL, min = NULL, max = NULL) {
  check_string(string, allow_na = TRUE, allow_null = TRUE)
  check_number_decimal(min, allow_null = TRUE)
  check_number_decimal(max, allow_null = TRUE)
  if (is.na(string) || is.null(string) || string == "any") return(list())
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
  } else if (string %in% c("date", "date-time", "byte", "binary")) {
    type <- list(
      type = "string",
      format = string
    )
  } else {
    type <- list(
      type = string
    )
  }
  if (!is.null(min)) {
    if (!type$type %in% c("integer", "number")) {
      cli::cli_abort("Range boundaries requires integer or number type")
    }
    type$minimum <- min
  }
  if (!is.null(max)) {
    if (!type$type %in% c("integer", "number")) {
      cli::cli_abort("Range boundaries requires integer or number type")
    }
    type$maximum <- max
  }
  if (!is.null(default)) {
    caster <- type_caster(type, FALSE, "", "")
    type$default <- caster(default)
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
  params <- lapply(unlist(values[tags == tag_name]), function(param) {
    p <- split_param_spec(param)
    list(
      `in` = type, # Will get overwritten if present in path
      name = p$name,
      description = p$description,
      required = type == "path" || p$required,
      style = "form", # TODO: Should this be user definable
      schema = parse_openapi_type(
        p$type,
        default = if (p$required) NULL else p$default,
        min = p$min,
        max = p$max
      )
    )
  })
  set_names(params, vapply(params, `[[`, character(1), "name"))
}

parse_body_params <- function(params, parsers) {
  parsers <- sub("multipart/*", "multipart/form-data", parsers, fixed = TRUE)
  if (
    length(params) == 1 && (is.na(params[[1]]$name) || params[[1]]$name == "")
  ) {
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
    request_body <- list(
      description = body_description,
      content = rep_named(parsers, list(list(schema = schema)))
    )
  } else {
    request_body <- NULL
  }
  request_body
}

split_param_spec <- function(x) {
  x <- stringi::stri_match_first_regex(
    gsub("\n", " ", x),
    "^(\\w*)(:?(.*?))?((?<!,|:)\\s(.*))?$"
  )

  list2(
    name = x[2],
    !!!split_type_spec(x[4]),
    description = if (is.na(x[6])) "" else trimws(x[6])
  )
}
split_type_spec <- function(x) {
  x <- stringi::stri_match_first_regex(
    x,
    "^([^\\|\\(]*)(\\|([^,]*),?\\s?(.*)\\|)?(\\((.*)\\))?(\\*)?$"
  )
  list(
    type = if (is.na(x[2]) || x[2] == "") NULL else x[2],
    default = if (is.na(x[7]) || x[7] == "") NULL else jsonlite::fromJSON(x[7]),
    min = if (is.na(x[4]) || x[4] == "") NULL else as.numeric(x[4]),
    max = if (is.na(x[5]) || x[5] == "") NULL else as.numeric(x[5]),
    required = !is.na(x[8])
  )
}
