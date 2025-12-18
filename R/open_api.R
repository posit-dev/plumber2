parse_global_api <- function(tags, values, env = caller_env()) {
  if (any(tags == "noDoc")) {
    return(NULL)
  }
  values <- set_names(values, tags)
  if (!is.null(values$contact)) {
    contact <- strsplit(trimws(values$contact), " ")[[1]]
    def_names <- seq_len(max(length(contact) - 2, 1))
    contact_email <- grepl("@", contact[-def_names], fixed = TRUE)
    contact_url <- grepl("/|:|^www\\.", contact[-def_names]) & !contact_email
    email <- if (any(contact_email)) {
      contact[length(def_names) + which(contact_email)]
    }
    url <- if (any(contact_url)) contact[length(def_names) + which(contact_url)]
    not_name <- which(c(
      rep_along(def_names, FALSE),
      contact_email | contact_url
    ))[1]
    name <- if (is.na(not_name)) contact else contact[seq_len(not_name - 1)]
    name <- paste0(name, collapse = " ")
    values$contact <- openapi_contact(name, url, email)
  }
  if (!is.null(values$license)) {
    license <- strsplit(trimws(values$license), " ")[[1]]
    if (grepl("/|:|^www\\.", license[length(license)])) {
      url <- license[length(license)]
      name <- license[-length(license)]
    } else {
      url <- NULL
      name <- license
    }
    name <- paste0(name, collapse = " ")
    values$license <- openapi_license(name, url)
  }
  openapi(
    info = openapi_info(
      title = trimws(values$title),
      description = trimws(paste0(values$description, "\n\n", values$details)),
      terms_of_service = trimws(values$tos),
      contact = values$contact,
      license = values$license,
      version = trimws(values$version)
    ),
    tags = unname(lapply(values[tags == "tag"], function(tag) {
      tag <- stringi::stri_match_all_regex(tag, "^((\".+?\")|(\\S+))(.*)")[[1]]
      openapi_tag(
        name = gsub('^"|"$', "", tag[2]),
        description = trimws(if (is.na(tag[5])) NULL else tag[5])
      )
    }))
  )
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
    params[[arg$name]] <- openapi_parameter(
      name = arg$name,
      location = "path",
      required = TRUE,
      schema = parse_openapi_type(
        arg$type,
        NULL,
        min = arg$min,
        max = arg$max,
        enum = arg$enum,
        pattern = arg$pattern
      ),
      style = "simple" # TODO: Should this be user definable
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
        "The type information for {.arg {param}} provided in the path doesn't match the information provided in {if (from_block) '@param' else '`doc`'}"
      )
    }
  }
  unname(utils::modifyList(doc, path))
}

parse_responses <- function(tags, values, serializers) {
  responses <- lapply(unlist(values[tags == "response"]), function(response) {
    response <- split_param_spec(response)
    list2(
      !!response$name := openapi_response(
        description = response$description,
        content = openapi_content(
          !!!rep_named(
            if (response$name == "200") serializers else "*/*",
            list(parse_openapi_type(
              response$type,
              min = response$min,
              max = response$max,
              enum = response$enum,
              pattern = response$pattern
            ))
          )
        )
      )
    )
  })
  responses <- unlist(responses, recursive = FALSE) %||% list()
  default_responses[["200"]]$content <- openapi_content(
    !!!rep_named(
      serializers,
      list(parse_openapi_type(NULL))
    )
  )

  utils::modifyList(default_responses, responses)
}

parse_block_api <- function(tags, values, parsers, serializers) {
  api <- list()
  summary <- if ("title" %in% tags) values[[which(tags == "title")]]
  description <- paste0(
    unlist(values[tags %in% c("description", "details")]),
    collapse = "\n\n"
  )
  if (description == "") {
    description <- NULL
  }
  path_params <- parse_params(tags, values, "path")
  query_params <- parse_params(tags, values, "query")
  body_params <- parse_params(tags, values, "body")

  request_body <- parse_body_params(body_params, parsers)

  responses <- parse_responses(tags, values, serializers)

  tag <- trimws(unlist(values[tags == "tag"]))

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
        "patch",
        "report"
      )
  )
  paths <- trimws(unlist(values[methods]))
  methods <- split(tags[methods], paths)

  for (path in names(methods)) {
    path_info <- parse_path(path)
    local_params <- combine_parameters(path_info$params, path_params)
    local_params <- unname(c(local_params, query_params))
    endpoint <- openapi_path()
    for (method in methods[[path]]) {
      if (method == "report") {
        method <- "get"
      }
      endpoint[[method]] <- openapi_operation(
        summary = summary,
        description = description,
        operation_id = paste0(path_info$path, "-", method),
        parameters = local_params,
        request_body = request_body,
        responses = responses,
        tags = tag
      )
      if (any(tags == "noDoc")) {
        class(endpoint[[method]]) <- c(
          "plumber_noDoc",
          class(endpoint[[method]])
        )
      }
    }
    api[[path_info$path]] <- endpoint
  }
  api
}

parse_openapi_type <- function(
  string,
  default = NULL,
  min = NULL,
  max = NULL,
  enum = NULL,
  pattern = NULL
) {
  check_string(string, allow_na = TRUE, allow_null = TRUE)
  check_number_decimal(min, allow_null = TRUE)
  check_number_decimal(max, allow_null = TRUE)
  if (is.na(string) || is.null(string) || string == "any") {
    return(list())
  }
  string <- trimws(string)
  if (grepl("^\\{", string)) {
    content <- gsub("^\\{|\\}$", "", string)
    last <- 0
    seps <- stringi::stri_locate_all_fixed(content, ",")[[1]][, 1]
    curly_open <- stringi::stri_locate_all_fixed(content, "{")[[1]][, 1]
    curly_close <- stringi::stri_locate_all_fixed(content, "}")[[1]][, 1]
    brack_open <- stringi::stri_locate_all_fixed(content, "[")[[1]][, 1]
    brack_close <- stringi::stri_locate_all_fixed(content, "]")[[1]][, 1]
    paren_open <- stringi::stri_locate_all_fixed(content, "(")[[1]][, 1]
    paren_close <- stringi::stri_locate_all_fixed(content, ")")[[1]][, 1]
    pipe <- stringi::stri_locate_all_fixed(content, "|")[[1]][, 1]
    if (
      length(curly_open) != length(curly_close) ||
        length(brack_open) != length(brack_close) ||
        length(paren_open) != length(paren_close) ||
        (!is.na(pipe) && length(pipe) %% 2 != 0)
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
          ) ||
          isFALSE(
            sum(paren_open > last & paren_open < seps[i]) ==
              sum(paren_close > last & paren_close < seps[i])
          ) ||
          isFALSE(
            sum(pipe > last & pipe < seps[i]) %% 2 == 0
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
    required <- stringi::stri_detect_regex(content_split[, 3], "\\*$")
    type <- list(
      type = "object",
      properties = set_names(
        lapply(content_split[, 3], function(x) {
          x <- split_type_spec(x)
          parse_openapi_type(x$type, x$default, x$min, x$max, x$enum, x$pattern)
        }),
        content_split[, 2] %|% content
      ),
      required = content_split[required, 2]
    )
  } else if (grepl("^\\[", string)) {
    x <- split_type_spec(gsub("^\\[|\\]$", "", string))
    type <- list(
      type = "array",
      items = parse_openapi_type(
        x$type,
        x$default,
        x$min,
        x$max,
        x$enum,
        x$pattern
      )
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
  if (!is.null(enum)) {
    if (!type$type == "string") {
      cli::cli_abort("Enum requires string type")
    }
    type$enum <- enum
  }
  if (!is.null(pattern)) {
    if (!type$type == "string") {
      cli::cli_abort("Pattern requires string type")
    }
    type$pattern <- pattern
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
  if (type == "body") {
    type <- "header"
  } # We use `header` because "body" is not allowed in openapi_parameter(). It will be stripped away in the next call
  params <- lapply(unlist(values[tags == tag_name]), function(param) {
    p <- split_param_spec(param)
    openapi_parameter(
      name = p$name,
      location = type, # Will get overwritten if present in path
      description = p$description,
      required = type == "path" || p$required,
      schema = parse_openapi_type(
        p$type,
        default = if (p$required) NULL else p$default,
        min = p$min,
        max = p$max,
        enum = p$enum,
        pattern = p$pattern
      ),
      style = "form" # TODO: Should this be user definable
    )
  })
  set_names(params, vapply(params, `[[`, character(1), "name"))
}

parse_body_params <- function(params, parsers) {
  parsers <- sub("multipart/*", "multipart/form-data", parsers, fixed = TRUE)
  if (
    length(params) == 1 && (is.na(params[[1]]$name) || params[[1]]$name == "")
  ) {
    request_body <- openapi_request_body(
      description = params[[1]]$description,
      required = params[[1]]$required,
      content = openapi_content(!!!rep_named(parsers, list(params[[1]]$schema)))
    )
  } else if (length(params) > 1) {
    schema <- openapi_schema(
      x = I("object"),
      properties = lapply(
        params,
        function(p) c(p$schema, list(description = p$description))
      ),
      required = names(params)[vapply(
        params,
        `[[`,
        logical(1),
        "required"
      )]
    )
    request_body <- openapi_request_body(
      content = openapi_content(!!!rep_named(parsers, list(schema)))
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
    "^(.*?)(\\|([^,]*),?\\s?(.*)\\|)?(\\((.*)\\))?(\\*)?$"
  )
  if (isTRUE(x[2] == "enum")) {
    list(
      type = "string",
      default = if (is.na(x[7]) || x[7] == "") {
        NULL
      } else {
        jsonlite::fromJSON(x[7])
      },
      enum = trimws(c(
        x[4],
        stringi::stri_split_fixed(x[5], ",", omit_empty = TRUE)[[1]]
      )),
      required = !is.na(x[8])
    )
  } else if (isTRUE(x[2] == "pattern")) {
    if (is.na(x[3])) {
      cli::cli_abort(
        "The pattern type must specify a regex using the |<regex>| syntax"
      )
    }
    list(
      type = "string",
      default = if (is.na(x[7]) || x[7] == "") {
        NULL
      } else {
        jsonlite::fromJSON(x[7])
      },
      pattern = gsub("^\\||\\|$", "", x[3]),
      required = !is.na(x[8])
    )
  } else {
    list(
      type = if (is.na(x[2]) || x[2] == "") NULL else x[2],
      default = if (is.na(x[7]) || x[7] == "") {
        NULL
      } else {
        jsonlite::fromJSON(x[7])
      },
      min = if (is.na(x[4]) || x[4] == "") NULL else as.numeric(x[4]),
      max = if (is.na(x[5]) || x[5] == "") NULL else as.numeric(x[5]),
      required = !is.na(x[8])
    )
  }
}
