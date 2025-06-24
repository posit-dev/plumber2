create_type_casters <- function(doc) {
  casters <- list(
    path = identity,
    query = identity,
    body = function(x, ...) x
  )
  if (is.null(doc)) {
    return(casters)
  }
  path_or_query <- vapply(doc$parameters, `[[`, character(1), "in")
  path_par <- doc$parameters[path_or_query == "path"]
  if (length(path_par) != 0) {
    casters$path <- create_par_caster(path_par, "path")
  }
  query_par <- doc$parameters[path_or_query == "query"]
  if (length(query_par) != 0) {
    casters$query <- create_par_caster(query_par, "query")
  }
  if (!is.null(doc$requestBody)) {
    casters$body <- create_body_caster(doc$requestBody)
  }
  casters
}

create_par_caster <- function(parameters, loc) {
  schemas <- lapply(parameters, `[[`, "schema")
  required <- vapply(parameters, `[[`, logical(1), "required")
  name <- vapply(parameters, `[[`, character(1), "name")
  has_schema <- lengths(schemas) != 0
  schemas <- schemas[has_schema | required]
  if (length(schemas) == 0) {
    return(identity)
  }
  required <- required[has_schema | required]
  name <- name[has_schema | required]
  casters <- lapply(seq_along(schemas), function(i) {
    type_caster(schemas[[i]], required[[i]], name[[i]], loc)
  })
  names(casters) <- name
  function(parameters) {
    for (par in names(casters)) {
      parameters[[par]] <- casters[[par]](parameters[[par]])
    }
    parameters
  }
}

type_caster <- function(schema, required, name, loc, scalar = TRUE) {
  switch(
    schema$type %||% "none",
    string = string_caster(schema, required, name, loc, scalar = scalar),
    number = number_caster(schema, required, name, loc, scalar = scalar),
    integer = integer_caster(schema, required, name, loc, scalar = scalar),
    boolean = bool_caster(schema, required, name, loc, scalar = scalar),
    array = array_caster(schema, required, name, loc, scalar = scalar),
    object = object_caster(schema, required, name, loc, scalar = scalar),
    required_caster(schema, required, name, loc, scalar = scalar)
  )
}

caster_constructor <- function(coercer, ...) {
  args <- list2(...)
  function(schema, required, name, loc, scalar) {
    error_string <- missing_required_error_string(name, loc)
    format_error_string <- bad_format_error_string(name, schema)
    scalar_error_string <- not_scalar_error_string(name, schema)
    default <- schema$default
    enum <- schema$enum
    if (!is.null(enum)) {
      if (!is.null(default)) {
        default <- suppressWarnings(factor(default, enum))
        if (anyNA(default) || (scalar && length(default) != 1)) {
          cli::cli_abort("{.arg default} does not fit the provided type")
        }
      }
      enum_error_string <- unmatched_enum_error_string(name, enum)
      fun <- function(val, call = caller_env()) {
        if (is.null(val)) {
          if (required) {
            reqres::abort_bad_request(error_string, call = call)
          }
          default
        } else {
          if (scalar && length(val) > 1) {
            reqres::abort_bad_request(scalar_error_string, call = call)
          }
          val <- suppressWarnings(factor(val, enum))
          if (anyNA(val)) {
            reqres::abort_bad_request(enum_error_string, call = call)
          }
          val
        }
      }
      return(fun)
    }
    pattern <- schema$pattern
    if (!is.null(pattern)) {
      if (!is.null(default)) {
        if (
          !all(grepl(default, val, perl = TRUE)) ||
            (scalar && length(default) != 1)
        ) {
          cli::cli_abort("{.arg default} does not fit the provided type")
        }
      }
      pattern_error_string <- unmatched_pattern_error_string(name, pattern)
      fun <- function(val, call = caller_env()) {
        if (is.null(val)) {
          if (required) {
            reqres::abort_bad_request(error_string, call = call)
          }
          default
        } else {
          if (scalar && length(val) > 1) {
            reqres::abort_bad_request(scalar_error_string, call = call)
          }
          if (!all(grepl(pattern, val, perl = TRUE))) {
            reqres::abort_bad_request(pattern_error_string, call = call)
          }
          val
        }
      }
      return(fun)
    }
    min <- schema$minimum
    max <- schema$maximum
    range_check <- identity
    if (!is.null(min) || !is.null(max)) {
      range_error_string <- outside_range_error_string(name, min, max)
      range_check <- function(val, call = caller_env()) {
        if (
          (!is.null(min) && any(val < min)) || (!is.null(max) && any(val > max))
        ) {
          reqres::abort_bad_request(range_error_string, call = call)
        }
        val
      }
    }
    if (!is.null(default)) {
      default <- suppressWarnings(inject(coercer(default, !!!args)))
      if (
        anyNA(default) ||
          (scalar && length(default) != 1) ||
          (!is.null(min) && any(default < min)) ||
          (!is.null(max) && any(default > max))
      ) {
        cli::cli_abort("{.arg default} does not fit the provided type")
      }
    }
    function(val, call = caller_env()) {
      if (is.null(val)) {
        if (required) {
          reqres::abort_bad_request(error_string, call = call)
        }
        default
      } else {
        if (scalar && length(val) > 1) {
          reqres::abort_bad_request(scalar_error_string, call = call)
        }
        val <- suppressWarnings(inject(coercer(val, !!!args)))
        if (anyNA(val)) {
          reqres::abort_bad_request(I(format_error_string), call = call)
        }
        range_check(val)
      }
    }
  }
}
string_caster0 <- caster_constructor(as.character)
number_caster <- caster_constructor(as.numeric)
integer_caster <- caster_constructor(as.integer)
bool_caster <- caster_constructor(as.logical)
date_caster <- caster_constructor(as.Date, format = "%Y-%m-%d")
datetime_caster <- caster_constructor(function(x, ...) {
  as.POSIXlt(
    sub(":(\\d\\d)$", "\\1", sub("Z$", "+0000", sub(" ", "T", toupper(x)))),
    format = "%FT%T%z"
  )
})
#' @importFrom base64enc base64decode
byte_caster <- caster_constructor(base64decode)
binary_caster <- caster_constructor(function(x, ...) if (!is.raw(x)) NA else x)
required_caster <- caster_constructor(function(x, ...) x)

string_caster <- function(schema, required, name, loc, scalar) {
  switch(
    schema$format %||% "none",
    date = date_caster(schema, required, name, loc, scalar = scalar),
    "date-time" = datetime_caster(schema, required, name, loc, scalar = scalar),
    byte = byte_caster(schema, required, name, loc, scalar = scalar),
    binary = binary_caster(schema, required, name, loc, scalar = FALSE),
    none = string_caster0(schema, required, name, loc, scalar = scalar),
    required_caster(schema, required, name, loc, scalar = scalar)
  )
}

array_caster <- function(schema, required, name, loc, scalar) {
  error_string <- missing_required_error_string(name, loc)
  caster <- type_caster(schema$items, required, name, loc, FALSE)
  if (
    schema$items$type %in%
      c("array", "object") ||
      (isTRUE(schema$items$type == "string") &&
        isTRUE(schema$items$format == "binary"))
  ) {
    caster <- function(val) {
      lapply(val, caster)
    }
  }
  default <- schema$default
  if (is_string(default)) {
    default <- jsonlite::fromJSON(default)
  }
  if (!is.null(default)) {
    default <- try_fetch(caster(default), error = function(err) {
      cli::cli_abort("{.arg default} does not fit the provided type")
    })
  }
  function(val, call = caller_env()) {
    if (is.null(val)) {
      if (required) {
        reqres::abort_bad_request(error_string, call = call)
      }
      default
    } else {
      caster(val)
    }
  }
}

object_caster <- function(schema, required, name, loc, scalar) {
  error_string <- missing_required_error_string(name, loc)
  name <- names(schema$properties)
  required_prop <- name %in% (schema$required %||% "")
  casters <- lapply(seq_along(schema$properties), function(i) {
    type_caster(schema$properties[[i]], required_prop[[i]], name[[i]], loc)
  })
  names(casters) <- name
  default <- schema$default
  if (is_string(default)) {
    default <- jsonlite::fromJSON(default)
  }
  no_default <- is.null(default)
  default <- default %||% list()
  try_fetch(
    {
      for (par in names(casters)) {
        default[[par]] <- casters[[par]](default[[par]])
      }
    },
    error = function(err) {
      cli::cli_abort("{.arg default} does not fit the provided type")
    }
  )
  if (no_default && all(lengths(default) == 0)) {
    default <- NULL
  }
  function(val, call = caller_env()) {
    if (is.null(val)) {
      if (required) {
        reqres::abort_bad_request(error_string, call = call)
      }
      default
    } else {
      for (par in names(casters)) {
        val[[par]] <- casters[[par]](val[[par]])
      }
      val
    }
  }
}

create_body_caster <- function(desc) {
  requires_caster <- vapply(
    desc$content,
    function(content) {
      !is.null(content$schema$type)
    },
    logical(1)
  )

  if (!any(requires_caster)) {
    if (desc$required) {
      caster <- required_caster(list(), TRUE, loc = "body")
    } else {
      caster <- identity
    }
    return(function(val, type) {
      caster(val)
    })
  }

  casters <- lapply(desc$content, function(content) {
    type_caster(content$schema, isTRUE(desc$required), loc = "body")
  })
  names(casters) <- stringi::stri_replace_all_fixed(names(casters), "*", ".+?")
  function(val, type) {
    if (is.null(type)) {
      type <- 1L
    } else {
      type <- stringi::stri_split_fixed(type, ";", 2)[[1]][1]
    }
    caster <- casters[[type]]
    if (is.null(caster)) {
      type <- which(stringi::stri_detect_regex(type, names(casters)))
      if (length(type) == 0) {
        return(val)
      }
      caster <- casters[[type[1]]]
    }
    caster(val)
  }
}

missing_required_error_string <- function(name, loc) {
  if (missing(name) && loc == "body") {
    cli::format_inline("A request body is a required but missing")
  } else {
    cli::format_inline(
      "{.arg {name}} is a required {loc} parameter but is missing"
    )
  }
}

bad_format_error_string <- function(name, schema) {
  cli::format_inline(
    "{.arg {name}} must match the type {.val {jsonlite::toJSON(schema, auto_unbox = TRUE)}}"
  )
}

not_scalar_error_string <- function(name, schema) {
  cli::format_inline(
    "{.arg {name}} must be a scalar value"
  )
}

outside_range_error_string <- function(name, min, max) {
  if (!is.null(min) && !is.null(max)) {
    cli::format_inline(
      "{.arg {name}} must be between {min} and {max}"
    )
  } else if (is.null(max)) {
    cli::format_inline(
      "{.arg {name}} must be greater than or equal to {min}"
    )
  } else {
    cli::format_inline(
      "{.arg {name}} must be less than or equal to {max}"
    )
  }
}

unmatched_enum_error_string <- function(name, values) {
  cli::format_inline(
    "{.arg {name}} must be one of {.or {.val {values}}}"
  )
}

unmatched_pattern_error_string <- function(name, pattern) {
  cli::format_inline(
    "{.arg {name}} must match the pattern {.val {pattern}}"
  )
}
