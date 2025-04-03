create_type_casters <- function(doc) {
  casters <- list(
    path = identity,
    query = identity,
    body = function(x, ...) x
  )
  if (is.null(doc)) return(casters)
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

type_caster <- function(schema, required, name, loc) {
  switch(
    schema$type %||% "none",
    string = string_caster(schema, required, name, loc),
    number = number_caster(schema, required, name, loc),
    integer = integer_caster(schema, required, name, loc),
    boolean = bool_caster(schema, required, name, loc),
    array = array_caster(schema, required, name, loc),
    object = object_caster(schema, required, name, loc),
    required_caster(schema, required, name, loc)
  )
}

caster_constructor <- function(coercer, ...) {
  args <- list2(...)
  function(schema, required, name, loc) {
    error_string <- missing_required_error_string(name, loc)
    default <- schema$default
    enum <- schema$enum
    if (!is.null(enum)) {
      fun <- function(val, call = caller_env()) {
        if (is.null(val)) {
          if (required) {
            reqres::abort_bad_request(error_string, call = call)
          }
          val <- default
        }
        suppressWarnings(factor(val, enum))
      }
      return(fun)
    }
    min <- schema$minimum
    max <- schema$maximum
    range_check <- identity
    if (!is.null(min) || !is.null(max)) {
      range_error_string <- outside_range_error_string(name, min, max)
      range_check <- function(val, call = caller_env()) {
        if ((!is.null(min) && val < min) || (!is.null(max) && val > max)) {
          reqres::abort_bad_request(range_error_string, call = call)
        }
        val
      }
    }
    function(val, call = caller_env()) {
      if (is.null(val)) {
        if (required) {
          reqres::abort_bad_request(error_string, call = call)
        }
        default
      } else {
        val <- suppressWarnings(inject(coercer(val, !!!args)))
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
    sub(":(\\d\\d)$", "\\1", sub("Z$", "+0000", x)),
    format = "%FT%T%z"
  )
})
#' @importFrom base64enc base64decode
byte_caster <- caster_constructor(base64decode)
required_caster <- caster_constructor(function(x, ...) x)

string_caster <- function(schema, required, name, loc) {
  switch(
    schema$format %||% "none",
    date = date_caster(schema, required, name, loc),
    "date-time" = datetime_caster(schema, required, name, loc),
    byte = byte_caster(schema, required, name, loc),
    none = string_caster0(schema, required, name, loc),
    required_caster(schema, required, name, loc)
  )
}

array_caster <- function(schema, required, name, loc) {
  error_string <- missing_required_error_string(name, loc)
  default <- schema$default
  if (is_string(default)) {
    default <- jsonlite::fromJSON(default)
  }
  caster <- type_caster(schema$items, required, name, loc)
  if (schema$items$type %in% c("array", "object")) {
    caster <- function(val) {
      lapply(val, caster)
    }
  }
  function(val) {
    val <- val %||% default
    if (is.null(val)) {
      if (required) {
        call <- caller_env()
        reqres::abort_bad_request(error_string, call = call)
      }
      NULL
    } else {
      if (is_string(val)) {
        val <- stringi::stri_split_fixed(val, ",")
      }
      caster(val)
    }
  }
}

object_caster <- function(schema, required, name, loc) {
  error_string <- missing_required_error_string(name, loc)
  default <- schema$default
  if (is_string(default)) {
    default <- jsonlite::fromJSON(default)
  }
  name <- names(schema$properties)
  required <- name %in% (schema$required %||% "")
  casters <- lapply(seq_along(schema$properties), function(i) {
    type_caster(schema$properties[[i]], required[[i]], name[[i]], loc)
  })
  names(casters) <- name
  function(val) {
    val <- val %||% default
    if (is.null(val)) {
      if (required) {
        call <- caller_env()
        reqres::abort_bad_request(error_string, call = call)
      }
      NULL
    } else {
      for (par in names(casters)) {
        val[[par]] <- casters[[par]](val[[par]])
      }
      val
    }
  }
}

create_body_caster <- function(desc) {
  requires_caster <- vapply(desc$content, function(content) {
    !is.null(content$schema$type)
  }, logical(1))

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
    type <- stringi::stri_split_fixed(type, ";", 2)[[1]][1]
    caster <- casters[[type]]
    if (is.null(caster)) {
      type <- which(stringi::stri_detect_regex(type, names(casters)))
      if (length(type) == 0) return(val)
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
      "{.arg name} is a required {loc} parameter but is missing"
    )
  }
}

outside_range_error_string <- function(name, min, max) {
  if (!is.null(min) && !is.null(max)) {
    cli::format_inline(
      "{.arg name} must be between {min} and {max}"
    )
  } else if (is.null(max)) {
    cli::format_inline(
      "{.arg name} must be greater than or equal to {min}"
    )
  } else {
    cli::format_inline(
      "{.arg name} must be less than or equal to {max}"
    )
  }
}
