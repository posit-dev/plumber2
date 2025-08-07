registry$parsers <- list()

#' Register or fetch a parser
#'
#' plumber2 comes with many parsers that should cover almost all standard
#' use cases. Still you might want to provide some of your own, which this
#' function facilitates.
#'
#' If you want to register your own parser, then the function you register must
#' be a factory function, i.e. a function returning a function. The returned
#' function must accept two arguments, the first being a raw vector
#' corresponding to the request body, the second being the parsed directives
#' from the request `Content-Type` header. All arguments to the factory function
#' should be optional.
#'
#' @param name The name to register the parser function to. If already
#' present the current parser will be overwritten by the one provided by you
#' @param fun A function that, when called, returns a binary function that can
#' parse a request body. The first argument takes a raw vector with the binary
#' encoding of the request body, the second argument takes any additional
#' directives given by the requests `Content-Type` header
#' @param mime_types One or more mime types that this parser can handle. The
#' mime types are allowed to contain wildcards, e.g. `"text/*"`
#' @param default Should this parser be part of the default set of parsers
#'
#' @return For `get_parsers` a named list of parser functions named by their
#' mime types. The order given in `parsers` is preserved.
#'
#' @seealso [parsers]
#' @seealso [register_serializer()]
#' @export
#'
register_parser <- function(name, fun, mime_types, default = TRUE) {
  check_function(fun)
  check_character(mime_types)
  check_string(name)
  if (grepl("/", name, fixed = TRUE)) {
    cli::cli_abort(
      "{.arg name} must not contain the forward slash character ({.field /})"
    )
  }
  if (name %in% c("...", "none")) {
    cli::cli_abort(
      "{.arg name} must not be {.val {c('...', 'none')}}"
    )
  }
  registry$parsers[[name]] <- list(
    fun = fun,
    types = mime_types,
    default = default
  )
  invisible(NULL)
}
#' @rdname register_parser
#' @export
show_registered_parsers <- function() {
  res <- data.frame(
    name = names(registry$parsers),
    mime_types = I(lapply(registry$parsers, `[[`, "types")),
    default = vapply(registry$parsers, `[[`, logical(1), "default")
  )
  attr(res, "row.names") <- .set_row_names(nrow(res))
  res
}
#' @rdname register_parser
#' @param parsers Parsers to collect. This can either be a character vector of
#' names of registered parsers or a list. If it is a list then the following
#' expectations apply:
#' * Any unnamed elements containing a character vector will be considered as
#'   names of registered parsers constructed with default values. The special
#'   value `"..."` will fetch all the parsers that are otherwise not specified
#'   in the call
#' * Any element containing a function are considered as a provided parser and
#'   the element must be named by the mime type the parser understands
#'   (wildcards allowed)
#' * Any remaining named elements will be considered names of registered parsers
#'   that should be constructed with the arguments given in the element
#'
#' @export
get_parsers <- function(parsers = NULL) {
  defaults <- vapply(registry$parsers, `[[`, logical(1), "default")
  if (is.null(parsers)) {
    parsers <- names(registry$parsers)[defaults]
  }
  elem_names <- names(parsers) %||% rep_along(parsers, "")
  named_parsers <- unlist(lapply(seq_along(parsers), function(i) {
    if (elem_names[i] == "") {
      if (is_character(parsers[[i]])) {
        parsers[[i]]
      } else {
        NULL
      }
    } else {
      elem_names[i]
    }
  }))
  if (sum(named_parsers == "...") > 1) {
    cli::cli_abort("{.val ...} can only be used once in {.arg parsers}")
  }
  named_parsers <- named_parsers[!grepl("/|^\\.\\.\\.$", named_parsers)]
  dots_parsers <- setdiff(names(registry$parsers)[defaults], named_parsers)
  parsers <- lapply(seq_along(parsers), function(i) {
    if (is_function(parsers[[i]])) {
      if (length(fn_fmls(parsers[[i]])) != 2) {
        cli::cli_abort(
          "Provided parsers must be binary functions"
        )
      }
      if (!grepl("/", elem_names[i], fixed = TRUE)) {
        cli::cli_abort(
          "Parsers provided as functions must be named by their mime type"
        )
      }
      return(list2(!!elem_names[i] := parsers[[i]]))
    }
    if (elem_names[i] == "" && is_character(parsers[[i]])) {
      return(get_parsers_internal(
        parsers[[i]],
        env = env,
        dots_parsers = dots_parsers
      ))
    }
    if (elem_names[i] != "") {
      if (is.null(registry$parsers[[elem_names[i]]])) {
        cli::cli_abort(
          "No parser registered with {.val {elem_names[i]}} as name"
        )
      }
      if (!is.list(parsers[[i]])) parsers[[i]] <- list(parsers[[i]])
      funs <- rep_named(
        registry$parsers[[elem_names[i]]]$types,
        list(registry$parsers[[elem_names[i]]]$fun)
      )
      return(lapply(funs, function(f) inject(f(!!!parsers[[i]]))))
    }
    cli::cli_abort("Don't know how to parse element {i} in {.arg parsers}")
  })
  unlist(parsers, recursive = FALSE)
}

get_parsers_internal <- function(
  types = NULL,
  env = caller_env(),
  dots_parsers = NULL
) {
  if (isTRUE(tolower(types) == "none")) {
    return(NULL)
  }
  if (is.null(types)) {
    types <- "..."
  }
  dots <- which(types == "...")
  if (length(dots) != 0) {
    defaults <- vapply(registry$parsers, `[[`, logical(1), "default")
    types <- c(
      types[seq_len(dots - 1)],
      dots_parsers %||% setdiff(names(registry$parsers)[defaults], types),
      types[dots + seq_len(length(types) - dots)]
    )
  }
  parsers <- lapply(types, function(type) {
    type <- stringi::stri_split_regex(type, "\\{|\\s", n = 2)[[1]]
    if (stringi::stri_count_fixed(type[[1]], "/") == 1) {
      parser_fun <- if (length(type) == 2)
        eval_bare(parse_expr(type[2]), env = env) else function(x, ...) x
      check_function(parser_fun)
      parser <- list(
        fun = parser_fun,
        types = type[1]
      )
    } else {
      parser <- registry$parsers[[type[[1]]]]
      if (is.null(parser)) {
        cli::cli_abort("No parser registered as {type}")
      }
      if (length(type) == 1) {
        args <- list()
      } else {
        args <- eval_bare(
          parse_expr(paste0("list(", sub("\\}$", "", type[[2]]), ")")),
          env = env
        )
      }
      parser$fun <- inject(parser$fun(!!!args))
    }
    if (length(fn_fmls(parser$fun)) != 2) {
      cli::cli_abort(
        "The parser provided for {.field {type}} must be a binary function"
      )
    }
    parser
  })
  fun <- lapply(parsers, `[[`, "fun")
  type <- lapply(parsers, `[[`, "types")
  fun <- rep(fun, lengths(type))
  names(fun) <- unlist(type)
  fun
}

# Default parsers --------------------------------------------------------------

#' Parser functions provided by plumber2
#'
#' These functions cover a large area of potential request body formats. They
#' are all registered to their standard mime types but users may want to use
#' them to register them to alternative types if they know it makes sense.
#'
#' # Provided parsers
#' * `parse_csv()` uses [readr::read_csv()] for parsing. It is registered as
#'   `"csv"` for the mime types `application/csv`, `application/x-csv`,
#'   `text/csv`, and `text/x-csv`
#' * `parse_multipart` uses [webutils::parse_multipart()] for the initial
#'   parsing. It then goes through each part and tries to find a parser that
#'   matches the content type (either given directly or guessed from the file
#'   extension provided). If a parser is not found it leaves the value as a raw
#'   vector. It is registered as "`multi`" for the mime type `multipart/*`
#' * `parse_octet()` passes the raw data through unchanged. It is registered as
#'   `"octet"` for the mime type `application/octet-stream`
#' * `parse_rds()` uses [unserialize()] for parsing. It is registered as
#'   `"rds"` for the mime type `application/rds`
#' * `parse_feather()` uses `arrow::read_feather()` for parsing. It is
#'   registered as `"feather"` for the mime types
#'   `application/vnd.apache.arrow.file` and `application/feather`
#' * `parse_parquet()` uses `arrow::read_parquet()` for parsing. It is
#'   registered as `"parquet"` for the mime type `application/vnd.apache.parquet`
#' * `parse_text()` uses [rawToChar()] for parsing. It is registered as
#'   `"text"` for the mime types `text/plain` and `text/*`
#' * `parse_tsv()` uses [readr::read_tsv()] for parsing. It is registered as
#'   `"tsv"` for the mime types `application/tab-separated-values` and
#'   `text/tab-separated-values`
#' * `parse_yaml()` uses [yaml::yaml.load()] for parsing. It is registered as
#'   `"yaml"` for the mime types `text/vnd.yaml`, `application/yaml`,
#'   `application/x-yaml`, `text/yaml`, and `text/x-yaml`
#' * `parse_geojson()` uses `geojsonsf::geojson_sf()` for parsing. It is
#'   registered as `"geojson"` for the mime types `application/geo+json` and
#'   `application/vdn.geo+json`
#'
#' ## Additional registered parsers
#' * [reqres::parse_json()] is registered as "`json`" for the mime types
#'   `application/json` and `text/json`
#' * [reqres::parse_queryform()] is registered as "`form`" for the mime type
#'   `application/x-www-form-urlencoded`
#' * [reqres::parse_xml()] is registered as "`xml`" for the mime types
#'   `application/xml` and `text/xml`
#' * [reqres::parse_html()] is registered as "`html`" for the mime type
#'   `text/html`
#'
#' @param ... Further argument passed on to the internal parsing function. See
#' Details for information on which function handles the parsing internally in
#' each parser
#'
#' @return A function accepting a raw vector along with a `directives` argument
#' that provides further directives from the `Content-Type` to be passed along
#'
#' @seealso [register_parser()]
#' @rdname parsers
#' @name parsers
#'
NULL

#' @rdname parsers
#' @export
#'
parse_csv <- function(...) {
  function(raw, directives) {
    readr::read_csv(raw, ...)
  }
}
#' @rdname parsers
#' @export
#'
parse_octet <- function() {
  function(raw, directives) {
    raw
  }
}
#' @rdname parsers
#' @export
#'
parse_rds <- function(...) {
  function(raw, directives) {
    unserialize(raw, ...)
  }
}
#' @rdname parsers
#' @export
#'
parse_feather <- function(...) {
  check_installed("arrow")
  function(raw, directives) {
    arrow::read_feather(raw, ...)
  }
}
#' @rdname parsers
#' @export
#'
parse_parquet <- function(...) {
  check_installed("arrow")
  function(raw, directives) {
    arrow::read_parquet(raw, ...)
  }
}
#' @rdname parsers
#' @inheritParams base::rawToChar
#' @export
#'
parse_text <- function(multiple = FALSE) {
  function(raw, directives) {
    rawToChar(raw, multiple = multiple)
  }
}
#' @rdname parsers
#' @export
#'
parse_tsv <- function(...) {
  function(raw, directives) {
    readr::read_tsv(raw, ...)
  }
}
#' @rdname parsers
#' @export
#'
parse_yaml <- function(...) {
  function(raw, directives) {
    yaml::yaml.load(rawToChar(raw), eval.expr = FALSE, ...)
  }
}
#' @rdname parsers
#' @export
#'
parse_geojson <- function(...) {
  check_installed("geojsonsf")
  function(raw, directives) {
    geojsonsf::geojson_sf(rawToChar(raw), ...)
  }
}
#' @rdname parsers
#' @param parsers A list of parsers to use for parsing the parts of the body
#' @export
#'
parse_multipart <- function(parsers = get_parsers()) {
  # We need to do this dance to avoid recursion hell
  parsers <- enquo(parsers)
  evaled_parsers <- NULL
  function(raw, directives) {
    res <- webutils::parse_multipart(raw, directives$boundary)
    lapply(res, function(part) {
      filename <- part$filename
      type <- part$content_type
      if (!is.null(type)) {
        type <- stringi::stri_split_fixed(type, ";", n = 2)[[1]][1]
      }

      if (!is.null(filename)) {
        if (is.null(type) || type == "application/octet-stream") {
          type <- reqres::mime_type_from_file(filename)$name
        } else {
          type <- reqres::mime_type_info(type)$name
        }
      } else {
        type <- type %||% "text/plain"
      }

      parser_fun <- NULL
      if (length(type) > 0) {
        if (is.null(evaled_parsers)) {
          # Second part of the dance described above
          parsers <- eval_tidy(parsers) %||% list()
          parsers <- parsers[order(stringi::stri_count_fixed(
            names(parsers),
            "*"
          ))]
          names(parsers) <- gsub("*", ".+?", names(parsers), fixed = TRUE)
          evaled_parsers <<- parsers
        }
        type <- type[[1]][1]
        parser <- which(stringi::stri_detect_regex(type, names(evaled_parsers)))
        if (length(parser) != 0) {
          parser_fun <- evaled_parsers[[parser[1]]]
        }
      }
      val <- part$value
      if (!is.null(parser_fun)) {
        val <- parser_fun(val, list())
      }
      attributes(val) <- c(attributes(val), part[names(part) != "value"])
      val
    })
  }
}

on_load({
  register_parser(
    "json",
    reqres::parse_json,
    c("application/json", "text/json")
  )
  register_parser(
    "csv",
    parse_csv,
    c("application/csv", "application/x-csv", "text/csv", "text/x-csv")
  )
  register_parser(
    "tsv",
    parse_tsv,
    c("application/tab-separated-values", "text/tab-separated-values")
  )
  register_parser("multi", parse_multipart, "multipart/*")
  register_parser("octet", parse_octet, "application/octet-stream")
  register_parser(
    "form",
    reqres::parse_queryform,
    "application/x-www-form-urlencoded"
  )
  register_parser("rds", parse_rds, "application/rds")
  register_parser("text", parse_text, c("text/plain", "text/*"))
  register_parser(
    "yaml",
    parse_yaml,
    c(
      "text/vnd.yaml",
      "application/yaml",
      "application/x-yaml",
      "text/yaml",
      "text/x-yaml"
    )
  )
  register_parser("xml", reqres::parse_xml, c("application/xml", "text/xml"))
  register_parser("html", reqres::parse_html, c("text/html"))
  register_parser(
    "feather",
    parse_feather,
    c("application/vnd.apache.arrow.file", "application/feather"),
    default = FALSE
  )
  register_parser(
    "parquet",
    parse_parquet,
    "application/vnd.apache.parquet",
    default = FALSE
  )
  register_parser(
    "geojson",
    parse_geojson,
    c("application/geo+json", "application/vdn.geo+json"),
    default = FALSE
  )
})
