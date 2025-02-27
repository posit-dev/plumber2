registry$parsers <- list()

#' @export
register_parser <- function(name, fun, mime_types) {
  check_function(fun)
  check_character(mime_types)
  if (length(fn_fmls(fun())) != 2) {
    cli::cli_abort("{.arg fun} must be a binary function")
  }
  registry$parsers[[name]] <- list(fun = fun, types = mime_types)
  invisible(NULL)
}

get_parsers <- function(types = NULL, env = caller_env()) {
  if (isTRUE(tolower(types) == "none")) {
    return(NULL)
  }
  if (is.null(types)) {
    types <- names(registry$parsers)
  }
  dots <- which(types == "...")
  if (length(dots) != 0) {
    types <- c(
      types[seq_len(dots-1)],
      setdiff(names(registry$parsers), types),
      types[dots + seq_len(length(types) - dots)]
    )
  }
  parsers <- lapply(types, function(type) {
    type <- stringi::stri_split_fixed(type, " ", n = 2)[[1]]
    if (stringi::stri_count_fixed(type[[1]], "/") == 1) {
      parser_fun <- if (length(type) == 2) eval_bare(parse_expr(type[2]), env = env) else identity
      check_function(parser_fun)
      if (length(fn_fmls(parser_fun)) != 2) {
        cli::cli_abort("Provided parser function must be a binary function")
      }
      parser <- list(
        fun = parser_fun,
        type = type[1]
      )
    } else {
      parser <- registry$parsers[[type[[1]]]]
      if (is.null(parser)) {
        cli::cli_abort("No parser registered as {type}")
      }
      if (length(type) == 1) {
        args <- list()
      } else {
        args <- eval_bare(parse_expr(paste0("list(", type[[2]], ")")), env = env)
      }
      parser$fun <- inject(parser$fun(!!!args))
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
parse_csv <- function(...) {
  check_installed("readr")
  function(raw, directives) {
    readr::read_csv(raw, ...)
  }
}
parse_octet <- function() {
  function(raw, directives) {
    raw
  }
}
parse_rds <- function(...) {
  function(raw, directives) {
    unserialize(raw, ...)
  }
}
parse_feather <- function(...) {
  check_installed("arrow")
  function(raw, directives) {
    arrow::read_feather(raw, ...)
  }
}
parse_parquet <- function(...) {
  check_installed("arrow")
  function(raw, directives) {
    arrow::read_parquet(raw, ...)
  }
}
parse_text <- function(multiple = FALSE) {
  function(raw, directives) {
    rawToChar(raw, multiple = multiple)
  }
}
parse_tsv <- function(...) {
  check_installed("readr")
  function(raw, directives) {
    readr::read_tsv(raw, ...)
  }
}
parse_yaml <- function(...) {
  check_installed("yaml")
  function(raw, directives) {
    yaml::yaml.load(rawToChar(raw), eval.expr = FALSE, ...)
  }
}
parse_geojson <- function(...) {
  check_installed("geojsonsf")
  function(raw, directives) {
    geojsonsf::geojson_sf(rawToChar(raw), ...)
  }
}

on_load({
  register_parser("csv", parse_csv, c("application/csv", "application/x-csv", "text/csv", "text/x-csv"))
  register_parser("json", reqres::parse_json, c("application/json", "text/json"))
  register_parser("multi", reqres::parse_multiform, "multipart/*")
  register_parser("octet", parse_octet, "application/octet-stream")
  register_parser("form", reqres::parse_queryform, "application/x-www-form-urlencoded")
  register_parser("rds", parse_rds, "application/rds")
  register_parser("feather", parse_feather, c("application/vnd.apache.arrow.file", "application/feather"))
  register_parser("parquet", parse_parquet, "application/vnd.apache.parquet")
  register_parser("text", parse_text, c("text/plain", "text/*"))
  register_parser("tsv", parse_tsv, c("application/tab-separated-values", "text/tab-separated-values"))
  register_parser("yaml", parse_yaml, c("text/vnd.yaml", "application/yaml", "application/x-yaml", "text/yaml", "text/x-yaml"))
  register_parser("geojson", parse_geojson, c("application/geo+json", "application/vdn.geo+json"))
})
