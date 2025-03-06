registry$serializers <- list()

#' Register or fetch a serializer
#'
#' plumber2 comes with many serializers that should cover almost all standard
#' use cases. Still you might want to provide some of your own, which this
#' function facilitates.
#'
#' If you want to register your own serializer, then the function you register
#' must be a factory function, ie. a function returning a function. The returned
#' function must accept a single argument which is the response body. All
#' arguments to the factory function should be optional.
#'
#' @param name The name to register the serializer function to. If already
#' present the current serializer will be overwritten by the one provided by you
#' @param fun A function that, when called, returns a unary function that can
#' serialize a response body to the mime type defined in `mime_type`
#' @param mime_type The format this serializer creates. You should take care to
#' ensure that the value provided is a standard mime type for the format
#'
#' @return For `get_serializers` a named list of serializer functions named by
#' their mime type. The order given in `serializers` is preserved.
#'
#' @seealso [serializers]
#' @seealso [register_serializer()]
#'
#' @export
#'
register_serializer <- function(name, fun, mime_type) {
  check_function(fun)
  check_string(mime_type)
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
  registry$serializers[[name]] <- list(fun = fun, type = mime_type)
  invisible(NULL)
}

#' @rdname register_serializer
#' @param serializers Serializers to collect. This can either be a character
#' vector of names of registered serializers or a list. If it is a list then the
#' following expectations apply:
#' * Any unnamed elements containing a character vector will be considered as
#'   names of registered serializers constructed with default values. The
#'   special value `"..."` will fetch all the serializers that are otherwise not
#'   specified in the call
#' * Any element containing a function are considered as a provided serializer
#'   and the element must be named by the mime type the serializer understands
#' * Any remaining named elements will be considered names of registered
#'   serializers that should be constructed with the arguments given in the
#'   element
#'
#' @export
get_serializers <- function(serializers = NULL) {
  if (is.null(serializers)) {
    serializers <- names(registry$serializers)
  }
  elem_names <- names(serializers) %||% rep_along(serializers, "")
  named_serializers <- unlist(lapply(seq_along(serializers), function(i) {
    if (elem_names[i] == "") {
      if (is_character(serializers[[i]])) {
        serializers[[i]]
      } else {
        NULL
      }
    } else {
      elem_names[i]
    }
  }))
  if (sum(named_serializers == "...") > 1) {
    cli::cli_abort("{.val ...} can only be used once in {.arg serializers}")
  }
  named_serializers <- named_serializers[
    !grepl("/|^\\.\\.\\.$", named_serializers)
  ]
  dots_serializers <- setdiff(names(registry$serializers), named_serializers)
  serializers <- lapply(seq_along(serializers), function(i) {
    if (is_function(serializers[[i]])) {
      if (length(fn_fmls(serializers[[i]])) != 1) {
        cli::cli_abort(
          "Provided serializers must be unary functions"
        )
      }
      if (!grepl("/", elem_names[i], fixed = TRUE)) {
        cli::cli_abort(
          "Serializers provided as functions must be named by their mime type"
        )
      }
      return(list2(!!elem_names[i] := serializers[[i]]))
    }
    if (elem_names[i] == "" && is_character(serializers[[i]])) {
      if (any(grepl("/", serializers[[i]], fixed = TRUE))) {
        cli::cli_abort("mime types must be provided with a function")
      }
      return(get_serializers_internal(
        serializers[[i]],
        env = env,
        dots_serializers = dots_serializers
      ))
    }
    if (elem_names[i] != "") {
      if (is.null(registry$serializers[[elem_names[i]]])) {
        cli::cli_abort(
          "No serializer registered with {.val {elem_names[i]}} as name"
        )
      }
      if (!is.list(serializers[[i]])) serializers[[i]] <- list(serializers[[i]])
      type <- registry$serializers[[elem_names[i]]]$type
      fun <- registry$serializers[[elem_names[i]]]$fun
      return(list2(!!type := inject(fun(!!!serializers[[i]]))))
    }
    cli::cli_abort("Don't know how to parse element {i} in {.arg serializers}")
  })
  unlist(serializers, recursive = FALSE)
}

get_serializers_internal <- function(
  types = NULL,
  env = caller_env(),
  dots_serializers = NULL
) {
  if (isTRUE(tolower(types) == "none")) {
    return(NULL)
  }
  if (is.null(types)) {
    types <- names(registry$serializers)
  }
  dots <- which(types == "...")
  if (length(dots) != 0) {
    types <- c(
      types[seq_len(dots - 1)],
      dots_serializers %||% setdiff(names(registry$serializers), types),
      types[dots + seq_len(length(types) - dots)]
    )
  }
  serializers <- lapply(types, function(type) {
    type <- stringi::stri_split_fixed(type, " ", n = 2)[[1]]
    if (stringi::stri_count_fixed(type[[1]], "/") == 1) {
      serializer_fun <- if (length(type) == 2)
        eval_bare(parse_expr(type[2]), env = env) else identity
      check_function(serializer_fun)
      serializer <- list(
        fun = serializer_fun,
        type = type[1]
      )
    } else {
      serializer <- registry$serializers[[type[[1]]]]
      if (is.null(serializer)) {
        cli::cli_abort("No serializer registered as {type}")
      }
      if (length(type) == 1) {
        args <- list()
      } else {
        args <- eval_bare(
          parse_expr(paste0("list(", type[[2]], ")")),
          env = env
        )
      }
      serializer$fun <- inject(serializer$fun(!!!args))
    }
    if (length(fn_fmls(serializer$fun)) != 1) {
      cli::cli_abort(
        "The serializer provided for {.field {type}} must be a unary function"
      )
    }
    serializer
  })
  serializers <- set_names(
    lapply(serializers, `[[`, "fun"),
    vapply(serializers, `[[`, character(1), "type")
  )
  serializers[!duplicated(names(serializers))]
}

# Default serializers ----------------------------------------------------------

#' Serializer functions provided by plumber2
#'
#' These functions cover a large area of potential response body formats. They
#' are all registered to their standard mime type but users may want to use
#' them to register them to alternative types if they know it makes sense.
#'
#' # Provided serializers
#' * `format_csv()` uses [readr::format_csv()] for formatting. It is registered
#'   as `"csv"` to the mime type `text/csv`
#' * `format_tsv()` uses [readr::format_tsv()] for formatting. It is registered
#'   as `"tsv"` to the mime type `text/tsv`
#' * `format_rds()` uses [serialize()] for formatting. It is registered as
#'   `"rds"` to the mime type `application/rds`
#' * `format_geojson`uses [geojsonsf::sfc_geojson()] or [geojsonsf::sf_geojson()]
#'   for formatting depending on the class of the response body. It is
#'   registered as `"geojson"` to the mime type `application/geo+json`
#' * `format_feather`uses [arrow::write_feather()] for formatting. It is
#'   registered as `"feather"` to the mime type
#'   `application/vnd.apache.arrow.file`
#' * `format_parquet`uses [nanoparquet::write_parquet()] for formatting. It is
#'   registered as `"parquet"` to the mime type `application/vnd.apache.parquet`
#' * `format_yaml`uses [yaml::as.yaml()] for formatting. It is registered
#'   as `"yaml"` to the mime type `text/yaml`
#' * `format_htmlwidget`uses [htmlwidgets::saveWidget()] for formatting. It is
#'   registered as `"htmlwidget"` to the mime type `text/html`
#' * `format_format`uses [format()] for formatting. It is registered
#'   as `"format"` to the mime type `text/plain`
#' * `format_print`uses [print()] for formatting. It is registered
#'   as `"print"` to the mime type `text/plain`
#' * `format_cat`uses [cat()] for formatting. It is registered
#'   as `"cat"` to the mime type `text/plain`
#' * `format_unboxed`uses [reqres::format_json()] with `auto_unbox = TRUE` for
#'   formatting. It is registered as `"unboxedJSON"` to the mime type
#'   `application/json`
#'
#' ## Additional registered serializers
#' * [reqres::format_json()] is registered as "`json`" to the mime type
#'   `application/json`
#' * [reqres::format_html()] is registered as "`html`" to the mime
#'   type `text/html`
#' * [reqres::format_xml()] is registered as "`xml`" to the mime type
#'   `text/xml`
#' * [reqres::format_plain()] is registered as "`text`" to the mime type
#'   `text/plain`
#'
#' @param ... Further argument passed on to the internal formatting function.
#' See Details for information on which function handles the formatting
#' internally in each serializer
#'
#' @return A function accepting the response body
#'
#' @seealso [register_serializer()]
#' @rdname serializers
#' @name serializers
#'
NULL

#' @rdname serializers
#' @export
#'
format_csv <- function(...) {
  check_installed("readr")
  function(x) {
    readr::format_csv(x, ...)
  }
}
#' @rdname serializers
#' @export
#'
format_tsv <- function(...) {
  check_installed("readr")
  function(x) {
    readr::format_tsv(x, ...)
  }
}
#' @rdname serializers
#' @inheritParams base::serialize
#' @export
#'
format_rds <- function(version = "3", ascii = FALSE, ...) {
  function(x) {
    serialize(x, NULL, ascii = ascii, version = version, ...)
  }
}
#' @rdname serializers
#' @export
#'
format_geojson <- function(...) {
  check_installed("geojsonsf")
  function(x) {
    if (inherits(x, "sfc")) return(geojsonsf::sfc_geojson(x, ...))
    if (inherits(x, "sf")) return(geojsonsf::sf_geojson(x, ...))
    cli::cli_abort(
      "{.fun format_geojson} did not receive an `sf` or `sfc` object."
    )
  }
}
#' @rdname serializers
#' @export
#'
format_feather <- function(...) {
  check_installed("arrow")
  function(x) {
    tmpfile <- tempfile()
    on.exit(unlink(tmpfile))
    arrow::write_feather(x, tmpfile, ...)
    readBin(tmpfile, what = "raw", n = file.info(tmpfile)$size)
  }
}
#' @rdname serializers
#' @export
#'
format_parquet <- function(...) {
  check_installed("nanoparquet")
  function(x) {
    nanoparquet::write_parquet(x, file = ":raw:", ...)
  }
}
#' @rdname serializers
#' @export
#'
format_yaml <- function(...) {
  check_installed("yaml")
  function(x) {
    yaml::as.yaml(x, ...)
  }
}
#' @rdname serializers
#' @export
#'
format_htmlwidget <- function(...) {
  check_installed("htmlwidgets")
  function(x) {
    tmpfile <- tempfile(fileext = ".html")
    on.exit(unlink(tmpfile))
    htmlwidgets::saveWidget(x, tmpfile, selfcontained = TRUE, ...)
    readLines(tmpfile)
  }
}
#' @rdname serializers
#' @param sep The separator between multiple elements
#' @export
#'
format_format <- function(..., sep = "\n") {
  function(x) {
    paste0(format(x, ...), collapse = sep)
  }
}
#' @rdname serializers
#' @export
#'
format_print <- function(..., sep = "\n") {
  function(x) {
    paste0(
      utils::capture.output({
        print(x, ...)
      }),
      sep = "\n"
    )
  }
}
#' @rdname serializers
#' @export
#'
format_cat <- function(..., sep = "\n") {
  function(x) {
    paste0(
      utils::capture.output({
        cat(x, ...)
      }),
      sep = "\n"
    )
  }
}
#' @rdname serializers
#' @export
#'
format_unboxed <- function(...) {
  reqres::format_json(auto_unbox = TRUE, ...)
}

on_load({
  register_serializer("json", reqres::format_json, "application/json")
  register_serializer("unboxedJSON", format_unboxed, "application/json")
  register_serializer("html", reqres::format_html, "text/html")
  register_serializer("rds", format_rds, "text/html")
  register_serializer("geojson", format_geojson, "application/geo+json")
  register_serializer("csv", format_csv, "text/csv")
  register_serializer("tsv", format_tsv, "text/tab-separated-values")
  register_serializer(
    "feather",
    format_feather,
    "application/vnd.apache.arrow.file"
  )
  register_serializer(
    "parquet",
    format_parquet,
    "application/vnd.apache.parquet"
  )
  register_serializer("yaml", format_yaml, "text/yaml")
  register_serializer("htmlwidget", format_htmlwidget, "text/html")
  register_serializer("xml", reqres::format_xml, "text/xml")
  register_serializer("text", reqres::format_plain, "text/plain")
  register_serializer("format", format_format, "text/plain")
  register_serializer("print", format_print, "text/plain")
  register_serializer("cat", format_cat, "text/plain")
})

# Device serializers -----------------------------------------------------------

# TODO: Somehow, we need to keep these distinct from the other serializers so
# that ... when used together with a device serializer only retrieves other
# device serializers and vice versa

device_formatter <- function(dev_open, dev_close = grDevices::dev.off()) {
  dev_name <- caller_arg(dev_open)
  check_function(dev_open)
  if (!"filename" %in% fn_fmls_names(dev_open)) {
    if ("file" %in% fn_fmls_names(dev_open)) {
      fn_fmls_names(dev_open)[fn_fmls_names(dev_open) == "file"] <- "filename"
    } else {
      cli::cli_abort(
        "{.arg dev_open} must be a function with a {.arg filename} or {.arg file} argument"
      )
    }
  }
  function(...) {
    provided_args <- names(enquos(...))
    dev_args <- fn_fmls_names(dev_open)
    extra_args <- setdiff(provided_args, dev_args)
    if (length(extra_args) != 0 && !"..." %in% dev_args) {
      cli::cli_abort(
        "Provided arguments does not match arguments in {.fun {dev_name}}"
      )
    }
    init_dev <- function() {
      output_file <- tempfile()
      dev_open(filename = output_file, ...)
      dev_id <- grDevices::dev.cur()
      list(path = output_file, dev = dev_id)
    }
    close_dev <- function(info) {
      grDevices::dev.set(info$dev)
      grDevices::dev.off()
      if (!file.exists(info$path)) {
        return(NULL)
      }
      con <- file(info$path, "rb")
      on.exit(
        {
          close(con)
          unlink(info$path)
        },
        add = TRUE
      )
      readBin(con, "raw", file.info(info$path)$size)
    }
    clean_dev <- function(info) {
      grDevices::dev.set(info$dev)
      grDevices::dev.off()
      unlink(info$path)
    }
    structure(
      identity,
      init = init_dev,
      close = close_dev,
      clean = clean_dev
    )
  }
}

init_formatter <- function(formatter) {
  init_fun <- attr(formatter, "init")
  if (is.null(init_fun)) {
    return(NULL)
  }
  init_fun()
}

close_formatter <- function(formatter, info) {
  close_fun <- attr(formatter, "close")
  if (is.null(info) || is.null(close_fun)) {
    return(NULL)
  }
  close_fun(info)
}

clean_formatter <- function(formatter, info) {
  clean_fun <- attr(formatter, "clean")
  if (is.null(info) || is.null(clean_fun)) {
    return(NULL)
  }
  clean_fun(info)
}

#' @importFrom ragg agg_png
format_png <- device_formatter(agg_png)
#' @importFrom ragg agg_jpeg
format_jpeg <- device_formatter(agg_jpeg)
#' @importFrom ragg agg_tiff
format_tiff <- device_formatter(agg_tiff)
#' @importFrom svglite svglite
format_svg <- device_formatter(svglite)
format_bmp <- device_formatter(grDevices::bmp)
format_pdf <- device_formatter(grDevices::pdf)

on_load({
  register_serializer("png", format_png, "image/png")
  register_serializer("jpeg", format_jpeg, "image/jpeg")
  register_serializer("tiff", format_tiff, "image/tiff")
  register_serializer("svg", format_svg, "image/svg+xml")
  register_serializer("bmp", format_bmp, "aimage/bmp")
  register_serializer("pdf", format_pdf, "application/pdf")
})
