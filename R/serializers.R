registry$serializers <- list()

#' Register a serializer to a name for use with the `@serializer` tag
#'
#' plumber2 comes with many serializers that should cover almost all standard
#' use cases. Still you might want to provide some of your own, which this
#' function facilitates.
#'
#' @param name The name to register the serializer function to. If already
#' present the current serializer will be overwritten by the one provided by you
#' @param fun A function that, when called, returns a unary function that can
#' serialize a response body to the mime type defined in `mime_type`
#' @param mime_type The format this serializer creates. You should take care to
#' ensure that the value provided is a standard mime type for the format
#'
#' @return This function is called for its side effects
#'
#' @seealso [register_parser()]
#' @export
#'
register_serializer <- function(name, fun, mime_type) {
  check_function(fun)
  check_string(mime_type)
  registry$serializers[[name]] <- list(fun = fun, type = mime_type)
  invisible(NULL)
}

get_serializers <- function(types = NULL, env = caller_env()) {
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
      setdiff(names(registry$serializers), types),
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
format_csv <- function(...) {
  check_installed("readr")
  function(x) {
    readr::format_csv(x, ...)
  }
}
format_tsv <- function(...) {
  check_installed("readr")
  function(x) {
    readr::format_tsv(x, ...)
  }
}
format_rds <- function(version = "3", ascii = FALSE, ...) {
  function(x) {
    serialize(x, NULL, ascii = ascii, version = version, ...)
  }
}
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
format_feather <- function(...) {
  check_installed("arrow")
  function(x) {
    tmpfile <- tempfile()
    arrow::write_feather(x, tmpfile, ...)
    readBin(tmpfile, what = "raw", n = file.info(tmpfile)$size)
  }
}
format_parquet <- function(...) {
  check_installed("nanoparquet")
  function(x) {
    nanoparquet::write_parquet(x, file = ":raw:", ...)
  }
}
format_yaml <- function(...) {
  check_installed("yaml")
  function(x) {
    yaml::as.yaml(x, ...)
  }
}
format_htmlwidget <- function(...) {
  check_installed("htmlwidgets")
  function(x) {
    tmpfile <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(x, tmpfile, selfcontained = TRUE, ...)
    readLines(tmpfile)
  }
}
format_format <- function(..., sep = "\n") {
  function(x) {
    paste0(format(x, ...), collapse = sep)
  }
}
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
