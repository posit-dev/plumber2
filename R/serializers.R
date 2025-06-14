registry$serializers <- list()

#' Register or fetch a serializer
#'
#' plumber2 comes with many serializers that should cover almost all standard
#' use cases. Still you might want to provide some of your own, which this
#' function facilitates.
#'
#' If you want to register your own serializer, then the function you register
#' must be a factory function, i.e. a function returning a function. The returned
#' function must accept a single argument which is the response body. All
#' arguments to the factory function should be optional.
#'
#' @param name The name to register the serializer function to. If already
#' present the current serializer will be overwritten by the one provided by you
#' @param fun A function that, when called, returns a unary function that can
#' serialize a response body to the mime type defined in `mime_type`
#' @param mime_type The format this serializer creates. You should take care to
#' ensure that the value provided is a standard mime type for the format
#' @param default Should this serializer be part of the default set of
#' serializers
#'
#' @return For `get_serializers` a named list of serializer functions named by
#' their mime type. The order given in `serializers` is preserved.
#'
#' @seealso [serializers]
#' @seealso [register_serializer()]
#'
#' @export
#'
register_serializer <- function(name, fun, mime_type, default = TRUE) {
  check_function(fun)
  check_string(mime_type)
  check_string(name)
  check_bool(default)
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
  registry$serializers[[name]] <- list(
    fun = fun,
    type = mime_type,
    default = default
  )
  invisible(NULL)
}
#' @rdname register_serializer
#' @export
show_registered_serializers <- function() {
  res <- data.frame(
    name = names(registry$serializers),
    mime_type = vapply(registry$serializers, `[[`, character(1), "type"),
    graphic = vapply(
      registry$serializers,
      function(x) is_device_constructor(x$fun),
      logical(1)
    ),
    default = vapply(registry$serializers, `[[`, logical(1), "default")
  )
  attr(res, "row.names") <- .set_row_names(nrow(res))
  res
}

#' @rdname register_serializer
#' @param serializers Serializers to collect. This can either be a character
#' vector of names of registered serializers or a list. If it is a list then the
#' following expectations apply:
#' * Any unnamed elements containing a character vector will be considered as
#'   names of registered serializers constructed with default values. The
#'   special value `"..."` will fetch all the serializers that are otherwise not
#'   specified in the call.
#' * Any element containing a function are considered as a provided serializer
#'   and the element must be named by the mime type the serializer understands
#' * Any remaining named elements will be considered names of registered
#'   serializers that should be constructed with the arguments given in the
#'   element
#'
#' @note Using the `...` will provide remaining graphics serializers if a
#' graphics serializer is explicitely requested elsewhere. Otherwise it will
#' provide the remaining non-graphics serializers. A warning is thrown if a mix
#' of graphics and non-graphics serializers are requested.
#'
#' @export
get_serializers <- function(serializers = NULL) {
  defaults <- vapply(registry$serializers, `[[`, logical(1), "default")
  if (is.null(serializers)) {
    serializers <- names(registry$serializers)[defaults]
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
  dots_serializers <- setdiff(
    names(registry$serializers)[defaults],
    named_serializers
  )
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
      return(get_serializers_internal(
        serializers[[i]],
        env = env,
        dots_serializers = dots_serializers,
        prune_dots = FALSE
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
  from_dots <- unlist(lapply(
    serializers,
    function(x) attr(x, "from_dots") %||% rep_along(x, FALSE)
  ))
  serializers <- unlist(serializers, recursive = FALSE)
  is_graphics <- vapply(serializers, is_device_formatter, logical(1))
  use_graphics <- if (all(from_dots)) FALSE else is_graphics[!from_dots][1]
  keep <- !from_dots | is_graphics == use_graphics
  serializers <- serializers[keep]
  is_graphics <- is_graphics[keep]
  if (!(all(is_graphics) || all(!is_graphics))) {
    cli::cli_warn(
      "Serializers are a mix of standard and graphics serializers"
    )
  }
  serializers
}

get_serializers_internal <- function(
  types = NULL,
  env = caller_env(),
  dots_serializers = NULL,
  prune_dots = TRUE
) {
  if (isTRUE(tolower(types) == "none")) {
    return(NULL)
  }
  if (is.null(types)) {
    types <- "..."
  }
  dots <- which(types == "...")
  from_dots <- rep_along(types, FALSE)
  if (length(dots) != 0) {
    defaults <- vapply(registry$serializers, `[[`, logical(1), "default")
    if (length(dots) > 1) {
      cli::cli_abort("{.val ...} can only be used once")
    }
    dnames <- dots_serializers %||%
      setdiff(names(registry$serializers)[defaults], types)
    from_dots <- rep(
      c(FALSE, TRUE, FALSE),
      c(dots - 1, length(dnames), length(types) - dots)
    )
    types <- c(
      types[seq_len(dots - 1)],
      dnames,
      types[dots + seq_len(length(types) - dots)]
    )
  }
  serializers <- lapply(types, function(type) {
    type <- stringi::stri_split_regex(type, "\\{|\\s", n = 2)[[1]]
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
          parse_expr(paste0("list(", sub("\\}$", "", type[[2]]), ")")),
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
  is_graphics <- vapply(serializers, is_device_formatter, logical(1))
  if (prune_dots) {
    use_graphics <- if (all(from_dots)) FALSE else is_graphics[!from_dots][1]
    keep <- !from_dots | is_graphics == use_graphics
    serializers <- serializers[keep]
    is_graphics <- is_graphics[keep]
    if (!(all(is_graphics) || all(!is_graphics))) {
      cli::cli_warn(
        "Serializers are a mix of standard and graphics serializers"
      )
    }
  }
  structure(
    serializers[!duplicated(names(serializers))],
    from_dots = from_dots[!duplicated(names(serializers))]
  )
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
#' * `format_geojson()` uses `geojsonsf::sfc_geojson()` or `geojsonsf::sf_geojson()`
#'   for formatting depending on the class of the response body. It is
#'   registered as `"geojson"` to the mime type `application/geo+json`
#' * `format_feather()` uses `arrow::write_feather()` for formatting. It is
#'   registered as `"feather"` to the mime type
#'   `application/vnd.apache.arrow.file`
#' * `format_parquet()` uses `nanoparquet::write_parquet()` for formatting. It is
#'   registered as `"parquet"` to the mime type `application/vnd.apache.parquet`
#' * `format_yaml()` uses [yaml::as.yaml()] for formatting. It is registered
#'   as `"yaml"` to the mime type `text/yaml`
#' * `format_htmlwidget()` uses `htmlwidgets::saveWidget()` for formatting. It is
#'   registered as `"htmlwidget"` to the mime type `text/html`
#' * `format_format()` uses [format()] for formatting. It is registered
#'   as `"format"` to the mime type `text/plain`
#' * `format_print()` uses [print()] for formatting. It is registered
#'   as `"print"` to the mime type `text/plain`
#' * `format_cat()` uses [cat()] for formatting. It is registered
#'   as `"cat"` to the mime type `text/plain`
#' * `format_unboxed()` uses [reqres::format_json()] with `auto_unbox = TRUE` for
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
#' # Provided graphics serializers
#' Serializing graphic output is special because it requires operations before
#' and after the handler is executed. Further, handlers creating graphics are
#' expected to do so through side-effects (i.e. call to graphics rendering) or
#' by returning a ggplot2 object. If you want to create your own graphics
#' serializer you should use [device_formatter()] for constructing it.
#' * `format_png()` uses [ragg::agg_png()] for rendering. It is registered
#'   as `"png"` to the mime type `image/png`
#' * `format_jpeg()` uses [ragg::agg_jpeg()] for rendering. It is registered
#'   as `"jpeg"` to the mime type `image/jpeg`
#' * `format_tiff()` uses [ragg::agg_tiff()] for rendering. It is registered
#'   as `"tiff"` to the mime type `image/tiff`
#' * `format_svg()` uses [svglite::svglite()] for rendering. It is registered
#'   as `"svg"` to the mime type `image/svg+xml`
#' * `format_bmp()` uses [grDevices::bmp()] for rendering. It is registered
#'   as `"bmp"` to the mime type `image/bmp`
#' * `format_pdf()` uses [grDevices::pdf()] for rendering. It is registered
#'   as `"pdf"` to the mime type `application/pdf`
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
  function(x) {
    readr::format_csv(x, ...)
  }
}
#' @rdname serializers
#' @export
#'
format_tsv <- function(...) {
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
  register_serializer("rds", format_rds, "application/rds")
  register_serializer("csv", format_csv, "text/csv")
  register_serializer("tsv", format_tsv, "text/tab-separated-values")
  register_serializer("xml", reqres::format_xml, "text/xml")
  register_serializer("text", reqres::format_plain, "text/plain")
  register_serializer("format", format_format, "text/plain")
  register_serializer("print", format_print, "text/plain")
  register_serializer("cat", format_cat, "text/plain")
  register_serializer("yaml", format_yaml, "text/yaml")
  register_serializer(
    "feather",
    format_feather,
    "application/vnd.apache.arrow.file",
    default = FALSE
  )
  register_serializer(
    "parquet",
    format_parquet,
    "application/vnd.apache.parquet",
    default = FALSE
  )
  register_serializer(
    "htmlwidget",
    format_htmlwidget,
    "text/html",
    default = FALSE
  )
  register_serializer(
    "geojson",
    format_geojson,
    "application/geo+json",
    default = FALSE
  )
})

# Device serializers -----------------------------------------------------------

#' Create a graphics device formatter
#'
#' This internal function facilitates creating a formatter that uses a specific
#' device for rendering.
#'
#' @param dev_open The function that opens the device
#' @param dev_close The function closing the device. Usually this would be
#' [grDevices::dev.off()]
#'
#' @return A device formatter function
#' @keywords internal
#' @export
#'
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
  structure(
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
      with_dev <- function(x, info) {
        promises::with_promise_domain(
          create_graphics_device_promise_domain(info$dev),
          x
        )
      }
      structure(
        identity,
        init = init_dev,
        close = close_dev,
        clean = clean_dev,
        with = with_dev,
        class = "device_formatter"
      )
    },
    class = "device_constructor"
  )
}
is_device_formatter <- function(x) inherits(x, "device_formatter")
is_device_constructor <- function(x) inherits(x, "device_constructor")

#' Formatter orchestration
#'
#' These functions are for internal use and only exported to ease async
#' evaluation
#'
#' @param formatter A serializer function
#' @param info A structure returned by `init_formatter()`
#' @param expr An expression to evaluate in the context of the formatter
#'
#' @return `init_formatter()` returns a opaque structure capturing information
#' used by the other functions. `close_formatter()` may return a value that
#' should be used as response body. `with_formatter()` returns the result of
#' `expr`. `clean_formatter()` is called for it's side effects and should only
#' be called if `close_formatter()` never evaluated.
#'
#' @export
#' @keywords internal
init_formatter <- function(formatter) {
  init_fun <- attr(formatter, "init")
  if (is.null(init_fun)) {
    return(NULL)
  }
  init_fun()
}
#' @rdname init_formatter
#' @export
close_formatter <- function(formatter, info) {
  close_fun <- attr(formatter, "close")
  if (is.null(info) || is.null(close_fun)) {
    return(NULL)
  }
  close_fun(info)
}
#' @rdname init_formatter
#' @export
clean_formatter <- function(formatter, info) {
  clean_fun <- attr(formatter, "clean")
  if (is.null(info) || is.null(clean_fun)) {
    return(NULL)
  }
  clean_fun(info)
}
#' @rdname init_formatter
#' @export
with_formatter <- function(expr, formatter, info) {
  with_fun <- attr(formatter, "with")
  if (is.null(info) || is.null(with_fun)) {
    return(expr)
  }
  with_fun(expr, info)
}

create_graphics_device_promise_domain <- function(
  which = grDevices::dev.cur()
) {
  force(which)

  promises::new_promise_domain(
    wrapOnFulfilled = function(onFulfilled) {
      force(onFulfilled)
      function(...) {
        old <- grDevices::dev.cur()
        grDevices::dev.set(which)
        on.exit(grDevices::dev.set(old))

        onFulfilled(...)
      }
    },
    wrapOnRejected = function(onRejected) {
      force(onRejected)
      function(...) {
        old <- grDevices::dev.cur()
        grDevices::dev.set(which)
        on.exit(grDevices::dev.set(old))

        onRejected(...)
      }
    },
    wrapSync = function(expr) {
      old <- grDevices::dev.cur()
      grDevices::dev.set(which)
      on.exit(grDevices::dev.set(old))

      force(expr)
    }
  )
}

#' @rdname serializers
#' @export
#' @importFrom ragg agg_png
#'
format_png <- device_formatter(agg_png)
#' @rdname serializers
#' @export
#' @importFrom ragg agg_jpeg
#'
format_jpeg <- device_formatter(agg_jpeg)
#' @rdname serializers
#' @export
#' @importFrom ragg agg_tiff
#'
format_tiff <- device_formatter(agg_tiff)
#' @rdname serializers
#' @export
#' @importFrom svglite svglite
#'
format_svg <- device_formatter(svglite)
#' @rdname serializers
#' @export
#'
format_bmp <- device_formatter(grDevices::bmp)
#' @rdname serializers
#' @export
#'
format_pdf <- device_formatter(grDevices::pdf)

on_load({
  register_serializer("png", format_png, "image/png")
  register_serializer("jpeg", format_jpeg, "image/jpeg")
  register_serializer("tiff", format_tiff, "image/tiff")
  register_serializer("svg", format_svg, "image/svg+xml")
  register_serializer("bmp", format_bmp, "image/bmp")
  register_serializer("pdf", format_pdf, "application/pdf")
})
