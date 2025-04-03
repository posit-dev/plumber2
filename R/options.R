get_opts <- function(x, default = NULL) {
  getOption(paste0("plumber2.", x), default = {
    env_name <- toupper(paste0("PLUMBER2_", x))
    res <- Sys.getenv(env_name)
    if (res == "") {
      res <- default
    } else {
      if (is.atomic(default)) {
        mode(res) <- mode(default)
      }
    }
    res
  })
}

all_opts <- function() {
  compact(list(
    host = get_opts("host"),
    port = get_opts("port"),
    docType = get_opts("docType"),
    docPath = get_opts("docPath"),
    rejectMissingMethods = get_opts("rejectMissingMethods"),
    ignoreTrailingSlash = get_opts("ignoreTrailingSlash"),
    maxRequestSize = get_opts("maxRequestSize"),
    compressionLimit = get_opts("compressionLimit"),
    async = get_opts("async")
  ))
}
