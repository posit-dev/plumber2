options_plumber2 <- function(
  ...,
  port                 = getOption("plumber.port"),
  docs                 = getOption("plumber.docs"),
  docs.callback        = getOption("plumber.docs.callback"),
  trailingSlash        = getOption("plumber.trailingSlash"),
  methodNotAllowed     = getOption("plumber.methodNotAllowed"),
  apiURL               = getOption("plumber.apiURL"),
  apiScheme            = getOption("plumber.apiScheme"),
  apiHost              = getOption("plumber.apiHost"),
  apiPort              = getOption("plumber.apiPort"),
  apiPath              = getOption("plumber.apiPath"),
  maxRequestSize       = getOption("plumber.maxRequestSize"),
  sharedSecret         = getOption("plumber.sharedSecret")
) {
  rlang::check_dots_empty()

  # Make sure all fallback options are disabled
  if (!missing(docs.callback) && is.null(docs.callback)) {
    options("plumber.swagger.url" = NULL)
  }

  options(
    plumber.port                 =   port,
    plumber.docs                 =   docs,
    plumber.docs.callback        =   docs.callback,
    plumber.trailingSlash        =   trailingSlash,
    plumber.methodNotAllowed     =   methodNotAllowed,
    plumber.apiURL               =   apiURL,
    plumber.apiScheme            =   apiScheme,
    plumber.apiHost              =   apiHost,
    plumber.apiPort              =   apiPort,
    plumber.apiPath              =   apiPath,
    plumber.maxRequestSize       =   maxRequestSize,
    plumber.sharedSecret         =   sharedSecret,
    plumber.legacyRedirects      =   legacyRedirects
  )
}

get_opts <- function(x, default = NULL, prefix = c("plumber2", "plumber")) {
  getOption(paste0(prefix[1], ".", x), default = {
    env_name <- toupper(paste0(prefix[1], "_", x))
    res <- Sys.getenv(env_name)
    if (res == "") {
      if (length(prefix) == 1) {
        res <- default
      } else {
        res <- get_opts(x, default = default, prefix = prefix[-1])
      }
    } else {
      mode(res) <- mode(default)
    }
    res
  })
}
