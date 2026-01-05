# Extending plumber2

plumber2 was from the inception built as a modular system meant for
extension. In this article we’ll go over the process of creating a
plumber2 extension as well as talk a bit about the architecture of
plumber2 and how it relates to extending it.

### What does it mean to extend plumber2

Before we continue we should qualify what we mean by extending plumber2.
It of course involves adding new functionality to plumber2 in some way,
but more specifically it entails adding functionality in such a way that
it *feels* native to plumber2. Consider this function:

``` r
say_hi <- function(language, api) {
  hi <- c(
    "english" = "hi",
    "danish" = "hej",
    "french" = "salut",
    "spanish" = "hola",
    "japanese" = "konnichiwa"
  )[tolower(language)]
  api$on("start", function(...) cat(hi, "\n"))
} 
```

While the above does extend plumber2’ API, it does so in a very
non-native way. First, the function name is not prefixed with `api_`,
and second, the function is not pipe-friendly. However, most importantly
for a plumber2 function it does not have an associated tag that can be
used to add this functionality in annotation. While certain
functionality isn’t accessible through tags, you should try your hardest
to allow it as this is how most users will create APIs.

There is another type of extension that we will discuss in the end as
well, which is to add new parsers, serializers, or async engines. All of
these revolves around registering new functions that can then be
accessed with the relevant tags.

## Adding new tags

Central to plumber2’ api is the annotation tags. Thankfully, tags are
not exclusive to the functionality provided in the plumber2 package but
can be provided by other packages that somehow extends what plumber2 can
do.

Adding a new tags is relatively simple but it does require a bit of
knowledge about how plumber2 parses a file. For a single annotation
block, the process is roughly like this:

1.  The annotation is parsed and split into it’s constituents. Each tag
    will at this point be made up of the tag name and an optional text
    string that contains anything that followed the tag. At this point,
    the expression that follows the annotation block will also be parsed
    and evaluated.
2.  plumber2 will determine which, if any, of the basic block types this
    is, e.g. a handler block, a shiny block, or something else
3.  If plumber2 recognizes the block type it will create an S3 object
    with a name following the `plumber2_*_block` class name structure.
    The object will contain all information necessary to apply the
    annotation to a Plumber2 object.
4.  After this, plumber2 will go over all tags in the block and see if
    one or more handlers have been registered for the tag and call the
    handlers one by one. The handlers have the option to modify the
    block object, either by adding to or modifying its elements (it is
    not allowed to delete them). Further, it may choose to subclass the
    object (but not remove a class).
5.  Once the annotation file has been converted into a list of objects
    they are one by one applied to the Plumber2 object by calling the
    [`apply_plumber2_block()`](https://plumber2.posit.co/reference/apply_plumber2_block.md)
    generic on the block and passing in the block object created in 3
    and 4 along with the Plumber2 object.

It follows from the above that there are two places you should be
concerned about when it comes to extending plumber2 tags: *tag
registration* and *apply_plumber2_block* methods. Depending on your need
you’ll have to add one of them or potentially both. We’ll go through
both below:

### Tag registration

When adding a new tag, or, if you want to add functionality to an
already existing tag, you’ll have to add a new handler to the tag. This
is done with the
[`add_plumber2_tag()`](https://plumber2.posit.co/reference/add_plumber2_tag.md)
function which takes the tag name and a handler function. In general,
you *can* add a handler to an already existing tag (a tag can have
multiple handlers and they will each be called in turn), but you should
exert caution when doing this to not mess with the original behavior of
the tag. Because adding a handler for a new tag that you control, this
is what we will focus on.

An additional aspect to think about is whether the tag is meant to fit
into an already existing block type (e.g. a standard handler block) or
be the basis of a new block type. Below we will show both.

#### New block type

In this example we want to create a new type of annotation block that
sets up a simple recurring message from the server. We will call the tag
`tictoc` and allow it to take a single additional parameter which is the
interval in seconds between the message. Further, we will allow the user
to add the actual message as a string after the annotation block.

``` r
add_plumber2_tag("tictoc", function(block, call, tags, values, env) {
  if (!inherits(block, "plumber2_empty_block")) {
    cli::cli_abort(
      "{.field @tictoc} cannot be used with other types of annotation blocks"
    )
  }
  if (!rlang::is_string(call)) {
    cli::cli_abort(
      "The expression following a {.field tictoc} block must be a string"
    )
  }

  interval <- as.integer(values[[which(tags == "tictoc")[1]]])
  if (is.na(interval)) interval <- 5
  message <- call

  type <- "message"
  if (any(tags == "logType") && values[[which(tags == "logType")[1]]] != "") {
    type <- values[[which(tags == "logType")[1]]]
  }

  structure(
    list(
      interval = interval,
      message = message,
      type = type
    ),
    class = "plumber2_tictoc_block"
  )
})
add_plumber2_tag("logType")
```

Above we define two new tags, `tictoc` and `logType`. Only `tictoc` has
an associated handler and that handler uses both tags. The `logType` tag
is still registered but only to ensure that plumber2/roxygen2 knows of
its existence.

In the handler function above we can see the two different modes of
input for an annotation block: the `tags`/`values` arguments and the
`call` argument. `tags` contains a character vector with all the tags in
the block, and `values` is a list of all the additional arguments
supplied to the tag. The values are provided as single, unprocessed,
strings which means that if a tag does not have additional arguments
then values will be an empty string. Further, the string may contain one
or more new-line characters in the end etc. It is up to the provider of
the tag (you) to make sense of any argument the tag takes by parsing the
associated value however you chose. The `call` argument contains
whatever comes after the annotation block, parsed and evaluated.

In the handler above we first check to see if `block` is a
`plumber2_empty_block` object. This is not strictly necessary but if we
are implementing a new block type we should check that it is not being
mixed with other block types. Apart from this, the handler is mainly
parsing the relevant tags and collecting them in a new object that it
gives the class `plumber2_tictoc_block`.

#### Existing block type

You may want to augment an already existing block type with a new tag.
The process of doing so is similar to the above, but there are other
considerations to take into account for the handler function. As an
example we will look at the `@cors` tag which is implemented as an
extension inside plumber2 itself:

``` r
add_plumber2_tag("cors", function(block, call, tags, values, env) {
  class(block) <- c("plumber2_cors_block", class(block))
  block$cors <- trimws(strsplit(values[[which(tags == "cors")[1]]], ",")[[1]])
  if (block$cors == "") {
    block$cors <- "*"
  }
  block
})
```

In the handler above, because we are adding to an existing block type,
we do not create a new block object but rather modifies the one passed
in. In this case we add a new class to it (not replace it, which is
disallowed), and add a new element to it as well. We could check the
block class in case we had an exact requirement on which block type to
use it with, but since `@cors` can be used together with any block type
that defines an endpoint we don’t bother here.

### `apply_plumber2_block` methods

Both examples above doesn’t really do anything to the Plumber2 object,
rather, they gather information from the annotations for later use. This
“later use” comes in the form of `apply_plumber2_block` methods for the
block object classes created (in the examples above,
`plumber2_tictoc_block` and `plumber2_cors_block`). The method must take
the following arguments:

- `block`, which is the object we constructed above during tag parsing
- `api`, which is the Plumber2 object being constructed
- `route_name`, which is the name of the route being created in the
  given file
- `root`, which is the root of the all endpoints in the file
- `...`, which is currently always ignored but should be there for
  future use

Of these, `block` and `api` are always relevant whereas the others can
be ignored unless you add endpoints or in some other way interact with
the route that the annotation file defines. As above there are a few
different things to keep in mind depending on whether you subclass an
existing block or not.

#### Single main class

For an example of a method for a block object with only a single class
we return to our tictoc block above. The
[`apply_plumber2_block()`](https://plumber2.posit.co/reference/apply_plumber2_block.md)
method could look like this:

``` r
apply_plumber2_block.plumber2_tictoc_block <- function(
  block, 
  api, 
  route_name, 
  root, 
  ...
) {
  api$time(
    api$log(event = block$type, message = block$message),
    after = block$interval,
    loop = TRUE
  )
  api
}
```

As you can see, there is little magic in what goes on in the
`apply_plumber2_block` method. You take the information gathered from
the tags and do whatever you need to do to the Plumber2 object to put it
into effect. Here we use the
[`time()`](https://rdrr.io/r/stats/time.html) method from the Plumber2
object to add a recurring timed logging expression.

The method above returns the Plumber2 object. This is strictly not
necessary due to the reference semantics of the object but it is good to
do for clarity. Especially given that *if* a Plumber2 object is
returned, even a different one than passed in, it will take over from
the original object. It is of course important that you don’t pull the
carpet from under the feet of the user and do something completely
unpredictable. However, it does mean that you can cast the Plumber2
object to a subclass, should your extension require that.

#### Subclass

For an example of an
[`apply_plumber2_block()`](https://plumber2.posit.co/reference/apply_plumber2_block.md)
method for a block object that has been subclassed, we look again at how
the `@cors` tag has been implemented:

``` r
apply_plumber2_block.plumber2_cors_block <- function(
  block,
  api,
  route_name,
  root,
  ...
) {
  NextMethod()
  for (i in seq_along(block$endpoints)) {
    for (path in block$endpoints[[i]]$path) {
      api <- api_security_cors(
        api,
        paste0(root, path),
        block$cors,
        methods = block$endpoints[[i]]$method
      )
    }
  }
  api
}
```

The most important takeaway from the above is that it starts off by
calling [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) thus
ensuring that the parent methods have been called first. Another thing
to note is that the method is using the programmatic api under the hood
to support the annotation functionality (the use of
[`api_security_cors()`](https://plumber2.posit.co/reference/api_security_cors.md)).
This is a good design that ensures the annotation and programmatic
interface stays aligned. We didn’t do it for the `tictoc` tag above
because we had not yet created a programmatic interface but we will get
to that in a second.

## Adding a programmatic interface

While the annotations are what makes plumber2 stand out, you should
ensure that whatever functionality your extension provides should also
be available to users who build their api programmatically. There does
not need to be a one-to-one correspondence between annotations and
functions but keep it as closely aligned as possible. To ensure the
programmatic interface feels native you need to make it pipe-friendly,
meaning that it should take a Plumber2 object as the first argument
(generally named `api`) and return it back modified in the end. Apart
from this the only other ask is that you prefix the function name with
`api_` to align it with the rest of the interface. For the `tictoc`
example we created above it would look something like:

``` r
api_tictoc <- function(api, interval, message, type = "message") {
  api$time(
    api$log(event = type, message = message),
    after = interval,
    loop = TRUE
  )
  api
}
```

## New parsers, serializers, and async engines

A simple way to expand on what plumber2 can do is to provide new parsers
and serializers for it, or even a new async backend (though the one
provided is for all intend and purposes the best choice). We will go
through how to approach all of these below

### Serializers

Serializers are responsible for converting the return value of your
functions into the format requested by the client. plumber2 supports
content negotiation so that the client can ask for a specific format (or
a priority of different formats) and plumber2 will serve it to the best
of its abilities. If you do not provide any additional details about
which serializers to support, plumber2 will use a set of default
serializers to cover the most common use cases:

``` r
names(get_serializers())
```

    #> [1] "application/json"          "text/html"
    #> [3] "application/rds"           "text/csv"
    #> [5] "text/tab-separated-values" "text/xml"
    #> [7] "text/plain"                "text/yaml"

There are serializers besides the defaults that are registered, usually
for niche situations. These can be accessed by name, either in
annotation or in the programmatic interface:

``` r
get_serializers("geojson")
```

    #> $`application/geo+json`
    #> function (x)
    #> {
    #>     if (inherits(x, "sfc")) {
    #>         return(geojsonsf::sfc_geojson(x, ...))
    #>     }
    #>     if (inherits(x, "sf")) {
    #>         return(geojsonsf::sf_geojson(x, ...))
    #>     }
    #>     cli::cli_abort("{.fun format_geojson} did not receive an `sf` or `sfc` object.")
    #> }
    #> <bytecode: 0x561cff483370>
    #> <environment: 0x561cfcacd478>

While you can always pass in a function directly as a serializer,
registering them by name makes it much easier to reuse, and making it a
default makes the serializer instantly available to all endpoints
(providing they are using the defaults). It follows that providing
additional serializers, both defaults and non-defaults, increases ways
plumber2 can interact with the client.

Creating and registering a new serializer is not involved and we will go
through the process below by creating a toml serializer based on the
[tomledit](https://extendr.rs/tomledit/) package.

The first thing to do is to create the serializer function:

``` r
format_toml <- function() {
  rlang::check_installed("tomledit")
  function(x) {
    tomledit::to_toml(x)
  }
}
```

As we can see, we create a function that returns the actual serializer
(which is a unary function taking the data to serialize). We do this for
two reasons: First, it allows us to run one-time code only once and not
every time something needs to be serialized and (not shown here), it
allows us to pass in arguments that modify the behavior of the
serializer, e.g. like for the rds serializer:

``` r
print(format_rds)
```

    #> function (version = "3", ascii = FALSE, ...)
    #> {
    #>     function(x) {
    #>         serialize(x, NULL, ascii = ascii, version = version,
    #>             ...)
    #>     }
    #> }
    #> <bytecode: 0x561cff450110>
    #> <environment: namespace:plumber2>

With our serializer in hand we can now proceed to registering it. For
that we need a name, the serializer itself, as well as the mime type of
the output it produces. The default is to register a serializer as
default and since this makes sense here we do that. The latter can
sometimes be a bit of a guessing game since mime types are an evolving
thing, especially for new formats. If one has been registered with
[IANA](https://www.iana.org/assignments/media-types/media-types.xhtml)
you should use that, but otherwise try to see what the developer of the
format or the community around it suggests. For toml, the
`application/toml` mime type has been registered at IANA so we’ll use
that.

``` r
register_serializer("toml", format_toml, "application/toml")
```

We can convince ourself that it works by trying to fetch it by name:

``` r
get_serializers("toml")
```

    #> $`application/toml`
    #> function (x)
    #> {
    #>     tomledit::to_toml(x)
    #> }
    #> <environment: 0x561cfe2a7a50>

We can also see that it is part of the defaults:

``` r
names(get_serializers())
```

    #> [1] "application/json"          "text/html"
    #> [3] "application/rds"           "text/csv"
    #> [5] "text/tab-separated-values" "text/xml"
    #> [7] "text/plain"                "text/yaml"
    #> [9] "application/toml"

#### Graphics output

There is a special category of output that requires their own breed of
serializers, namely graphics output. These produces image files through
the side-effect of rendering and the serializer needs to know to capture
that. Graphics serializers are not part of the default serializers and
you have to specifically request them.

``` r
names(get_serializers(c("png", "...")))
```

    #> [1] "image/png"       "image/jpeg"      "image/tiff"      "image/svg+xml"
    #> [5] "image/bmp"       "application/pdf"

In the above I request the graphics serializer “png” explicitly and then
use the `...` to ask for the remainder of the same type.

Since the act of capturing graphics output has been formalized in R
through the graphics device interface it is relatively straightforward
to add a new graphics serializer using a helper function from plumber2.
Below we create a PostScript serializer based on the
[`postscript()`](https://rdrr.io/r/grDevices/postscript.html) device:

``` r
format_postscript <- device_formatter(postscript)
```

This can now be registered in the same way as the toml serializer above.
plumber2 can see that the serializer is a graphics serializer, so there
is no special thing you need to do differently.

``` r
register_serializer(
  "postscript", 
  format_postscript, 
  "application/postscript",
  default = FALSE
)
```

### Parsers

Parsers are like serializers but for the other direction of
communication, that is, for converting the body of a request into an R
object. The process is very similar as the above and we will showcase it
by creating a toml parser as well.

Like with serializers there is also a concept of default parsers, and
there is the
[`get_parsers()`](https://plumber2.posit.co/reference/register_parser.md)
function to retrieve parsers from the registry.

``` r
parse_toml <- function() {
  rlang::check_installed("tomledit")
  function(x, directives) {
    tomledit::read_toml(rawToChar(x))
  }
}
```

This looks a lot like our toml serializer. It is a function returning a
function and we use the outer function for time-consuming one-off code
to ensure the inner function is as lean as possible. One difference is
that the inner function takes two arguments: `x` (the body of the
request as a raw vector), and `directives`. The last argument is for
additional parameters passed from the client as part of the
`Content-Type` header. You can often ignore this last argument but for
some parsers it is important.

Registering the parser is done like the serializer. The only difference
is that you can register a parser to multiple mime types rather than a
single one, so that it can respond to all of them no matter what the
client decides to call it. In general it is a good idea to include any
kind of mime type, official and otherwise, you can find since you are
not in control of what the client decides to use.

``` r
register_parser(
  "toml", 
  parse_toml, 
  c("application/toml", "text/x-toml", "text/toml")
)
```

### Asynchronous engines

plumber2 comes with build in support for asynchronous evaluation based
on mirai. We strongly believe this is the best choice for async eval in
R, but you may still use another engine if you se desire by registering
a new one. Below, we show how to create an async evaluator based on the
`future` package

``` r
future_async <- function(...) {
  rlang::check_installed("promises")
  function(expr, envir) {
    promises::future_promise(
      expr = expr,
      envir = envir,
      substitute = FALSE,
      ...
    )
  }
}
```

As with parsers and serializers we start be creating an evaluator
function which, again, is a function returning a function. The inner
function takes two arguments: an expression to evaluate, and an
environment that holds all the variables to use in the expression. The
inner function must return a promise (based on the
[promises](https://rstudio.github.io/promises/) package).

Once the evaluator is in place it can be registered to a name:

``` r
register_async("future", future_async, c("promises", "future"))
```

The last argument holds the package dependencies for the engine.

You can now use this engine, either with annotation (`@async future`) or
programmatically with `get_async("future")`.

## The Plumber2 object and its extension points

TBD
