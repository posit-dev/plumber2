# \_server.yml for framework developers

What good is a server framework if it cannot be easily deployed? After
all, surely you hope that your users won’t run the amazing servers
they’ve created with your tool on the machines they developed them on?

Ignoring the complications around getting all the files, secrets,
environment variables and libraries transferred to the machine that will
eventually run the server, one big question remains: How does the
machine know how to launch the server? How does it even know that the
files constitute a server implementation?

The R language has had comparably fewer server frameworks than other
languages for various reasons but this is slowly changing. This low
number has meant that it has generally been a viable strategy to let the
server just “know about” the various options rather than try to gather
around a common standard for describing these things. But, the time is
ripe and due to the lack of any standard at all, we won’t fall into the
trap of [yet another standard](https://xkcd.com/927/) when we now
propose just that: the \_server.yml standard for describing R servers.

## The standard

Recognizing that it is practically impossible to define a comprehensive
standard that covers all possible settings for all current and future
server frameworks in R, we propose a very “thin” standard that put the
onus on the server frameworks themselves to give it meaningful
structure.

### The \_server.yml file

The core of the standard is the `_server.yml` file. This file should be
placed at the root of your server specification and will
(unsurprisingly) contain a YAML formatted definition of your server
setup. The only required field is `engine` which should appear as the
first line. This element names the package responsible for launching the
server and should be installable by the server. A `_server.yml` file for
a plumber2 api will have:

``` yaml
engine: plumber2
```

as the first line.

The file can (and probably should) have additional fields, but it is up
to the package named in `engine` to interpret these and the standard
does not name *any* additional requirements to the file.

### The engine

For a package to be a valid engine in a `_server.yml` file it must
adhere to this one requirement: It should have a function,
`launch_server(settings, host = NULL, port = NULL, ...)`, that takes the
path to a `_server.yml` file as the first argument, the ip address to
launch the server on as the second, and the port to serve it on as the
third. The function needs not be exported. The `launch_server()`
function is responsible for making sense of the provided `_server.yml`
file and error if it is malformed according to the expectations the
engine sets. The function should launch a server in a blocking fashion,
i.e. it should not return control to R for as long as the server runs.

### The server

For a server to support serving `_server.yml`-defined R servers, it
should check for the existence of such a file at the root of the
deployment. It should then read the engine field and install the named
package. After that it should use the `launch_server()` function to
launch the server. A simple example of this logic is provided below with
no error handling or anything.

``` r
if (file.exist("_server.yml")) {
  # Figure out the engine for the server
  # You could probably easily do this without the yaml package
  engine <- yaml::read_yaml("_server.yml")$engine

  # Install the engine if not already available
  if (!requireNamespace(engine, quietly = TRUE)) {
    install.packages(engine)
  }
  
  # Find the launch_server() function in the engine
  launch_server <- get(
    "launch_server", 
    envir = asNamespace(engine), 
    mode = "function"
  )

  # Use the function to launch the server
  launch_server(
    settings = "_server.yml", 
    host = HOST_ADDR, # Variable holding the ip address
    port = PORT_NO    # Variable holding the port number
  )
} else {
  # Logic for other types of server specifications
}
```

## Recommendations for \_server.yml engines

While the content besides the `engine` field is completely at the
discretion of the engine developer the following are some
recommendations:

### File paths

It is likely that you’d want your `_server.yml` file to point to one or
more files. Make sure that all file and folder paths are interpreted as
relative to the location of the `_server.yml` path rather than to the
working directory of the R session.

### Options

You should aim for a separation of server logic (usually one or more R
files), and server settings. To that end, you should provide a way for
the user to record settings in the `_server.yml` file so that they do
not need to modify implementation files in order to change run time
behavior.

## Case study: plumber2

plumber2 adheres to the `_server.yml` standard and in the following we
will go through how it does so as an example for other engine developers
who wish to support it.

### YAML content

plumber2 currently defines 3 fields besides the `engine` field:

- `routes`: An array of R files or folders that contain the server logic
  as annotated functions and objects.
- `constructor`: A path to a single R file that constructs the Plumber2
  object.
- `options`: A list of options to use for the server. These all
  corresponds to options that can be seen in `?plumber2::all_opts()`.

plumber2 provide a function
([`create_server_yml()`](https://plumber2.posit.co/reference/create_server_yml.md))
that creates this file for you so that the user can be confident that it
is formatted correctly. This is generally a good idea and also provides
a place to document the content of the file.

### `launch_server()`

The `launch_server()` function is quite simple for plumber2 since the
[`api()`](https://plumber2.posit.co/reference/api.md) constructor itself
understands `_server.yml` files as input. Because of this the function
looks like this:

``` r
plumber2:::launch_server
```

    #> function (settings, host = NULL, port = NULL, ...)
    #> {
    #>     pa <- api(settings)
    #>     if (!is.null(host)) {
    #>         pa$host <- host
    #>     }
    #>     if (!is.null(port)) {
    #>         pa$port <- port
    #>     }
    #>     api_run(pa)
    #> }
    #> <bytecode: 0x56040a0ac2a0>
    #> <environment: namespace:plumber2>

## Supported frameworks

Below is a list with R webserver frameworks known to support the
`_server.yml` standard. If you have developed a framework with support
and wish for it to appear here, please create a pull request with the
addition:

- [plumber2](https://plumber2.posit.co)
- [fiery](https://fiery.data-imaginist.com)
