# Hosting

Once you have developed your plumber2 API, the next step is to find a
way to host it. If you haven’t dealt with hosting an application on a
server before, you may be tempted to run the
[`api_run()`](https://plumber2.posit.co/reference/api_run.md) function
from an interactive session on your development machine (either your
personal desktop or an RStudio Server instance) and direct traffic
there. This is a dangerous idea for a number of reasons:

1.  Your development machine likely has a dynamic IP address. This means
    that clients may be able to reach you at that address today, but it
    will likely break on you in the coming weeks/months.
2.  Networks may leverage firewalls to block incoming traffic to certain
    networks and machines. Again, it may appear that everything is
    working for you locally, but other users elsewhere in the network or
    external clients may not be able to connect to your development
    machine.
3.  If your plumber2 process crashes (for instance, due to your server
    running out of memory), the method of running plumber2 will not
    automatically restart the crashed service for you. This means that
    your API will be offline until you manually login and restart it.
    Likewise if your development machine gets rebooted, your API will
    not automatically be started when the machine comes back online.
4.  This technique relies on having your clients specify a port number
    manually. Non-technical users may be tripped up by this; some of the
    other techniques do not require clients specifying the port for an
    API.
5.  This approach will eternally run one R process for your API. Some of
    the other approaches will allow you to load-balance traffic between
    multiple R processes to handle more requests. [Posit
    Connect](#posit-connect) will even dynamically scale the number of
    running processes for you so that your API isn’t consuming more
    system resources than is necessary.
6.  Most importantly, serving public requests from your development
    environment can be a security hazard. Ideally, you should separate
    your development instances from the servers that are accessible by
    others.

For these reasons and more, you should consider setting up a separate
server on which you can host your plumber2 APIs. There are a variety of
options that you can consider.

## The `_server.yml` file

Since plumber2 API specifications can be spread out over multiple files
we need a single file that is the source of truth for what the API is
based on. plumber2 uses the `_server.yml` specification for this and you
can create a scaffold of such a file using
[`create_server_yml()`](https://plumber2.posit.co/reference/create_server_yml.md).

The `_server.yml` file not only contains the R files that make up your
API, but can also holds options that modify how the API is constructed
(see [`get_opts()`](https://plumber2.posit.co/reference/get_opts.md)).

Once you have a `_server.yml` file you can verify that it works as it
should by passing it in to
[`api()`](https://plumber2.posit.co/reference/api.md) and testing out
the constructed API:

``` r
# Create _server.yml in the working directory
create_server_yml(...) # Various settings to capture your server

# Test that it works
api("_server.yml") |> 
  api_run()
```

## Posit Connect

[Posit Connect](https://posit.co/products/enterprise/connect/) is an
enterprise publishing platform from Posit. It supports push-button
publishing from the RStudio IDE of a variety of R content types
including plumber2 APIs. Unlike all the other options listed here, Posit
Connect automatically manages the dependent packages and files your API
has and recreates an environment closely mimicking your local
development environment on the server.

Posit Connect automatically manages the number of R processes necessary
to handle the current load and balances incoming traffic across all
available processes. It can also shut down idle processes when they’re
not in use. This allows you to run the appropriate number of R processes
to scale your capacity to accommodate the current load.
