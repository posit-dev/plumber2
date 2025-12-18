# Package index

## Create an run an API

- [`api()`](https://plumber2.posit.co/reference/api.md)
  [`is_plumber_api()`](https://plumber2.posit.co/reference/api.md)
  [`api_parse()`](https://plumber2.posit.co/reference/api.md) : Create a
  new plumber API, optionally based on one or more plumber files
- [`api_package()`](https://plumber2.posit.co/reference/api_package.md)
  : Load up an API distributed with a package
- [`api_run()`](https://plumber2.posit.co/reference/api_run.md)
  [`api_stop()`](https://plumber2.posit.co/reference/api_run.md) :
  Launch the API
- [`create_server_yml()`](https://plumber2.posit.co/reference/create_server_yml.md)
  : Create a \_server.yml file to describe your API

## Adding handlers and routes

- [`api_get()`](https://plumber2.posit.co/reference/api_request_handlers.md)
  [`api_head()`](https://plumber2.posit.co/reference/api_request_handlers.md)
  [`api_post()`](https://plumber2.posit.co/reference/api_request_handlers.md)
  [`api_put()`](https://plumber2.posit.co/reference/api_request_handlers.md)
  [`api_delete()`](https://plumber2.posit.co/reference/api_request_handlers.md)
  [`api_connect()`](https://plumber2.posit.co/reference/api_request_handlers.md)
  [`api_options()`](https://plumber2.posit.co/reference/api_request_handlers.md)
  [`api_trace()`](https://plumber2.posit.co/reference/api_request_handlers.md)
  [`api_patch()`](https://plumber2.posit.co/reference/api_request_handlers.md)
  [`api_any()`](https://plumber2.posit.co/reference/api_request_handlers.md)
  : Add a handler for a request
- [`api_get_header()`](https://plumber2.posit.co/reference/api_request_header_handlers.md)
  [`api_head_header()`](https://plumber2.posit.co/reference/api_request_header_handlers.md)
  [`api_post_header()`](https://plumber2.posit.co/reference/api_request_header_handlers.md)
  [`api_put_header()`](https://plumber2.posit.co/reference/api_request_header_handlers.md)
  [`api_delete_header()`](https://plumber2.posit.co/reference/api_request_header_handlers.md)
  [`api_connect_header()`](https://plumber2.posit.co/reference/api_request_header_handlers.md)
  [`api_options_header()`](https://plumber2.posit.co/reference/api_request_header_handlers.md)
  [`api_trace_header()`](https://plumber2.posit.co/reference/api_request_header_handlers.md)
  [`api_patch_header()`](https://plumber2.posit.co/reference/api_request_header_handlers.md)
  [`api_any_header()`](https://plumber2.posit.co/reference/api_request_header_handlers.md)
  : Add a handler for a request header
- [`api_assets()`](https://plumber2.posit.co/reference/api_assets.md)
  [`api_statics()`](https://plumber2.posit.co/reference/api_assets.md) :
  Serve resources from your file system
- [`api_message()`](https://plumber2.posit.co/reference/api_message.md)
  : Add a handler to a WebSocket message
- [`api_add_route()`](https://plumber2.posit.co/reference/api_add_route.md)
  : Add a new route to either the request or header router
- [`api_redirect()`](https://plumber2.posit.co/reference/api_redirect.md)
  : Redirect request to another resource
- [`api_shiny()`](https://plumber2.posit.co/reference/api_shiny.md) :
  Serve a Shiny app from a plumber2 api
- [`api_forward()`](https://plumber2.posit.co/reference/api_forward.md)
  : Set up a plumber2 api to act as a reverse proxy

## Parsers

- [`register_parser()`](https://plumber2.posit.co/reference/register_parser.md)
  [`show_registered_parsers()`](https://plumber2.posit.co/reference/register_parser.md)
  [`get_parsers()`](https://plumber2.posit.co/reference/register_parser.md)
  : Register or fetch a parser
- [`parse_csv()`](https://plumber2.posit.co/reference/parsers.md)
  [`parse_octet()`](https://plumber2.posit.co/reference/parsers.md)
  [`parse_rds()`](https://plumber2.posit.co/reference/parsers.md)
  [`parse_feather()`](https://plumber2.posit.co/reference/parsers.md)
  [`parse_parquet()`](https://plumber2.posit.co/reference/parsers.md)
  [`parse_text()`](https://plumber2.posit.co/reference/parsers.md)
  [`parse_tsv()`](https://plumber2.posit.co/reference/parsers.md)
  [`parse_yaml()`](https://plumber2.posit.co/reference/parsers.md)
  [`parse_geojson()`](https://plumber2.posit.co/reference/parsers.md)
  [`parse_multipart()`](https://plumber2.posit.co/reference/parsers.md)
  : Parser functions provided by plumber2

## Serializers

- [`register_serializer()`](https://plumber2.posit.co/reference/register_serializer.md)
  [`show_registered_serializers()`](https://plumber2.posit.co/reference/register_serializer.md)
  [`get_serializers()`](https://plumber2.posit.co/reference/register_serializer.md)
  : Register or fetch a serializer
- [`format_csv()`](https://plumber2.posit.co/reference/serializers.md)
  [`format_tsv()`](https://plumber2.posit.co/reference/serializers.md)
  [`format_rds()`](https://plumber2.posit.co/reference/serializers.md)
  [`format_geojson()`](https://plumber2.posit.co/reference/serializers.md)
  [`format_feather()`](https://plumber2.posit.co/reference/serializers.md)
  [`format_parquet()`](https://plumber2.posit.co/reference/serializers.md)
  [`format_yaml()`](https://plumber2.posit.co/reference/serializers.md)
  [`format_htmlwidget()`](https://plumber2.posit.co/reference/serializers.md)
  [`format_format()`](https://plumber2.posit.co/reference/serializers.md)
  [`format_print()`](https://plumber2.posit.co/reference/serializers.md)
  [`format_cat()`](https://plumber2.posit.co/reference/serializers.md)
  [`format_unboxed()`](https://plumber2.posit.co/reference/serializers.md)
  [`format_png()`](https://plumber2.posit.co/reference/serializers.md)
  [`format_jpeg()`](https://plumber2.posit.co/reference/serializers.md)
  [`format_tiff()`](https://plumber2.posit.co/reference/serializers.md)
  [`format_svg()`](https://plumber2.posit.co/reference/serializers.md)
  [`format_bmp()`](https://plumber2.posit.co/reference/serializers.md)
  [`format_pdf()`](https://plumber2.posit.co/reference/serializers.md) :
  Serializer functions provided by plumber2

## Async evaluation

- [`register_async()`](https://plumber2.posit.co/reference/register_async.md)
  [`show_registered_async()`](https://plumber2.posit.co/reference/register_async.md)
  [`get_async()`](https://plumber2.posit.co/reference/register_async.md)
  : Register an async evaluator
- [`mirai_async()`](https://plumber2.posit.co/reference/async_evaluators.md)
  : Async evaluators provided by plumber

## Security

- [`api_security_cors()`](https://plumber2.posit.co/reference/api_security_cors.md)
  : Set up CORS for a path in your plumber2 API
- [`api_security_headers()`](https://plumber2.posit.co/reference/api_security_headers.md)
  : Add various security related headers to your plumber2 API
- [`api_security_resource_isolation()`](https://plumber2.posit.co/reference/api_security_resource_isolation.md)
  : Set up resource isolation for a path

## OpenAPI documentation

- [`api_doc_setting()`](https://plumber2.posit.co/reference/api_docs.md)
  [`api_doc_add()`](https://plumber2.posit.co/reference/api_docs.md) :
  Configure your API for serving documentation for itself
- [`openapi()`](https://plumber2.posit.co/reference/openapi.md)
  [`openapi_info()`](https://plumber2.posit.co/reference/openapi.md)
  [`openapi_contact()`](https://plumber2.posit.co/reference/openapi.md)
  [`openapi_license()`](https://plumber2.posit.co/reference/openapi.md)
  [`openapi_path()`](https://plumber2.posit.co/reference/openapi.md)
  [`openapi_operation()`](https://plumber2.posit.co/reference/openapi.md)
  [`openapi_parameter()`](https://plumber2.posit.co/reference/openapi.md)
  [`openapi_header()`](https://plumber2.posit.co/reference/openapi.md)
  [`openapi_schema()`](https://plumber2.posit.co/reference/openapi.md)
  [`openapi_content()`](https://plumber2.posit.co/reference/openapi.md)
  [`openapi_request_body()`](https://plumber2.posit.co/reference/openapi.md)
  [`openapi_response()`](https://plumber2.posit.co/reference/openapi.md)
  [`openapi_tag()`](https://plumber2.posit.co/reference/openapi.md) :
  Construct OpenAPI specifications

## Advanced

- [`Plumber2`](https://plumber2.posit.co/reference/Plumber2.md) : The
  Plumber2 Class
- [`api_logger()`](https://plumber2.posit.co/reference/api_logger.md)
  [`logger_null()`](https://plumber2.posit.co/reference/api_logger.md)
  [`logger_console()`](https://plumber2.posit.co/reference/api_logger.md)
  [`logger_file()`](https://plumber2.posit.co/reference/api_logger.md)
  [`logger_logger()`](https://plumber2.posit.co/reference/api_logger.md)
  [`logger_switch()`](https://plumber2.posit.co/reference/api_logger.md)
  [`common_log_format`](https://plumber2.posit.co/reference/api_logger.md)
  [`combined_log_format`](https://plumber2.posit.co/reference/api_logger.md)
  : Set logging function and access log format for the API
- [`api_on()`](https://plumber2.posit.co/reference/api_on.md)
  [`api_off()`](https://plumber2.posit.co/reference/api_on.md) : Add a
  handler to an event
- [`api_session_cookie()`](https://plumber2.posit.co/reference/api_session_cookie.md)
  : Turn on session cookie data storage for your API
- [`api_datastore()`](https://plumber2.posit.co/reference/api_datastore.md)
  : Persistent server-side data storage
- [`Next`](https://plumber2.posit.co/reference/Next.md)
  [`Break`](https://plumber2.posit.co/reference/Next.md)
  [`should_break()`](https://plumber2.posit.co/reference/Next.md) :
  Router control flow
- [`add_plumber2_tag()`](https://plumber2.posit.co/reference/add_plumber2_tag.md)
  : Add a tag extension to plumber2
- [`apply_plumber2_block()`](https://plumber2.posit.co/reference/apply_plumber2_block.md)
  : Generic for applying information from a plumber2 block to an api
- [`get_opts()`](https://plumber2.posit.co/reference/get_opts.md)
  [`all_opts()`](https://plumber2.posit.co/reference/get_opts.md) :
  Retrieve options for creating a plumber2 api
