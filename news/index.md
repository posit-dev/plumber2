# Changelog

## plumber2 (development version)

- Fix a bug in constructing empty blocks for extending
- You can now pass in additional arguments to the OpenAPI doc UI with
  [`api_doc_setting()`](https://plumber2.posit.co/reference/api_docs.md)
- More powerful report support. Added
  [`api_report()`](https://plumber2.posit.co/reference/api_report.md) as
  a parallel to `@report` but with even more settings. Parameters are
  now type checked using the same facilities as the request handlers,
  and much more comprehensive OpenAPI documentation is provided.
- Fixed a bug in the documentation UI when serving the api through a
  proxy, affecting e.g. Posit Workbench users
- Default styling of docs ui now reflect plumber2 aesthetics
- Support for instrumentation with otel, along with a vignette
  describing said support.
- Support for authentication through the fireproof package.
  Authentication can be setup programmatically with
  [`api_auth_guard()`](https://plumber2.posit.co/reference/api_auth_guard.md)
  and [`api_auth()`](https://plumber2.posit.co/reference/api_auth.md) as
  well as directly in all functions creating endpoints. Further it can
  be added with annotation using the `@auth`, `@authScope` and
  `@authenticator` tags
- Added `@datastore` tag to set up persistent data storage natively in
  annotated files
- [`api_get()`](https://plumber2.posit.co/reference/api_request_handlers.md)
  and other endpoint functions now defaults to using the default
  serializers and parsers
  ([\#57](https://github.com/posit-dev/plumber2/issues/57))
- Fixed a bug when setting serializers to `NULL`
  ([\#73](https://github.com/posit-dev/plumber2/issues/73))

## plumber2 0.1.1

CRAN release: 2025-12-18

- Updated doc path generation in preparation for routr 2.0.0

## plumber2 0.1.0

CRAN release: 2025-09-22

- Initial CRAN submission.
