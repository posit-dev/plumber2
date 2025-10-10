# plumber2 (development version)

* Fix a bug in constructing empty blocks for extending
* You can now pass in additional arguments to the OpenAPI doc UI with
  `api_doc_setting()`
* More powerful report support. Added `api_report()` as a parallel to `@report``
  but with even more settings. Parameters are now type checked using the same
  facilities as the request handlers, and much more comprehensive OpenAPI
  documentation is provided.
* Fixed a bug in the documentation UI when serving the api through a proxy,
  affecting e.g. Posit Workbench users
* Default styling of docs ui now reflect plumber2 aesthetics
* Support for instrumentation with otel, along with a vignette describing said
  support.
* Support for authentication through the fireproof package. Authentication can
  be setup programmatically with `api_auth_guard()` and
  `api_auth()` as well as directly in all functions creating
  endpoints. Further it can be added with annotation using the `@auth`,
  `@authScope` and `@authenticator` tags
* Added `@datastore` tag to set up persistent data storage natively in annotated
  files
* `api_get()` and other endpoint functions now defaults to using the default
  serializers and parsers (#57)

# plumber2 0.1.0

* Initial CRAN submission.
