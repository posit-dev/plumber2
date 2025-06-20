real_doc <- openapi(
  info = openapi_info(
    title = "This is the title of the api",
    description = "Then comes some descriptive text\n\nMultiple paragraphs are of course supported",
    terms_of_service = "https://example.com",
    contact = openapi_contact(
      name = "Thomas Lin Pedersen",
      url = "https://data-imaginist.com",
      email = "thomas.pedersen@posit.co"
    ),
    license = openapi_license(
      name = "MIT license",
      url = "https://mit-license.org"
    ),
    version = "1.0.0"
  ),
  tags = list(
    openapi_tag(
      name = "single",
      description = "A tag based on a single word"
    ),
    openapi_tag(
      name = "multiple words",
      description = "A tag with space in it"
    )
  )
)

test_that("Global API blocks are parsed correctly", {
  papi <- api("annotations/global_api.R")
  doc <- private(papi, "OPENAPI")

  real_doc <- openapi(
    info = openapi_info(
      title = "This is the title of the api",
      description = "Then comes some descriptive text\n\nMultiple paragraphs are of course supported",
      terms_of_service = "https://example.com",
      contact = openapi_contact(
        name = "Thomas Lin Pedersen",
        url = "https://data-imaginist.com",
        email = "thomas.pedersen@posit.co"
      ),
      license = openapi_license(
        name = "MIT license",
        url = "https://mit-license.org"
      ),
      version = "1.0.0"
    ),
    tags = list(
      openapi_tag(
        name = "single",
        description = "A tag based on a single word"
      ),
      openapi_tag(
        name = "multiple words",
        description = "A tag with space in it"
      )
    )
  )

  expect_equal(doc, unclass(real_doc))
})
