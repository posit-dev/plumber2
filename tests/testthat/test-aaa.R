test_that("as_openapi_path modifies the path correctly", {
  expect_equal(as_openapi_path("/test/nothing"), "/test/nothing")
  expect_equal(as_openapi_path("/test/<arg>"), "/test/{arg}")
  expect_equal(
    as_openapi_path("/test/<arg>/and/<arg2>"),
    "/test/{arg}/and/{arg2}"
  )
  expect_equal(
    as_openapi_path("/test/<arg:number>/and/<arg2:[integer]>"),
    "/test/{arg}/and/{arg2}"
  )
})

test_that("as_routr_path modifies the path correctly", {
  expect_equal(as_routr_path("/test/nothing"), "/test/nothing")
  expect_equal(as_routr_path("/test/<arg>"), "/test/:arg")
  expect_equal(
    as_routr_path("/test/<arg>/and/<arg2>"),
    "/test/:arg/and/:arg2"
  )
  expect_equal(
    as_routr_path("/test/<arg:number>/and/<arg2:[integer]>"),
    "/test/:arg/and/:arg2"
  )
})
