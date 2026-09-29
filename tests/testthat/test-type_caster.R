test_that("array_caster casts arrays of objects without infinite recursion", {
  schema <- list(
    type = "array",
    items = list(
      type = "object",
      properties = list(
        name = list(type = "string"),
        email = list(type = "string")
      )
    )
  )

  caster <- array_caster(schema, required = TRUE, name = "items", loc = "body", scalar = FALSE)

  val <- list(list(name = "foo", email = "foo@bar.com"))
  expect_equal(caster(val), val)
})

test_that("array_caster casts arrays of arrays without infinite recursion", {
  schema <- list(
    type = "array",
    items = list(
      type = "array",
      items = list(type = "string")
    )
  )

  caster <- array_caster(schema, required = TRUE, name = "items", loc = "body", scalar = FALSE)

  val <- list(list("a", "b"), list("c"))
  expect_equal(caster(val), list(c("a", "b"), "c"))
})

test_that("array_caster casts arrays of scalars", {
  schema <- list(type = "array", items = list(type = "string"))

  caster <- array_caster(schema, required = TRUE, name = "items", loc = "body", scalar = FALSE)

  expect_equal(caster(list("a", "b", "c")), c("a", "b", "c"))
})
