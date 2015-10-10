context("ref")

test_that("ref", {
  r1 <- ref(1:10)
  expect_is(r1, "ref")
  expect_true(is.environment(r1))
})

test_that("deref", {
  before <- 1:10
  r1 <- ref(before)
  after <- deref(r1)
  expect_identical(before, after)
})
