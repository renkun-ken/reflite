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

test_that("constraint", {
  r1 <- ref(1:10, is.integer)
  expect_identical(constraint(r1), is.integer)
  r1[1] <- 0L
  expect_identical(deref(r1), c(0L, 2:10))
  expect_error(r1[2] <- "a")
  expect_error(update(r1, "abc"))
  
  r2 <- ref(rnorm(100), function(x) is.numeric(x) && length(x) >= 100)
  new_data <- rnorm(200)
  expect_identical({
    update(r2, new_data)
    deref(r2)
  }, new_data)
  expect_error(r2[1] <- "a")
  expect_error(update(r2, rnorm(50)))
})
