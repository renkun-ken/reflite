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

test_that("arithmetics", {
  x <- rnorm(100)
  y <- rnorm(100)
  r1 <- ref(x)
  expect_identical(r1 + y, x + y)
  expect_identical(y + r1, y + x)
  expect_identical(r1 - y, x - y)
  expect_identical(y - r1, y - x)
  expect_identical(-r1, -x)
  expect_identical(r1 * y, x * y)
  expect_identical(y * r1, y * x)
  expect_identical(r1 / (1 + abs(y)), x / (1 + abs(y)))
  expect_identical(y / (1 + r1 * r1), y / (1 + x * x))
  expect_identical(r1 ^ 2, x ^ 2)

  x <- rbinom(10, 3, 0.5)
  y <- 1L + rbinom(5, 5, 0.8)
  r2 <- ref(x)
  expect_identical(r2 %% y, x %% y)
})

test_that("subsetting", {
  x <- rnorm(100)
  y <- sample(seq_along(x), 10, replace = FALSE)
  z <- sample(seq_along(x), 1)
  r1 <- ref(x)
  expect_identical(r1[y], x[y])
  expect_identical(r1[[z]], x[[z]])
  expect_identical(r1[], x)
  
  x <- list(a = rnorm(10), b = rbinom(10, 5, 0.3))
  r1 <- ref(x)
  expect_identical(r1["a"], x["a"])
  expect_identical(r1[["a"]], x[["a"]])
  expect_identical(r1$a, x$a)
  expect_identical(r1[], x)
})

test_that("sub-assignment", {
  x <- rnorm(100)
  y <- sample(seq_along(x), 10, replace = FALSE)
  z <- sample(seq_along(x), 1)
  
  expect_identical({
    r1 <- ref(x)
    r1[y] <- 0
    deref(r1)
  }, local({
    x[y] <- 0
    x
  }))
  expect_identical({
    r1 <- ref(x)
    r1[[z]] <- 0
    deref(r1)
  }, local({
    x[[z]] <- 0
    x
  }))
  
  f1 <- function(ref_x) {
    ref_x[1] <- 0
    NULL
  }
  
  expect_identical({
    r1 <- ref(c(1, 2, 3))
    f1(r1)
    deref(r1)
  }, c(0, 2, 3))
  
  expect_identical({
    r1 <- ref(c(1, 2, 3))
    local(r1[1] <- 0)
    deref(r1)
  }, c(0, 2, 3))
  
  x <- list(a = rnorm(10), b = rbinom(10, 5, 0.3))
  
  f2 <- function(ref_list) {
    ref_list[["a"]] <- 0
    NULL
  }
  
  f3 <- function(ref_list) {
    ref_list$a <- 0
    NULL
  }
  
  expect_identical({
    r1 <- ref(x)
    f2(r1)
    r1["a"]
  }, list(a = 0))
  
  expect_identical({
    r1 <- ref(x)
    f2(r1)
    r1[["a"]]
  }, 0)
  
  expect_identical({
    r1 <- ref(x)
    f3(r1)
    r1$a
  }, 0)
})

test_that("methods", {
  x <- c(a = 1, b = 2, c = 3)
  r1 <- ref(x)
  expect_identical(length(r1), length(x))
  expect_identical(names(r1), names(x))
  expect_identical(head(r1), head(x))
  expect_identical(tail(r1), tail(x))
  expect_identical(as.integer(r1), as.integer(x))
  
  x <- head(mtcars)
  r1 <- ref(x)
  expect_identical(row.names(r1), row.names(x))
  
  expect_identical(with(r1, mpg), with(x, mpg))
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
