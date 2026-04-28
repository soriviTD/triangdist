library(testthat)

# Parameters
min_val <- 0
max_val <- 10
mode_val <- 2

test_that("Arguments validation", {
  expect_error(dtriang(5, min = 10, max = 0, mode = 5))
  expect_error(ptriang(5, min = 0, max = 10, mode = 11))
  expect_error(rtriang(5, min = 0, max = 10, mode = -1))

  #Probability out of range
  expect_error(qtriang(1.5, min = 0, max = 10, mode = 2))
  expect_error(qtriang(-0.1, min = 0, max = 10, mode = 2))

})

test_that("dtriang", {
  #Case x == mode
  expect_equal(dtriang(2, 0, 10, 2), 2 / (10 - 0))

  #Case x out of limits
  expect_equal(dtriang(-1, 0, 10, 2), 0)
  expect_equal(dtriang(11, 0, 10, 2), 0)

  #Case x < mode
  expect_true(dtriang(1, 0, 10, 2) > 0)

  #Case x > mode
  expect_true(dtriang(5, 0, 10, 2) > 0)
})

test_that("ptriang", {
  #Case q <= min
  expect_equal(ptriang(0, 0, 10, 2), 0)

  #Case q >= max
  expect_equal(ptriang(10, 0, 10, 2), 1)

  #Case q == mode
  p_at_mode <- ptriang(2, 0, 10, 2)
  expect_true(p_at_mode > 0 && p_at_mode < 1)

  #Case q < mode
  expect_true(ptriang(1, 0, 10, 2) < p_at_mode)

  #Case q > mode
  expect_true(ptriang(5, 0, 10, 2) > p_at_mode)
})

test_that("qtriang", {
  p_m <- ptriang(mode_val, min_val, max_val, mode_val)

  #Case p == p_mode
  expect_equal(qtriang(p_m, min_val, max_val, mode_val), mode_val)

  #Case p < p_mode
  val_bajo <- qtriang(0.1, min_val, max_val, mode_val)
  expect_true(val_bajo < mode_val)

  #Case p > p_mode
  val_alto <- qtriang(0.9, min_val, max_val, mode_val)
  expect_true(val_alto > mode_val)
})

test_that("rtriang", {
  n <- 50
  values <- rtriang(n, min_val, max_val, mode_val)

  expect_length(values, n)
  expect_true(all(values >= min_val & values <= max_val))
})
