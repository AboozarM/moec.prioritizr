test_that("single", {
  x <- matrix(c(1, 5, 8), ncol = 3)
  y <- matrix(
    c(
      2, 2, 3
    ),
    ncol = 3, byrow = TRUE
  )
  expect_equal(which_hier_best_match(x, y), 1)
})

test_that("no ties", {
  x <- matrix(c(1, 5, 8), ncol = 3)
  y <- matrix(
    c(
      2, 2, 3,
      1, 1, 1,
      5, 5, 8
    ),
    ncol = 3, byrow = TRUE
  )
  expect_equal(which_hier_best_match(x, y), 2)
})

test_that("ties", {
  x <- matrix(c(1, 5, 8), ncol = 3)
  y <- matrix(
    c(
      1, 2, 3,
      1, 5, 9,
      1, 5, 8.5,
      5, 5, 8
    ),
    ncol = 3, byrow = TRUE
  )
  expect_equal(which_hier_best_match(x, y), 3)
})
