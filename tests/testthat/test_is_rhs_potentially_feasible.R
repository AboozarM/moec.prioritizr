test_that("zero infeasible solution", {
  expect_true(
    is_rhs_potentially_feasible(
      rhs = matrix(c(5, 10), ncol = 2),
      sense = c("<=", ">="),
      infeasible = matrix(ncol = 2, nrow = 0)
    )
  )
})

test_that("single infeasible solution", {
  # run tests
  ## is feasible
  expect_true(
    is_rhs_potentially_feasible(
      rhs = matrix(c(5, 10), ncol = 2),
      sense = c("<=", ">="),
      infeasible = matrix(c(3, 12), ncol = 2)
    )
  )
  ## is not feasible
  expect_false(
    is_rhs_potentially_feasible(
      rhs = matrix(c(5, 10), ncol = 2),
      sense = c("<=", ">="),
      infeasible = matrix(c(7, 9), ncol = 2)
    )
  )
})

test_that("multiple infeasible solutions", {
  # run tests
  ## is feasible
  expect_true(
    is_rhs_potentially_feasible(
      rhs = matrix(c(5, 10, 800), ncol = 3),
      sense = c("<=", ">=", "<="),
      infeasible = matrix(
        c(
          3, 12, 10,
          4, 11, 20,
          1, 15, 100
        ),
        ncol = 3, byrow = TRUE
      )
    )
  )
  ## is not feasible
  expect_false(
    is_rhs_potentially_feasible(
      rhs = matrix(c(5, 10, 800), ncol = 3),
      sense = c("<=", ">=", "<="),
      infeasible = matrix(
        c(
          3, 12, 10,
          7, 8, 900,
          1, 15, 100
        ),
        ncol = 3, byrow = TRUE
      )
    )
  )
})
