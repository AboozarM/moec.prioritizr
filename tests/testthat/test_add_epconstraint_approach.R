test_that("two objectives", {
  # define skips
  skip_on_cran()
  skip_if_not_installed("highs")
  # import data
  pu <- terra::rast(matrix(c(1, 1, 1, 1, 1, 1)))
  ft1 <- terra::rast(matrix(c(5, 0.5, 0, 0, 0, 0)))
  ft2 <- terra::rast(matrix(c(0, 0, 0, 0, 3, 2)))
  # create multi-object problem
  p <-
    prioritizr::multi_problem(
      obj1 =
        prioritizr::problem(pu, ft1) %>%
        prioritizr::add_max_wtd_sum_objective(budget = 2) %>%
        prioritizr::add_binary_decisions(),
      obj2 =
        prioritizr::problem(pu, ft2) %>%
        prioritizr::add_max_wtd_sum_objective(budget = 2) %>%
        prioritizr::add_binary_decisions()
    ) %>%
    add_epconstraint_approach(n = 1, verbose = FALSE) %>%
    prioritizr::add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p, run_checks = FALSE)
  # run tests
  expect_type(s, "list")
  expect_length(s, 3)
  expect_s4_class(s[[1]], "SpatRaster")
  expect_s4_class(s[[2]], "SpatRaster")
  expect_s4_class(s[[3]], "SpatRaster")
  expect_equal(
    c(terra::values(s$solution_1)),
    c(1, 1, 0, 0, 0, 0)
  )
  expect_equal(
    c(terra::values(s$solution_2)),
    c(0, 0, 0, 0, 1, 1)
  )
  expect_equal(
    c(terra::values(s$solution_3)),
    c(1, 0, 0, 0, 1, 0)
  )
  expect_equal(
    attr(s, "objective")[1, ],
    c(obj1 = 5.5, obj2 =0)
  )
  expect_equal(
    attr(s, "objective")[2, ],
    c(obj1 = 0, obj2 = 5)
  )
  expect_equal(
    attr(s, "objective")[3, ],
    c(obj1 = 5, obj2 = 3)
  )
})

test_that("three objectives", {
  # define skips
  skip_on_cran()
  skip_if_not_installed("terra")
  skip_if_not_installed("highs")
  # import data
  pu <- terra::rast(matrix(c(1, 1, 1, 1, 1, 1)))
  ft1 <- terra::rast(matrix(c(5, 0.5, 0, 0, 0, 0)))
  ft2 <- terra::rast(matrix(c(0, 0, 0, 0, 3, 2)))
  ft3 <- terra::rast(matrix(c(0, 0, 1, 10, 0, 0)))
  # create multi-object problem
  p <-
    prioritizr::multi_problem(
      obj1 =
        prioritizr::problem(pu, ft1) %>%
        prioritizr::add_max_wtd_sum_objective(budget = 2) %>%
        prioritizr::add_binary_decisions(),
      obj2 =
        prioritizr::problem(pu, ft2) %>%
        prioritizr::add_max_wtd_sum_objective(budget = 2) %>%
        prioritizr::add_binary_decisions(),
      obj3 =
        prioritizr::problem(pu, ft3) %>%
        prioritizr::add_max_wtd_sum_objective(budget = 2) %>%
        prioritizr::add_binary_decisions()
    ) %>%
    add_epconstraint_approach(n = 1, verbose = FALSE) %>%
    prioritizr::add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p, run_checks = FALSE)
  # run tests
  expect_type(s, "list")
  expect_length(s, 4)
  expect_s4_class(s[[1]], "SpatRaster")
  expect_s4_class(s[[2]], "SpatRaster")
  expect_s4_class(s[[3]], "SpatRaster")
  expect_s4_class(s[[4]], "SpatRaster")
  expect_equal(
    c(terra::values(s$solution_1)),
    c(1, 1, 0, 0, 0, 0)
  )
  expect_equal(
    c(terra::values(s$solution_2)),
    c(0, 0, 0, 0, 1, 1)
  )
  expect_equal(
    c(terra::values(s$solution_3)),
    c(0, 0, 1, 1, 0, 0)
  )
  expect_equal(
    c(terra::values(s$solution_4)),
    c(0, 0, 0, 1, 1, 0)
  )
  expect_equal(
    attr(s, "objective")[1, ],
    c(obj1 = 5.5, obj2 =0, obj3 = 0)
  )
  expect_equal(
    attr(s, "objective")[2, ],
    c(obj1 = 0, obj2 = 5, obj3 = 0)
  )
  expect_equal(
    attr(s, "objective")[3, ],
    c(obj1 = 0, obj2 = 0, obj3 = 11)
  )
  expect_equal(
    attr(s, "objective")[4, ],
    c(obj1 = 0, obj2 = 3, obj3 = 10)
  )
})

test_that("multiple solutions", {
  # define skips
  skip_on_cran()
  skip_if_not_installed("highs")
  # import data
  pu <- terra::rast(matrix(c(1, 1, 1, 1, 1, 1)))
  ft1 <- terra::rast(matrix(c(5, 0.5, 0, 0, 0, 0)))
  ft2 <- terra::rast(matrix(c(0, 0, 0, 0, 3, 2)))
  # create multi-object problem
  p <-
    prioritizr::multi_problem(
      obj1 =
        prioritizr::problem(pu, ft1) %>%
        prioritizr::add_max_wtd_sum_objective(budget = 2) %>%
        prioritizr::add_binary_decisions(),
      obj2 =
        prioritizr::problem(pu, ft2) %>%
        prioritizr::add_max_wtd_sum_objective(budget = 2) %>%
        prioritizr::add_binary_decisions()
    ) %>%
    add_epconstraint_approach(n = 3, verbose = FALSE) %>%
    prioritizr::add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p, run_checks = FALSE)
  # run tests
  expect_type(s, "list")
  expect_length(s, 5)
  expect_s4_class(s[[1]], "SpatRaster")
  expect_s4_class(s[[2]], "SpatRaster")
  expect_s4_class(s[[3]], "SpatRaster")
  expect_s4_class(s[[4]], "SpatRaster")
  expect_s4_class(s[[5]], "SpatRaster")
  expect_equal(
    c(terra::values(s$solution_1)),
    c(1, 1, 0, 0, 0, 0)
  )
  expect_equal(
    c(terra::values(s$solution_2)),
    c(0, 0, 0, 0, 1, 1)
  )
  expect_equal(
    c(terra::values(s$solution_3)),
    c(0, 0, 0, 0, 1, 1)
  )
  expect_equal(
    c(terra::values(s$solution_4)),
    c(1, 0, 0, 0, 1, 0)
  )
  expect_equal(
    c(terra::values(s$solution_5)),
    c(1, 0, 0, 0, 1, 0)
  )
  expect_equal(
    attr(s, "objective")[1, ],
    c(obj1 = 5.5, obj2 =0)
  )
  expect_equal(
    attr(s, "objective")[2, ],
    c(obj1 = 0, obj2 = 5)
  )
  expect_equal(
    attr(s, "objective")[3, ],
    c(obj1 = 0, obj2 = 5)
  )
  expect_equal(
    attr(s, "objective")[4, ],
    c(obj1 = 5, obj2 = 3)
  )
  expect_equal(
    attr(s, "objective")[5, ],
    c(obj1 = 5, obj2 = 3)
  )
})
