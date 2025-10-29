test_that("two objectives", {
  # define skips
  skip_on_cran(
  skip_if_not_installed("highs")
  # import data
  load_all()
  sim_zones_pu_raster <- prioritizr::get_sim_zones_pu_raster()
  sim_features <- prioritizr::get_sim_features()
  weights <- runif(2)
  # create multi-object problem
  p <-
    prioritizr::multi_problem(
      obj1 = prioritizr::problem(sim_zones_pu_raster[[1]], sim_features) %>%
        prioritizr::add_min_set_objective() %>%
        prioritizr::add_absolute_targets(
          seq_along(terra::nlyr(sim_features))
        ) %>%
        prioritizr::add_binary_decisions(),
      obj2 = prioritizr::problem(sim_zones_pu_raster[[2]], sim_features) %>%
        prioritizr::add_min_set_objective() %>%
        prioritizr::add_absolute_targets(
          rev(seq_along(terra::nlyr(sim_features)))
        ) %>%
        prioritizr::add_binary_decisions()
    ) %>%
    add_epconstraint_approach(n = 3, verbose = FALSE) %>%
    prioritizr::add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # run tests
  expect_s3_class(s, "list")
  expect_length(s, 3)
  expect_s4_class(s[[1]], "SpatRaster")
  expect_s4_class(s[[2]], "SpatRaster")
  expect_equal(
    c(terra::values(s$solution_1)),
    c(
      0, 0, 0, 0, 0, 0, NaN, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, NaN, 0,
      0, 0, 0, NaN, NaN, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, NaN, 0, 0, NaN, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      NaN, 0, NaN, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NaN, 0, 0,
      1, NaN, 0, 0
    )
  )
  expect_equal(
    c(terra::values(s$solution_2)),
    c(
      1, 0, 0, 0, 1, 0, NaN, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, NaN, 0,
      0, 0, 0, NaN, NaN, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, NaN, 0, 0, NaN, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      NaN, 0, NaN, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NaN, 0, 0,
      0, NaN, 0, 0
    )
  )
})

test_that("three objectives", {
  # TODO
})
