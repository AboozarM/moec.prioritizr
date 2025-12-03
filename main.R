  # import data
  load_all()
  sim_zones_pu_raster <- prioritizr::get_sim_zones_pu_raster()
  sim_features <- prioritizr::get_sim_features()
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
        prioritizr::add_min_shortfall_objective(budget = 600) %>%
        prioritizr::add_absolute_targets(
          rev(seq_along(terra::nlyr(sim_features))) * 5
        ) %>%
        prioritizr::add_binary_decisions(),
      obj3 = prioritizr::problem(sim_zones_pu_raster[[2]], sim_features) %>%
        prioritizr::add_max_utility_objective(budget = 600) %>%
        prioritizr::add_feature_weights(
          runif(terra::nlyr(sim_features)) * 100
        ) %>%
        prioritizr::add_binary_decisions()
    ) %>%
    prioritizr::add_highs_solver(gap = 0, verbose = FALSE)

p1 <-
  p %>% add_epconstraint_approach(n_per_problem = 4, verbose = TRUE)
# p2 <-
#   p %>% add_epconstraint_approach_v1(n_per_problem = 4, verbose = TRUE)
  # solve problem
  s1 <- try(solve(p1))
  # s2 <- try(solve(p2))

  # 
  #
  #
  # print(attr(s, "objective"))
  # plot(attr(s, "objective"))
