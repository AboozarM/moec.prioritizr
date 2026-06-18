# Add epsilon constraint approach

Add an epsilon-constraint approach for multi-objective optimization to a
conservation planning problem.

## Usage

``` r
add_epconstraint_approach(x, n_per_problem, verbose = TRUE)
```

## Arguments

- x:

  [`prioritizr::multi_problem()`](https://prioritizr.net/reference/multi_problem.html)
  object.

- n_per_problem:

  `integer` number of solutions to generate per problem in `x`. Note
  that this does not include solutions that represent optimizing
  exclusively on each objective individually (i.e., extreme points of
  the Pareto frontier). In particular, the total number of solutions is
  equal to \\o + n^(o - 1)\\, where \\o\\ is the number of problems in
  `x` and \\n\\ is equal to `n_per_problem`. For example, if `x` had
  three problems and `n_per_problem = 4`, then 19 solutions would be
  generated.

- verbose:

  `logical` (`TRUE`/`FALSE`) should progress on generating solutions
  displayed? Defaults to `TRUE`.

## Value

An updated
[`prioritizr::multi_problem()`](https://prioritizr.net/reference/multi_problem.html)
object with the approach added to it.

## Details

TODO.

## Mathematical formulation

TODO.

## See also

See
[prioritizr::approaches](https://prioritizr.net/reference/approaches.html)
for other functions for adding an approach.

## Examples

``` r
# in this example, we aim to identify a set of planning units that will
# not exceed a particular budget and meet objectives for
# (i) representing species that are important for ecosystem
# functioning (hereafter, keystone species) and (ii) representing species
# that have high social or cultural value (hereafter, iconic species)

# load packages
library(prioritizr)
library(terra)
#> terra 1.9.27
#> 
#> Attaching package: ‘terra’
#> The following object is masked from ‘package:prioritizr’:
#> 
#>     ncell

# import data
con_cost <- get_sim_pu_raster()
keystone_spp <- get_sim_features()[[1:3]]
iconic_spp <- get_sim_features()[[4:5]]

# define a total conservation budget (20% of total cost)
budget <- terra::global(con_cost, "sum", na.rm = TRUE)[[1]] * 0.2

# define a single-objective problem for the keystone species objective
p1 <-
  problem(con_cost, keystone_spp) %>%
  add_min_shortfall_objective(budget) %>%
  add_relative_targets(0.4) %>%
  add_binary_decisions()

# define a single-objective problem for the iconic species objective
p2 <-
  problem(con_cost, iconic_spp) %>%
  add_min_shortfall_objective(budget) %>%
  add_relative_targets(0.8) %>%
  add_binary_decisions()

# now create multi-objective problem with epsilon-constraint approach,
# with settings to generate (i) a solution for each objective
# and (ii) three additional solutions that represent a varying
# degree of trade-offs between these objectives
mp <-
  multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
  add_epconstraint_approach(n_per_problem = 4, verbose = TRUE) %>%
  add_default_solver(gap = 0, verbose = FALSE)

# solve problem
ms <- solve(mp)
#> Generating solutions ■■■■■■■■■■■■■■■■                 | 3/6 |  50% | ETA: 2s
#> Generating solutions ■■■■■■■■■■■■■■■■■■■■■■■■■■       | 5/6 |  83% | ETA: 1s
#> Generating solutions ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  | 6/6 | 100% | ETA: 0s

# convert solutions to a multi-layer raster object
sol <- rast(ms)

# assign names to each solution
names(sol) <- c(
  "Keystone species", "Iconic species",
  paste("trade-off", seq_len(nlyr(sol) - 2))
)

# plot solutions to view selection of priority areas,
# here, the first two maps show solutions for best achieving
# each of the objectives, and the subsequent maps show solutions
# that aim to reach varying degrees of compromise between the objectives
plot(sol, axes = FALSE)


# extract objective values for the solutions
obj_matrix <- attributes(ms)$objective

# print the objective values
print(obj_matrix)
#>            keystone_obj iconic_obj
#> solution_1     1.559674   1.563225
#> solution_2     1.755484   1.463960
#> solution_3     1.680262   1.483804
#> solution_4     1.633465   1.503609
#> solution_5     1.600177   1.523389
#> solution_6     1.574416   1.543313

# plot the objectives values to visualize trade-offs
# (note that smaller values are better because these objectives seek to
# minimize representation shortfalls)
plot(
  obj_matrix,
  main = "Trade-offs between objectives",
  xlab = "Keystone objective (shortfall)",
  ylab = "Iconic objective (shortfall)"
)
```
