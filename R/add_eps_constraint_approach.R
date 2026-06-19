#' @include import-standalone-assertions_class.R import-standalone-assertions_handlers.R import-standalone-assertions_functions.R import-standalone-assertions_misc.R import-standalone-cli.R
NULL

#' Add epsilon constraint approach
#'
#' Add an epsilon-constraint approach for multi-objective optimization to a
#' conservation planning problem (Eichfelder 2008).
#'
#' @param x [prioritizr::multi_problem()] object.
#'
#' @param n_per_problem `integer` number of solutions to attempt to generate per
#' problem in `x`. Note that this number does not include solutions that
#' represent optimizing exclusively on each objective individually (i.e.,
#' extreme points of the Pareto frontier). In particular, the total number of
#' solutions that this approach attempts to generate is is equal to
#' \eqn{o + n^{(o - 1)}}{o + n^(o - 1)}, where
#' \eqn{o} is the number of problems in `x` and \eqn{n} is equal to
#' `n_per_problem`. For example, if `x` had three problems and
#' `n_per_problem = 4`, then this approach will attempt to generate 19
#' solutions. Due to the mathematical details of this approach (see below), it
#' is not guaranteed to generate a solution for each attempt and will
#' often generate fewer solutions than it attempts.
#'
#' @param verbose `logical` (`TRUE`/`FALSE`) should progress on generating
#' solutions displayed? Defaults to `TRUE`.
#'
#' @details
#' This multi-objective optimization approach is especially useful for
#' characterizing the full range of trade-offs between objectives when
#' there is a well-defined order of importance among the objectives in a
#' planning exercise.
#' In general, we recommend using this approach for characterizing
#' the full range of trade-offs between objectives because it has procedures
#' that automatically account for the best and worst possible levels for
#' achievement when optimizing each of the different objectives.
#' Due to these procedures, this approach does not require the user
#' to specify trade-off parameters (e.g., unlike `rel_tol` for
#' [prioritizr::add_hier_approach()]) and instead only requires
#' the user to specify a parameter that reflects the maximum number of
#' desired solutions. Furthermore, this approach is not sensitive to
#' differences in scale among different objectives (unlike the weighted sum
#' approach, [prioritizr::add_wtd_sum_approach()];
#' see Das and Dennis 1997 for details), and so it
#' can be readily applied to a wide range of objectives.
#'
#' @section Mathematical formulation:
#' This approach can be expressed mathematically for a set of
#' objectives associated with the [problem()] objects in `x`.
#' Let \eqn{O}{O} denote the set of objectives (indexed by \eqn{o}{o}).
#' For brevity, we will assume that all of the objectives should ideally be
#' maximized and have been sorted in order of
#' priority (per `priority`), such that the objective with the highest priority
#' is \eqn{o=1}{o=1}, objective with the second highest priority is
#' \eqn{o=2}{o=2}, and so on.
#' Although this approach can be applied to an arbitrary number
#' of objectives, we will assume that \eqn{O}{O} has three objectives when
#' explaining this approach.
#' Also, let \eqn{f_o(x)}{fo(x)} denote the objective function for each
#' objective \eqn{o \in O}{o in O}, where \eqn{x} represents all the decision
#' variables for calculating the objective values (e.g., planning unit selection
#' status values).
#' Additionally, let \eqn{S}{S} represent the set (region) of feasible
#' values for \eqn{x} based on the constraints for all of the objectives
#' (e.g., if the first problem in `x` has locked in constraints and the
#' second problem has locked out constraints, then \eqn{S}{S} would
#' account for both the locked in and locked out constraints).
#' Given this terminology, the approach starts by formulating a set of
#' multi-objective optimization problems to generate a solution
#' that focuses primarily on each objective whilst accounting for the
#' the other objectives too (hereafter, extreme points).
#' Note that these multi-objective optimization problems are solved
#' using the hierarchical approach to optimization with relative tolerance
#' values set to zero (see [prioritizr::add_hier_approach()]) for details).
#'
#' Multi-objective problem for first extreme point:
#' \deqn{
#' \mathit{Maximize} \space f_1(x), f_2(x), f_3(x) \\
#' \mathit{subject \space to \space} x \in S
#' }{
#' Maximize f1(x), f2(x), f3(x), subject to x in S
#' }
#'
#' Multi-objective problem for second extreme point:
#' \deqn{
#' \mathit{Maximize} \space f_2(x), f_1(x), f_3(x) \\
#' \mathit{subject \space to \space} x \in S
#' }{
#' Maximize f2(x), f1(x), f3(x), subject to x in S
#' }
#'
#' Multi-objective problem for third extreme point:
#' \deqn{
#' \mathit{Maximize} \space f_3(x), f_1(x), f_2(x) \\
#' \mathit{subject \space to \space} x \in S
#' }{
#' Maximize f3(x), f1(x), f2(x), subject to x in S
#' }
#'
#' After solving these problems to generate solutions,
#' let \eqn{b_o}{bo} denote the best
#' objective value for each objective. Also let \eqn{w_o}{wo} denote the
#' worst value for each objective. For each objective -- except for
#' the first objective -- we then generate
#' a sequence of (evenly distributed) numbers ranging from the worst objective
#' value (per \eqn{w_o}{wo}) to the best objective value (per \eqn{b_o}{bo}),
#' based on a predefined number of values (per `n_per_problem`).
#' For example, if the second objective had \eqn{w_o = 1}{wo = 1} and
#' \eqn{b_o = 5}{bo = 5} and `n_per_problem = 3`, then
#' the sequence of numbers for this objective would be equal to
#' \eqn{\{2, 3, 4\}}.
#' Similarly, if the third objective had \eqn{w_o = 5}{wo = 5} and
#' \eqn{b_o = 15}{bo = 15} and `n_per_problem = 3`, then
#' the sequence of numbers for this objective would be equal to
#' \eqn{\{7.5, 10.0, 12.5\}}.
#'
#' We then use these sequences of numbers to generate a set of numbers
#' that represents all possible combinations of numbers for each objective.
#' For example, if we considered the previous sequences of
#' (second objective) \eqn{\{2, 3, 4\}} and (third objective)
#' \eqn{\{7.5, 10.0, 12.5\}}, then we would generate the following
#' set of nine combinations of numbers:
#' \eqn{\{2, 7.5\}}, \eqn{\{3, 7.5\}}, \eqn{\{4, 7.5\}}, \eqn{\{2, 10\}},
#' \eqn{\{3, 10\}}, \eqn{\{4, 10\}}, \eqn{\{2, 12.5\}}, \eqn{\{3, 12.5\}},
#' \eqn{\{4, 12.5\}}.
#' For brevity, we let \eqn{v_{io}}{vio} denote the number for the
#' objective \eqn{o} from the i'th sequence (e.g,. the number for the
#' third objective in the first combination would be
#' \eqn{v_{12} = 7.5}{v12 = 7.5}).
#' Given this, the approach involves formulating an optimization problem
#' based on each combination of numbers and solving it.
#'
#' Problem based on first combination:
#' \deqn{
#' \mathit{Maximize} \space f_1(x) \\
#' \mathit{subject \space to \space} x \in S \\
#' f_2(x) >= v_{11}, \\
#' f_3(x) >= v_{12}, \\
#' }{
#' Maximize f1(x) subject to x in S, f2(x) >= v11, f3(x) >= v12
#' }
#'
#' Problem based on second combination:
#' \deqn{
#' \mathit{Maximize} \space f_1(x) \\
#' \mathit{subject \space to \space} x \in S \\
#' f_2(x) >= v_{21}, \\
#' f_3(x) >= v_{22}, \\
#' }{
#' Maximize f1(x) subject to x in S, f2(x) >= v21, f3(x) >= v22
#' }
#'
#' Problem based on third combination:
#' \deqn{
#' \mathit{Maximize} \space f_1(x) \\
#' \mathit{subject \space to \space} x \in S \\
#' f_2(x) >= v_{31}, \\
#' f_3(x) >= v_{32}, \\
#' }{
#' Maximize f1(x) subject to x in S, f2(x) >= v31, f3(x) >= v32
#' }

#' Problem based on i'th combination:
#' \deqn{
#' \mathit{Maximize} \space f_1(x) \\
#' \mathit{subject \space to \space} x \in S \\
#' f_2(x) >= v_{i1}, \\
#' f_3(x) >= v_{i2}, \\
#' }{
#' Maximize f1(x) subject to x in S, f2(x) >= vi1, f3(x) >= vi2
#' }
#'
#' In this manner, the approach sequentially formulated and solves optimization
#' problems until it has attempted to generate a solution for each
#' combination of numbers. The resulting set of solutions represent varying
#' degrees of compromise between different objectives.
#' Note that because it may be impossible to generate
#' a solution for some combinations of numbers, this approach may not
#' actually generate a solution each and every combination.
#' Finally, the approach then returns all of the solutions
#' that were successfully generated (including the extreme points).
#'
#' @return
#' An updated [prioritizr::multi_problem()] object with the approach
#' added to it.
#'
#' @seealso
#' See [prioritizr::approaches] for other functions for adding an approach.
#'
#' @references
#' Das I and Dennis JE (1997) A closer look at drawbacks of minimizing weighted
#' sums of objectives for Pareto set generation in multicriteria optimization
#' problems. _Structural Optimization_, 14: 63--69.
#'
#' Eichfelder G (2008) _Adaptive Scalarization Methods in Multiobjective
#' Optimization_. Springer Berlin Heidelberg.
#'
#' @examplesIf prioritizr::do_run_example()
#' # in this example, we aim to identify a set of planning units that will
#' # not exceed a particular budget and meet objectives for
#' # (i) representing species that are important for ecosystem
#' # functioning (hereafter, keystone species) and (ii) representing species
#' # that have high social or cultural value (hereafter, iconic species)
#'
#' # load packages
#' library(prioritizr)
#' library(terra)
#'
#' # import data
#' con_cost <- get_sim_pu_raster()
#' keystone_spp <- get_sim_features()[[1:3]]
#' iconic_spp <- get_sim_features()[[4:5]]
#'
#' # define a total conservation budget (20% of total cost)
#' budget <- terra::global(con_cost, "sum", na.rm = TRUE)[[1]] * 0.2
#'
#' # define a single-objective problem for the keystone species objective
#' p1 <-
#'   problem(con_cost, keystone_spp) %>%
#'   add_min_shortfall_objective(budget) %>%
#'   add_relative_targets(0.4) %>%
#'   add_binary_decisions()
#'
#' # define a single-objective problem for the iconic species objective
#' p2 <-
#'   problem(con_cost, iconic_spp) %>%
#'   add_min_shortfall_objective(budget) %>%
#'   add_relative_targets(0.8) %>%
#'   add_binary_decisions()
#'
#' # now create multi-objective problem with epsilon-constraint approach,
#' # with settings to generate (i) a solution for each objective
#' # and (ii) three additional solutions that represent a varying
#' # degree of trade-offs between these objectives
#' mp <-
#'   multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
#'   add_eps_constraint_approach(n_per_problem = 4, verbose = TRUE) %>%
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # solve problem
#' ms <- solve(mp)
#'
#' # convert solutions to a multi-layer raster object
#' sol <- rast(ms)
#'
#' # assign names to each solution
#' names(sol) <- c(
#'   "Keystone species", "Iconic species",
#'   paste("trade-off", seq_len(nlyr(sol) - 2))
#' )
#'
#' # plot solutions to view selection of priority areas,
#' # here, the first two maps show solutions for best achieving
#' # each of the objectives, and the subsequent maps show solutions
#' # that aim to reach varying degrees of compromise between the objectives
#' plot(sol, axes = FALSE)
#'
#' # extract objective values for the solutions
#' obj_matrix <- attributes(ms)$objective
#'
#' # print the objective values
#' print(obj_matrix)
#'
#' # plot the objectives values to visualize trade-offs
#' # (note that smaller values are better because these objectives seek to
#' # minimize representation shortfalls)
#' plot(
#'   obj_matrix,
#'   main = "Trade-offs between objectives",
#'   xlab = "Keystone objective (shortfall)",
#'   ylab = "Iconic objective (shortfall)"
#' )
#'
#' @export
add_eps_constraint_approach <- function(x, n_per_problem, verbose = TRUE) {
  # assert arguments are valid,
  assert_required(x)
  assert_required(n_per_problem)
  assert_required(verbose)
  assert(
    is_multi_conservation_problem(x),
    assertthat::is.count(n_per_problem),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )

  # add approach
  x$add_approach(
    R6::R6Class(
      "EpsilonConstraintApproach",
      inherit = prioritizr::MultiObjApproach,
      public = list(
        name = "epsilon constraint approach",
        data = list(
          n_per_problem = n_per_problem,
          verbose = verbose
        ),
        run = function(x, solver) {
          ## initialization
          n_per_problem <- self$get_data("n_per_problem")
          verbose <- self$get_data("verbose")

          ## preliminary calculations
          n_obj <- nrow(x$obj)
          n_constraints <- length(x$opt$rhs())
          n <- n_obj + (n_per_problem^(n_obj - 1))
          sols <- vector(mode = "list", length = n)
          x_obj <- x$obj
          x_modelsense <- x$modelsense

          ## if needed, set up progress bar
          if (isTRUE(verbose)) {
            pb <- cli::cli_progress_bar(
              format = cli_progress_bar_format("Generating solutions"),
              total = n,
              .envir = parent.frame()
            )
          }

          ## generate solutions that are extreme points
          for (i in seq_len(n_obj)) {
            ### generate solution with i'th objective as most important
            sols[[i]] <- solver$solve_multiobj(
              x,
              priority = replace(seq(n_obj, 1L), i, n_obj + 1L),
              rel_tol = rep(0, n_obj - 1L)
            )
            ### verify feasibility
            assert(
              is_valid_raw_solution(sols[[i]], multiple = FALSE),
              call = rlang::expr(solve())
            )
            ### if needed, update progress bar
            if (isTRUE(verbose)) {
              cli::cli_progress_update(id = pb)
            }
          }

          ## calculate objective values for extreme points
          ## matrix where each row is a different objective, and
          ## each column is a different extreme point
          extreme_obj_val <- vapply(
            seq_len(n_obj), FUN.VALUE = numeric(n_obj),
            function(i) sols[[i]]$objective[rownames(x_obj)]
          )

          ## calculate right-hand-side value for epsilon constraints
          epsilon_rhs <- vapply(
            seq(2L, n_obj), FUN.VALUE = numeric(n_per_problem),
            function(j) {
              extreme_obj_val[j, j] +
              (ifelse(identical(x$modelsense[j], "min"), 1, -1) *
               seq_len(n_per_problem) *
              (
                abs(extreme_obj_val[j, 1] - extreme_obj_val[j, j]) /
                (n_per_problem + 1)
              ))
            }
          )
          if (!is.matrix(epsilon_rhs)) {
            epsilon_rhs <- matrix(epsilon_rhs, nrow = 1)
          }
          epsilon_rhs <- do.call(
            what = expand.grid,
            args = as.list(as.data.frame(epsilon_rhs))
          )

          ## generate remaining solutions
          for (i in seq_len(nrow(epsilon_rhs))) {
            ### solve the optimization for the primary objective
            ### modify problem to solve with primary objective
            x$opt$set_modelsense(x_modelsense[[1]])
            x$opt$set_obj(x$obj[1, ])
            #### add in the linear constraints
            for (j in seq(2L, n_obj)) {
              ### add linear constraint with right-hand-side value
              x$opt$append_linear_constraints(
                rhs = epsilon_rhs[i, j - 1L],
                sense = ifelse(x_modelsense[[j]] == "min", "<=", ">="),
                A = Matrix::sparseMatrix(
                  i = rep(1, ncol(x$obj)),
                  j = seq_len(ncol(x$obj)),
                  x = x$obj[j, ],
                  dims = c(1, ncol(x$obj))
                ),
                row_ids = "mobj"
              )
            }
            ### solve problem
            temp_sol <- solver$solve(x$opt)
            ### if possible, update the starting solution for the solver
            solver$set_start_solution(temp_sol$x, warn = FALSE)
            ### perform tie-breaking runs
            #### add constraint based on the obj value for the primary run
            x$opt$append_linear_constraints(
              rhs = sum(x$obj[1, ] * temp_sol$x),
              sense = ifelse(x$modelsense[[1]] == "min", "<=", ">="),
              A = Matrix::sparseMatrix(
                i = rep(1, ncol(x$obj)),
                j = seq_len(ncol(x$obj)),
                x = x$obj[1, ],
                dims = c(1, ncol(x$obj))
              ),
              row_ids = "mobj"
            )
            ### perform hierarchical optimization based on remaining objectives
            for (j in seq(2L, n_obj)) {
              ### modify problem to solve with j'th objective
              x$opt$set_modelsense(x$modelsense[[j]])
              x$opt$set_obj(x$obj[j, ])
              ### solve problem
              temp_sol <- solver$solve(x$opt)
              ### if possible, update the starting solution for the solver
              solver$set_start_solution(temp_sol$x, warn = FALSE)
              ### if needed, then add constraint so that
              ### next run will be constrained based on the j'th objective
              if (!identical(j, n_obj)) {
                x$opt$append_linear_constraints(
                  rhs = sum(x$obj[j, ] * temp_sol$x),
                  sense = ifelse(x$modelsense[[j]] == "min", "<=", ">="),
                  A = Matrix::sparseMatrix(
                    i = rep(1, ncol(x$obj)),
                    j = seq_len(ncol(x$obj)),
                    x = x$obj[j, ],
                    dims = c(1, ncol(x$obj))
                  ),
                  row_ids = "tbobj"
                )
              }
            }
            ### store the resulting solution as the extreme point
            sols[[i + n_obj]] <- temp_sol
            ### reset problem for next iteration
            for (i in seq_len(length(x$opt$rhs()) - n_constraints)) {
              x$opt$remove_last_linear_constraint()
            }
            ### if needed, update progress bar
            if (isTRUE(verbose)) {
              cli::cli_progress_update(id = pb)
            }
          }

          ## if needed, clean up progress bar
          if (isTRUE(verbose)) {
            cli::cli_progress_done(id = pb)
          }

          ## if neeeded, remove infeasible solutions
          is_valid <- !vapply(sols, function(x) is.null(x$x), logical(1))
          sols <- sols[is_valid]
          assert(
            length(sols) > 1,
            .internal = TRUE,
            msg = "Couldn't any feasible solutions."
          )

          ## compute and store objective values for each solution
          for (i in seq_along(sols)) {
            if (!is.null(sols[[i]]$x)) {
              sols[[i]]$objective <- stats::setNames(
                rowSums(
                  x$obj *
                  matrix(
                    sols[[i]]$x, ncol = ncol(x$obj),
                    nrow = nrow(x$obj), byrow = TRUE
                  )
                ),
                rownames(x$obj)
              )
            }
          }

          ## return solutions
          sols
        }
      )
    )$new()
  )
}
