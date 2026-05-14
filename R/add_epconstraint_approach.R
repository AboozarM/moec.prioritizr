#' @include internal.R
NULL

#' Add epsilon constraint approach
#'
#' Add an epsilon-constraint approach for multi-objective optimization to a
#' conservation planning problem.
#'
#' @param x [prioritizr::multi_problem()] object.
#'
#' @param n_per_problem `integer` number of solutions to generate per
#' problem in `x`. Note that this does not include solutions that represent
#' optimizing exclusively on each objective individually (i.e., extreme points
#' of the Pareto frontier). In particular, the total number of solutions
#' is equal to \eqn{o + n^(o - 1)}{o + n^(o - 1)}, where
#' \eqn{o} is the number of problems in `x` and \eqn{n} is equal to
#' `n_per_problem`. For example, if `x` had three problems and
#' `n_per_problem = 4`, then 19 solutions would be generated.
#'
#' @param remove_duplicates `logical` (`TRUE`/`FALSE`) value indicating
#' if duplicate solutions should be removed from the result.
#' Defaults to `TRUE`.
#'
#' @param verbose `logical` (`TRUE`/`FALSE`) should progress on generating
#' solutions displayed? Defaults to `TRUE`.
#'
#' @details
#' TODO.
#'
#' @section Mathematical formulation:
#' TODO.
#'
#' @return
#' An updated `multi_problem()` object with the approach
#' added to it.
#'
#' @seealso
#' See [prioritizr::approaches] for other functions for adding an approach.
#'
#' @examples
#' \dontrun{
#' # in this example, we aim to identify a set of planning units that will
#' # not exceed a particular budget and meet objectives for
#' # (i) representing species that are important for ecosystem
#' # functioning (hereafter, keystone species) and (ii) representing species
#' # that have high social or cultural value (hereafter, iconic species)
#'
#' # import data
#' con_cost <- get_sim_pu_raster()
#' keystone_spp <- get_sim_features()[[1:3]]
#' iconic_spp <- get_sim_features()[[4:5]]
#'
#' # define a total conservation budget (30% of total cost)
#' budget <- terra::global(con_cost, "sum", na.rm = TRUE)[[1]] * 0.3
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
#'   add_relative_targets(0.45) %>%
#'   add_binary_decisions()
#'
#' # solve the single-objective problems
#' s1 <-
#'   p1 %>%
#'   add_default_solver(verbose = FALSE) %>%
#'   solve()
#' s2 <-
#'   p2 %>%
#'   add_default_solver(verbose = FALSE) %>%
#'   solve()
#'
#' # plot the solutions to the single-objective problems
#' plot(s1, main = "Keystone species", axes = FALSE)
#' plot(s2, main = "Iconic species", axes = FALSE)
#'
#' # now we will a create multi-objective problem that simultaneously
#' # considers both of these objectives
#'
#' # the first objective for keystone species will have a higher order of
#' # priority than the second objective for iconic species -- because
#' # the long-term persistence of iconic species depends on ecosystem
#' # functioning -- and we will then specify that we want to generate
#' # 5 different solutions for each objective
#' mp <-
#'   multi_problem(keystone_obj = p1, iconic_obj = p2) %>%
#'   add_epconstraint_approach(n_per_objective = 5, verbose = FALSE) %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve multi-objective problem
#' ms <- solve(mp)
#'
#' # plot solutions to multi-objective problem
#' plot(ms, main = "multi-objective solution", axes = FALSE)
#'
#' # plot the objectives values to visualize the Pareto frontier
#' # (note that smaller values are better because these objectives seek to
#' # minimize representation shortfalls)
#' plot(
#'   obj_matrix,
#'   main = "Pareto frontier",
#'   xlab = "Keystone objective (shortfall)",
#'   ylab = "Iconic objective (shortfall)",
#' )
#' }
#' @export
add_epconstraint_approach <- function(x, n_per_problem,
                                      remove_duplicates = TRUE,
                                      verbose = TRUE) {
  # assert arguments are valid,
  assert_required(x)
  assert_required(n_per_problem)
  assert_required(remove_duplicates)
  assert_required(verbose)
  assert(
    is_multi_conservation_problem(x),
    assertthat::is.count(n_per_problem),
    assertthat::is.flag(remove_duplicates),
    assertthat::noNA(remove_duplicates),
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
          remove_duplicates = remove_duplicates,
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
            cli::cli_inform(paste("Generating", n, "solutions..."))
            pb <- cli::cli_progress_bar("Generating solutions", total = n)
          }

          ## generate solutions that are extreme points
          for (i in seq_len(n_obj)) {
            ### generate solution with i'th objective as most important
            sols[[i]] <- solver$solve_multiobj(
              x,
              priority = replace(seq(n_obj, 1L), i, n_obj + 1L),
              rel_tol = rep(0, n_obj - 1L)
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
            seq(2L, n_obj), FUN.VALUE = numeric(n_per_problem), function(j) {
              extreme_obj_val[j, j] +
              (ifelse(identical(x$modelsense[j], "min"), 1, -1) *
               seq_len(n_per_problem) *
              (
                abs(extreme_obj_val[j, 1] - extreme_obj_val[j, j]) /
                (n_per_problem + 1)
              ))
            }
          )
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

          ## if needed, remove duplicate values
          if (isTRUE(remove_duplicates)) {
            ### create a key for each solution to identify duplicates
            int_idx <- x$opt$vtype() %in% c("B", "I")
            sol_keys <- vapply(
              sols, FUN.VALUE = character(1),
              function(x) paste(x$x[int_idx], collapse = " ")
            )
            ### remove duplicate solutions
            sols <- sols[!duplicated(sol_keys)]
          }

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
