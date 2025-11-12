#' @include internal.R
NULL

#' Add epsilon constraint approach
#'
#' Add an epsilon-constraint approach for multi-objective optimization to a
#' conservation planning problem.
#'
#' @param x [prioritizr::multi_problem()] object.
#'
#' @param n `integer` number of solutions. Note that `n` must be greater
#' than or equal to the number of objectives in `x`.
#'
#' @param verbose `logical` should progress on generating solutions
#' displayed? Defaults to `TRUE`.
#'
#' @details
#' TODO.
#'
#' @section Mathematical formulation:
#' TODO.
#'
#' @return
#' TODO.
#'
#' @seealso
#' TODO.
#'
#' @examples
#' \dontrun{
#' # TODO
#' }
#'
#' @export
add_epconstraint_approach <- function(x, n, verbose = TRUE) {
  # assert arguments are valid,
  assert_required(x)
  assert_required(n)
  assert_required(verbose)
  assert(
    is_multi_conservation_problem(x),
    assertthat::is.count(n),
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
        data = list(n = n, verbose = verbose),
        run = function(x, solver) {
          ## initialization
          n <- self$get_data("n")
          verbose <- self$get_data("verbose")
          sols <- vector(mode = "list", length = n)
          ## preliminary calculations
          n_obj <- nrow(x$obj)
          n_extra <- n - nrow(x$obj)
          ## if needed, set up progress bar
          if (isTRUE(verbose)) {
            pb <- cli::cli_progress_bar("Generating solutions", total = n)
          }

          ## generate solutions that are extreme points
          for (i in seq_len(n_obj)) {
            ### identify the lexicographic order for the objectives
            idx <- c(i, seq_len(n_obj)[-i])
            ### hierahically solve the optimization problem based on this order
            for (j in idx) {
              ### modify problem to solve with j'th objective
              x$opt$set_modelsense(x$modelsense[[j]])
              x$opt$set_obj(x$obj[j, ])
              ### solve problem
              temp_sol <- solver$solve(x$opt)
              ### if possible, update the starting solution for the solver
              if (
                !identical(j, idx[length(idx)]) &&
                !is.null(solver$data) &&
                !is.null(temp_sol$x) &&
                isTRUE("start_solution" %in% names(solver$data))
              ) {
                solver$data$start_solution <- temp_sol$x
              }
              ### if needed, then add constraint so that
              ### next run will be constrained based on the j'th objective
              if (!identical(j, idx[length(idx)])) {
                x$opt$append_linear_constraints(
                  rhs = sum(x$obj[j, ] * temp_sol$x),
                  sense = ifelse(x$modelsense[[j]] == "min", "<=", ">="),
                  A = Matrix::sparseMatrix(
                    i = rep(1, ncol(x$obj)),
                    j = seq_len(ncol(x$obj)),
                    x = x$obj[j, ],
                    dims = c(1, ncol(x$obj))
                  ),
                  row_ids = "mobj"
                )
              }
            }
            ### store the resulting solution as the extreme point
            sols[[i]] <- temp_sol
            ### reset problem for next extreme point
            for (i in seq_len(n_obj - 1)) {
              x$opt$remove_last_linear_constraint()
            }
            ### if needed, update progress bar
            if (isTRUE(verbose)) {
              cli::cli_progress_update(id = pb)
            }
          }

          ## extract solutions into matrix format
          ## matrix where each row is a different extreme point solution, and
          ## each column is a different solution value for a decision variable
          extreme_sols_matrix <- t(vapply(sols, `[[`, numeric(ncol(n_obj)), 1))

          ## calculate objective values for extreme points
          ## matrix where each row is a different extreme point solution, and
          ## each column is a different objective
          extreme_obj_val <- vapply(
            seq_len(n_obj), FUN.VALUE = numeric(n_obj), function(i) {
              rowSums(x$obj * extreme_sols_matrix)
            }
          )

          ## generate remaining solutions
          for (i in seq_len(n_extra)) {
            ### solve the optimization for the primary objective
            ### modify problem to solve with primary objective
            x$opt$set_modelsense(x$modelsense[[1]])
            x$opt$set_obj(x$obj[1, ])
            #### calculate rhs side values for epsilon constraints
            #### e.g., if we have 3 obj, then this will have 2 values
            curr_rhs <- 1 #TODO
            #### add in the linear constraints
            for (j in seq_along(curr_rhs)) {
              x$opt$append_linear_constraints(
                rhs = curr_rhs[[j]],
                sense = ifelse(x$modelsense[[j + 1]] == "min", "<=", ">="),
                A = Matrix::sparseMatrix(
                  i = rep(1, ncol(x$obj)),
                  j = seq_len(ncol(x$obj)),
                  x = x$obj[j + 1, ],
                  dims = c(1, ncol(x$obj))
                ),
                row_ids = "mobj"
              )
            }
            ### solve problem
            temp_sol <- solver$solve(x$opt)
            ### if possible, update the starting solution for the solver
            if (
              !is.null(solver$data) &&
              !is.null(temp_sol$x) &&
              isTRUE("start_solution" %in% names(solver$data))
            ) {
              solver$data$start_solution <- temp_sol$x
            }

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
            ### perform hierachical optimization based on remaining objectives
            for (j in seq(2L, n_obj)) {
              ### modify problem to solve with j'th objective
              x$opt$set_modelsense(x$modelsense[[j]])
              x$opt$set_obj(x$obj[j, ])
              ### solve problem
              temp_sol <- solver$solve(x$opt)
              ### if possible, update the starting solution for the solver
              if (
                !is.null(solver$data) &&
                !is.null(temp_sol$x) &&
                isTRUE("start_solution" %in% names(solver$data))
              ) {
                solver$data$start_solution <- temp_sol$x
              }
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
            ### reset problem for next extreme point
            for (i in seq_len((n_obj - 1) + 1 + (n_obj - 1)) {
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
