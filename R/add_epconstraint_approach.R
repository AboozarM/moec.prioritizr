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
    prioritizr::number_of_problems(x) == 2,
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
          n_extra <- n - nrow(x$obj)
          ## if needed, set up progress bar
          if (isTRUE(verbose)) {
            pb <- cli::cli_progress_bar("Generating solutions", total = n)
          }

          ## generate solution for first extreme point
          ### modify problem to solve with primary objective
          x$opt$set_modelsense(x$modelsense[[1]])
          x$opt$set_obj(x$obj[1, ])
          ### solve problem
          temp_sol <- solver$solve(x$opt)
          ### modify problem to solve with secondary objective
          x$opt$set_modelsense(x$modelsense[[2]])
          x$opt$set_obj(x$obj[2, ])
          x$opt$append_linear_constraints(
            rhs = sum(x$obj[1, ] * temp_sol$x),
            sense = ifelse(x$modelsense[[1]] == "min", "<=", ">="),
            A = Matrix::sparseMatrix(
              i = rep(1, ncol(x$obj)),
              j = seq_len(ncol(x$obj)),
              x = x$obj[1, ],
              dims = c(1, ncol(x$obj))
            ),
            row_ids = "obj1"
          )
          ### if possible, update the starting solution for the solver
          if (
            !is.null(solver$data) &&
            !is.null(temp_sol$x) &&
            isTRUE("start_solution" %in% names(solver$data))
          ) {
            solver$data$start_solution <- temp_sol$x
          }
          ### solve problem
          sols[[1]] <- solver$solve(x$opt)
          ### reset problem to remove last linear constraint
          x$opt$remove_last_linear_constraint()
          ### if needed, update progress bar
          if (isTRUE(verbose)) {
            cli::cli_progress_update(id = pb)
          }

          ## generate solution for second extreme point
          ### modify problem to solve with secondary objective
          x$opt$set_modelsense(x$modelsense[[2]])
          x$opt$set_obj(x$obj[2, ])
          ### solve problem
          temp_sol <- solver$solve(x$opt)
          ### modify problem to solve with secondary objective
          x$opt$set_modelsense(x$modelsense[[1]])
          x$opt$set_obj(x$obj[1, ])
          x$opt$append_linear_constraints(
            rhs = sum(x$obj[2, ] * temp_sol$x),
            sense = ifelse(x$modelsense[[2]] == "min", "<=", ">="),
            A = Matrix::sparseMatrix(
              i = rep(1, ncol(x$obj)),
              j = seq_len(ncol(x$obj)),
              x = x$obj[2, ],
              dims = c(1, ncol(x$obj))
            ),
            row_ids = "obj2"
          )
          ### if possible, update the starting solution for the solver
          if (
            !is.null(solver$data) &&
            !is.null(temp_sol$x) &&
            isTRUE("start_solution" %in% names(solver$data))
          ) {
            solver$data$start_solution <- temp_sol$x
          }
          ### solve problem
          sols[[2]] <- solver$solve(x$opt)
          ### reset problem to remove last linear constraint
          x$opt$remove_last_linear_constraint()
          ### if needed, update progress bar
          if (isTRUE(verbose)) {
            cli::cli_progress_update(id = pb)
          }

          ## calculate objective values for extreme points
          s1_obj1 <- sum(x$obj[1, ] * sols[[1]]$x)
          s1_obj2 <- sum(x$obj[2, ] * sols[[1]]$x)
          s2_obj1 <- sum(x$obj[1, ] * sols[[2]]$x)
          s2_obj2 <- sum(x$obj[2, ] * sols[[2]]$x)

          ## generate remaining solutions
          for (i in seq_len(n_extra)) {
            ### calculate the epsilon value for i'th solution
            ep <- s2_obj2 + (i) * (s1_obj2 - s2_obj2) / (n_extra + 1)
            ### modify problem with epsilon constraint and then
            ### solve with primary objective
            x$opt$set_modelsense(x$modelsense[[1]])
            x$opt$set_obj(x$obj[1, ])
            x$opt$append_linear_constraints(
              rhs = ep,
              sense = ifelse(x$modelsense[[2]] == "min", "<=", ">="),
              A = Matrix::sparseMatrix(
                i = rep(1, ncol(x$obj)),
                j = seq_len(ncol(x$obj)),
                x = x$obj[2, ],
                dims = c(1, ncol(x$obj))
              ),
              row_ids = "obj2"
            )
            ### solve problem
            temp_sol <- solver$solve(x$opt)
            ### reset problem to remove last linear constraint
            x$opt$remove_last_linear_constraint()
            ### modify problem with new constraint based on previous
            ### solution and then solve with secondary objective
            x$opt$set_modelsense(x$modelsense[[2]])
            x$opt$set_obj(x$obj[2, ])
            x$opt$append_linear_constraints(
              rhs =  sum(x$obj[1, ] * temp_sol$x),
              sense = ifelse(x$modelsense[[1]] == "min", "<=", ">="),
              A = Matrix::sparseMatrix(
                i = rep(1, ncol(x$obj)),
                j = seq_len(ncol(x$obj)),
                x = x$obj[1, ],
                dims = c(1, ncol(x$obj))
              ),
              row_ids = "obj1"
            )
            ### if possible, update the starting solution for the solver
            if (
              !is.null(solver$data) &&
              !is.null(temp_sol$x) &&
              isTRUE("start_solution" %in% names(solver$data))
            ) {
              solver$data$start_solution <- temp_sol$x
            }
            ### solve problem
            sols[[i + nrow(x$obj)]] <- solver$solve(x$opt)
            ### reset problem for next ep solution
            x$opt$remove_last_linear_constraint()
            ## if needed, update progress bar
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
