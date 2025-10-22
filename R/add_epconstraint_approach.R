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
    assertthat::is.flag(n),
    n > prioritizr::number_of_problems(x),
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
          ## generate solutions that represent extreme points
          for (i in seq_len(nrow(x$opt))) {
            ### modify problem
            x$opt$set_modelsense(x$modelsense[[i]])
            x$opt$set_obj(x$obj[i, ])
            ### solve problem
            sols[[i]] <- solver$solve(x$opt)
            ### compute and store objective values for each objective
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
            ## if needed, update progress bar
            if (isTRUE(verbose)) {
              cli::cli_progress_update(id = pb)
            }
          }
          ## set objective for subsequent optimization runs
          ## according to the first objective
          x$opt$set_modelsense(x$modelsense[[1]])
          x$opt$set_obj(x$obj[1, ])
          ## calculate constraint values for the remaining solutions
          # TODO
          ## generate remaining solutions
          for (i in seq_len(n_extra)) {
            ### modify problem
            # TODO: add linear constraint for each objective (except first)
            # this will use x$opt$append_linear_constraints()
            ### solve problem
            sols[[i + nrow(x$obj)]] <- solver$solve(x$opt)
            ### reset problem,
            ### this involves removing the extra constraints that were
            ### previously added as part of the epsilon constraint approach
            for (j in seq_len(nrow(x$obj) - 1)) {
              x$opt$remove_last_linear_constraint()
            }
            ### compute and store objective values for each objective
            if (!is.null(sols[[i]]$x)) {
              sols[[i + nrow(x$obj)]]$objective <- stats::setNames(
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
            ## if possible, update the starting solution for the solver
            if (
              !is.null(solver$data) &&
              !is.null(sols[[i]]$x) &&
              isTRUE("start_solution" %in% names(solver$data))
            ) {
              solver$data$start_solution <- sols[[i]]$x
            }
            ## if needed, update progress bar
            if (isTRUE(verbose)) {
              cli::cli_progress_update(id = pb)
            }
          }
          ## if needed, clean up progress bar
          if (isTRUE(verbose)) {
            cli::cli_progress_done(id = pb)
          }
          ## return solutions
          sols
        }
      )
    )$new()
  )
}
