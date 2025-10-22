#' Add epsilon constraint approach
#'
#' Add a epsilon constraint for multi-objective optimization to a
#' conservation planning problem.
#'
#' @param x [prioritizr::multi_problem()] object.
#'
#' @param n `integer` number of solutions to generate.
#'
#' @param verbose `logical` should progress be displayed when generating
#' solutions? Defaults to `TRUE`.
#'
#' @details
#' TODO.
#'
#' @section Mathematical formulation:
#'
#' @return
#' TODO.
#'
#' @seealso
#' TODO.
#'
#' @references
#' TODO.
#'
#' @export
add_epconstraint_approach <- function(x, n, verbose = TRUE) {
  # assert arguments are valid
  assert_required(x)
  assert_required(n)
  assert(
    is_multi_conservation_problem(x),
    assertthat::is.count(n),
    assertthat::noNA(n),
    n >= number_problems(x),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )

  # add approach
  x$add_approach(
    R6::R6Class(
      "EpConstraintApproach",
      inherit = prioritizr::MultiObjApproach,
      public = list(
        name = "epsilon constraint approach",
        data = list(n = n, verbose = verbose),
        run = function(x, solver) {
          ## initialization
          n <- self$get_data("n")
          verbose <- self$get_data("verbose")
          sols <- vector(mode = "list", length = n);
          ## preliminary calculations
          n_extra <- n - prioritizr::number_problems(x)
          # TODO.
          ## if needed, set up progress bar
          if (isTRUE(verbose)) {
            pb <- cli::cli_progress_bar("Generating solutions", total = n)
          }
          ## generate extreme points
          for (i in seq_len(prioritizr::number_problems(x))) {
            ### modify optimization
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
          ## generate additional solutions
          for (i in seq_len(n_extra)) {
            ## modify problem
            # TODO, setting the linear constraints
            ### solve problem
            sols[[i]] <- solver$solve(x$opt)
            # reset problem
            # TODO, remove extra linear constraints
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
