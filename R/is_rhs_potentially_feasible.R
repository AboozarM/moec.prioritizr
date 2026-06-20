#' Identify if RHS values are potentially feasible.
#'
#' @param rhs `matrix` of right-hand-side values.
#' Note that this must have a single row.
#'
#' @param sense `character` vector of sense values.
#' Note that `sense` must have a value of each column of `rhs`.
#'
#' @param infeasible `matrix` of right-hand-side values that are known
#' to result in infeasible solutions.
#' Note that `infeasible` and `rhs` must have the same number of columns.
#'
#' @details
#' A set of right-hand-side values are identified as not being
#' potentially feasible if they are equal to or more strict, for each and every
#' objective, than a set of infeasible right-hand-values.
#'
#' @return A `logical` value.
#'
#' @noRd
is_rhs_potentially_feasible <- function(rhs, sense, infeasible) {
  # assert arguments are valid
  assert(
    ## rhs
    is.numeric(rhs),
    is.matrix(rhs),
    identical(nrow(rhs), 1L),
    assertthat::noNA(rhs),
    ## sense
    is.character(sense),
    assertthat::noNA(sense),
    all(sense %in% c("<=", ">=")),
    identical(ncol(rhs), length(sense)),
    ## infeasible
    is.matrix(infeasible),
    assertthat::noNA(c(infeasible)),
    identical(ncol(rhs), ncol(infeasible)),
    .internal = TRUE
  )
  # if infeasible is empty, then return TRUE
  if (identical(nrow(infeasible), 0L)) return(TRUE)
  # standardize all values for minimization
  is_max <- sense == ">="
  if (any(is_max)) {
    rhs[is_max] <- rhs[is_max] * -1
    infeasible[, is_max] <- infeasible[, is_max] * -1
  }
  # check if rhs has values that are equal to or smaller than all values in
  # any of the rows in infeasible
  !any(
    rowSums(rhs[rep(1, nrow(infeasible)), , drop = FALSE] <= infeasible) ==
    ncol(infeasible)
  )
}
