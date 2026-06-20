#' Which best match for hierarchical optimization?
#'
#' Identify the index of which set of parameters is most similar to a
#' reference set of parameters for hierarchical optimization.
#'
#' @param x `numeric` matrix with a set of parameters.
#' Note that `x` must have a single row.
#'
#' @param y `numeric` matrix with a collection of parameter sets.
#' Note that `y` should have at least one row and the same
#' number of columns as `x`.
#'
#' @details
#' The most similar parameter set is determined by based Euclidean distances
#' that are applied sequentially to each parameter. Thus parameters in
#' lower index columns (e.g., 1, 2, 3) will have a stronger influence
#' on the matching process than higher index columns (e.g., 4, 5, 6).
#'
#' @return
#' An `integer` value denoting the row number of `y` that has
#' the most similar parameters to those in `x`.
#'
#' @noRd
which_hier_best_match <- function(x, y) {
  # assert valid arguments
  assert(
    is.numeric(x),
    is.numeric(y),
    is.matrix(x),
    is.matrix(y),
    identical(ncol(x), ncol(y)),
    identical(nrow(x), 1L),
    nrow(y) >= 1,
    .internal = TRUE
  )
  # generate set of indices
  idx <- seq_len(nrow(y))

  # iterate over each column
  for (i in seq_len(ncol(y))) {
    # if y has a single parameter set remaining, then return index
    if (identical(nrow(y), 1L)) return(idx[[1]])
    # calculate distances for i'th column
    d <- abs(y[, i] - x[1, i])
    # remove parameter sets from y that are not equal to the minimum distance
    keep <- d == min(d)
    y <- y[keep, , drop = FALSE]
    idx <- idx[keep]
  }

  # return first index of remaining indices
  return(idx[[1]])
}
