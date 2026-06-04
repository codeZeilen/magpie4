#' Filter MAgPIE output by model status
#'
#' Masks years in a magpie object where the model did not achieve an acceptable
#' solution status. Starting from the first year with an unacceptable status,
#' all subsequent years are set to \code{NA}. If \code{mstat} is \code{NULL},
#' the input is returned unchanged with a warning.
#'
#' @param x A magpie object to be filtered.
#' @param mstat Model status information, either a character path passed to
#'   \code{\link{modelstat}} or an already-loaded magpie object of model
#'   statuses.
#' @param filter Integer vector of acceptable model status codes. Defaults to
#'   \code{c(2, 7)} (optimal and feasible solutions).
#'
#' @return The input magpie object \code{x} with years from the first
#'   unacceptable model status onward set to \code{NA}. If all years match the
#'   filter, \code{x} is returned unmodified.
#'
#' @importFrom magclass magpiesort nyears setNames
#' @noRd

.filtermagpie <- function(x, mstat, filter = c(2, 7)) {
  if (is.character(mstat)) {
    mstat <- modelstat(mstat)
  }
  if (is.null(mstat)) {
    warning("Modelstat information not found!")
    return(x)
  }
  mstat <- magpiesort(mstat)

  .applyFilter <- function(mstat, filter) {
    tmp <- FALSE
    for (f in filter) {
      tmp <- tmp | (mstat == f)
    }
    return(tmp)
  }

  matchesFilter <- .applyFilter(mstat, filter)
  if (all(matchesFilter)) {
    return(x)
  } else {
    nastart <- min(which(!matchesFilter, arr.ind = TRUE)[, 2])
    matchesFilter[, , ] <- 1
    matchesFilter[, nastart:nyears(matchesFilter), ] <- NA
    tmp <- setNames(matchesFilter[1, , 1], NULL)
    return(x * tmp)
  }
}
