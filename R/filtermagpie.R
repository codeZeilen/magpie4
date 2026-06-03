#' @importFrom magclass magpiesort nyears setNames

.filtermagpie <- function(x, mstat, filter = c(2, 7)) {
  if (is.character(mstat)) {
    mstat <- modelstat(mstat)
  }
  if (is.null(mstat)) {
    warning("Modelstat information not found!")
    return(x)
  }
  mstat <- magpiesort(mstat)

  # Returns a logical array: TRUE where mstat equals any value in filter.
  .matchesFilter <- function(mstat, filter) {
    tmp <- FALSE
    for (f in filter) {
      tmp <- tmp | (mstat == f)
    }
    return(tmp)
  }

  tmp <- .matchesFilter(mstat, filter)
  if (all(tmp)) {
    return(x)
  } else {
    nastart <- min(which(!tmp, arr.ind = TRUE)[, 2])
    tmp[, , ] <- 1
    tmp[, nastart:nyears(tmp), ] <- NA
    tmp <- setNames(tmp[1, , 1], NULL)
    return(x * tmp)
  }
}
