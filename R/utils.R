
#' Reduce a list to a single value by iteratively applying a binary function
#'
#' Inspired by \code{reduce()} from the \code{purrr} package
#'
#' @param .x A list or atomic vector.
#' @param .f A 2-argument function. The function will be
#'   passed the accumulated value as the first argument and the "next" value
#'   as the second argument.
#' @param ... Additional arguments passed on to `.f`.
#' @param .init If supplied, will be used as the first value to start
#'   the accumulation, rather than using \code{x[[1]]}. This is useful if
#'   you want to ensure that `reduce` returns a correct value when `.x`
#'   is empty. If missing, and `x` is empty, will throw an error.
#'
#' @author Jonathon Love <jon@thon.cc>
#'
reduce <- function(.x, .f, ..., .init) {

  if (missing(.init)) {
    if (length(.x) == 0)
      stop('`.x` is empty, and no `.init` supplied')
    v <- .x[[1]]
    i <- 2
  }
  else {
    v <- .init
    i <- 1
  }

  while (i <= length(.x)) {
    v <- .f(v, .x[[i]], ...)
    i <- i + 1
  }

  v
}
