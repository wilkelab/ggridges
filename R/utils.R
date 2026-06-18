
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
#' @keywords internal
#'
reduce <- function(.x, .f, ..., .init) {

  if (missing(.init)) {
    if (length(.x) == 0)
      rlang::abort(
        c(
          "`.x` is empty, and no `.init` was supplied.",
          "i" = "Provide `.init` or ensure `.x` is non-empty."
        ),
        call = rlang::caller_env()
      )
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

check_vline_size <- function(data) {
  if (is.null(data$vline_width) && !is.null(data$vline_size)) {
    rlang::warn(
      c(
        "The `vline_size` aesthetic is deprecated.",
        "i" = "Use `vline_width` instead."
      ),
      call = rlang::caller_env()
    )
    data$vline_width <- data$vline_size
  }
  data
}

check_size <- function(data) {
  if (is.null(data$linewidth) && !is.null(data$size)) {
    rlang::warn(
      c(
        "The `size` aesthetic is deprecated.",
        "i" = "Use `linewidth` instead."
      ),
      call = rlang::caller_env()
    )
    data$width <- data$size
  }
  data
}

check_vline_size_param <- function(params) {
  if ("vline_size" %in% names(params)) {
    rlang::warn(
      c(
        "The `vline_size` and `size` aesthetics are deprecated.",
        "i" = "Use `vline_width` and `linewidth` instead."
      ),
      call = rlang::caller_env()
    )
    params$vline_width <- params$vline_size
  }
  params
}

check_size_param <- function(params) {
  if ("size" %in% names(params)) {
    rlang::warn(
      c(
        "The `size` aesthetic is deprecated.",
        "i" = "Use `linewidth` instead."
      ),
      call = rlang::caller_env()
    )
    params$linewidth <- params$size
  }
  params
}

# Internal vertical scaling shared by the density-ridgeline geoms
# (`GeomDensityRidges`, `GeomDensityRidgesGradient`). Groups with no density
# estimate contribute non-finite or non-positive heights; a panel made up only
# of such groups gets a scale of 1, so raw points at the baseline still draw and
# `max()` is never called on an all-missing vector. For ordinary (finite,
# positive) heights this matches the original scaling. `hmax` is per-row under
# `panel_scaling` and scalar otherwise, with non-finite values normalised to 0
# so callers can use it directly for `min_height`.
ridgeline_internal_scale <- function(data, panel_scaling) {
  finite_pos_max <- function(x) {
    x <- x[is.finite(x) & x > 0]
    if (length(x) == 0) NA_real_ else max(x)
  }
  yrange <- max(data$y) - min(data$y)
  n <- length(unique(data$y))
  if (n < 2) {
    hmax <- finite_pos_max(data$height)
    iscale <- 1
  } else if (isTRUE(panel_scaling)) {
    heights <- split(data$height, data$PANEL)
    max_heights <- vapply(heights, finite_pos_max, numeric(1))
    hmax <- max_heights[data$PANEL]
    iscale <- yrange / ((n - 1) * hmax)
  } else {
    hmax <- finite_pos_max(data$height)
    iscale <- yrange / ((n - 1) * hmax)
  }
  # no positive finite height: draw at baseline (scale 1, min_height 0)
  iscale[!is.finite(iscale)] <- 1
  hmax[!is.finite(hmax)] <- 0
  list(iscale = iscale, hmax = hmax)
}
