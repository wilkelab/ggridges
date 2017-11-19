#' Scales for point aesthetics
#'
#' These are various scales that can be applied to point aesthetics, such as
#' `point_color`, `point_fill`, `point_size`. The individual scales all have the
#' same usage as existing standard ggplot2 scales, only the name differs.
#'
#' @name scale_point
#' @aliases NULL
NULL

#' `scale_point_shape`: Equivalent to [`scale_shape`].
#' @rdname scale_point
#' @usage NULL
#' @export
scale_point_shape <- function(..., solid = TRUE)
{
  discrete_scale("point_shape", "shape_d", scales::shape_pal(solid), ...)
}

#' `scale_point_size_continuous`: Equivalent to [`scale_size_continuous`].
#' @rdname scale_point
#' @usage NULL
#' @export
scale_point_size_continuous <- function(name = ggplot2::waiver(), breaks = ggplot2::waiver(), labels = ggplot2::waiver(),
                                  limits = NULL, range = c(1, 6),
                                  trans = "identity", guide = "legend", aesthetics = "point_size") {
  ggplot2::continuous_scale(aesthetics, "area", scales::area_pal(range), name = name,
                   breaks = breaks, labels = labels, limits = limits, trans = trans,
                   guide = guide)
}



#' `scale_point_color_hue`: Equivalent to [`scale_colour_hue`].
#' @rdname scale_point
#' @usage NULL
#' @export
scale_point_color_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                                  direction = 1, na.value = "grey50", aesthetics = "point_color")
{
  ggplot2::discrete_scale(aesthetics, "hue",
                          scales::hue_pal(h, c, l, h.start, direction), na.value = na.value, ...)
}

#' `scale_point_fill_hue`: Equivalent to [`scale_fill_hue`].
#' @rdname scale_point
#' @usage NULL
#' @export
scale_point_fill_hue <- function(...) scale_point_color_hue(..., aesthetics = "point_fill")


#' Generic discrete manual scale
#'
#' Generic discrete manual scale.
#'
#' @param aesthetics The aesthetics for which this scale should be used
#' @param values List of values to be used as palette
#' @param ... Other parameters handed off to [discrete_scale]
#' @export
scale_discrete_manual <- function(aesthetics, values, ...)
{
  pal <- function(n) {
    if (n > length(values)) {
      stop("Insufficient values in manual scale. ", n,
           " needed but only ", length(values), " provided.",
           call. = FALSE)
    }
    values
  }
  discrete_scale(aesthetics, "manual", pal, ...)
}


# default scales
#' @rdname scale_point
#' @usage NULL
#' @export
scale_point_shape_discrete <- scale_point_shape

#' @rdname scale_point
#' @usage NULL
#' @export
scale_point_color_discrete <- scale_point_color_hue

#' @rdname scale_point
#' @usage NULL
#' @export
scale_point_fill_discrete <- scale_point_fill_hue
