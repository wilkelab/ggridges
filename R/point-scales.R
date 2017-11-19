#' Scales for point aesthetics
#'
#' @name scale_point
#' @aliases NULL
NULL

#' @rdname scale_point
#' @export
scale_point_shape <- function(..., solid = TRUE)
{
  discrete_scale("point_shape", "shape_d", scales::shape_pal(solid), ...)
}

#' @rdname scale_point
#' @export
scale_point_color_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                                  direction = 1, na.value = "grey50", aesthetic = "point_color")
{
  ggplot2::discrete_scale(aesthetic, "hue",
                          scales::hue_pal(h, c, l, h.start, direction), na.value = na.value, ...)
}

#' @rdname scale_point
#' @export
scale_point_fill_hue <- function(...) scale_point_color_hue(..., aesthetic = "point_fill")


#' Generic discrete manual scale
#'
#' Generic discrete manual scale.
#'
#' @param aesthetic The aesthetic for which this scale should be used
#' @param values List of values to be used as palette
#' @param ... Other parameters handed off to [discrete_scale]
#' @export
scale_discrete_manual <- function(aesthetic, values, ...)
{
  pal <- function(n) {
    if (n > length(values)) {
      stop("Insufficient values in manual scale. ", n,
           " needed but only ", length(values), " provided.",
           call. = FALSE)
    }
    values
  }
  discrete_scale(aesthetic, "manual", pal, ...)
}


# default scales
#' @rdname scale_point
#' @export
scale_point_shape_discrete <- scale_point_shape

#' @rdname scale_point
#' @export
scale_point_color_discrete <- scale_point_color_hue

#' @rdname scale_point
#' @export
scale_point_fill_discrete <- scale_point_fill_hue
