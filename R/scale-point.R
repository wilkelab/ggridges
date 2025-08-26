#' Scales for point aesthetics
#'
#' These are various scales that can be applied to point aesthetics, such as
#' `point_color`, `point_fill`, `point_size`. The individual scales all have the
#' same usage as existing standard ggplot2 scales, only the name differs.
#'
#' @name scale_point
#' @seealso See [`scale_vline_color_hue()`] for specific scales for vline aesthetics
#' and [ggplot2::scale_discrete_manual()] for a general discrete scale.
#' @examples
#' library(ggplot2)
#'
#' # default scales
#' ggplot(iris, aes(x=Sepal.Length, y=Species, fill = Species)) +
#'   geom_density_ridges(
#'     aes(
#'       point_color = Species, point_fill = Species,
#'       point_shape = Species
#'     ),
#'     alpha = .4, jittered_points = TRUE
#'   ) +
#'   theme_ridges()
#'
#' # modified scales
#' ggplot(iris, aes(x=Sepal.Length, y=Species, fill = Species)) +
#'   geom_density_ridges(
#'     aes(
#'       point_color = Species, point_fill = Species,
#'       point_shape = Species
#'     ),
#'     alpha = .4, point_alpha = 1,
#'     jittered_points = TRUE
#'   ) +
#'   scale_fill_hue(l = 50) +
#'   scale_point_color_hue(l = 20) +
#'   scale_point_fill_hue(l = 70) +
#'   scale_discrete_manual("point_shape", values = c(21, 22, 23)) +
#'   theme_ridges()
#' @aliases NULL
NULL

#' `scale_point_shape()`: Equivalent to [ggplot2::scale_shape()].
#' @rdname scale_point
#' @usage NULL
#' @export
scale_point_shape <- function(..., solid = TRUE, aesthetics = "point_shape")
{
  ggplot2::discrete_scale(aesthetics, palette = scales::shape_pal(solid), ...)
}

#' `scale_point_size_continuous()`: Equivalent to [ggplot2::scale_size_continuous()].
#' @rdname scale_point
#' @usage NULL
#' @export
scale_point_size_continuous <- function(name = ggplot2::waiver(), breaks = ggplot2::waiver(), labels = ggplot2::waiver(),
                                  limits = NULL, range = c(1, 6),
                                  trans = "identity", guide = "legend", aesthetics = "point_size") {
  ggplot2::continuous_scale(aesthetics, palette = scales::area_pal(range), name = name,
                   breaks = breaks, labels = labels, limits = limits, trans = trans,
                   guide = guide)
}

#' `scale_point_colour_hue()`: Equivalent to [ggplot2::scale_colour_hue()].
#' @rdname scale_point
#' @usage NULL
#' @export
scale_point_colour_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                                  direction = 1, na.value = "grey50", aesthetics = "point_colour")
{
  ggplot2::discrete_scale(aesthetics,
                          palette = scales::hue_pal(h, c, l, h.start, direction), na.value = na.value, ...)
}

#' @rdname scale_point
#' @usage NULL
#' @export
scale_point_color_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                                  direction = 1, na.value = "grey50", aesthetics = "point_color")
{
  ggplot2::discrete_scale(aesthetics,
                          palette = scales::hue_pal(h, c, l, h.start, direction), na.value = na.value, ...)
}

#' `scale_point_fill_hue()`: Equivalent to [ggplot2::scale_fill_hue()].
#' @rdname scale_point
#' @usage NULL
#' @export
scale_point_fill_hue <- function(...) scale_point_color_hue(..., aesthetics = "point_fill")

#' `scale_point_colour_gradient()`: Equivalent to [ggplot2::scale_colour_gradient()].
#' @rdname scale_point
#' @usage NULL
#' @export
scale_point_colour_gradient <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab",
                                        na.value = "grey50", guide = "none", aesthetics = "point_colour")
{
  ggplot2::continuous_scale(aesthetics, palette = scales::seq_gradient_pal(low, high, space),
                            na.value = na.value, guide = guide, ...)
}

#' @rdname scale_point
#' @usage NULL
#' @export
scale_point_color_gradient <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab",
                                       na.value = "grey50", guide = "none", aesthetics = "point_color")
{
  ggplot2::continuous_scale(aesthetics, palette = scales::seq_gradient_pal(low, high, space),
                            na.value = na.value, guide = guide, ...)
}


#' `scale_point_fill_gradient()`: Equivalent to [ggplot2::scale_fill_gradient()]. Note that this scale cannot
#'   draw a legend, however, because of limitations in [ggplot2::guide_colorbar()].
#' @rdname scale_point
#' @usage NULL
#' @export
scale_point_fill_gradient <- function(...) scale_point_color_gradient(..., aesthetics = "point_fill")


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
scale_point_colour_discrete <- scale_point_colour_hue

#' @rdname scale_point
#' @usage NULL
#' @export
scale_point_fill_discrete <- scale_point_fill_hue

#' @rdname scale_point
#' @usage NULL
#' @export
scale_point_color_continuous <- scale_point_color_gradient

#' @rdname scale_point
#' @usage NULL
#' @export
scale_point_colour_continuous <- scale_point_colour_gradient

#' @rdname scale_point
#' @usage NULL
#' @export
scale_point_fill_continuous <- scale_point_fill_gradient
