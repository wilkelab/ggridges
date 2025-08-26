#' Scales for vline aesthetics
#'
#' These are various scales that can be applied to vline aesthetics, such as
#' `vline_color`, `vline_width`, `vline_linetype`. The individual scales all have the
#' same usage as existing standard ggplot2 scales, only the name differs.
#'
#' @name scale_vline
#' @seealso See [`scale_point_color_hue()`] for specific scales for point aesthetics
#' and [ggplot2::scale_discrete_manual()] for a general discrete scale.
#' @examples
#' library(ggplot2)
#'
#' # default scales
#' ggplot(iris, aes(x=Sepal.Length, y=Species, fill = Species, color = Species)) +
#'   geom_density_ridges(
#'     aes(vline_color = Species, vline_linetype = Species),
#'     alpha = .4, quantile_lines = TRUE
#'   ) +
#'   theme_ridges()
#'
#' # modified scales
#' ggplot(iris, aes(x=Sepal.Length, y=Species, fill = Species, color = Species)) +
#'   geom_density_ridges(
#'     aes(vline_color = Species),
#'     alpha = .4, quantile_lines = TRUE
#'   ) +
#'   scale_fill_hue(l = 50) +
#'   scale_vline_color_hue(l = 30) +
#'   theme_ridges()
#' @aliases NULL
NULL

#' `scale_vline_linetype()`: Equivalent to [ggplot2::scale_linetype()].
#' @rdname scale_vline
#' @usage NULL
#' @export
scale_vline_linetype <- function(..., na.value = "blank", aesthetics = "vline_linetype")
{
  ggplot2::discrete_scale(aesthetics, palette = scales::linetype_pal(),
                 na.value = na.value, ...)
}

#' `scale_vline_width_continuous()`: Equivalent to [ggplot2::scale_linewidth_continuous()].
#' @rdname scale_vline
#' @usage NULL
#' @export
scale_vline_width_continuous <- function(name = ggplot2::waiver(), breaks = ggplot2::waiver(), labels = ggplot2::waiver(),
                                  limits = NULL, range = c(1, 6),
                                  trans = "identity", guide = "legend", aesthetics = "vline_width") {
  ggplot2::continuous_scale(aesthetics, palette = scales::area_pal(range), name = name,
                   breaks = breaks, labels = labels, limits = limits, trans = trans,
                   guide = guide)
}

#' `scale_vline_colour_hue()`: Equivalent to [ggplot2::scale_colour_hue()].
#' @rdname scale_vline
#' @usage NULL
#' @export
scale_vline_colour_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                                  direction = 1, na.value = "grey50", aesthetics = "vline_colour")
{
  ggplot2::discrete_scale(aesthetics,
                          palette = scales::hue_pal(h, c, l, h.start, direction), na.value = na.value, ...)
}

#' @rdname scale_vline
#' @usage NULL
#' @export
scale_vline_color_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                                  direction = 1, na.value = "grey50", aesthetics = "vline_color")
{
  ggplot2::discrete_scale(aesthetics,
                          palette = scales::hue_pal(h, c, l, h.start, direction), na.value = na.value, ...)
}


#' `scale_vline_colour_gradient()`: Equivalent to [ggplot2::scale_colour_gradient()].
#' @rdname scale_vline
#' @usage NULL
#' @export
scale_vline_colour_gradient <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab",
                                        na.value = "grey50", guide = "none", aesthetics = "vline_colour")
{
  ggplot2::continuous_scale(aesthetics, palette = scales::seq_gradient_pal(low, high, space),
                            na.value = na.value, guide = guide, ...)
}

#' @rdname scale_vline
#' @usage NULL
#' @export
scale_vline_color_gradient <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab",
                                       na.value = "grey50", guide = "none", aesthetics = "vline_color")
{
  ggplot2::continuous_scale(aesthetics, palette = scales::seq_gradient_pal(low, high, space),
                            na.value = na.value, guide = guide, ...)
}

# default scales
#' @rdname scale_vline
#' @usage NULL
#' @export
scale_vline_linetype_discrete <- scale_vline_linetype

#' @rdname scale_vline
#' @usage NULL
#' @export
scale_vline_color_discrete <- scale_vline_color_hue

#' @rdname scale_vline
#' @usage NULL
#' @export
scale_vline_colour_discrete <- scale_vline_colour_hue

#' @rdname scale_vline
#' @usage NULL
#' @export
scale_vline_color_continuous <- scale_vline_color_gradient

#' @rdname scale_vline
#' @usage NULL
#' @export
scale_vline_colour_continuous <- scale_vline_colour_gradient
