#' Smoothed density estimates drawn with a ridgeline rather than area
#'
#' This function is a drop-in replacement for ggplot2's [ggplot2::geom_density()]. The only difference is that
#' the geom draws a ridgeline (line with filled area underneath) rather than a polygon.
#'
#' @seealso See [ggplot2::geom_density()].
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 geom_density
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_density
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(diamonds, aes(carat)) +
#'   geom_density_line()
#'
#' ggplot(diamonds, aes(carat)) +
#'   geom_density_line(adjust = 1/5)
#' ggplot(diamonds, aes(carat)) +
#'   geom_density_line(adjust = 5)
#'
#' ggplot(diamonds, aes(depth, colour = cut)) +
#'   geom_density_line(alpha = 0.5) +
#'   xlim(55, 70)
#' ggplot(diamonds, aes(depth, fill = cut, colour = cut)) +
#'   geom_density_line(alpha = 0.1) +
#'   xlim(55, 70)
geom_density_line <- function(mapping = NULL, data = NULL,
                         stat = "density", position = "identity",
                         ...,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDensityLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_density_line
#' @format NULL
#' @usage NULL
#' @export
GeomDensityLine <- ggproto("GeomDensityLine", GeomRidgeline,

  required_aes = c("x", "y"),

  setup_data = function(self, data, params) {

  if (!"min_height" %in% names(data)){
    if (!"min_height" %in% names(params))
      data <- cbind(data, min_height = self$default_aes$min_height)
    else
      data <- cbind(data, min_height = params$min_height)
    }
    transform(data, ymin = 0, ymax = y)
  }
)
