#' Generic discrete manual scale
#'
#' Generic discrete manual scale. This scale can be used to manually set the values
#' for any aesthetics, and it is equivalent to [`scale_colour_manual()`], [`scale_fill_manual()`],
#' etc. For example, instead of writing `scale_colour_manual(values = c(...))`, we can
#' write `scale_discrete_manual("colour", values = c(...))`.
#'
#' @param aesthetics The aesthetics for which this scale should be used
#' @param values List of values to be used as palette
#' @param ... Other parameters handed off to [discrete_scale]
#' @examples
#' library(ggplot2)
#'
#' ggplot(iris, aes(x=Sepal.Length, y=Species, fill = Species)) +
#'   geom_density_ridges(aes(point_color = Species, point_fill = Species,
#'                           point_shape = Species),
#'                       alpha = .2, jittered_points = TRUE) +
#'   scale_fill_manual(values = c("#0072b2", "#D55E00", "#009e73")) +
#'   scale_discrete_manual("point_color", values = c("#0072b2", "#D55E00", "#009e73")) +
#'   scale_discrete_manual("point_fill", values = c("#0072b280", "#D55E0080", "#009e7380")) +
#'   scale_discrete_manual("point_shape", values = c(21, 22, 23)) +
#'   theme_ridges()
#' @seealso See [scale_point_color_hue()] for specific scales for point aesthetics and
#'   [scale_vline_color_hue()] for specific scales for vline aesthetics.
#' @export
scale_discrete_manual <- function(aesthetics, ..., values)
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
