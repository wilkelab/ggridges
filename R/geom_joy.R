#' Joy plots
#'
#' `geom_joy` arranges multiple density plots in a staggered fashion, as in the cover of the famous Joy Division album.
#'
#' @name geom_joy
#' @importFrom ggplot2 layer
#' @export
#' @examples
#' p1 <- ggplot(iris, aes(x=Sepal.Length, y=Species, group=Species, height = ..density..)) +
#'   geom_joy() +
#'   scale_y_discrete(expand=c(0,0)) +
#'   scale_x_continuous(expand=c(0,0))
#' print(p1)
#'
#'
#' # set the scale argument in `geom_joy()` to determine how much overlap there is among the plots
#' p2 <- ggplot(diamonds, aes(x=price, y=cut, group=cut, height=..density..)) +
#'   geom_joy(scale=4) +
#'   scale_y_discrete(expand=c(0,0)) +
#'   scale_x_continuous(expand=c(0,0))
#' print(p2)
geom_joy <- function(mapping = NULL, data = NULL, stat = "density",
                      position = "identity", na.rm = FALSE, show.legend = NA, scale = 1.8,
                      inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomJoy,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      scale = scale,
      ...
    )
  )
}

#' @rdname geom_joy
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto GeomRibbon
#' @importFrom grid gTree gList
#' @export
GeomJoy <- ggproto("GeomJoy", GeomRibbon,
  default_aes =
    aes(colour = "black",
        fill = "grey70",
        size = 0.5,
        linetype = 1,
        alpha = NA,
        scale = 2),

  required_aes = c("x", "y", "height"),

  setup_data = function(data, params) {
    yrange = max(data$y) - min(data$y)
    hmax = max(data$height)
    n = length(unique(data$y))
    # calculate internal scale
    if (n>1) iscale = yrange/((n-1)*hmax)
    else iscale = 1

    transform(data, ymin = y, ymax = y + iscale*params$scale*height)
  },

  draw_panel = function(self, data, panel_params, coord, ...) {
     groups <- split(data, factor(data$group))
     groups <- rev(groups) # reverse to draw back to front
     grobs <- lapply(groups, function(group) {
       self$draw_group(group, panel_params, coord, ...)
     })

     ggname(snake_class(self), gTree(
                       children = do.call("gList", grobs)
     ))
  }
)
