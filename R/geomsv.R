#' Plot a vertical ridgeline (ridgeline rotated 90 degrees)
#'
#' Plots the sum of the `x` and `width` aesthetics versus `y`, filling the area between `x` and `x + width` with a color.
#' Just like [geom_ridgeline()], but with y and x replaced.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#'   If specified and `inherit.aes = TRUE` (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply `mapping` if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to `ggplot()`.
#'
#'    A `data.frame`, or other object, will override the plot
#'    data.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame.`, and
#'    will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this
#'    layer, as a string.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them.
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... other arguments passed on to [ggplot2::layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `color = "red"` or `linewidth = 3`. They may also be parameters
#'   to the paired geom/stat.
#'
#' @section Aesthetics:
#'
#' Required aesthetics are in bold.
#'
#' * **`x`**
#' * **`y`**
#' * **`width`** Width of the ridgeline, measured from the respective `x` value. Assumed to be positive, though this is not required.
#' * `group` Defines the grouping. Required when the dataset contains multiple distinct ridgelines. Will typically be the same
#' variable as is mapped to `x`.
#' * `scale` A scaling factor to scale the widths of the ridgelines.
#' A value of 1 indicates that the widths are taken as is. This aesthetic can be used to convert
#' `width` units into `x` units.
#' * `min_width` A width cutoff on the drawn ridgelines. All values that fall below this cutoff will be removed.
#' The main purpose of this cutoff is to remove long tails right at the baseline level, but other uses are possible.
#' The cutoff is applied before any width
#' scaling is applied via the `scale` aesthetic. Default is 0, so negative values are removed.
#' * `color` Color of the ridgeline
#' * `fill` Fill color of the area under the ridgeline
#' * `alpha` Transparency level of `fill`. Not applied to `color`. If you want transparent lines, you can set their
#'   color as RGBA value, e.g. #FF0000A0 for partially transparent red.
#' * `group` Grouping, to draw multiple ridgelines from one dataset
#' * `linetype` Linetype of the ridgeline
#' * `linewidth` Line thickness
#'
#' @examples
#' library(ggplot2)
#'
#' d <- data.frame(y = rep(1:5, 3), x = c(rep(0, 5), rep(1, 5), rep(3, 5)),
#'                 width = c(0, 1, 3, 4, 0, 1, 2, 3, 5, 4, 0, 5, 4, 4, 1))
#' ggplot(d, aes(x, y, width = width, group = x)) + geom_vridgeline(fill="lightblue")
#'
#' ggplot(iris, aes(x=Species, y=Sepal.Width, width = after_stat(density), fill=Species)) +
#'   geom_vridgeline(stat="ydensity", trim=FALSE, alpha = 0.85, scale = 2)
#'
#' @importFrom ggplot2 layer
#' @export
geom_vridgeline <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomVRidgeline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_vridgeline
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Geom draw_key_polygon
#' @export
GeomVRidgeline <- ggproto("GeomVRidgeline", Geom,
  default_aes = aes(color = "black", fill = "grey80", x = 0, linewidth = 0.5, linetype = 1,
        min_width = 0, scale = 1, alpha = NA),

  required_aes = c("x", "y", "width"),

  setup_data = function(self, data, params) {

    if (!"scale" %in% names(data)) {
      if (!"scale" %in% names(params))
        data <- cbind(data, scale = self$default_aes$scale)
      else
        data <- cbind(data, scale = params$scale)
    }

    if (!"min_width" %in% names(data)){
      if (!"min_width" %in% names(params))
        data <- cbind(data, min_width = self$default_aes$min_width)
      else
        data <- cbind(data, min_width = params$min_width)
    }

    transform(data, xmin = x, xmax = x + scale*width)
  },

  draw_key = draw_key_polygon,

  handle_na = function(data, params) {
    data
  },

  draw_panel = function(self, data, panel_params, coord, ...) {
    groups <- split(data, factor(data$group))

    # sort list so highest xmin values are in the front
    # we take a shortcut here and look only at the first xmin value given
    o <- order(unlist(lapply(groups, function(data){data$xmin[1]})), decreasing = TRUE)
    groups <- groups[o]

    grobs <- lapply(groups, function(group) {
      self$draw_group(group, panel_params, coord, ...)
    })

    ggname(snake_class(self), gTree(
      children = do.call("gList", grobs)
    ))
  },

  draw_group = function(self, data, panel_params, coord, na.rm = FALSE) {
    if (na.rm) data <- data[stats::complete.cases(data[c("y", "xmin", "xmax")]), ]

    #if dataframe is empty there's nothing to draw
    if (nrow(data) == 0) return(grid::nullGrob())

    data <- data[order(data$group, data$y), ]

    # remove all points that fall below the minimum width
    data$xmax[data$width < data$min_width] <- NA

    # Check that aesthetics are constant
    aes <- unique(data[c("colour", "fill", "linewidth", "linetype", "alpha")])
    if (nrow(aes) > 1) {
      stop("Aesthetics can not vary along a ridgeline")
    }
    aes <- as.list(aes)

    # Instead of removing NA values from the data and plotting a single
    # polygon, we want to "stop" plotting the polygon whenever we're
    # missing values and "start" a new polygon as soon as we have new
    # values.  We do this by creating an id vector for polygonGrob that
    # has distinct polygon numbers for sequences of non-NA values and NA
    # for NA values in the original data.  Example: c(NA, 2, 2, 2, NA, NA,
    # 4, 4, 4, NA)
    missing_pos <- !stats::complete.cases(data[c("y", "xmin", "xmax")])
    ids <- cumsum(missing_pos) + 1
    ids[missing_pos] <- NA

    # munching for polygon
    positions <- with(data, data.frame(
      y = c(y, rev(y)),
      x = c(xmax, rev(xmin)),
      id = c(ids, rev(ids))
    ))
    munched_poly <- ggplot2::coord_munch(coord, positions, panel_params)

    # munching for line
    positions <- with(data, data.frame(
      y = y,
      x = xmax,
      id = ids
    ))
    munched_line <- ggplot2::coord_munch(coord, positions, panel_params)

    # placing the actual grob generation into a separate function allows us to override for geom_density_ridges2
    self$make_group_grob(munched_line, munched_poly, aes)
  },

  make_group_grob = function(munched_line, munched_poly, aes) {
    lg <- ggname("geom_ridgeline",
               grid::polylineGrob(
                 munched_line$x, munched_line$y, id = munched_line$id,
                 default.units = "native",
                 gp = grid::gpar(
                   col = aes$colour,
                   lwd = aes$linewidth * .pt,
                   lty = aes$linetype)
               ))

    ag <- ggname("geom_ridgeline",
               grid::polygonGrob(
                 munched_poly$x, munched_poly$y, id = munched_poly$id,
                 default.units = "native",
                 gp = grid::gpar(
                   fill = ggplot2::fill_alpha(aes$fill, aes$alpha),
                   lty = 0)
               ))
    grid::grobTree(ag, lg)
  }

)



