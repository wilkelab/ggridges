#' Plot a ridgeline (line with filled area underneath)
#'
#' Plots the sum of the `y` and `height` aesthetics versus `x`, filling the area between `y` and `y + height` with a color.
#' Thus, the data mapped onto y and onto height must be in the same units.
#' If you want relative scaling of the heights, you can use [geom_joy] with `stat = "identity"`.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()] or
#'   [ggplot2::aes_()]. If specified and `inherit.aes = TRUE` (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply `mapping` if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to [ggplot2::ggplot()].
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
#'   `color = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#'
#' @section Aesthetics:
#'
#' Required aesthetics are in bold.
#'
#' * **`x`**
#' * **`y`**
#' * **`height`** Height of the ridgeline, measured from the respective `y` value. Assumed to be positive, though this is not required.
#' * `group` Defines the grouping. Required when the dataset contains multiple distinct ridgelines. Will typically be the same
#' variable as is mapped to `y`.
#' * `scale` A scaling factor to scale the height of the ridgelines.
#' A value of 1 indicates that the heights are taken as is. This aesthetic can be used to convert
#' `height` units into `y` units.
#' * `min_height` A height cutoff on the drawn ridgelines. All values that fall below this cutoff will be removed.
#' The main purpose of this cutoff is to remove long tails right at the baseline level, but other uses are possible.
#' The cutoff is applied before any height
#' scaling is applied via the `scale` aesthetic. Default is 0, so negative values are removed.
#' * `color` Color of the ridgeline
#' * `fill` Fill color of the area under the ridgeline
#' * `group` Grouping, to draw multiple ridgelines from one dataset
#' * `linetype` Linetype of the ridgeline
#' * `size` Line thickness
#'
#' @examples
#' d <- data.frame(x = rep(1:5, 3), y = c(rep(0, 5), rep(1, 5), rep(3, 5)),
#'                 height = c(0, 1, 3, 4, 0, 1, 2, 3, 5, 4, 0, 5, 4, 4, 1))
#' ggplot(d, aes(x, y, height = height, group = y)) + geom_ridgeline(fill="lightblue")
#'
#' @importFrom ggplot2 layer
#' @export
geom_ridgeline <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRidgeline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_ridgeline
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Geom draw_key_polygon
#' @export
GeomRidgeline <- ggproto("GeomRidgeline", Geom,
  default_aes = aes(color = "black", fill = "grey80", y = 0, size = 0.5, linetype = 1,
        min_height = 0, scale = 1, alpha = NA),

  required_aes = c("x", "y", "height"),

  setup_data = function(self, data, params) {

    if (!"scale" %in% names(data)) {
      if (!"scale" %in% names(params))
        data <- cbind(data, scale = self$default_aes$scale)
      else
        data <- cbind(data, scale = params$scale)
    }

    if (!"min_height" %in% names(data)){
      if (!"min_height" %in% names(params))
        data <- cbind(data, min_height = self$default_aes$min_height)
      else
        data <- cbind(data, min_height = params$min_height)
    }

    transform(data, ymin = y, ymax = y + scale*height)
  },

  draw_key = draw_key_polygon,

  handle_na = function(data, params) {
    data
  },

  draw_panel = function(self, data, panel_params, coord, ...) {
    groups <- split(data, factor(data$group))

    # sort list so highest ymin values are in the front
    # we take a shortcut here and look only at the first ymin value given
    o <- order(unlist(lapply(groups, function(data){data$ymin[1]})), decreasing = TRUE)
    groups <- groups[o]

    grobs <- lapply(groups, function(group) {
      self$draw_group(group, panel_params, coord, ...)
    })

    ggname(snake_class(self), gTree(
      children = do.call("gList", grobs)
    ))
  },

  draw_group = function(self, data, panel_params, coord, na.rm = FALSE) {
    if (na.rm) data <- data[stats::complete.cases(data[c("x", "ymin", "ymax")]), ]
    data <- data[order(data$group, data$x), ]

    # remove all points that fall below the minimum height
    data$ymax[data$height < data$min_height] <- NA

    # Check that aesthetics are constant
    aes <- unique(data[c("colour", "fill", "size", "linetype", "alpha")])
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
    missing_pos <- !stats::complete.cases(data[c("x", "ymin", "ymax")])
    ids <- cumsum(missing_pos) + 1
    ids[missing_pos] <- NA

    # munching for polygon
    positions <- plyr::summarise(data,
                                 x = c(x, rev(x)), y = c(ymax, rev(ymin)), id = c(ids, rev(ids)))
    munched_poly <- ggplot2::coord_munch(coord, positions, panel_params)

    # munching for line
    positions <- plyr::summarise(data, x = x, y = ymax, id = ids)
    munched_line <- ggplot2::coord_munch(coord, positions, panel_params)

    # placing the actual grob generation into a separate function allows us to override for geom_joy2
    self$make_group_grob(munched_line, munched_poly, aes)
  },

  make_group_grob = function(munched_line, munched_poly, aes) {
    lg <- ggname("geom_ridgeline",
               grid::polylineGrob(
                 munched_line$x, munched_line$y, id = munched_line$id,
                 default.units = "native",
                 gp = grid::gpar(
                   col = aes$colour,
                   lwd = aes$size * .pt,
                   lty = aes$linetype)
               ))

    ag <- ggname("geom_ridgeline",
               grid::polygonGrob(
                 munched_poly$x, munched_poly$y, id = munched_poly$id,
                 default.units = "native",
                 gp = grid::gpar(
                   fill = ggplot2::alpha(aes$fill, aes$alpha),
                   lty = 0)
               ))
    grid::grobTree(ag, lg)
  }

)



#' Create joyplot
#'
#' `geom_joy` arranges multiple density plots in a staggered fashion, as in the cover of the famous Joy Division album Unknown Pleasures.
#'
#' By default, this geom calculates densities from the point data mapped onto the x axis. If density calculation is
#' not wanted, use `stat="identity"` or use [geom_ridgeline]. The difference between `geom_joy` and [geom_ridgeline]
#' is that `geom_joy` will provide automatic scaling of the ridgelines (controlled by the `scale` aesthetic), whereas
#' [geom_ridgeline] will plot the data as is. Note that when you set `stat="identity"`, the `height` aesthetic must
#' be provided.
#'
#' Note that the default [stat_joy] makes joint density estimation across all datasets. This may not generate
#' the desired result when using faceted plots. As an alternative, you can set `stat = "density"` to use [stat_density].
#' In this case, it is required to add the aesthetic mapping `height = ..density..` (see examples).
#'
#' @inheritParams geom_ridgeline
#' @param panel_scaling Should scaling be calculated separately for each panel (default) or
#'  (if set to `FALSE`) globally.
#'
#' @section Aesthetics:
#'
#' Required aesthetics are in bold.
#'
#' * **`x`**
#' * **`y`**
#' * `group` Defines the grouping. Not needed if a categorical variable is mapped onto `y`, but needed otherwise. Will typically be the same
#' variable as is mapped to `y`.
#' * `height` The height of each ridgeline at the respective x value. Automatically calculated and
#' provided by [stat_joy] if the default stat is not changed.
#' * `scale` A scaling factor to scale the height of the ridgelines relative to the spacing between them.
#' A value of 1 indicates that the maximum point of any ridgeline touches the baseline right above, assuming
#' even spacing between baselines.
#' * `rel_min_height` Lines with heights below this cutoff will be removed. The cutoff is measured relative to the
#' overall maximum, so `rel_min_height=0.01` would remove everything that is 1\% or less than the highest point among all
#' ridgelines. Default is 0, so nothing is removed.
#' alpha
#' * `color`, `fill`, `group`, `linetype`, `size`, as in [geom_ridgeline].
#'
#' @importFrom ggplot2 layer
#' @export
#' @examples
#' # set the `rel_min_height` argument to remove tails
#' ggplot(iris, aes(x = Sepal.Length, y = Species)) +
#'   geom_joy(rel_min_height = 0.005) +
#'   scale_y_discrete(expand = c(0.01, 0)) +
#'   scale_x_continuous(expand = c(0.01, 0)) +
#'   theme_joy()
#'
#' # set the `scale` to determine how much overlap there is among the plots
#' ggplot(diamonds, aes(x = price, y = cut)) +
#'   geom_joy(scale = 4) +
#'   scale_y_discrete(expand=c(0.01, 0)) +
#'   scale_x_continuous(expand=c(0.01, 0)) +
#'   theme_joy()
#'
#' # the same figure with colors, and using the ggplot2 density stat
#' ggplot(diamonds, aes(x = price, y = cut, fill = cut, height = ..density..)) +
#'   geom_joy(scale = 4, stat = "density") +
#'   scale_y_discrete(expand = c(0.01, 0)) +
#'   scale_x_continuous(expand = c(0.01, 0)) +
#'   scale_fill_brewer(palette = 4) +
#'   theme_joy() + theme(legend.position = "none")
#' \donttest{
#' # evolution of movie lengths over time
#' # requires the ggplot2movies package
#' library(ggplot2movies)
#' ggplot(movies[movies$year>1912,], aes(x = length, y = year, group = year)) +
#'   geom_joy(scale = 10, size = 0.25, rel_min_height = 0.03) +
#'   theme_joy() +
#'   scale_x_continuous(limits = c(1, 200), expand = c(0.01, 0)) +
#'   scale_y_reverse(breaks = c(2000, 1980, 1960, 1940, 1920, 1900), expand = c(0.01, 0))
#' }
geom_joy <- function(mapping = NULL, data = NULL, stat = "joy",
                     panel_scaling = TRUE,
                     na.rm = TRUE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomJoy,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      panel_scaling = panel_scaling,
      ...
    )
  )
}

#' @rdname geom_joy
#' @format NULL
#' @usage NULL
#' @importFrom grid gTree gList
#' @export
GeomJoy <- ggproto("GeomJoy", GeomRidgeline,
  default_aes =
    aes(color = "black",
        fill = "grey70",
        size = 0.5,
        linetype = 1,
        alpha = NA,
        scale = 1.8,
        rel_min_height = 0),

   required_aes = c("x", "y", "height"),

   extra_params = c("na.rm", "panel_scaling"),

   setup_data = function(self, data, params) {
     # calculate internal scale
     yrange = max(data$y) - min(data$y)
     n = length(unique(data$y))
     if (n<2) {
       hmax <- max(data$height, na.rm = TRUE)
       iscale <- 1
     }
     else {
       # scale per panel or globally?
       if (params$panel_scaling) {
         heights <- split(data$height, data$PANEL)
         max_heights <- vapply(heights, max, numeric(1), na.rm = TRUE)
         hmax <- max_heights[data$PANEL]
         iscale <- yrange/((n-1)*hmax)
       }
       else {
         hmax <- max(data$height, na.rm = TRUE)
         iscale <- yrange/((n-1)*hmax)
       }
     }

     #print(iscale)
     #print(hmax)

     data <- cbind(data, iscale)

     if (!"scale" %in% names(data)) {
       if (!"scale" %in% names(params))
         data <- cbind(data, scale = self$default_aes$scale)
       else
         data <- cbind(data, scale = params$scale)
     }

     if (!"rel_min_height" %in% names(data)){
       if (!"rel_min_height" %in% names(params))
         data <- cbind(data, rel_min_height = self$default_aes$rel_min_height)
       else
         data <- cbind(data, rel_min_height = params$rel_min_height)
     }

     transform(data,
               ymin = y,
               ymax = y + iscale*scale*height,
               min_height = hmax*rel_min_height)
  }
)


#' \code{geom_joy2} is identical to \code{geom_joy} except it draws closed polygons rather than ridgelines.
#'
#' @rdname geom_joy
#' @importFrom ggplot2 layer
#' @export
#' @examples
#'
#' # use geom_joy2() instead of geom_joy() for solid polygons
#' ggplot(iris, aes(x = Sepal.Length, y = Species)) +
#'   geom_joy2() +
#'   scale_y_discrete(expand = c(0.01, 0)) +
#'   scale_x_continuous(expand = c(0.01, 0)) +
#'   theme_joy()
geom_joy2 <- function(mapping = NULL, data = NULL, stat = "joy",
                      panel_scaling = TRUE,
                      na.rm = TRUE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomJoy2,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      panel_scaling = panel_scaling,
      ...
    )
  )
}

#' @rdname geom_joy
#' @format NULL
#' @usage NULL
#' @export
GeomJoy2 <- ggproto("GeomJoy2", GeomJoy,
  make_group_grob = function(munched_line, munched_poly, aes) {
    ggname("geom_joy2",
           grid::polygonGrob(
             munched_poly$x, munched_poly$y, id = munched_poly$id,
             default.units = "native",
             gp = grid::gpar(
             fill = ggplot2::alpha(aes$fill, aes$alpha),
             col = aes$colour,
             lwd = aes$size * .pt,
             lty = aes$linetype)
           ))
  }
)
