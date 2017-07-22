#' Plot ridgelines and joyplots with fill gradients along the x axis
#'
#' The geoms `geom_ridgeline_gradient` and `geom_joy_gradient` work just like [geom_ridgeline] and [geom_joy] except
#' that the `fill` aesthetic can vary along the x axis. Because filling with color gradients is fraught with issues,
#' these geoms should be considered experimental. Don't use them unless you really need to. Note that due to limitations
#' in R's graphics system, transparency (`alpha`) has to be disabled for gradient fills.
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
#' @examples
#' # Example for `geom_ridgeline_gradient()`
#' library(viridis)
#' d <- data.frame(x = rep(1:5, 3) + c(rep(0, 5), rep(0.3, 5), rep(0.6, 5)),
#'                 y = c(rep(0, 5), rep(1, 5), rep(3, 5)),
#'                 height = c(0, 1, 3, 4, 0, 1, 2, 3, 5, 4, 0, 5, 4, 4, 1))
#' ggplot(d, aes(x, y, height = height, group = y, fill = factor(x+y))) +
#'   geom_ridgeline_gradient() +
#'   scale_fill_viridis(discrete = TRUE, direction = -1) +
#'   theme(legend.position = 'none')
#' @importFrom ggplot2 layer
#' @export
geom_ridgeline_gradient <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRidgelineGradient,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_ridgeline_gradient
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Geom draw_key_polygon
#' @export
GeomRidgelineGradient <- ggproto("GeomRidgelineGradient", Geom,
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

    #if dataframe is empty there's nothing to draw
    if (nrow(data) == 0) return(grid::nullGrob())

    # remove all points that fall below the minimum height
    data$ymax[data$height < data$min_height] <- NA

    # Check that aesthetics are constant
    aes <- unique(data[c("colour", "size", "linetype")])
    if (nrow(aes) > 1) {
      stop("These aesthetics can not vary along a ridgeline: color, size, linetype")
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
    data <- cbind(data, ids)
    data <- data[!missing_pos,]

    # munching for line
    positions <- plyr::summarise(data, x = x, y = ymax, id = ids)
    munched_line <- ggplot2::coord_munch(coord, positions, panel_params)

    # We now break down the polygons further by fill color, since
    # we need to draw a separate polygon for each color

    # calculate all the positions where the fill type changes
    fillchange <- c(FALSE, data$fill[2:nrow(data)] != data$fill[1:nrow(data)-1])
    # and where the id changes
    idchange <- c(TRUE, data$ids[2:nrow(data)] != data$ids[1:nrow(data)-1])

    # make new ids from all changes in fill style or original id
    data$ids <- cumsum(fillchange | idchange)
    # get fill color for all ids
    fill <- data$fill[fillchange | idchange]
    # append to aes list
    aes <- c(aes, list(fill=fill))

    # rows to be duplicated
    dupl_rows <- which(fillchange & !idchange)
    if (length(dupl_rows)>0){
      rows <- data[dupl_rows, ]
      rows$ids <- data$ids[dupl_rows-1]
      # combine original and duplicated data
      data <- rbind(data, rows)
    }

    # munching for polygon
    positions <- plyr::summarise(data,
                                 x = c(x, rev(x)), y = c(ymax, rev(ymin)), id = c(ids, rev(ids)))
    munched_poly <- ggplot2::coord_munch(coord, positions, panel_params)


    # placing the actual grob generation into a separate function allows us to override for geom_joy2
    self$make_group_grob(munched_line, munched_poly, aes)
  },

  make_group_grob = function(munched_line, munched_poly, aes) {
    lg <- ggname("geom_ridgeline_gradient",
               grid::polylineGrob(
                 munched_line$x, munched_line$y, id = munched_line$id,
                 default.units = "native",
                 gp = grid::gpar(
                   col = aes$colour,
                   lwd = aes$size * .pt,
                   lty = aes$linetype)
               ))

    ag <- ggname("geom_ridgeline_gradient",
               grid::polygonGrob(
                 munched_poly$x, munched_poly$y, id = munched_poly$id,
                 default.units = "native",
                 gp = grid::gpar(
                   fill = aes$fill,
                   col = aes$fill,  # we need to draw polygons with colored outlines
                   lwd = 0.5,       # to prevent drawing artifacts at polygon boundaries
                   lty = 1)
               ))
    grid::grobTree(ag, lg)
  }

)



#' @param panel_scaling Argument only to `geom_joy_gradient`. If `TRUE`, the default, relative scaling is calculated separately
#' for each panel. If `FALSE`, relative scaling is calculated globally.
#'
#' @rdname geom_ridgeline_gradient
#' @importFrom ggplot2 layer
#' @export
geom_joy_gradient <- function(mapping = NULL, data = NULL, stat = "joy",
                     panel_scaling = TRUE,
                     na.rm = TRUE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomJoyGradient,
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

#' @rdname geom_ridgeline_gradient
#' @format NULL
#' @usage NULL
#' @importFrom grid gTree gList
#' @examples
#'
#' # Example for `geom_joy_gradient()`
#' ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = `Month`, fill = ..x..)) +
#'   geom_joy_gradient(scale = 3, rel_min_height = 0.01) +
#'   scale_x_continuous(expand = c(0.01, 0)) +
#'   scale_y_discrete(expand = c(0.01, 0)) +
#'   scale_fill_viridis(name = "Temp. [F]", option = "C") +
#'   labs(title = 'Temperatures in Lincoln NE in 2016') +
#'   theme_joy(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())
#' @export
GeomJoyGradient <- ggproto("GeomJoyGradient", GeomRidgelineGradient,
  default_aes =
    aes(color = "black",
        fill = "grey70",
        size = 0.5,
        linetype = 1,
        scale = 1.8,
        rel_min_height = 0,
        alpha = NA),

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
