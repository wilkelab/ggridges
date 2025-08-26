#' Plot ridgelines and ridgeline plots with fill gradients along the x axis
#'
#' The geoms `geom_ridgeline_gradient` and `geom_density_ridges_gradient` work just like [`geom_ridgeline`] and [`geom_density_ridges`] except
#' that the `fill` aesthetic can vary along the x axis. Because filling with color gradients is fraught with issues,
#' these geoms should be considered experimental. Don't use them unless you really need to. Note that due to limitations
#' in R's graphics system, transparency (`alpha`) has to be disabled for gradient fills.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]. If specified and `inherit.aes = TRUE` (the
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
#' @param gradient_lwd A parameter to needed to remove rendering artifacts inside the
#'   rendered gradients. Should ideally be 0, but often needs to be around 0.5 or higher.
#' @param ... other arguments passed on to [ggplot2::layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `color = "red"` or `linewidth = 3`. They may also be parameters
#'   to the paired geom/stat.
#'
#' @examples
#' library(ggplot2)
#'
#' # Example for `geom_ridgeline_gradient()`
#' d <- data.frame(
#'   x = rep(1:5, 3) + c(rep(0, 5), rep(0.3, 5), rep(0.6, 5)),
#'   y = c(rep(0, 5), rep(1, 5), rep(3, 5)),
#'   height = c(0, 1, 3, 4, 0, 1, 2, 3, 5, 4, 0, 5, 4, 4, 1)
#' )
#' ggplot(d, aes(x, y, height = height, group = y, fill = factor(x+y))) +
#'   geom_ridgeline_gradient() +
#'   scale_fill_viridis_d(direction = -1) +
#'   theme(legend.position = 'none')
#' @importFrom ggplot2 layer
#' @export
geom_ridgeline_gradient <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE, gradient_lwd = 0.5, show.legend = NA,
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
      gradient_lwd = gradient_lwd,
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
  default_aes = aes(
    # ridgeline aesthetics
    color = "black", fill = "grey70", y = 0, linewidth = 0.5, linetype = 1,
    min_height = 0, scale = 1, alpha = NA, datatype = "ridgeline",

    # point aesthetics with default
    point_shape = 19, point_size = 1.5, point_stroke = 0.5,

    # point aesthetics, inherited
    point_colour = NULL, #point_color = NULL,
    point_fill = NULL, point_alpha = NULL,

    # vline aesthetics, all inherited
    vline_colour = NULL, #vline_color = NULL,
    vline_width = NULL, vline_linetype = NULL,
    vline_size = NULL #<- line size deprecated in ggplot2 3.4.0
  ),

  required_aes = c("x", "y", "height"),

  optional_aes = c("point_color", "vline_color", "vline_size", "vline_width"),

  extra_params = c("na.rm", "jittered_points"),

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

  draw_key = function(data, params, linewidth) {

    lwd <- min(data$linewidth, min(linewidth) / 4)

    rect_grob <- grid::rectGrob(
      width = grid::unit(1, "npc") - grid::unit(lwd, "mm"),
      height = grid::unit(1, "npc") - grid::unit(lwd, "mm"),
      gp = grid::gpar(
        col = data$colour,
        fill = data$fill,
        lty = data$linetype,
        lwd = lwd * .pt,
        linejoin = "mitre"
      ))

    # if vertical lines were drawn then we need to add them to the legend also
    if (is.null(params$quantile_lines) || !params$quantile_lines) {
      vlines_grob <- grid::nullGrob()
    }
    else {
      vlines_grob <- grid::segmentsGrob(0.5, 0.1, 0.5, 0.9,
        gp = grid::gpar(
          col = data$vline_colour %||% data$vline_color %||% data$colour,
          lwd = (data$vline_width %||% data$linewidth) * .pt,
          lty = data$vline_linetype %||% data$linetype,
          lineend = "butt"
        )
      )
    }

    # if jittered points were drawn then we need to add them to the legend also
    if (is.null(params$jittered_points) || !params$jittered_points) {
      point_grob <- grid::nullGrob()
    }
    else {
      point_grob <- grid::pointsGrob(0.5, 0.5,
        pch = data$point_shape,
        gp = grid::gpar(
          col = alpha(
            data$point_colour %||% data$point_color %||% data$colour,
            data$point_alpha %||% data$alpha
          ),
          fill = ggplot2::fill_alpha(
            data$point_fill %||% data$fill,
            data$point_alpha %||% data$alpha
          ),
          fontsize = data$point_size * .pt + data$point_stroke * .stroke / 2,
          lwd = data$point_stroke * .stroke / 2
        )
      )
    }
    grid::grobTree(rect_grob, vlines_grob, point_grob)
  },


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

  draw_group = function(self, data, panel_params, coord, na.rm = FALSE, gradient_lwd = 0.5) {
    if (na.rm) data <- data[stats::complete.cases(data[c("x", "ymin", "ymax")]), ]

    # split data into data types (ridgeline, vline, point)
    data_list <- split(data, factor(data$datatype))

    point_grob <- self$make_point_grob(data_list[["point"]], panel_params, coord)
    vline_grob <- self$make_vline_grob(data_list[["vline"]], panel_params, coord)

    data <- data_list[["ridgeline"]]

    # if the final data set is empty then we're done here
    if (is.null(data)) {
      return(grid::grobTree(vline_grob, point_grob))
    }

    # otherwise, continue. First we order the data, in preparation for polygon drawing
    data <- data[order(data$group, data$x), ]

    # remove all points that fall below the minimum height
    data$ymax[data$height < data$min_height] <- NA

    # Check that aesthetics are constant
    aes <- unique(data[c("colour", "linewidth", "linetype")])
    if (nrow(aes) > 1) {
      stop("These aesthetics can not vary along a ridgeline: color, linewidth, linetype")
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
    positions <- with(data, data.frame(
      x = x,
      y = ymax,
      id = ids
    ))
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
    positions <- with(data, data.frame(
      x = c(x, rev(x)),
      y = c(ymax, rev(ymin)),
      id = c(ids, rev(ids))
    ))
    munched_poly <- ggplot2::coord_munch(coord, positions, panel_params)

    # calculate line and area grobs
    line_grob <- self$make_line_grob(munched_line, aes)
    area_grob <- self$make_area_grob(munched_poly, aes, gradient_lwd)

    # combine everything and return
    grid::grobTree(area_grob, vline_grob, line_grob, point_grob)
  },

  make_point_grob = function(data, panel_params, coord) {
    if (is.null(data)) {
      return(grid::nullGrob())
    }
    data$y <- data$ymin
    coords <- coord$transform(data, panel_params)
    ggname("geom_ridgeline_gradient",
           grid::pointsGrob(
             coords$x, coords$y,
             pch = coords$point_shape,
             gp = grid::gpar(
               col = alpha(
                 data$point_colour %||% data$point_color %||% data$colour,
                 data$point_alpha %||% data$alpha
               ),
               fill = ggplot2::fill_alpha(
                 data$point_fill %||% data$fill,
                 data$point_alpha %||% data$alpha
               ),
               # Stroke is added around the outside of the point
               fontsize = coords$point_size * .pt + coords$point_stroke * .stroke / 2,
               lwd = coords$point_stroke * .stroke / 2
             )
           )
    )
  },

  make_vline_grob = function(data, panel_params, coord) {
    if (is.null(data)) {
      return(grid::nullGrob())
    }

    data <- check_vline_size(data)
    data <- check_size(data)

    data$xend <- data$x
    data$y <- data$ymin
    data$yend <- data$ymax
    data$alpha <- NA

    # copy vline aesthetics over if set
    data$colour <- data$vline_colour %||% data$vline_color %||% data$colour
    data$linetype <- data$vline_linetype %||% data$linetype
    data$linewidth <- data$vline_width %||% data$linewidth
    ggplot2::GeomSegment$draw_panel(data, panel_params, coord)
  },

  make_line_grob = function(munched_line, aes) {
    ggname("geom_ridgeline_gradient",
           grid::polylineGrob(
             munched_line$x, munched_line$y, id = munched_line$id,
             default.units = "native",
             gp = grid::gpar(
               col = aes$colour,
               lwd = aes$linewidth * .pt,
               lty = aes$linetype)
           )
    )
  },

  make_area_grob = function(munched_poly, aes, gradient_lwd) {
    ggname("geom_ridgeline_gradient",
           grid::polygonGrob(
             munched_poly$x, munched_poly$y, id = munched_poly$id,
             default.units = "native",
             gp = grid::gpar(
               fill = aes$fill,
               col = aes$fill,  # we need to draw polygons with colored outlines
               lwd = gradient_lwd,       # to prevent drawing artifacts at polygon boundaries
               lty = 1)
           )
    )
  },

  make_group_grob_delete = function(munched_line, munched_poly, aes, gradient_lwd) {
    lg <- ggname("geom_ridgeline_gradient",
               grid::polylineGrob(
                 munched_line$x, munched_line$y, id = munched_line$id,
                 default.units = "native",
                 gp = grid::gpar(
                   col = aes$colour,
                   lwd = aes$linewidth * .pt,
                   lty = aes$linetype)
               ))

    ag <- ggname("geom_ridgeline_gradient",
               grid::polygonGrob(
                 munched_poly$x, munched_poly$y, id = munched_poly$id,
                 default.units = "native",
                 gp = grid::gpar(
                   fill = aes$fill,
                   col = aes$fill,  # we need to draw polygons with colored outlines
                   lwd = gradient_lwd,       # to prevent drawing artifacts at polygon boundaries
                   lty = 1)
               ))
    grid::grobTree(ag, lg)
  }

)



#' @param panel_scaling Argument only to `geom_density_ridges_gradient`. If `TRUE`, the default, relative scaling is calculated separately
#' for each panel. If `FALSE`, relative scaling is calculated globally.
#'
#' @rdname geom_ridgeline_gradient
#' @importFrom ggplot2 layer
#' @export
geom_density_ridges_gradient <- function(mapping = NULL, data = NULL, stat = "density_ridges",
                                         position = "points_sina", panel_scaling = TRUE,
                                         na.rm = TRUE, gradient_lwd = 0.5, show.legend = NA,
                                         inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDensityRidgesGradient,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      gradient_lwd = gradient_lwd,
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
#' # Example for `geom_density_ridges_gradient()`
#' ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = `Month`, fill = stat(x))) +
#'   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
#'   scale_x_continuous(expand = c(0, 0)) +
#'   scale_y_discrete(expand = c(0, 0)) +
#'   scale_fill_viridis_c(name = "Temp. [F]", option = "C") +
#'   coord_cartesian(clip = "off") +
#'   labs(title = 'Temperatures in Lincoln NE in 2016') +
#'   theme_ridges(font_size = 13, grid = TRUE) +
#'   theme(axis.title.y = element_blank())
#' @export
GeomDensityRidgesGradient <- ggproto("GeomDensityRidgesGradient", GeomRidgelineGradient,
  default_aes = aes(
    # ridgeline aesthetics
    color = "black", fill = "grey70", linewidth = 0.5, linetype = 1,
    rel_min_height = 0, scale = 1.8, alpha = NA, datatype = "ridgeline",

    # point aesthetics with default
    point_shape = 19, point_size = 1.5, point_stroke = 0.5,

    # point aesthetics, inherited
    point_colour = NULL,# point_color = NULL,
    point_fill = NULL, point_alpha = NULL,

    # vline aesthetics, all inherited
    vline_colour = NULL,# vline_color = NULL,
    vline_width = NULL, vline_linetype = NULL,
    vline_size = NULL #<- line size deprecated in ggplot2 3.4.0
  ),

  required_aes = c("x", "y", "height"),

  optional_aes = c("point_color", "vline_color", "vline_size", "vline_width"),

  extra_params = c("na.rm", "panel_scaling", "jittered_points"),

  setup_data = function(self, data, params) {

    params <- check_vline_size_param(params)
    params <- check_size_param(params)

    # provide default for panel scaling parameter if it doesn't exist,
    # happens if the geom is called from a stat
    if (is.null(params$panel_scaling)) {
      params$panel_scaling <- TRUE
    }

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
