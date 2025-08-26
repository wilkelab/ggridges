#' Plot a ridgeline (line with filled area underneath)
#'
#' Plots the sum of the `y` and `height` aesthetics versus `x`, filling the area between `y` and `y + height` with a color.
#' Thus, the data mapped onto y and onto height must be in the same units.
#' If you want relative scaling of the heights, you can use [`geom_density_ridges`] with `stat = "identity"`.
#'
#' In addition to drawing ridgelines, this geom can also draw points if they are provided as part of the dataset.
#' The stat [`stat_density_ridges()`] takes advantage of this option to generate ridgeline plots with overlaid
#' jittered points.
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
#' * `colour` Color of the ridgeline
#' * `fill` Fill color of the area under the ridgeline
#' * `alpha` Transparency level of `fill`. Not applied to `color`. If you want transparent lines, you can set their
#'   color as RGBA value, e.g. #FF0000A0 for partially transparent red.
#' * `group` Grouping, to draw multiple ridgelines from one dataset
#' * `linetype` Linetype of the ridgeline
#' * `linewidth` Line thickness
#' * `point_shape`, `point_colour`, `point_size`, `point_fill`, `point_alpha`, `point_stroke` Aesthetics applied
#' to points drawn in addition to ridgelines.
#'
#' @examples
#' library(ggplot2)
#'
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
#' @importFrom ggplot2 ggproto Geom
#' @export
GeomRidgeline <- ggproto("GeomRidgeline", Geom,
  default_aes = aes(
    # ridgeline aesthetics
    color = "black", fill = "grey70", y = 0, linewidth = 0.5, linetype = 1,
    min_height = 0, scale = 1, alpha = NA, datatype = "ridgeline",

    # point aesthetics with default
    point_shape = 19, point_size = 1.5, point_stroke = 0.5,

    # point aesthetics, inherited
    point_colour = NULL,# point_color = NULL,
    point_fill = NULL, point_alpha = NULL,

    # vline aesthetics, all inherited
    vline_colour = NULL, #vline_color = NULL,
    vline_width = NULL, vline_linetype = NULL,
    vline_size = NULL #<- line size deprecated in ggplot2 3.4.0
  ),

  required_aes = c("x", "y", "height"),

  optional_aes = c("point_color", "vline_color", "vline_width", "vline_size"),

  extra_params = c("na.rm", "jittered_points"),

  setup_data = function(self, data, params) {

    params <- check_vline_size_param(params)
    params <- check_size_param(params)

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

    data <- check_vline_size(data)
    data <- check_size(data)

    lwd <- min(data$linewidth, min(linewidth) / 4)

    rect_grob <- grid::rectGrob(
      width = grid::unit(1, "npc") - grid::unit(lwd, "mm"),
      height = grid::unit(1, "npc") - grid::unit(lwd, "mm"),
      gp = grid::gpar(
        col = data$colour,
        fill = ggplot2::fill_alpha(data$fill, data$alpha),
        lty = data$linetype,
        lwd = lwd * .pt,
        linejoin = "mitre"
      )
    )

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

  draw_group = function(self, data, panel_params, coord, na.rm = FALSE) {
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
    missing_pos <- !stats::complete.cases(data[c("x", "ymin", "ymax")])
    ids <- cumsum(missing_pos) + 1
    ids[missing_pos] <- NA

    # munching for polygon
    positions <- with(data, data.frame(
      x = c(x, rev(x)),
      y = c(ymax, rev(ymin)),
      id = c(ids, rev(ids))
    ))
    munched_poly <- ggplot2::coord_munch(coord, positions, panel_params)

    # munching for line
    positions <- with(data, data.frame(
      x = x,
      y = ymax,
      id = ids
    ))
    munched_line <- ggplot2::coord_munch(coord, positions, panel_params)

    # calculate line and area grobs
    line_grob <- self$make_line_grob(munched_line, munched_poly, aes)
    area_grob <- self$make_area_grob(munched_poly, aes)

    # combine everything and return
    grid::grobTree(area_grob, vline_grob, line_grob, point_grob)
  },


  make_point_grob = function(data, panel_params, coord) {
    if (is.null(data)) {
      return(grid::nullGrob())
    }
    data$y <- data$ymin
    coords <- coord$transform(data, panel_params)
    ggname("geom_ridgeline",
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

  make_line_grob = function(munched_line, munched_poly, aes) {
    ggname("geom_ridgeline",
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

  make_area_grob = function(munched_poly, aes) {
    ggname("geom_ridgeline",
           grid::polygonGrob(
             munched_poly$x, munched_poly$y, id = munched_poly$id,
             default.units = "native",
             gp = grid::gpar(
               fill = ggplot2::fill_alpha(aes$fill, aes$alpha),
               lty = 0)
             )
           )
  }


)



#' Create ridgeline plot
#'
#' `geom_density_ridges` arranges multiple density plots in a staggered fashion, as in the cover of the famous Joy Division album Unknown Pleasures.
#'
#' By default, this geom calculates densities from the point data mapped onto the x axis. If density calculation is
#' not wanted, use `stat="identity"` or use [`geom_ridgeline`]. The difference between `geom_density_ridges` and [`geom_ridgeline`]
#' is that `geom_density_ridges` will provide automatic scaling of the ridgelines (controlled by the `scale` aesthetic), whereas
#' [geom_ridgeline] will plot the data as is. Note that when you set `stat="identity"`, the `height` aesthetic must
#' be provided.
#'
#' Note that the default [`stat_density_ridges`] makes joint density estimation across all datasets. This may not generate
#' the desired result when using faceted plots. As an alternative, you can set `stat = "density"` to use
#' [ggplot2::stat_density].
#' In this case, it is required to add the aesthetic mapping `height = after_stat(density)` (see examples).
#'
#' @param panel_scaling If `TRUE`, the default, relative scaling is calculated separately
#' for each panel. If `FALSE`, relative scaling is calculated globally.
#' @inheritParams geom_ridgeline
#'
#' @section Aesthetics:
#'
#' Required aesthetics are in bold.
#'
#' * **`x`**
#' * **`y`**
#' * `weight` Optional case weights passed to `stats::density` to calculate a weighted density estimate
#' * `group` Defines the grouping. Not needed if a categorical variable is mapped onto `y`, but needed otherwise. Will typically be the same
#' variable as is mapped to `y`.
#' * `height` The height of each ridgeline at the respective x value. Automatically calculated and
#' provided by [`stat_density_ridges`] if the default stat is not changed.
#' * `scale` A scaling factor to scale the height of the ridgelines relative to the spacing between them.
#' A value of 1 indicates that the maximum point of any ridgeline touches the baseline right above, assuming
#' even spacing between baselines.
#' * `rel_min_height` Lines with heights below this cutoff will be removed. The cutoff is measured relative to the
#' overall maximum, so `rel_min_height=0.01` would remove everything that is 1\% or less than the highest point among all
#' ridgelines. Default is 0, so nothing is removed.
#' alpha
#' * `colour`, `fill`, `group`, `alpha`, `linetype`, `linewidth`, as in [`geom_ridgeline`].
#' * `point_shape`, `point_colour`, `point_size`, `point_fill`, `point_alpha`, `point_stroke`, as in [`geom_ridgeline`].
#'
#' @importFrom ggplot2 layer
#' @export
#' @examples
#' library(ggplot2)
#'
#' # set the `rel_min_height` argument to remove tails
#' ggplot(iris, aes(x = Sepal.Length, y = Species)) +
#'   geom_density_ridges(rel_min_height = 0.005) +
#'   scale_y_discrete(expand = c(0.01, 0)) +
#'   scale_x_continuous(expand = c(0.01, 0)) +
#'   theme_ridges()
#'
#' # set the `scale` to determine how much overlap there is among the plots
#' ggplot(diamonds, aes(x = price, y = cut)) +
#'   geom_density_ridges(scale = 4) +
#'   scale_y_discrete(expand = c(0.01, 0)) +
#'   scale_x_continuous(expand = c(0.01, 0)) +
#'   theme_ridges()
#'
#' # the same figure with colors, and using the ggplot2 density stat
#' ggplot(diamonds, aes(x = price, y = cut, fill = cut, height = after_stat(density))) +
#'   geom_density_ridges(scale = 4, stat = "density") +
#'   scale_y_discrete(expand = c(0.01, 0)) +
#'   scale_x_continuous(expand = c(0.01, 0)) +
#'   scale_fill_brewer(palette = 4) +
#'   theme_ridges() + theme(legend.position = "none")
geom_density_ridges <- function(mapping = NULL, data = NULL, stat = "density_ridges",
                                position = "points_sina", panel_scaling = TRUE,
                                na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDensityRidges,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      panel_scaling = panel_scaling,
      ...
    )
  )
}

#' @rdname geom_density_ridges
#' @format NULL
#' @usage NULL
#' @importFrom grid gTree gList
#' @export
GeomDensityRidges <- ggproto("GeomDensityRidges", GeomRidgeline,
  default_aes = aes(
    # ridgeline aesthetics
    color = "black", fill = "grey70", linewidth = 0.5, linetype = 1,
    rel_min_height = 0, scale = 1.8, alpha = NA, datatype = "ridgeline",

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

  optional_aes = c("point_color", "vline_color", "vline_size", "vline_width", "weight"),

  extra_params = c("na.rm", "panel_scaling"),

  setup_data = function(self, data, params) {

    # check for size deprecation
    params <- check_vline_size_param(params)
    params <- check_size(params)

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

    # warn for vline_size or size arg
    data <- check_vline_size(data)
    data <- check_size(data)

    transform(data,
              ymin = y,
              ymax = y + iscale*scale*height,
              min_height = hmax*rel_min_height)
  }
)


#' `geom_density_ridges2` is identical to `geom_density_ridges` except it draws closed polygons rather than ridgelines.
#'
#' @rdname geom_density_ridges
#' @importFrom ggplot2 layer
#' @export
#' @examples
#'
#' # use geom_density_ridges2() instead of geom_density_ridges() for solid polygons
#' ggplot(iris, aes(x = Sepal.Length, y = Species)) +
#'   geom_density_ridges2() +
#'   scale_y_discrete(expand = c(0.01, 0)) +
#'   scale_x_continuous(expand = c(0.01, 0)) +
#'   theme_ridges()
geom_density_ridges2 <- function(mapping = NULL, data = NULL, stat = "density_ridges",
                      position = "points_sina", panel_scaling = TRUE,
                      na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDensityRidges2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      panel_scaling = panel_scaling,
      ...
    )
  )
}

#' @rdname geom_density_ridges
#' @format NULL
#' @usage NULL
#' @export
GeomDensityRidges2 <- ggproto("GeomDensityRidges2", GeomDensityRidges,
  make_line_grob = function(munched_line, munched_poly, aes) {
    grid::nullGrob()
  },

  make_area_grob = function(munched_poly, aes) {
    ggname("geom_density_ridges2",
           grid::polygonGrob(
             munched_poly$x, munched_poly$y, id = munched_poly$id,
             default.units = "native",
             gp = grid::gpar(
             fill = ggplot2::fill_alpha(aes$fill, aes$alpha),
             col = aes$colour,
             lwd = aes$linewidth * .pt,
             lty = aes$linetype)
           ))
  }
)
