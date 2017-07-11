#' Plot a ridgeline (line with filled area underneath)
#'
#' @examples
#'
#' d <- data.frame(x = 1:5, y = c(2, 1, 3, 4,0))
#' ggplot(d, aes(x, y)) + geom_ridgeline()
#'
#' @export
geom_ridgeline <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "stack", na.rm = FALSE, show.legend = NA,
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

#' @export
GeomRidgeline <- ggproto("GeomRidgeline", GeomRibbon,
  default_aes = plyr::defaults(
    aes(colour = "blue", fill = "grey80", size = 0.5, linetype = 1, alpha = NA),
    GeomRibbon$default_aes
  ),

  required_aes = c("x", "y"),

  setup_data = function(data, params) {
    transform(data, ymin = 0, ymax = y)
  },

  draw_group = function(data, panel_params, coord, na.rm = FALSE) {
    if (na.rm) data <- data[stats::complete.cases(data[c("x", "ymin", "ymax")]), ]
    data <- data[order(data$group, data$x), ]

    # Check that aesthetics are constant
    aes <- unique(data[c("colour", "fill", "size", "linetype", "alpha")])
    if (nrow(aes) > 1) {
      stop("Aesthetics can not vary with a ribbon")
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
    munched_poly <- coord_munch(coord, positions, panel_params)

    # munching for line
    positions <- plyr::summarise(data, x = x, y = ymax, id = ids)
    munched_line <- coord_munch(coord, positions, panel_params)

    lg <- ggname("geom_ridgeline",
           polylineGrob(
                        munched_line$x, munched_line$y, id = munched_line$id,
                        default.units = "native",
                        gp = gpar(
                          col = aes$colour,
                          lwd = aes$size * .pt,
                          lty = aes$linetype)
                      ))

    ag <- ggname("geom_ridgeline",
                 polygonGrob(
                   munched_poly$x, munched_poly$y, id = munched_poly$id,
                   default.units = "native",
                   gp = gpar(
                     fill = alpha(aes$fill, aes$alpha),
                     lty = 0)
                 ))
    grobTree(ag, lg)
    }
)
