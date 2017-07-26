#' Create a discrete scale that cycles between values
#'
#' @examples
#' ggplot(mtcars, aes(mpg, wt, color = factor(carb))) +
#'   geom_point() +
#'   scale_color_cyclical(values = c("blue", "green"))
#'
#'
#' @name scale_cyclical
#' @aliases NULL
NULL

#' @rdname scale_cyclical
#' @export
scale_colour_cyclical <- function(..., values) {
  cyclical_scale("colour", values, ...)
}

#' @rdname scale_cyclical
#' @export
#' @usage NULL
scale_color_cyclical <- scale_colour_cyclical

#' @rdname scale_cyclical
#' @export
scale_fill_cyclical <- function(..., values) {
  cyclical_scale("fill", values, ...)
}

#' Cyclical scale constructor
#'
#' @importFrom ggplot2 ggproto ScaleDiscrete
#' @rdname scale_cyclical
#' @export
cyclical_scale <- function(aesthetics, values, name = waiver(),
                           breaks = waiver(), labels = waiver(), limits = NULL, expand = waiver(),
                           na.translate = TRUE, na.value = NA, drop = TRUE,
                           guide = "legend", position = "left") {

  ggplot2:::check_breaks_labels(breaks, labels)

  position <- match.arg(position, c("left", "right", "top", "bottom"))

  if (is.null(breaks) && !is_position_aes(aesthetics) && guide != "none") {
    guide <- "none"
  }

  # palette function
  pal <- function(n) {
    rep_len(values, n)
  }

  ggproto(NULL, ScaleCyclical,
    # standard fields of ScaleDiscrete
    call = match.call(),

    aesthetics = aesthetics,
    scale_name = "cyclical",
    palette = pal,

    range = ggplot2:::discrete_range(),
    limits = limits,
    na.value = na.value,
    na.translate = na.translate,
    expand = expand,

    name = name,
    breaks = breaks,
    labels = labels,
    drop = drop,
    guide = guide,
    position = position,

    # new fields for ScaleCyclical
    cycle_length = length(values)
  )
}


#' @rdname scale_cyclical
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto ScaleDiscrete
#' @export
ScaleCyclical <- ggproto("ScaleCyclical", ScaleDiscrete,
  get_breaks = function(self, limits = self$get_limits()){
    breaks <- ggproto_parent(ScaleDiscrete, self)$get_breaks(limits)
    return(breaks[1:self$cycle_length])
  },

  get_labels = function(self, breaks = self$get_breaks()){
    labels <- ggproto_parent(ScaleDiscrete, self)$get_labels(breaks)
    return(labels[1:self$cycle_length])
  }
)
