#' Create a discrete scale that cycles between values
#'
#' The readability of ridgeline plots can often be improved by alternating between fill colors and
#' other aesthetics. The various cyclical scales make it easy to create plots with this feature,
#' simply map your grouping variable to the respective aesthetic (e.g., `fill`) and then use
#' `scale_fill_cyclical` to define the fill colors between you want to alternate. Note that the
#' cyclical scales do not draw legends by default, because the legends will usually be wrong
#' unless the labels are properly adjusted. To draw legends, set the `guide` argument to `"legend"`,
#' as shown in the examples.
#'
#' @param values The aesthetic values that the scale should cycle through, e.g. colors if it is
#'   a scale for the color or fill aesthetic.
#' @param ... Common discrete scale parameters: `name`, `breaks`, `labels`, `na.value`, `limits` and `guide`.
#'   See [ggplot2::discrete_scale] for more details.
#'
#' @examples
#' library(ggplot2)
#'
#' # By default, scale_cyclical sets `guide = "none"`, i.e., no legend
#' # is drawn
#' ggplot(diamonds, aes(x = price, y = cut, fill = cut)) +
#'   geom_density_ridges(scale = 4) +
#'   scale_fill_cyclical(values = c("#3030D0", "#9090F0"))
#'
#' # However, legends can be turned on by setting `guide = "legend"`
#' ggplot(diamonds, aes(x = price, y = cut, fill = cut)) +
#'   geom_density_ridges(scale = 4) +
#'   scale_fill_cyclical(values = c("#3030D0", "#9090F0"),
#'                       guide = "legend", name = "Fill colors",
#'                       labels = c("dark blue", "light blue"))
#'
#' # Cyclical scales are also available for the various other aesthetics
#' ggplot(diamonds, aes(x = price, y = cut, fill = cut,
#'                      color = cut, linewidth = cut,
#'                      alpha = cut, linetype = cut)) +
#'   geom_density_ridges(scale = 4, fill = "blue") +
#'   scale_fill_cyclical(values = c("blue", "green")) +
#'   scale_color_cyclical(values = c("black", "white")) +
#'   scale_alpha_cyclical(values = c(0.4, 0.8)) +
#'   scale_linewidth_cyclical(values = c(2, 1)) +
#'   scale_linetype_cyclical(values = c(1, 2))
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

#' @rdname scale_cyclical
#' @export
scale_alpha_cyclical <- function(..., values) {
  cyclical_scale("alpha", values, ...)
}

#' @rdname scale_cyclical
#' @export
scale_linetype_cyclical <- function(..., values) {
  cyclical_scale("linetype", values, ...)
}

#' @rdname scale_cyclical
#' @export
scale_size_cyclical <- function(..., values) {
  cyclical_scale("size", values, ...)
}

#' @rdname scale_cyclical
#' @export
scale_linewidth_cyclical <- function(..., values) {
  cyclical_scale("linewidth", values, ...)
}



#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto ScaleDiscrete
#' @rdname scale_cyclical
#' @export
cyclical_scale <- function(aesthetics, values, name = waiver(),
                           breaks = waiver(), labels = waiver(), limits = NULL, expand = waiver(),
                           na.translate = TRUE, na.value = NA, drop = TRUE,
                           guide = "none", position = "left") {

  check_breaks_labels(breaks, labels)

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
    # scale_name = "cyclical", # deprecated
    palette = pal,

    range = discrete_range(),
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
    return(na.omit(breaks[1:self$cycle_length]))
  },

  get_labels = function(self, breaks = self$get_breaks()){
    labels <- ggproto_parent(ScaleDiscrete, self)$get_labels(breaks)
    return(na.omit(labels[1:self$cycle_length]))
  }
)


# internal functions, copied over from ggplot2
check_breaks_labels <- function(breaks, labels) {
  if (is.null(breaks)) return(TRUE)
  if (is.null(labels)) return(TRUE)

  bad_labels <- is.atomic(breaks) && is.atomic(labels) &&
    length(breaks) != length(labels)
  if (bad_labels) {
    stop("`breaks` and `labels` must have the same length", call. = FALSE)
  }

  TRUE
}
