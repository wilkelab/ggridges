# Code for stat_joy based on stat_density_common in the "extending ggplot2" vignette

#' Stat for density joyplots
#'
#' This stat is the default stat used by [geom_joy]. It is very similar to [ggplot2::stat_density],
#' however there are a few differences. Most importantly, the density bandwidth is chosen across
#' the entire dataset.
#'
#' @param geom The geometric object to use to display the data.
#' @param bandwidth Bandwidth used for density calculation. If not provided, is estimated from the data.
#' @param from,to The left and right-most points of the grid at which the density is to be estimated,
#'   as in [density()]. If not provided, there are estimated from the data range and the bandwidth.
#' @inheritParams geom_ridgeline
#' @importFrom ggplot2 layer
#' @export
stat_joy <- function(mapping = NULL, data = NULL, geom = "joy",
                     position = "identity", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE, bandwidth = NULL, from = NULL, to = NULL, ...)
{
  layer(
    stat = StatJoy,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(bandwidth = bandwidth,
                  from = from,
                  to = to,
                  na.rm = na.rm, ...)
  )
}


#' @rdname stat_joy
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @export
StatJoy <- ggproto("StatJoy", Stat,
  required_aes = "x",
  default_aes = aes(height = ..density..),

  calc_panel_params = function(data, params) {
    if (is.null(params$bandwidth)) {
      xdata <- na.omit(data.frame(x=data$x, group=data$group))
      xs <- split(xdata$x, xdata$group)
      xs_mask <- vapply(xs, length, numeric(1)) > 1
      bws <- vapply(xs[xs_mask], bw.nrd0, numeric(1))
      bw <- mean(bws, na.rm = TRUE)
      message("Picking joint bandwidth of ", signif(bw, 3))

      params$bandwidth <- bw
    }

    if (is.null(params$from)) {
      params$from <- min(data$x, na.rm=TRUE) - 3 * params$bandwidth
    }

    if (is.null(params$to)) {
      params$to <- max(data$x, na.rm=TRUE) + 3 * params$bandwidth
    }

    data.frame(
      bandwidth = params$bandwidth,
      from = params$from,
      to = params$to
    )
  },

  setup_params = function(self, data, params) {
    # calculate bandwidth, min, and max for each panel separately
    panels <- split(data, data$PANEL)
    pardata <- lapply(panels, self$calc_panel_params, params)
    pardata <- purrr::reduce(pardata, rbind)

    list(
      bandwidth = pardata$bandwidth,
      from = pardata$from,
      to = pardata$to,
      na.rm = params$na.rm
    )
  },

  compute_group = function(data, scales, from, to, bandwidth = 1) {
    # ignore too small groups
    if(nrow(data) < 3) return(data.frame())

    panel <- unique(data$PANEL)
    if (length(panel) > 1) {
      stop("Error: more than one panel in compute group; something's wrong.")
    }
    panel_id <- as.numeric(panel)

    d <- density(data$x, bw = bandwidth[panel_id], from = from[panel_id], to = to[panel_id], na.rm = TRUE)
    data.frame(x = d$x, density = d$y)
  }
)


#' Stat for histogram joyplots
#'
#' Works like `stat_bin` except that the output is a ridgeline describing the histogram rather than
#' a set of counts.
#'
#' @param draw_baseline If `FALSE`, removes lines along 0 counts. Defaults to `TRUE`.
#' @param pad If `TRUE`, adds empty bins at either end of x. This ensures that the binline always goes
#'   back down to 0. Defaults to `TRUE`.
#' @inheritParams ggplot2::stat_bin
#'
#' @examples
#' ggplot(iris, aes(x = Sepal.Length, y = Species, group = Species, fill = Species)) +
#'   geom_joy(stat = "binline", bins = 20, scale = 2.2) +
#'   scale_y_discrete(expand = c(0.01, 0)) +
#'   scale_x_continuous(expand = c(0.01, 0)) +
#'   theme_joy()
#'
#' ggplot(iris, aes(x = Sepal.Length, y = Species, group = Species, fill = Species)) +
#'   stat_binline(bins = 20, scale = 2.2, draw_baseline = FALSE) +
#'   scale_y_discrete(expand = c(0.01, 0)) +
#'   scale_x_continuous(expand = c(0.01, 0)) +
#'   scale_fill_grey() +
#'   theme_joy() + theme(legend.position = 'none')
#'
#' require(ggplot2movies)
#' require(viridis)
#' ggplot(movies[movies$year>1989,], aes(x = length, y = year, fill = factor(year))) +
#'    stat_binline(scale = 1.9, bins = 40) +
#'    theme_joy() + theme(legend.position = "none") +
#'    scale_x_continuous(limits = c(1, 180), expand = c(0.01, 0)) +
#'    scale_y_reverse(expand = c(0.01, 0)) +
#'    scale_fill_viridis(begin = 0.3, discrete = TRUE, option = "B") +
#'    labs(title = "Movie lengths 1990 - 2005")
#'
#' @export
stat_binline <- function(mapping = NULL, data = NULL,
                     geom = "joy", position = "identity",
                     ...,
                     binwidth = NULL,
                     bins = NULL,
                     center = NULL,
                     boundary = NULL,
                     breaks = NULL,
                     closed = c("right", "left"),
                     pad = TRUE,
                     draw_baseline = TRUE,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = StatBinline,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      binwidth = binwidth,
      bins = bins,
      center = center,
      boundary = boundary,
      breaks = breaks,
      closed = closed,
      pad = pad,
      draw_baseline = draw_baseline,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname stat_binline
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto StatBin
#' @export
StatBinline <- ggproto("StatBinline", StatBin,
  required_aes = "x",

  default_aes = aes(height = ..density..),

  setup_params = function(data, params) {
    # provide default value if not given, happens when stat is called from a geom
    if (is.null(params$pad)) {
      params$pad <- TRUE
    }

    # provide default value if not given, happens when stat is called from a geom
    if (is.null(params$draw_baseline)) {
      params$draw_baseline <- TRUE
    }

    if (!is.null(params$boundary) && !is.null(params$center)) {
      stop("Only one of `boundary` and `center` may be specified.", call. = FALSE)
    }

    if (is.null(params$breaks) && is.null(params$binwidth) && is.null(params$bins)) {
      message("`stat_binline()` using `bins = 30`. Pick better value with `binwidth`.")
      params$bins <- 30
    }

    params
  },

  compute_group = function(self, data, scales, binwidth = NULL, bins = NULL,
                           center = NULL, boundary = NULL,
                           closed = c("right", "left"), pad = TRUE,
                           breaks = NULL, origin = NULL, right = NULL,
                           drop = NULL, width = NULL, draw_baseline = TRUE) {
    binned <- ggproto_parent(StatBin, self)$compute_group(data = data,
                  scales = scales, binwidth = binwidth,
                  bins = bins, center = center, boundary = boundary,
                  closed = closed, pad = pad, breaks = breaks)

    result <- rbind(transform(binned, x=xmin), transform(binned, x=xmax-0.00001*width))
    result <- result[order(result$x), ]

    # remove zero counts if requested
    if (!draw_baseline) {
      zeros <- result$count == 0
      protected <- (zeros & !c(zeros[2:length(zeros)], TRUE)) | (zeros & !c(TRUE, zeros[1:length(zeros)-1]))
      to_remove <- zeros & !protected
      result$count[to_remove] <- NA
      result$density[to_remove] <- NA
    }

    result
  }

)
