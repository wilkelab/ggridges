# Code for stat_joy based on stat_density_common in the "extending ggplot2" vignette

#' Stat for density joyplots
#'
#' This stat is the default stat used by \code{geom_joy}. It is very similar to \code{stat_density},
#' however there are a few differences. Most importantly, the density bandwidth is chosen across
#' the entire dataset.
#'
#' @importFrom ggplot2 layer
#' @export
stat_joy <- function(mapping = NULL, data = NULL, geom = "joy",
                     position = "identity", na.rm = TRUE, show.legend = NA,
                     inherit.aes = TRUE, bandwidth = NULL, ...)
{
  layer(
    stat = StatJoy,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(bandwidth = bandwidth, na.rm = na.rm, ...)
  )
}


#' @importFrom ggplot2 ggproto Stat
#' @export
StatJoy <- ggproto("StatJoy", Stat,
  required_aes = "x",
  default_aes = aes(height = ..density..),

  setup_params = function(data, params) {
    if (is.null(params$bandwidth)) {
      xdata <- na.omit(data.frame(x=data$x, group=data$group))
      xs <- split(xdata$x, xdata$group)
      bws <- vapply(xs, bw.nrd0, numeric(1))
      bw <- mean(bws, na.rm = TRUE)
      message("Picking joint bandwidth of ", signif(bw, 3))

      params$bandwidth <- bw
    }

    min <- min(data$x, na.rm=TRUE) - 3 * params$bandwidth
    max <- max(data$x, na.rm=TRUE) + 3 * params$bandwidth

    list(
      bandwidth = params$bandwidth,
      min = min,
      max = max,
      #na.rm = params$na.rm
      na.rm = TRUE
    )
  },

  compute_group = function(data, scales, min, max, bandwidth = 1, na.rm = TRUE) {
    d <- density(data$x, bw = bandwidth, from = min, to = max, na.rm)
    data.frame(x = d$x, density = d$y)
  }
)
