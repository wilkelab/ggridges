# Code for stat_density_ridges based on stat_density_common in the "extending ggplot2" vignette

#' Stat for density ridgeline plots
#'
#' This stat is the default stat used by [`geom_density_ridges`]. It is very similar to [`stat_density`],
#' however there are a few differences. Most importantly, the density bandwidth is chosen across
#' the entire dataset.
#'
#' @param geom The geometric object to use to display the data.
#' @param bandwidth Bandwidth used for density calculation. If not provided, is estimated from the data.
#' @param from,to The left and right-most points of the grid at which the density is to be estimated,
#'   as in [`density()`]. If not provided, these are estimated from the data range and the bandwidth.
#' @param jittered_points If `TRUE`, carries the original point data over to the processed data frame,
#'   so that individual points can be drawn by the various ridgeline geoms. The specific position of these
#'   points is controlled by various position objects, e.g. [`position_points_sina()`] or [`position_raincloud()`].
#' @param quantile_lines If `TRUE`, enables the drawing of quantile lines. Overrides the `calc_ecdf` setting
#'   and sets it to `TRUE`.
#' @param calc_ecdf If `TRUE`, `stat_density_ridges` calculates an empirical cumulative distribution function (ecdf)
#'   and returns a variable `ecdf` and a variable `quantile`. Both can be mapped onto aesthetics via
#'   `stat(ecdf)` and `stat(quantile)`, respectively.
#' @param quantiles Sets the number of quantiles the data should be broken into. Used if either `calc_ecdf = TRUE`
#'   or `quantile_lines = TRUE`. If `quantiles` is an integer then the data will be cut into that many equal quantiles.
#'   If it is a vector of probabilities then the data will cut by them.
#' @param quantile_fun Function that calculates quantiles. The function needs to accept two parameters,
#'   a vector `x` holding the raw data values and a vector `probs` providing the probabilities that
#'   define the quantiles. Default is `quantile`.
#' @param n The number of equally spaced points at which the density is to be estimated. Should be a power of 2. Default
#'   is 512.
#' @inheritParams geom_ridgeline
#' @importFrom ggplot2 layer
#' @examples
#' library(ggplot2)
#'
#' # Examples of coloring by ecdf or quantiles
#' ggplot(iris, aes(x = Sepal.Length, y = Species, fill = factor(stat(quantile)))) +
#'   stat_density_ridges(
#'     geom = "density_ridges_gradient",
#'     calc_ecdf = TRUE,
#'     quantiles = 5
#'   ) +
#'   scale_fill_viridis_d(name = "Quintiles") +
#'   theme_ridges()
#'
#' ggplot(iris,
#'   aes(
#'     x = Sepal.Length, y = Species, fill = 0.5 - abs(0.5-stat(ecdf))
#'   )) +
#'   stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
#'   scale_fill_viridis_c(name = "Tail probability", direction = -1) +
#'   theme_ridges()
#'
#' ggplot(iris,
#'   aes(
#'     x = Sepal.Length, y = Species, fill = factor(stat(quantile))
#'   )) +
#'   stat_density_ridges(
#'     geom = "density_ridges_gradient",
#'     calc_ecdf = TRUE, quantiles = c(0.025, 0.975)
#'   ) +
#'   scale_fill_manual(
#'     name = "Probability",
#'     values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
#'     labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
#'   ) +
#'   theme_ridges()
#' @export
stat_density_ridges <- function(mapping = NULL, data = NULL, geom = "density_ridges",
                     position = "identity", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE, bandwidth = NULL, from = NULL, to = NULL,
                     jittered_points = FALSE, quantile_lines = FALSE, calc_ecdf = FALSE, quantiles = 4,
                     quantile_fun = quantile, n = 512, ...)
{
  layer(
    stat = StatDensityRidges,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(bandwidth = bandwidth,
                  from = from,
                  to = to,
                  calc_ecdf = calc_ecdf,
                  quantiles = quantiles,
                  jittered_points = jittered_points,
                  quantile_lines = quantile_lines,
                  quantile_fun = quantile_fun,
                  n = n,
                  na.rm = na.rm, ...)
  )
}

#' @rdname stat_density_ridges
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @export
StatDensityRidges <- ggproto("StatDensityRidges", Stat,
  required_aes = "x",

  default_aes = aes(height = ..density.., weight = NULL),

  calc_panel_params = function(data, params) {
    if (is.null(params$bandwidth)) {
      xdata <- na.omit(data.frame(x=data$x, group=data$group))
			if ("weight" %in% names(data)) {
				xdata["weight"] <- data$weight
			} else {
				xdata["weight"] <- 1
			}
			xs <- split(xdata[c("x", "weight")], xdata$group)
      xs_mask <- vapply(xs, function(d) nrow(d), numeric(1)) > 1
      bws <- vapply(xs[xs_mask], function(d, ...) stats::density(d$x, weights = d$weight / sum(d$weight), ...)$bw, numeric(1))
      bw <- mean(bws, na.rm = TRUE)
      message("Picking joint bandwidth of ", signif(bw, 3))

      params$bandwidth <- bw
    }

    if (is.null(params$from)) {
			params$from <- min(data$x[is.finite(data$x)], na.rm = TRUE) - 3 * params$bandwidth
    }

    if (is.null(params$to)) {
			params$to <- max(data$x[is.finite(data$x)], na.rm = TRUE) + 3 * params$bandwidth
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
    pardata <- reduce(pardata, rbind)

    if (length(params$quantiles) > 1 &&
        (max(params$quantiles, na.rm = TRUE) > 1 || min(params$quantiles, na.rm = TRUE) < 0)) {
      stop('invalid quantiles used: c(', paste0(params$quantiles, collapse = ','), ') must be within [0, 1] range')
    }

    params$bandwidth <- pardata$bandwidth
    params$from <- pardata$from
    params$to <- pardata$to
    params
  },

  compute_group = function(data, scales, from, to, bandwidth = 1,
                           calc_ecdf = FALSE, jittered_points = FALSE, quantile_lines = FALSE,
                           quantiles = 4, quantile_fun = quantile, n = 512) {
    # ignore too small groups
    if(nrow(data) < 3) return(data.frame())
		if (!("weight" %in% names(data))) {
			data["weight"] <- 1
		}
    if (is.null(calc_ecdf)) calc_ecdf <- FALSE
    if (is.null(jittered_points)) jittered_points <- FALSE
    if (is.null(quantile_lines)) quantile_lines <- FALSE

    # when quantile lines are requested, we also calculate ecdf
    # this simplifies things for now; in principle, could disentangle
    # the two
    if (quantile_lines) calc_ecdf <- TRUE

    panel <- unique(data$PANEL)
    if (length(panel) > 1) {
      stop("Error: more than one panel in compute group; something's wrong.")
    }
    panel_id <- as.numeric(panel)

    d <- stats::density(
      data$x, weights = data$weight / sum(data$weight),
      bw = bandwidth[panel_id], from = from[panel_id], to = to[panel_id], na.rm = TRUE,
      n = n
    )

    # calculate maximum density for scaling
    maxdens <- max(d$y, na.rm = TRUE)

    # make interpolating function for density line
    densf <- approxfun(d$x, d$y, rule = 2)

    # calculate jittered original points if requested
    if (jittered_points) {
      df_jittered <- data.frame(
        x = data$x,
        # actual jittering is handled in the position argument
        density = densf(data$x),
        ndensity = densf(data$x) / maxdens,
        datatype = "point", stringsAsFactors = FALSE)

      # see if we need to carry over other point data
      # capture all data columns starting with "point", as those are relevant for point aesthetics
      df_points <- data[grepl("point_", names(data))]

      # uncomment following line to switch off carrying over data
      #df_points <- data.frame()

      if (ncol(df_points) == 0) {
        df_points <- NULL
        df_points_dummy <- NULL
      }
      else {
        # combine additional points data into results dataframe
        df_jittered <- cbind(df_jittered, df_points)
        # make a row of dummy data to merge with the other dataframes
        df_points_dummy <- na.omit(df_points)[1, , drop = FALSE]
      }
    } else {
      df_jittered <- NULL
      df_points_dummy <- NULL
    }

    # calculate quantiles, needed for both quantile lines and ecdf
    if ((length(quantiles)==1) && (all(quantiles >= 1))) {
      if (quantiles > 1) {
        probs <- seq(0, 1, length.out = quantiles + 1)[2:quantiles]
      }
      else {
        probs <- NA
      }
    } else {
      probs <- quantiles
      probs[probs < 0 | probs > 1] <- NA
    }
    qx <- na.omit(quantile_fun(data$x, probs = probs))

    # if requested, add data frame for quantile lines
    df_quantiles <- NULL

    if (quantile_lines && length(qx) > 0) {
      qy <- densf(qx)
      df_quantiles <- data.frame(
        x = qx,
        density = qy,
        ndensity = qy / maxdens,
        datatype = "vline",
        stringsAsFactors = FALSE
      )
      if (!is.null(df_points_dummy)){
        # add in dummy points data if necessary
        df_quantiles <- data.frame(df_quantiles, as.list(df_points_dummy))
      }
    }

    # combine the quantiles and jittered points data frames into one, the non-density frame
    df_nondens <- rbind(df_quantiles, df_jittered)

    if (calc_ecdf) {
      n <- length(d$x)
      ecdf <- c(0, cumsum(d$y[1:(n-1)]*(d$x[2:n]-d$x[1:(n-1)])))
      ecdf_fun <- approxfun(d$x, ecdf, rule = 2)
      ntile <- findInterval(d$x, qx, left.open = TRUE) + 1 # if make changes here, make them also below

      if (!is.null(df_nondens)) {
        # we add data for ecdf and quantiles back to all other data points
        df_nondens <- data.frame(
          df_nondens,
          ecdf = ecdf_fun(df_nondens$x),
          quantile = findInterval(df_nondens$x, qx, left.open = TRUE) + 1
        )
      }

      df_density <- data.frame(
        x = d$x,
        density = d$y,
        ndensity = d$y / maxdens,
        ecdf = ecdf,
        quantile = ntile,
        datatype = "ridgeline",
        stringsAsFactors = FALSE
      )
    }
    else {
      df_density <- data.frame(
        x = d$x,
        density = d$y,
        ndensity = d$y / maxdens,
        datatype = "ridgeline",
        stringsAsFactors = FALSE
      )
    }

    if (!is.null(df_points_dummy)){
      # add in dummy points data if necessary
      df_density <- data.frame(df_density, as.list(df_points_dummy))
    }

    # now combine everything and turn quantiles into factor
    df_final <- rbind(df_density, df_nondens)
    if ("quantile" %in% names(df_final)) {
      df_final$quantile <- factor(df_final$quantile)
    }

    df_final
  }
)

#' Stat for histogram ridgeline plots
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
#' library(ggplot2)
#'
#' ggplot(iris, aes(x = Sepal.Length, y = Species, group = Species, fill = Species)) +
#'   geom_density_ridges(stat = "binline", bins = 20, scale = 2.2) +
#'   scale_y_discrete(expand = c(0, 0)) +
#'   scale_x_continuous(expand = c(0, 0)) +
#'   coord_cartesian(clip = "off") +
#'   theme_ridges()
#'
#' ggplot(iris, aes(x = Sepal.Length, y = Species, group = Species, fill = Species)) +
#'   stat_binline(bins = 20, scale = 2.2, draw_baseline = FALSE) +
#'   scale_y_discrete(expand = c(0, 0)) +
#'   scale_x_continuous(expand = c(0, 0)) +
#'   scale_fill_grey() +
#'   coord_cartesian(clip = "off") +
#'   theme_ridges() +
#'   theme(legend.position = 'none')
#'
#' library(ggplot2movies)
#' ggplot(movies[movies$year>1989,], aes(x = length, y = year, fill = factor(year))) +
#'    stat_binline(scale = 1.9, bins = 40) +
#'    scale_x_continuous(limits = c(1, 180), expand = c(0, 0)) +
#'    scale_y_reverse(expand = c(0, 0)) +
#'    scale_fill_viridis_d(begin = 0.3, option = "B") +
#'    coord_cartesian(clip = "off") +
#'    labs(title = "Movie lengths 1990 - 2005")
#'    theme_ridges() +
#'    theme(legend.position = "none")
#'
#' count_data <- data.frame(
#'   group = rep(letters[1:5], each = 10),
#'   mean = rep(1:5, each = 10)
#' )
#' count_data$group <- factor(count_data$group, levels = letters[5:1])
#' count_data$count <- rpois(nrow(count_data), count_data$mean)
#'
#' ggplot(count_data, aes(x = count, y = group, group = group)) +
#'   geom_density_ridges2(
#'     stat = "binline",
#'     aes(fill = group),
#'     binwidth = 1,
#'     scale = 0.95
#'   ) +
#'   geom_text(
#'     stat = "bin",
#'     aes(y = group + 0.9*stat(count/max(count)),
#'     label = ifelse(stat(count) > 0, stat(count), "")),
#'     vjust = 1.2, size = 3, color = "white", binwidth = 1
#'   ) +
#'   scale_x_continuous(breaks = c(0:12), limits = c(-.5, 13), expand = c(0, 0)) +
#'   scale_y_discrete(expand = c(0, 0)) +
#'   scale_fill_cyclical(values = c("#0000B0", "#7070D0")) +
#'   guides(y = "none") +
#'   coord_cartesian(clip = "off") +
#'   theme_ridges(grid = FALSE)
#' @importFrom stats quantile
#' @export
stat_binline <- function(mapping = NULL, data = NULL,
                     geom = "density_ridges", position = "identity",
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
