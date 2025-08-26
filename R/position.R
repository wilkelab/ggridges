#' Randomly jitter the points in a ridgeline plot
#'
#' This is a position adjustment specifically for [`geom_density_ridges()`] and related geoms. It
#' only jitters the points drawn by these geoms, if any. If no points are present, the plot
#' remains unchanged. The effect is similar to [ggplot2::position_jitter()]: points are randomly shifted up and down
#' and/or left and right.
#'
#' @param width Width for horizontal jittering. By default set to 0.
#' @param height Height for vertical jittering, applied in both directions (up and down). By default 0.2.
#' @param yoffset Vertical offset applied in addition to jittering.
#' @param adjust_vlines If `TRUE`, adjusts vertical lines (as are drawn for
#'   quantile lines, for example) to align with the point cloud.
#' @param seed Random seed. If set to NULL, the current random number generator is used.
#'   If set to NA, a new random random seed is generated. If set to a number, this
#'   number is used as seed for jittering only.
#' @seealso Other position adjustments for ridgeline plots: [`position_points_sina`], [`position_raincloud`]
#' @examples
#' library(ggplot2)
#'
#' # default jittered points
#' ggplot(iris, aes(x = Sepal.Length, y = Species)) +
#'   geom_density_ridges(jittered_points = TRUE, position = "points_jitter", alpha = 0.7)
#'
#' # simulating a rug
#' ggplot(iris, aes(x = Sepal.Length, y = Species)) +
#'   geom_density_ridges(jittered_points = TRUE, point_shape = '|', alpha = 0.7, point_size = 2,
#'                       position = position_points_jitter(width = 0.02, height = 0))
#' @export
position_points_jitter <- function(width = 0, height = 0.2, yoffset = 0, adjust_vlines = FALSE, seed = NULL) {
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }

  ggproto(NULL, PositionPointsJitter,
    width = width,
    height = height,
    yoffset = yoffset,
    adjust_vlines = adjust_vlines,
    seed = seed
  )
}

#' @rdname position_points_jitter
#' @format NULL
#' @usage NULL
#' @export
PositionPointsJitter <- ggproto("PositionPointsJitter", Position,
  required_aes = c("x", "ymin", "ymax"),

  setup_params = function(self, data) {
    list(
      width = self$width %||% 0,
      height = self$height %||% 0.2,
      yoffset = self$yoffset %||% 0,
      adjust_vlines = self$adjust_vlines %||% FALSE,
      seed = self$seed
    )
  },

  compute_layer = function(data, params, panel) {
    # if there's no datatype aesthetic then we're done by default
    if (!"datatype" %in% names(data)) {
      return(data)
    }

    points <- data$datatype == "point"
    with_seed_null(params$seed, {
      if (params$width > 0) {
        data$x[points] <- data$x[points] - params$width +
          2 * params$width * runif(sum(points))
      };
      data$ymin[points] <- data$ymin[points] + params$yoffset - params$height +
        2 * params$height * runif(sum(points))
    })

    # do we need to adjust vertical lines as well?
    if (!params$adjust_vlines) {
      return(data) # no, we're done
    }

    vlines <- data$datatype == "vline"
    data$ymin[vlines] <- data$ymin[vlines] + params$yoffset - params$height
    data$ymax[vlines] <- data$ymin[vlines] + 2 * params$height

    data
  }
)


#' Create a cloud of randomly jittered points below a ridgeline plot
#'
#' This is a position adjustment specifically for [`geom_density_ridges()`] and related geoms. It
#' only jitters the points drawn by these geoms, if any. If no points are present, the plot
#' remains unchanged. The effect is similar to [`position_points_jitter()`], only that by default the
#' points lie all underneath the baseline of each individual ridgeline.
#'
#' The idea for this position adjustment comes from Micah Allen's work
#' on raincloud plots (Allen et al. 2021).
#'
#' @param width Width for horizontal jittering. By default set to 0.
#' @param height Total height of point cloud. By default 0.4.
#' @param ygap Vertical gap between ridgeline baseline and point cloud.
#' @param adjust_vlines If `TRUE`, adjusts vertical lines (as are drawn for
#'   quantile lines, for example) to align with the point cloud.
#' @param seed Random seed. See [`position_points_jitter`].
#' @seealso Other position adjustments for ridgeline plots: [`position_points_jitter`], [`position_points_sina`]
#' @references
#' Allen, M., Poggiali, D., Whitaker, K., Marshall, T. R.,
#' van Langen, J., Kievit, R. A. (2021) Raincloud plots:
#' a multi-platform tool for robust data visualization
#' \[version 2; peer review: 2 approved\]. Wellcome Open Res 4:63.
#' @examples
#' library(ggplot2)
#'
#' ggplot(iris, aes(x = Sepal.Length, y = Species)) +
#'   geom_density_ridges(jittered_points = TRUE, position = "raincloud", alpha = 0.7)
#' @export
position_raincloud <- function(width = 0, height = 0.4, ygap = 0.05, adjust_vlines = FALSE, seed = NULL) {
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }

  ggproto(NULL, PositionRaincloud,
          width = width,
          height = height,
          ygap = ygap,
          adjust_vlines = adjust_vlines,
          seed = seed
  )
}

#' @rdname position_raincloud
#' @format NULL
#' @usage NULL
#' @export
PositionRaincloud <- ggproto("PositionRaincloud", PositionPointsJitter,
  required_aes = c("x", "ymin", "ymax"),

  setup_params = function(self, data) {
    height <- (self$height %||% 0.4)/2
    yoffset <- -height - (self$ygap %||% 0.05)
    list(
      width = self$width %||% 0,
      height = height,
      yoffset = yoffset,
      adjust_vlines = self$adjust_vlines %||% FALSE,
      seed = self$seed
    )
  }
)

#' Randomly distribute points in a ridgeline plot between baseline and ridgeline
#'
#' This is a position adjustment specifically for [`geom_density_ridges()`] and related geoms. It
#' only jitters the points drawn by these geoms, if any. If no points are present, the plot
#' remains unchanged. The effect is similar to a sina plot: Points are randomly distributed to fill
#' the entire shaded area representing the data density.
#'
#' @param rel_min The relative minimum value at which a point can be placed.
#' @param rel_max The relative maximum value at which a point can be placed.
#' @param seed See [`position_points_jitter`].
#' @seealso Other position adjustments for ridgeline plots: [`position_points_jitter`], [`position_raincloud`]
#' @examples
#' library(ggplot2)
#'
#' ggplot(iris, aes(x = Sepal.Length, y = Species)) +
#'   geom_density_ridges(jittered_points = TRUE, position = "points_sina", alpha = 0.7)
#' @export
position_points_sina <- function(rel_min = 0.02, rel_max = 0.98, seed = NULL) {
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }

  ggproto(NULL, PositionPointsSina,
    rel_min = rel_min,
    rel_max = rel_max,
    seed = seed
  )
}

#' @rdname position_points_sina
#' @format NULL
#' @usage NULL
#' @export
PositionPointsSina <- ggproto("PositionPointsSina", Position,
  required_aes = c("x", "ymin", "ymax"),

  setup_params = function(self, data) {
    list(
      rel_min = self$rel_abc %||% 0.02,
      rel_max = self$rel_max %||% 0.98,
      seed = self$seed
    )
  },

  compute_layer = function(data, params, panel) {
    # if there's no datatype aesthetic then we're done by default
    if (!"datatype" %in% names(data)) {
      return(data)
    }

    points <- data$datatype == "point"

    with_seed_null(params$seed,
      data$ymin[points] <- data$ymin[points] +
        (params$rel_min + (params$rel_max - params$rel_min) * runif(sum(points))) *
          (data$ymax[points] - data$ymin[points])
    )
    data
  }
)
