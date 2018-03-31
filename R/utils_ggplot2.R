# code that needed to be copied from ggplot2

# Name ggplot grid object, from ggplot2/R/utilities-grid.r
# Convenience function to name grid objects
#
# @keyword internal
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

# From ggplot2/R/utilities.r
snakeize <- function(x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  tolower(x)
}

snake_class <- function(x) {
  snakeize(class(x)[1])
}

with_seed_null <- function(seed, code) {
  if (is.null(seed)) {
    code
  } else {
    withr::with_seed(seed, code)
  }
}

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

# ggplot2 range code, from ggplot2/R/range.r

#' @importFrom ggplot2 ggproto
#' @noRd
Range <- ggproto("Range", NULL,
  range = NULL,
  reset = function(self) {
    self$range <- NULL
  }
)

#' @importFrom scales train_discrete
#' @noRd
RangeDiscrete <- ggproto("RangeDiscrete", Range,
  train = function(self, x, drop = FALSE, na.rm = FALSE) {
    self$range <- scales::train_discrete(x, self$range, drop = drop, na.rm = na.rm)
  }
)

discrete_range <- function() {
  ggproto(NULL, RangeDiscrete)
}


# ggplot2 aes code, from ggplot2/R/aes.r

# Look up the scale that should be used for a given aesthetic
aes_to_scale <- function(var) {
  var[var %in% c("x", "xmin", "xmax", "xend", "xintercept")] <- "x"
  var[var %in% c("y", "ymin", "ymax", "yend", "yintercept")] <- "y"

  var
}

# Figure out if an aesthetic is a position aesthetic or not
is_position_aes <- function(vars) {
  aes_to_scale(vars) %in% c("x", "y")
}

