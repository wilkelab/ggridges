
#' A custom theme specifically for use with joyplots
#'
#' This theme has some special modifications that make joyplots look better, such as properly aligned y axis labels.
#'
#' @param font_size Overall font size. Default is 14.
#' @param font_family Default font family.
#' @param line_size Default line size.
#' @return The theme.
#' @examples
#' # still to do
#' @export
theme_joy <- function(font_size = 14, font_family = "", line_size = .5) {
  half_line <- font_size / 2
  small_rel <- 0.857
  small_size <- small_rel * font_size
  color <- "grey90"

  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      rect              = element_rect(fill = "transparent", colour = NA, color = NA, size = 0, linetype = 0),
      text              = element_text(family = font_family, face = "plain", colour = "black",
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = ggplot2::margin(), debug = FALSE),
      axis.text         = element_text(colour = "black", size = font_size),
      #axis.title        = element_text(face = "bold"),
      axis.text.x       = element_text(margin = ggplot2::margin(t = small_size / 4), vjust = 1),
      axis.text.y       = element_text(margin = ggplot2::margin(r = small_size / 4), hjust = 1, vjust = 0),
      axis.title.x      = element_text(
        margin = ggplot2::margin(t = small_size / 2, b = small_size / 4),
        hjust = 1
      ),
      axis.title.y      = element_text(
        angle = 90,
        margin = ggplot2::margin(r = small_size / 2, l = small_size / 4),
        hjust = 1
      ),
      axis.ticks        = element_line(colour = color, size = line_size),
      axis.line         = element_line(colour = color, size = line_size, lineend = "square"),
      axis.line.x       = element_blank(),
      legend.key        = element_blank(),
      legend.key.size   = grid::unit(1, "lines"),
      legend.text       = element_text(size = rel(small_rel)),
      legend.justification = c("left", "center"),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      # make vertical grid lines
      panel.grid.major  = element_line(colour = color, size = line_size),
      panel.grid.major.y = element_blank(),
      panel.grid.minor  = element_blank(),
      strip.text        = element_text(size = rel(small_rel)),
      strip.background  = element_rect(fill = color, colour = "grey50", size = 0),
      plot.background   = element_blank(),
      plot.title        = element_text(face = "bold",
                                       size = font_size,
                                       margin = ggplot2::margin(b = half_line)),

      complete = TRUE
    )
}

# vertical grid lines only
theme_dviz_vgrid <- function(font_size = 14, font_family = "") {
  color = "grey90"
  line_size = 0.5

  # Starts with theme_cowplot and then modify some parts
  theme_cowplot(font_size = font_size, font_family = font_family) %+replace%
    theme(

      # adjust axis tickmarks
      axis.ticks        = element_line(colour = color, size = line_size),

      # adjust y axis
      axis.line.y       = element_line(colour = color, size = line_size),
      # no x axis line
      axis.line.x       = element_blank()
    )
}
