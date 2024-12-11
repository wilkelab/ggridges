ggridges 0.5.6.9000
----------------------------------------------------------------
- Add support for weighted density estimates in `stat_density_ridges()` by 
  allowing the use of the `weight` aesthetic (@joranE, #90)
- Add support for `grid::pattern()`, `grid::linearGradient()`, and `grid::radialGradient()` values for `fill` and `point_fill` aesthetics
  by using [`ggplot2::fill_alpha()`](https://ggplot2.tidyverse.org/reference/fill_alpha.html) (@trevorld, #94)

ggridges 0.5.6
----------------------------------------------------------------
- fix issues with upcoming ggplot 3.5.0.

ggridges 0.5.5
----------------------------------------------------------------
- Replace `ggplot2::expand_scale()` with `ggplot2::expansion()` in vignettes to avoid deprecation warnings in [ggplot2 0.3.3](https://ggplot2.tidyverse.org/news/index.html#minor-improvements-and-bug-fixes-3-3-0) (@jthomasmock, #78) 
- Replace `size` argument in line functions with `linewidth` to avoid deprecation warnings in [ggplot2 0.3.4](https://ggplot2.tidyverse.org/news/index.html#breaking-changes-3-4-0) (@jthomasmock, #78)
- Replace use of `..density..` with `ggplot2::after_stat(density)` to avoid deprecation warnings in [ggplot2 0.3.4](https://ggplot2.tidyverse.org/news/index.html#breaking-changes-3-4-0) (@jthomasmock, #78)

ggridges 0.5.4
----------------------------------------------------------------
- Remove broken link in docs.

ggridges 0.5.3
----------------------------------------------------------------
- Make sure tests don't fail if vdiffr is missing.

ggridges 0.5.2
----------------------------------------------------------------
- There is now a project website at https://wilkelab.org/ggridges.
- A new example dataset has been added, `Aus_athletes`.
- `scale_discrete_manual()` has been removed from the ggridges package,
  as it has been available in ggplot2 since version 3.0.0.
- `stat_density_ridges()` now has a parameter `n` that determines at how many
  points along the x axis the density is estimated.

ggridges 0.5.1
----------------------------------------------------------------
- The `alpha` aesthetic is now by default applied to jittered points. If you don't
  want this to happen, set `point_alpha = 1`.
- Allow custom function to calculate the position of quantile lines. This makes
  it possible, for example, to place a line at the mean, via `quantile_fun = mean`.
- Allow coloring of quantile lines and jittered points by quantile.

ggridges 0.5.0
----------------------------------------------------------------
- Expanded documentation and gallery of example plots.
- Reworked stat_density_ridges, geom_ridgeline, geom_density_ridges,
  etc. so that jittered points and quantile lines can be drawn
  on top of the distributions. Points and quantile lines can be
  styled separately from the rest of the ridgeline plot, via
  special aesthetics.
- Added position options to control how the jittered points are drawn.
  Default is uniformly spaced within the density area.
- Added `geom_density_line()` which is a drop-in replacement for
  `geom_density()` but draws a ridgeline rather than a closed
  polygon.
- `theme_ridges()` has been modified so that font sizes match the cowplot
  themes. In particular, this means smaller axis tick labels.

ggridges 0.4.1
----------------------------------------------------------------
- Skip vdiffr visual tests when compiling on CRAN

ggridges 0.4.0
----------------------------------------------------------------
- Initial import of code base from ggjoy, and renaming:
  geom_joy -> geom_density_ridges
  stat_joy -> stat_density_ridges
  theme_joy -> theme_ridges

ggjoy 0.3.1
----------------------------------------------------------------
- Added an option to center axis labels to `theme_joy()`.

ggjoy 0.3.0
----------------------------------------------------------------
- Added cyclical scales that make it easy to alternate between fill colors
  and other aesthetics.

ggjoy 0.2.0
----------------------------------------------------------------
Numerous improvements:
- New stat `binline` that can be used to draw histogram joyplots.
- Various improvements in `stat_joy`. In particular, it now works properly
  with multiple panels. It also now has parameters `from` and `to` to limit the
  range of density estimation, just like `density()`.
- Improvements in the vignettes
- New geoms `geom_ridgeline_gradient` and `geom_joy_gradient` that can handle gradient fills.

ggjoy 0.1.0
----------------------------------------------------------------
First complete implementation ready for initial release
