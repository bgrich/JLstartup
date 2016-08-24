#' ggplot Themes
#'
#' Creates objects that are ggplot themes to be used in figures
#'
#' These themes are a set of commonly used parameters used in reports and
#' publications. The \code{report_theme} is for use with R markdown reports
#' and represents a fairly simple mix of the black and white theme from ggplot2
#' and changing the borders to black and the grid lines to black and dotted.
#'
#' @export
report_theme <- ggplot2::theme_bw() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = "black",
                                                      fill = NA),
                 panel.grid.major = ggplot2::element_line(color = "black",
                                                          linetype = "dotted"),
                 panel.grid.minor = ggplot2::element_line(color = "black",
                                                          linetype = "dotted"))

#' @rdname report_theme
publication_theme <- ggplot2::theme_bw() +
  ggplot2::theme(text = ggplot2::element_text(family = "Times"),
        axis.title = ggplot2::element_text(size = 13),
        axis.text = ggplot2::element_text(size = 8),
        plot.title = ggplot2::element_text(size = 8)) +
  ggplot2::theme(legend.position = "none",
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(color = "black"),
        axis.ticks.length = grid::unit(-0.15, "cm"),
        axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 0.25, unit = "cm")),
        axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 0.25, unit = "cm")))