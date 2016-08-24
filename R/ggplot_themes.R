#' Report Theme
#'
#' Theme
#'
#' Theme Descript
#'
#' @export
report_theme <- ggplot2::theme_bw() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = "black",
                                                      fill = NA),
                 panel.grid.major = ggplot2::element_line(color = "black",
                                                          linetype = "dotted"),
                 panel.grid.minor = ggplot2::element_line(color = "black",
                                                          linetype = "dotted"))