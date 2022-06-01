#' Plots Cell Survival Curves using \code{\link[ggplot2]{ggplot}}
#'
#' This function helps to plot the cell survival curves using
#' \code{\link[ggplot2]{ggplot}}, and is the recommended way to plot this curves
#' over \code{\link{plotCSCurve}}. A single function plots the curves, both for
#' a single or multiple cell type(s), and allows most of the options necessary
#' to customize the figures for publication.
#'
#' @param data A data frame containing at least the following five columns:
#'   \code{"cline"}, \code{"Exp"}, \code{"dose"}, \code{"ncells"}, and
#'   \code{"ncolonies"}.
#' @param ... The name of the cell type(s)/group(s). If entering multiple cell
#'   types, separate each by a comma.
#' @param method Method used for the fit. It's \code{"ml"} (maximum likelihood)
#'   by default. Can be \code{"ls"} (least squares) or \code{"franken"}
#'   (weighted least squares as described by Franken eta al.(2006)).
#' @param PEmethod Controls the value of the plating efficiencies. The default
#'   method, \code{"fit"}, calculates fitted plating efficiencies as model
#'   parameters, while \code{"fix"} uses fixed ones calculated from the observed
#'   zero dose data.
#' @param colors A vector of strings denoting the colors of the curves. Size of
#'   the vector should be equal to or more than the number of cell types
#'   entered. If no colors are chosen by the user, the function chooses them by
#'   random and they might change with each plotting.
#' @param title A string denoting the title of the plot. Default is \code{""}.
#' @param title_size An integer denoting the font size of the title. Default is
#'   \code{20}.
#' @param title_color A string denoting the color of the font of the title.
#'   Default is \code{"black"}.
#' @param title_face A string denoting the type of font face. Accepts
#'   \code{"bold"}, \code{"italic"}, or \code{"bold.italic"}. Default is
#'   \code{"bold"}.
#' @param title_align A string denoting the justification (alignment) of the
#'   plot title. Accepts \code{"center"}, \code{"left"}, or \code{"right"}.
#'   Default is \code{"center"}.
#' @param subtitle A string denoting the subtitle of the plot; default is
#'   \code{""}.
#' @param sub_size,sub_color,sub_face,sub_align Used to customize the font size,
#'   color, face, and alignment of the subtitle respectively, like the plot
#'   title. Default values of these are \code{15}, \code{"black"},
#'   \code{"italic"}, and \code{"center"} respectively.
#' @param xlab A string denoting the label of X-axis; default is \code{"Dose
#'   (Gy)"}.
#' @param xlab_size,xlab_color,xlab_face Used to customize the font size, color,
#'   face, and alignment of the label of the X-axis respectively, like the plot
#'   title. Default values of these are \code{16}, \code{"black"}, and
#'   \code{"bold"} respectively.
#' @param ylab A string denoting the label of y-axis; default is
#'   \code{"Surviving Fraction"}.
#' @param ylab_size,ylab_color,ylab_face Used to customize the font size, color,
#'   face, and alignment of the label of the Y-axis respectively, like the plot
#'   title. Default values of these are \code{16}, \code{"black"}, and
#'   \code{"bold"} respectively.
#' @param legend_title A string denoting the title of the legend; default is
#'   \code{""}.
#' @param ltitle_size,ltitle_color,ltitle_face,ltitle_align Used to customize
#'   the font size, color, face, and alignment of the legend title respectively,
#'   like the plot title. Default values of these are \code{20}, \code{"black"},
#'   \code{"bold"}, and \code{"left"} respectively.
#' @param ltext_size,ltext_color,ltext_face To change the font size, color, and
#'   face of the legend texts. Default values of these are \code{18},
#'   \code{"black"}, and \code{"bold"} respectively.
#' @param xtext_size,xtext_color,xtext_face To change the font size, color, and
#'   face of x axis ticks.Default values of these are \code{14}, \code{"black"},
#'   and \code{"bold"} respectively.
#' @param ytext_size,ytext_color,ytext_face To change the font size, color, and
#'   face of y axis ticks. Default values of these are \code{14},
#'   \code{"black"}, and \code{"bold"} respectively.
#' @param legend_pos A string denoting the position of the legend.
#'   \code{"inside"} is default, and places the legend at the bottom left of the
#'   figure. Also accepts \code{"outside"} (places the legend outside the figure
#'   on the right) and \code{"none"} (removes the legend).
#' @param legend_back A string denoting the color of the legend background.
#'   Default is \code{""}.
#' @param legend_border A string denoting the color of the legnd border. Default
#'   is \code{""}.
#' @param legend_border_width An integer denoting the width of the legend
#'   border. Default is \code{0}.
#' @param rem_minor_x If \code{"no"}, keeps the minor grids in the X-axis.
#'   Default is \code{"yes"}.
#' @param rem_minor_y If \code{"no"}, keeps the minor grids in the Y-axis.
#'   Default is \code{"yes"}.
#' @param emph_major_x If \code{"yes"}, allows to emphasize and customize the
#'   major grids in the X-axis. Default is \code{"no"}.
#' @param emph_major_y If \code{"yes"}, allows to emphasize and customize the
#'   major grids in the Y-axis. Default is \code{"no"}.
#' @param major_x_col,major_x_width Allows the change of the color and width of
#'   the major grids in the X-axis, when \code{emph_major_x = "yes"}. Default
#'   values of these are \code{"gray"} and \code{1} respectively.
#' @param major_y_col,major_y_width Allows the change of the color and width of
#'   the major grids in the Y-axis, when \code{emph_major_y = "yes"}. Default
#'   values of these are \code{"gray"} and \code{1} respectively.
#' @param point_shape An integer denoting the shape of the points in the plot.
#'   Default is \code{16}.
#' @param point_size An integer denoting the size of the points in the plot.
#'   Default is \code{3.5}.
#' @param segment_type An integer denoting the type of the straight line
#'   segments in the plot. Default is \code{1}.
#' @param segment_width An integer denoting the width of straight line segments
#'   in the plot. Default is \code{1}.
#' @param curve_width An integer denoting the width of curved line segments in
#'   the plot. Default is \code{1.5}.
#' @param curve_type An integer denoting the type of curved line segments in the
#'   plot. Default is \code{1}.
#' @param theme The name of the theme to be used in the plot. It should be
#'   preceded by the name of the package in the form: \code{package::theme()}.
#'   Default is \code{ggplot2::theme_bw()}.
#' @param ylim A vector denoting the limits of the y-axis. Defaults are chosen
#'   automatically by \code{ggplot()}.
#' @param ybreaks A vector denoting the y-axis breaks/ticks. Defaults are chosen
#'   automatically by \code{ggplot()}.
#' @param save To save the plot. \code{save = "yes"} saves the plot in the
#'   specified path and format.
#' @param save_path Path where the plot is to be saved. Default is the current
#'   working directory obtained by \code{getwd()}.
#' @param save_filename File name with extension for the plot. Accepts most of
#'   the common extensions, including \code{.pdf}, \code{.jpeg}, and
#'   \code{.png}.
#' @param plot_height A number denoting the height of the plot. Default is
#'   \code{4}.
#' @param plot_width A number denoting the width of the plot. Default is
#'   \code{5}.
#' @param units A string denoting the units of the height and width of the plot
#'   mentioned. Accepts \code{"in"}, \code{"cm"}, \code{"mm"}, or \code{"px"}.
#'   Default is \code{"in"}.
#' @importFrom magrittr %>%
#' @return A \code{ggplot} object.
#' @examples
#' datatab <- CASP8_data
#'
#' # Single curve
#'
#' ggplotCSCurve(datatab, "shCASP8-NT")
#'
#' ggplotCSCurve(datatab, "control-B", colors = "black", xlab = "X-axis", ylab = "Y-axis",
#' title = "Single Plot", point_shape = 1, point_size = 2, segment_width = 1, segment_type = 1,
#' curve_width = 1, curve_type = 1, legend_title = "Cell types", ylim = c(0.008, 1.05),
#' ybreaks = c(0.01, 0.05, 0.20, 0.50, 1), theme = ggplot2::theme_classic())
#'
#' # Multiple Curves
#'
#' ggplotCSCurve(datatab, "shCASP8-NT", "shCASP8-B", "shCASP8-B+Z", "shCASP8-B+Z+N")
#'
#' ggplotCSCurve(datatab, "shCASP8-NT", "shCASP8-B", "shCASP8-B+Z", "shCASP8-B+Z+N",
#' colors = c("darkgreen", "green", "orange", "magenta"), xlab = "X-axis", ylab = "Y-axis",
#' title = "Multiple Curves", point_shape = 15, point_size = 1, segment_width = 1, segment_type = 1,
#' curve_width = 1, curve_type = 1, legend_title = "Cell types", theme = ggplot2::theme_dark(),
#' legend_back = "white", legend_border = "white", legend_border_width = 0.5)
#'
#' \dontrun{
#' # Save a plot
#' ggplotCSCurve(datatab, "shCASP8-NT", "shCASP8-B", "shCASP8-B+Z", "shCASP8-B+Z+N", save = "yes")
#' }
#'
#' @seealso Please also refer to \code{\link{plotCSCurve}} for regular plots.
#' @export
ggplotCSCurve <- function(data, ..., method = "ml", PEmethod = "fit", colors = NULL,
                          title = "", title_size = 20, title_color = "black", title_face = "bold", title_align = "center",
                          subtitle = "", sub_size = 15, sub_color = "black", sub_face = "italic", sub_align = "center",
                          xlab = "Dose (Gy)", xlab_size = 16, xlab_color = "black", xlab_face = "bold",
                          rem_minor_x = "yes", emph_major_x = "no", major_x_col = "gray", major_x_width = 1,
                          ylab = "Surviving Fraction", ylab_size = 16, ylab_color = "black", ylab_face = "bold",
                          rem_minor_y = "yes", emph_major_y = "no", major_y_col = "gray", major_y_width = 1,
                          legend_title = "", ltitle_size = 20, ltitle_color = "black", ltitle_face = "bold",
                          ltitle_align = "left", legend_pos = "inside", legend_back = "", legend_border = "",
                          legend_border_width = 0,
                          ltext_size = 18, ltext_color = "black", ltext_face = "bold",
                          xtext_size = 14, xtext_color = "black", xtext_face = "bold",
                          ytext_size = 14, ytext_color = "black", ytext_face = "bold",
                          point_shape = 16, point_size = 3.5, segment_type = 1, segment_width = 1,
                          curve_width = 1.5, curve_type = 1, theme = ggplot2::theme_bw(), ylim = NULL, ybreaks = ggplot2::waiver(),
                          save = "no", plot_width = 5, plot_height = 4, units = "in", save_path = getwd(), save_filename = "myplot.pdf") {




  if (!is.null(colors)) {
    if (length(colors) < length(c(...))) {
      warning("Error! The number of colors chosen is less than the number of cell types chosen. It should be at least equal to that!")
    }
  }

  cell_types <- factor(c(...), levels = c(...))
  l <- length(cell_types)
  merged_df <- .dfforggPlot(data, cell_types[1], method, PEmethod)


  if (l > 1) {
    for (i in 2:l) {
      df <- .dfforggPlot(data, cell_types[i], method, PEmethod)
      merged_df <- rbind(merged_df, df)
    }
  }
  dose_0_df <- merged_df %>% dplyr::filter(doses == doses[1])
  alpha_vec <- c(dose_0_df$alpha)
  beta_vec <- c(dose_0_df$beta)
  doses <- unique(data$dose)
  n_seq <- 100
  df_sim <- data.frame(d = rep(with(merged_df, seq(0, max(doses), length.out = n_seq)), l),
                       ctype = rep(cell_types, each = n_seq),
                       alpha = rep(alpha_vec, each = n_seq),
                       beta = rep(beta_vec, each = n_seq)) %>%
    dplyr::mutate(y_hat = exp(alpha*d + beta*(d^2)))
  if (is.null(colors)) {
    colors <- randomcoloR::distinctColorPalette(l)
  }


  p <- ggplot2::ggplot(data = merged_df, ggplot2::aes(x = doses, y = pts, color = ctype)) +
    ggplot2::geom_point(ggplot2::aes(x = doses, y = pts), shape = point_shape, size = point_size) +
    ggplot2::geom_segment(ggplot2::aes(x = doses, xend = doses, y = pts_minus_sems, yend = pts_plus_sems), linetype = segment_type, size = segment_width) +
    ggplot2::geom_line(data = df_sim, ggplot2::aes(y = y_hat, x = d, color = ctype), linetype = curve_type, size = curve_width) +
    ggplot2::scale_x_continuous(breaks = doses) +
    ggplot2::scale_y_log10(limits = ylim, breaks = ybreaks) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = ylab, color = legend_title) +
    theme +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size, color = title_color, face = title_face),
      plot.subtitle = ggplot2::element_text(size = sub_size, color = sub_color, face = sub_face),
      legend.title = ggplot2::element_text(size = ltitle_size, color = ltitle_color, face = ltitle_face),
      legend.text = ggplot2::element_text(size = ltext_size, color = ltext_color, face = ltext_face),
      axis.title.x = ggplot2::element_text(size = xlab_size, color = xlab_color, face = xlab_face),
      axis.title.y = ggplot2::element_text(size = ylab_size, color = ylab_color, face = ylab_face),
      axis.text.x = ggplot2::element_text(size = xtext_size, color = xtext_color, face = xtext_face),
      axis.text.y = ggplot2::element_text(size = ytext_size, color = ytext_color, face = ytext_face),
      legend.background = ggplot2::element_rect(fill = legend_back, color = legend_border, size = legend_border_width))

  if (title_align == "center") {
    p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  } else if (title_align == "left") {
    p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0))
  } else {
    p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 1))
  }

  if (sub_align == "center") {
    p <- p + ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5))
  } else if (sub_align == "left") {
    p <- p + ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0))
  } else {
    p <- p + ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 1))
  }

  if (ltitle_align == "center") {
    p <- p + ggplot2::theme(legend.title.align = 0.5)
  } else if (ltitle_align == "left") {
    p <- p + ggplot2::theme(legend.title.align = 0)
  } else {
    p <- p + ggplot2::theme(legend.title.align = 1)
  }

  if (title == "") {
    p <- p + ggplot2::theme(plot.title = ggplot2::element_blank())
  }

  if (subtitle == "") {
    p <- p + ggplot2::theme(plot.subtitle = ggplot2::element_blank())
  }

  if (legend_title == "") {
    p <- p + ggplot2::theme(legend.title = ggplot2::element_blank())
  }

  if (xlab == "") {
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }

  if (ylab == "") {
    p <- p + ggplot2::theme(axis.title.y = ggplot2::element_blank())
  }

  if (legend_pos == "inside") {
    p <- p + ggplot2::theme(legend.justification = c(-0.2, -0.2), legend.position = c(0, 0),
                            legend.background = ggplot2::element_rect(fill = legend_back, color = legend_border,
                                                                      size = legend_border_width),
                            legend.margin = ggplot2::margin(0.2, 0.2, 0.2, 0.2, "cm"),
                            legend.spacing = ggplot2::unit(0.4, "cm"),
                            legend.key = ggplot2::element_blank(),
                            legend.key.size = ggplot2::unit(1.5, "line"))
  }

  if (legend_pos == "none") {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  if (legend_back == "") {
    p <- p + ggplot2::theme(legend.background = ggplot2::element_blank())
  }

  if (rem_minor_x == "yes") {
    p <- p + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
  }

  if (rem_minor_y == "yes") {
    p <- p + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
  }

  if (emph_major_x == "yes") {
    p <- p + ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = major_x_col, size = major_x_width))
  }

  if (emph_major_y == "yes") {
    p <- p + ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = major_y_col, size = major_y_width))
  }

  if (save == "yes") {
    ggplot2::ggsave(plot = p, filename = save_filename, path = save_path, width = plot_width, height = plot_height, units = units)
  }

  return(p)
}
