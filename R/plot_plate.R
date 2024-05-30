#' Plot Plates
#'
#' @param data A data frame containing the well locations and the values to fill
#' @param fill The column name of the value to fill consider wrapping in double brackets and quotes
#' @param well_id The column name of the character column of well locations
#' @param facet_rows The column name of the character column selected for the row facet
#' @param facet_cols The column name of the character column selected for the column facet
#' @param plate The size of the plate
#' @param size The size of the plotted data on the plate
#' @param shape The shape of each plotted data point on the plate
#' @param na_fill Fill color for the plate
#' @param na_size_ratio Background ratio
#' @param na_alpha Background transparency
#'
#' @return An image object
#' @export


plot_plate <- function(data,
                            fill,
                            well_id,
                            facet_rows = NULL,
                            facet_cols = NULL,
                            plate = 384,
                            size = 5,
                            shape = 22,
                            na_fill = "white",
                            na_size_ratio = 0.95,
                            na_alpha = 0.1) {

  plate_info <- platesize_check(plate)

  xlim <- c(0.5, plate_info$colmax + 0.5)
  ylim <- c(plate_info$rowmax + 0.5, 0.5)

  plotting_data <- to_rows_columns(data, well_id)

  # Bind fill data into the dataframe for plotting, as a new column
  plotting_data$fill <- as.factor(data[[fill]])

  p <- ggplot2::ggplot(plotting_data, ggplot2::aes(x = Column, y = Row, fill = fill)) +
    ggplot2::geom_point(colour = "gray20", shape = shape, size = size) +
    ggplot2::geom_point(data = expand.grid(Column = seq(plate_info$colmax), Row = seq(1, plate_info$rowmax)),
                        ggplot2::aes(x = Column, y = Row), color = "grey90", fill = na_fill,
               shape = shape, size = size * na_size_ratio, alpha = na_alpha) +
    ggplot2::coord_fixed(ratio = 1, xlim = xlim, ylim = ylim) +
    ggplot2::scale_y_reverse(breaks = seq(1, plate_info$rowmax), labels = LETTERS[1:plate_info$rowmax]) +
    ggplot2::scale_x_continuous(breaks = seq(1, plate_info$colmax), position = "top")

  # Optional faceting
  if (!is.null(facet_rows)) {
    if (!is.null(facet_cols)) {
      p <- p + ggplot2::facet_grid(rows = ggplot2::vars(.data[[facet_rows]]), cols = ggplot2::vars(.data[[facet_cols]]), labeller = ggplot2::label_both)
    } else {
      p <- p + ggplot2::facet_grid(rows = ggplot2::vars(.data[[facet_rows]]))
    }
  } else if (!is.null(facet_cols)) {
    p <- p + ggplot2::facet_grid(cols = ggplot2::vars(.data[[facet_cols]]))
  }

  return(p)
}
