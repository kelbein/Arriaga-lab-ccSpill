#' Plot Spillover Values Heatmap
#'
#' Creates a heatmap of the raw isotope-to-metal ratios, with ratio
#' and error values printed in the cells.
#'
#' @param ratios_dataframe The `ratios_dataframe` object returned from
#'   `create_spillover_matrix()`.
#'
#' @return A ggplot object.
#'
#' @details
#' This function requires the 'ggplot2' and 'dplyr' packages.
#'
#' @examples
#' \dontrun{
#'   # (Assumes `analysis_results` exists from `create_spillover_matrix()`)
#'
#'   plot <- plot_spillover_values(analysis_results$ratios_dataframe)
#'   print(plot)
#' }
#'
#' @importFrom rlang .data
#' @export
plot_spillover_values <- function(ratios_dataframe) {

  # Create the label column.
  # We use the input dataframe, which has Source_Channel and Measured_Channel
  df_ratios_to_plot <- ratios_dataframe %>%
    dplyr::mutate(
      ratio_label = paste0(
        sprintf("%.1f", .data$ratio),       # Format ratio to 1 decimal places
        "\U00B1",                          # Add a plus/minus symbol
        sprintf("%.1f", .data$ratio_error)  # Format error to 1 decimal places
      )
    )

  # Build the plot
  heatmap_plot_values <- df_ratios_to_plot %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$Measured_Channel, y = .data$Source_Channel, fill = .data$ratio)) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::geom_text(
      data = subset(df_ratios_to_plot, .data$ratio_norm != 0),
      ggplot2::aes(label = .data$ratio_label), size = 2
    ) +
    ggplot2::scale_fill_gradient(low = "white", high = "#4477AA") +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      title = "Metals per Antibody for Each Channel",
      x = "Channel Measured",
      y = "Assigned Antibody Channel",
      fill = "Antibody Value"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )

  return(heatmap_plot_values)
}
