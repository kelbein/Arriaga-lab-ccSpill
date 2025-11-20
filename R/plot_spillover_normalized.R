#' Plot Normalized Spillover Heatmap
#'
#' Creates a heatmap of the normalized spillover values, with
#' normalized values printed in the cells.
#'
#' @param ratios_dataframe The `ratios_dataframe` object returned from
#'   `create_spillover_matrix()`.
#'
#' @return A ggplot object.
#'
#' @details
#' This function requires the 'ggplot2' package.
#'
#' @examples
#' \dontrun{
#'   # (Assumes `analysis_results` exists from `create_spillover_matrix()`)
#'
#'   plot <- plot_spillover_normalized(analysis_results$ratios_dataframe)
#'   print(plot)
#' }
#'
#' @importFrom rlang .data
#' @export
plot_spillover_normalized <- function(ratios_dataframe) {

  # Note: Add dependency by running:
  # usethis::use_package("ggplot2")

  # Build the plot
  heatmap_plot_normalized <- ratios_dataframe %>%
    # --- FIX ---
    # Use .data$ for all column names in aes()
    ggplot2::ggplot(ggplot2::aes(x = .data$Measured_Channel, y = .data$Source_Channel, fill = .data$ratio_norm)) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::geom_text(
      # Corrected: subset() is a base R function, no dplyr:: prefix needed
      # --- FIX ---
      # Also use .data$ inside the filter/subset
      data = dplyr::filter(ratios_dataframe, .data$ratio_norm != 0),
      # --- FIX ---
      ggplot2::aes(label = round(.data$ratio_norm, 2)), size = 3
    ) +
    ggplot2::scale_fill_gradient(low = "white", high = "#228833") +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      title = "ccSpill Matrix",
      x = "Channel Measured",
      y = "Assigned Antibody Channel",
      fill = "Spillover"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )

  return(heatmap_plot_normalized)
}

