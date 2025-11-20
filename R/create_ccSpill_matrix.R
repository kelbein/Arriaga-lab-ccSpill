#' Create a Spillover Matrix from Loaded Data
#'
#' This function runs the complete analysis pipeline on the nested list
#' returned by `load_solution_data()`. It combines all data, pivots
#' to a long format, runs linear regressions (`lm(MeanSignal ~ concentration)`),
#' calculates isotope-to-metals ratios, normalizes the ratios, and
#' finally produces a wide-format spillover matrix. It also produces
#' a tidy dataframe for examination (plotting).
#'
#' @param data A nested list of data frames, the output of
#'   `load_solution_data()`.
#'
#' @return A list containing two elements:
#' \itemize{
#'   \item \code{spillover_matrix}: A spillover matrix, with source channels (isotopes) as columns and measured channels as rows.
#'   \item \code{ratios_dataframe}: The processed \code{df_ratios} data frame used to build the matrix.
#' }
#'
#' @details
#' This function requires the 'dplyr', 'tidyr', 'purrr', and 'broom' packages.
#'
#' @examples
#' \dontrun{
#'   # (Assumes `all_data` has been loaded using `load_solution_data()`)
#'
#'   # 1. Create the outputs
#'   analysis_results <- create_ccSpill_matrix(all_data)
#'
#'   # 2. View the results
#'   print(analysis_results$spillover_matrix)
#'   print(head(analysis_results$ratios_dataframe))
#' }
#'
#' @importFrom rlang .data
#' @export
create_ccSpill_matrix <- function(all_data) {

  # --- 1. COMBINE ALL NESTED LISTS INTO ONE DATA FRAME ---
  all_summary_data <- dplyr::bind_rows(
    lapply(all_data, dplyr::bind_rows)
  )


  # --- 2. PIVOT DATA TO A LONG (TIDY) FORMAT ---
  metadata_cols <- c("date", "subfolder", "level", "isotope", "concentration")

  # Identify any columns that are NOT metadata
  measurement_cols <- setdiff(names(all_summary_data), metadata_cols)

  if (length(measurement_cols) == 0) {
    stop(paste(
      "Error: No measurement columns found in the loaded data.",
      "This usually means the column names in your raw files (after renaming)",
      "did not match the 'isotope' names in your meta-data.csv.",
      "\n\nColumns found:", paste(names(all_summary_data), collapse = ", ")
    ))
  }

  long_data <- all_summary_data %>%
    tidyr::pivot_longer(
      cols = -dplyr::all_of(metadata_cols),
      names_to = "channel",
      values_to = "MeanSignal"
    )

  # --- 3. RUN "MANY MODELS" AND EXTRACT COEFFICIENTS ---
  regression_results <- long_data %>%
    dplyr::group_by(.data$date, .data$subfolder, .data$isotope, .data$channel) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      model = purrr::map(data, ~tryCatch(lm(MeanSignal ~ concentration, data = .x),
                                         error = function(e) NA)),
      tidied_model = purrr::map(.data$model, broom::tidy)
    ) %>%
    tidyr::unnest(.data$tidied_model) %>%
    dplyr::filter(.data$term == "concentration") %>%
    dplyr::select(
      .data$date,
      .data$subfolder,
      .data$isotope,
      .data$channel,
      slope = .data$estimate,
      slope_error = .data$std.error,
    ) %>%
    dplyr::ungroup()


  # --- 4. CALCULATE RATIOS AND ERROR ---
  calculate_isotope_ratios <- function(df_slopes) {

    df_iso <- df_slopes %>%
      dplyr::filter(grepl("^isotope", .data$subfolder)) %>%
      dplyr::rename(isotope_slope = .data$slope, isotope_error = .data$slope_error)

    df_metals <- df_slopes %>%
      dplyr::filter(.data$subfolder == "metals") %>%
      dplyr::rename(metals_slope = .data$slope, metals_error = .data$slope_error) %>%
      dplyr::select(.data$date, .data$channel, .data$metals_slope, .data$metals_error)

    df_merged <- dplyr::inner_join(df_iso, df_metals, by = c("date", "channel"))

    df_final_ratio <- df_merged %>%
      dplyr::mutate(
        ratio = .data$isotope_slope / .data$metals_slope,
        frac_error_iso_sq = ifelse(.data$isotope_slope == 0, 0, (.data$isotope_error / .data$isotope_slope)^2),
        frac_error_metals_sq = ifelse(.data$metals_slope == 0, 0, (.data$metals_error / .data$metals_slope)^2),
        ratio_error = ifelse(ratio == 0, 0,
                             abs(ratio) * sqrt(frac_error_iso_sq + frac_error_metals_sq))
      ) %>%
      dplyr::select(
        .data$isotope,
        .data$channel,
        .data$ratio,
        .data$ratio_error
      )

    return(df_final_ratio)
  }

  # --- 5. TURN INTO MATRIX  ---
  df_ratios <- calculate_isotope_ratios(regression_results)
  df_ratios <- df_ratios %>%
    dplyr::arrange(.data$channel) %>%
    dplyr::arrange(.data$isotope)

  df_ratios <- df_ratios %>%
    dplyr::group_by(.data$isotope) %>%
    dplyr::mutate(
      ratio_norm = ratio / ratio[isotope == channel],
      ratio_norm = ifelse(!is.finite(ratio_norm) | ratio_norm < 0.005, 0, ratio_norm)
    ) %>%
    dplyr::ungroup()

  # Rename for the matrix
  names(df_ratios)[names(df_ratios) == "isotope"] <- "Source_Channel"
  names(df_ratios)[names(df_ratios) == "channel"] <- "Measured_Channel"

  matrix_rows <- sort(unique(df_ratios$Measured_Channel))

  df_spillover <- df_ratios %>%
    dplyr::select(.data$Source_Channel, .data$Measured_Channel, .data$ratio_norm)

  ccSpill_matrix <- df_spillover %>%
    tidyr::pivot_wider(
      names_from = .data$Source_Channel,
      values_from = .data$ratio_norm,
      values_fill = 0
    ) %>%
    dplyr::arrange(.data$Measured_Channel) %>%
    dplyr::select(!.data$Measured_Channel) %>%
    as.matrix()

  rownames(ccSpill_matrix) <- matrix_rows

  # Return both the final matrix and the ratios dataframe
  return(
    list(
      spillover_matrix = ccSpill_matrix,
      ratios_dataframe = df_ratios
    )
  )
}
