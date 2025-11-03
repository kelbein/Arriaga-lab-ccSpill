#' Load and Process Concentration Lookup File
#'
#' Reads a concentration meta-data file, converts it from wide to long format,
#' and generates a vector of valid isotope names.
#'
#' @param file_path The full path to the 'meta-data.csv' file.
#'
#' @return A list with two elements:
#' \item{lookup_table}{A data frame in long format with columns
#'   'isotope', 'level', and 'concentration'.}
#' \item{valid_isotopes}{A character vector of unique isotope names found
#'   in the file, with the string "all" appended.}
#'
#' @details
#' This function requires the 'readr' and 'tidyr' packages.
#'
#' @examples
#' \dontrun{
#'   # --- Create a temporary fake metadata file for the example ---
#'   temp_file <- tempfile(fileext = ".csv")
#'   writeLines(
#'     "isotope,level-1,level-2,level-3\nisoA,10,20,30\nisoB,15,25,35",
#'     temp_file
#'   )
#'
#'   # --- Run the function ---
#'   conc_data <- concentration_lookup(file_path = temp_file)
#'
#'   # --- Access the outputs ---
#'   print("--- Lookup Table ---")
#'   print(conc_data$lookup_table)
#'
#'   print("--- Valid Isotopes ---")
#'   print(conc_data$valid_isotopes)
#'
#'   # --- Clean up ---
#'   file.remove(temp_file)
#' }
#'
#' @export
concentration_lookup <- function(file_path) {
  tryCatch({
    # Read the CSV file from the path provided as an argument
    conc_lookup_wide <- readr::read_csv(file_path, show_col_types = FALSE)

    # Convert from wide to long
    conc_lookup <- conc_lookup_wide %>%
      tidyr::pivot_longer(
        cols = tidyr::starts_with("level-"),
        names_to = "level",
        values_to = "concentration",
        values_drop_na = TRUE
      )

    # --- Check for 'isotope' column ---
    if (!"isotope" %in% names(conc_lookup)) {
      stop(
        "'isotope' column not found in 'meta-data.csv'. ",
        "Check spelling and case."
      )
    }

    # --- Create a vector of all valid isotope names ---
    # This vector will be returned as part of a list
    valid_isotope_names <- c(unique(conc_lookup$isotope), "all")

    # --- Return a list containing both objects ---
    # The user can access them using $lookup_table and $valid_isotopes
    return(list(
      lookup_table = conc_lookup,
      valid_isotopes = valid_isotope_names
    ))

  }, error = function(e) {
    stop(paste(
      "Could not find or read file at:", file_path,
      "\nOriginal error:", e$message
    ))
  })
}
