#' Rename Columns Based on Numeric Suffix
#'
#' Replaces the exact isotope number with the rounded isotope number by
#' extracting the first numeric block following a period (`.`) in a character
#' vector, incrementing this number by 1, and returning the result as a new
#' character vector.
#'
#' @param col_names_vector A character vector of column names or strings.
#'   (e.g., `c("X.123.999", "Y.456.998")`)
#'
#' @return A character vector of the same length, containing the
#'   incremented numbers as strings. (e.g., `c("124", "457")`)
#'
#' @details
#' This function requires the 'stringr' package.
#'
#' The regex `(?<=\\.)\\d+` specifically looks for a sequence of digits (`\\d+`)
#' that is preceded by a literal dot (`(?<=\\.)`).
#'
#' @examples
#' \dontrun{
#'   old_names <- c("some.name.123", "another.456", "no_number", "X.789")
#'   new_names <- rename_as_isotopes(old_names)
#'   # new_names would be: c("124", "457", NA_character_, "790")
#'   print(new_names)
#' }
#'
#' @export
rename_as_isotopes <- function(col_names_vector) {

  # Extract the first sequence of digits that follows a dot
  first_numbers <- stringr::str_extract(col_names_vector, "(?<=\\.)\\d+")

  # Convert to numeric, add 1, and convert back to character
  # This will naturally produce NA for any string that didn't have a match
  new_numbers <- as.numeric(first_numbers) + 1

  return(as.character(new_numbers))
}
