#' Load and Process All Solution Mode Data
#'
#' This is the main data loading function. It walks a nested directory
#' structure (Date > DataType > Level), reads all raw .txt files,
#' calculates the mean for each, and joins the results with metadata.
#'
#' @param base_data_dir The top-level path to the data.
#' @param conc_lookup_data The metadata lookup table. This should be the
#'   `$lookup_table` object returned from your `concentration_lookup()` function.
#' @param valid_isotope_names A character vector of valid isotope names.
#'   This should be the `$valid_isotopes` object from `concentration_lookup()`.
#' @param renaming_function The function to use for renaming columns. This
#'   should be `rename_as_isotopes` (or another function you can defined).
#'
#' @return A nested list of data frames, structured by date and data type
#'   (e.g., `all_data$'date-1'$'isotope-1'`).
#'
#' @details
#' This function requires the 'dplyr' package.
#'
#' @examples
#' \dontrun{
#'   # 1. First, get the metadata
#'   meta_path <- "path/to/your/meta-data.csv"
#'   meta <- concentration_lookup(meta_path)
#'
#'   # 2. Define the data directory
#'   data_dir <- "path/to/your/ccSpill/solution-mode-data"
#'
#'   # 3. Load all the data
#'   all_data <- load_solution_data(
#'     base_data_dir = data_dir,
#'     conc_lookup_data = meta$lookup_table,
#'     valid_isotope_names = meta$valid_isotopes,
#'     renaming_function = rename_as_isotopes
#'   )
#' }
#'
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @importFrom utils read.table
#' @export
load_solution_data <- function(base_data_dir,
                               conc_lookup_data,
                               valid_isotope_names,
                               renaming_function = rename_as_isotopes) {

  # --- 1. FIND ALL "DATE" SUBFOLDERS ---
  # Uses the argument `base_data_dir`
  date_folders <- list.dirs(base_data_dir, full.names = TRUE, recursive = FALSE)
  date_names <- list.dirs(base_data_dir, full.names = FALSE, recursive = FALSE)

  # --- 2. LOOP 1: Iterate over each DATE folder ---
  all_data <- lapply(date_folders, function(date_path) {

    current_date_name <- basename(date_path)

    # --- 3. Find all subfolders ---
    datatype_folders <- list.dirs(date_path, full.names = TRUE, recursive = FALSE)
    datatype_names <- list.dirs(date_path, full.names = FALSE, recursive = FALSE)

    # --- 4. LOOP 2: Iterate over each subfolder ---
    date_level_data <- lapply(datatype_folders, function(datatype_path) {

      current_datatype_name <- basename(datatype_path)

      # --- 5. Find all "level" subfolders ---
      level_folders <- list.dirs(datatype_path, full.names = TRUE, recursive = FALSE)

      # --- 6. LOOP 3: Process and combine ALL levels in this folder ---
      list_of_level_dfs <- lapply(level_folders, function(level_path) {

        current_level_name <- basename(level_path)

        file_paths <- list.files(level_path, full.names = TRUE)
        file_names <- list.files(level_path, full.names = FALSE)

        if (length(file_paths) == 0) return(NULL)

        # --- 7. Process all files and create ONE summary row ---
        processed_list <- lapply(file_paths, function(path) {
          # Use base R read.table
          read.table(path, header = TRUE, stringsAsFactors = FALSE, comment.char = "") %>%
            dplyr::slice(31:dplyr::n()) %>% # removes first 30 rows (instrument artifact)
            dplyr::select(dplyr::contains("Dual")) %>%
            # Use the function passed as an argument
            dplyr::rename_with(.cols = dplyr::everything(), .fn = renaming_function) %>%
            # Use the vector passed as an argument
            dplyr::select(dplyr::any_of(valid_isotope_names))
        })

        names(processed_list) <- file_names
        combined_df <- dplyr::bind_rows(processed_list, .id = "source_file")

        summary_df <- combined_df %>%
          dplyr::select(-.data$source_file) %>%
          # mutate(across(everything(), as.numeric)) %>% # This is commented out because it should not be needed
          dplyr::summarise(
            dplyr::across(
              .cols = dplyr::everything(),
              .fns = list(mean = ~mean(.x, na.rm = TRUE)),
              .names = "{.col}"
            )
          )

        # --- 8. Look up metadata using .env and lowercase ---
        # Use the data frame passed as an argument
        metadata_row <- conc_lookup_data %>%
          dplyr::filter(
            .data$date == .env$current_date_name,
            .data$subfolder == .env$current_datatype_name,
            .data$level == .env$current_level_name
          )

        if (nrow(metadata_row) == 0) {
          warning(paste(
            "No metadata found for:",
            current_date_name,
            current_datatype_name,
            current_level_name
          ))
          conc_value <- NA_real_
          isotope_value <- NA_character_

        } else {
          conc_value <- metadata_row$concentration[1]
          isotope_value <- metadata_row$isotope[1]
        }

        # Add all metadata to the single summary row
        summary_df_with_meta <- summary_df %>%
          dplyr::mutate(
            concentration = conc_value,
            isotope = isotope_value,
            level = current_level_name,
            date = current_date_name,
            subfolder = current_datatype_name
          ) %>%
          # Select lowercase columns
          dplyr::select(.data$date, .data$subfolder, .data$level, .data$isotope, .data$concentration,
                        dplyr::everything())

        return(summary_df_with_meta)

      }) # --- End Setting loop (Loop 3) ---

      # --- 9. Combine all setting-summary-rows into one data frame ---
      final_datatype_df <- dplyr::bind_rows(list_of_level_dfs)

      return(final_datatype_df)

    }) # --- End DataType loop (Loop 2) ---

    names(date_level_data) <- datatype_names
    return(date_level_data)

  }) # --- End Date loop (Loop 1) ---

  # --- 10. Name the top-level (Date) list ---
  names(all_data) <- date_names

  # --- 11. Return the final object ---
  return(all_data)
}
