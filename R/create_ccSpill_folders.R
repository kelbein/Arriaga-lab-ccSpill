#' Create a new project from the ccSpill template
#'
#' This function will copy the 'my_project_template' folder structure
#' into a new directory on your computer, so you can start a
#' new analysis.
#'
#' @param path The name of the new project directory to be created.
#' @export
#' @examples
#' \dontrun{
#' # Creates a new folder named "MyAnalysis" in your current directory
#' create_ccSpill_project("MyAnalysis")
#' }
create_ccSpill_folders <- function(path) {

  # 1. Find the path to the template folder inside your installed package
  template_path <- system.file(
    "data",
    package = "ccSpill"
  )

  # Check if the template exists
  if (template_path == "") {
    stop("Could not find package template. Try re-installing 'ccSpill'.", call. = FALSE)
  }

  # 2. Check if the target directory already exists
  if (dir.exists(path)) {
    stop(paste0("The directory '", path, "' already exists. Please choose a new name."), call. = FALSE)
  }

  # 3. Copy the entire folder structure
  file.copy(from = template_path, to = ".", recursive = TRUE)

  # 4. Rename the copied folder to the user's chosen name
  file.rename(from = "my_project_template", to = path)

  # 5. Print a success message
  message(paste0("Project '", path, "' created successfully in your working directory."))

}
