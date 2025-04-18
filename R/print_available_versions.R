#' Print Available Versions
#'
#' Prints a human-readable formatted list of all available versions.
#'
#' @return Invisibly returns NULL, but prints output to console.
#'
#' @examples
#' \dontrun{
#' print_available_versions()
#' }
#'
#' @export
print_available_versions <- function() {
  # Get available versions
  available_versions <- get_available_versions()

  # Format the list of available versions
  versions_list <- paste(
    mapply(
      function(ver, access, date) {
        sprintf("\n\"%s\" (%s - %s)", ver, access, date)
      },
      available_versions$version,
      available_versions$access_right,
      available_versions$publication_date
    ),
    collapse = ", "
  )

  # Print the formatted list to console
  cat("Available versions are: ", versions_list, "\n", sep = "")

  # Return invisibly
  invisible(NULL)
}
