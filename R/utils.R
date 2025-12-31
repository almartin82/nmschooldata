# ==============================================================================
# Utility Functions
# ==============================================================================

#' Pipe operator
#'
#' See \code{dplyr::\link[dplyr:reexports]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Convert to numeric, handling suppression markers
#'
#' NM PED uses various markers for suppressed data (*, <5, -, etc.)
#' and may use commas in large numbers.
#'
#' @param x Vector to convert
#' @return Numeric vector with NA for non-numeric values
#' @keywords internal
safe_numeric <- function(x) {
  # Remove commas and whitespace
  x <- gsub(",", "", x)
  x <- trimws(x)

  # Handle common suppression markers
  x[x %in% c("*", ".", "-", "-1", "<5", "<10", "N/A", "NA", "", "n/a")] <- NA_character_

  suppressWarnings(as.numeric(x))
}


#' Clean and standardize column names
#'
#' Converts column names to lowercase and replaces spaces/special chars
#' with underscores.
#'
#' @param names Vector of column names
#' @return Vector of cleaned column names
#' @keywords internal
clean_names <- function(names) {
  names <- tolower(names)
  names <- gsub("[[:space:]]+", "_", names)
  names <- gsub("[^a-z0-9_]", "", names)
  names <- gsub("_+", "_", names)
  names <- gsub("^_|_$", "", names)
  names
}


#' Get valid enrollment years
#'
#' Returns a vector of school years for which enrollment data is available.
#'
#' Data availability by year:
#' \itemize{
#'   \item 2016-2018: Enrollment by grade only (no demographic subgroups)
#'   \item 2019-2023, 2025: Full demographic subgroups available
#'   \item 2024: Only 80-Day data available (40-Day subgroup file not published)
#' }
#'
#' @return Vector of valid end years
#' @keywords internal
get_valid_years <- function() {

  # Available years based on NM PED website research:
  # - 2016-2018: Enrollment by district/location/grade only (no subgroups)
  # - 2019-2023, 2025: Full 40D subgroup enrollment data
  # - 2024: Only 80D data available (40D subgroup file not published)
  2016:2025
}


#' Validate year parameter
#'
#' @param end_year School year end to validate
#' @param allow_future If TRUE, allows years up to current + 1
#' @return TRUE if valid, otherwise throws error
#' @keywords internal
validate_year <- function(end_year, allow_future = FALSE) {
  valid_years <- get_valid_years()
  min_year <- min(valid_years)
  max_year <- if (allow_future) as.integer(format(Sys.Date(), "%Y")) + 1 else max(valid_years)

  if (!is.numeric(end_year) || length(end_year) != 1) {
    stop("end_year must be a single numeric value")
  }

  if (end_year < min_year || end_year > max_year) {
    stop(paste0(
      "end_year must be between ", min_year, " and ", max_year, ".\n",
      "Note: Year refers to the spring semester (e.g., 2024 = 2023-24 school year)"
    ))
  }

  TRUE
}
