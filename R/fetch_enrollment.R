# ==============================================================================
# Enrollment Data Fetching Functions
# ==============================================================================
#
# This file contains functions for downloading enrollment data from the
# New Mexico Public Education Department (PED) website.
#
# Data Availability:
# - 2016-2018: Enrollment by district/location/grade only (no demographic subgroups)
# - 2019-2025: Subgroup enrollment with race/ethnicity and special populations
# - Note: 2024 (SY 2023-24) only has 80D data; 40D subgroup file not yet published
#
# ==============================================================================

#' Fetch New Mexico enrollment data
#'
#' Downloads and processes enrollment data from the New Mexico Public Education
#' Department's 40-Day enrollment reports.
#'
#' @param end_year A school year. Year is the end of the academic year - eg 2023-24
#'   school year is year '2024'. Valid values are 2016-2025.
#' @param tidy If TRUE (default), returns data in long (tidy) format with subgroup
#'   column. If FALSE, returns wide format as downloaded.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from NM PED.
#' @return Data frame with enrollment data. Wide format includes columns for
#'   district_code, school_code, names, and enrollment counts by demographic/grade.
#'   Tidy format pivots these counts into subgroup and n_students columns.
#'
#' @section Data Availability:
#' \describe{
#'   \item{2016-2018}{Enrollment by grade only (no demographic subgroups)}
#'   \item{2019-2023, 2025}{Full demographic subgroups available}
#'   \item{2024}{Only 80-Day enrollment data available; 40-Day subgroup file not published}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get 2024 enrollment data (2023-24 school year)
#' enr_2024 <- fetch_enr(2024)
#'
#' # Get wide format
#' enr_wide <- fetch_enr(2024, tidy = FALSE)
#'
#' # Force fresh download (ignore cache)
#' enr_fresh <- fetch_enr(2024, use_cache = FALSE)
#'
#' # Filter to specific district
#' albuquerque <- enr_2024 %>%
#'   dplyr::filter(grepl("Albuquerque", district_name, ignore.case = TRUE))
#' }
fetch_enr <- function(end_year, tidy = TRUE, use_cache = TRUE) {

  # Validate year
  valid_years <- get_valid_years()
  if (!end_year %in% valid_years) {
    stop(paste0(
      "end_year must be between ", min(valid_years), " and ", max(valid_years), ".\n",
      "Note: Year refers to the spring semester (e.g., 2024 = 2023-24 school year)"
    ))
  }

  # Warn about 2024 data limitations
  if (end_year == 2024) {
    message("Note: 2024 (SY 2023-24) only has 80-Day enrollment data available.")
    message("      40-Day subgroup data has not been published by NM PED.")
  }

  # Determine cache type based on tidy parameter
  cache_type <- if (tidy) "tidy" else "wide"

  # Check cache first
  if (use_cache && cache_exists(end_year, cache_type)) {
    message(paste("Using cached data for", end_year))
    return(read_cache(end_year, cache_type))
  }

  # Get raw data from NM PED
  raw <- get_raw_enr(end_year)

  # Process to standard schema
  processed <- process_enr(raw, end_year)

  # Optionally tidy
  if (tidy) {
    processed <- tidy_enr(processed) %>%
      id_enr_aggs()
  }

  # Cache the result
  if (use_cache) {
    write_cache(processed, end_year, cache_type)
  }

  processed
}


#' Fetch enrollment data for multiple years
#'
#' Downloads and combines enrollment data for multiple school years.
#'
#' @param end_years Vector of school year ends (e.g., c(2022, 2023, 2024))
#' @param tidy If TRUE (default), returns data in long (tidy) format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#' @return Combined data frame with enrollment data for all requested years
#' @export
#' @examples
#' \dontrun{
#' # Get 3 years of data
#' enr_multi <- fetch_enr_multi(2022:2024)
#'
#' # Track enrollment trends
#' enr_multi %>%
#'   dplyr::filter(is_state, subgroup == "total", grade_level == "TOTAL") %>%
#'   dplyr::select(end_year, n_students)
#' }
fetch_enr_multi <- function(end_years, tidy = TRUE, use_cache = TRUE) {

  # Validate years
  valid_years <- get_valid_years()
  invalid_years <- end_years[!end_years %in% valid_years]
  if (length(invalid_years) > 0) {
    stop(paste0(
      "Invalid years: ", paste(invalid_years, collapse = ", "), "\n",
      "end_year must be between ", min(valid_years), " and ", max(valid_years)
    ))
  }

  # Fetch each year
  results <- purrr::map(
    end_years,
    function(yr) {
      message(paste("Fetching", yr, "..."))
      fetch_enr(yr, tidy = tidy, use_cache = use_cache)
    }
  )

  # Combine
  dplyr::bind_rows(results)
}
