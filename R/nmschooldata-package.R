#' nmschooldata: Fetch and Process New Mexico School Data
#'
#' Downloads and processes school data from the New Mexico Public Education
#' Department (PED). Provides functions for fetching enrollment data from
#' the STARS (Student Teacher Accountability Reporting System) and transforming
#' it into tidy format for analysis.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{fetch_enr}}}{Fetch enrollment data for a school year}
#'   \item{\code{\link{fetch_enr_multi}}}{Fetch enrollment data for multiple years}
#'   \item{\code{\link{tidy_enr}}}{Transform wide data to tidy (long) format}
#'   \item{\code{\link{id_enr_aggs}}}{Add aggregation level flags}
#'   \item{\code{\link{enr_grade_aggs}}}{Create grade-level aggregations}
#' }
#'
#' @section Cache functions:
#' \describe{
#'   \item{\code{\link{cache_status}}}{View cached data files}
#'   \item{\code{\link{clear_cache}}}{Remove cached data files}
#' }
#'
#' @section ID System:
#' New Mexico uses a hierarchical ID system:
#' \itemize{
#'   \item District codes: 3 digits (e.g., 001 = Albuquerque Public Schools)
#'   \item School codes: 3 digits within each district
#'   \item Full IDs combine district and school codes
#' }
#'
#' @section Data Sources:
#' Data is sourced from the New Mexico Public Education Department:
#' \itemize{
#'   \item PED Main: \url{https://webnew.ped.state.nm.us/}
#'   \item STARS: \url{https://web.ped.nm.gov/bureaus/information-technology/stars/}
#'   \item NM Vistas: \url{https://nmvistas.org/}
#' }
#'
#' @section Data Availability:
#' Enrollment data with subgroup demographics is available from:
#' \itemize{
#'   \item 2019 (SY 2018-2019) through present
#'   \item Earlier years may have different formats or require different sources
#'   \item Data uses 40-Day (40D) enrollment counts
#' }
#'
#' @section New Mexico Context:
#' New Mexico has approximately 89 school districts. The state has a
#' majority-minority student population with Hispanic students representing
#' the largest demographic group. The state also has significant Native
#' American student enrollment, particularly in districts near tribal lands.
#'
#' @docType package
#' @name nmschooldata-package
#' @aliases nmschooldata
#' @keywords internal
"_PACKAGE"

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
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
