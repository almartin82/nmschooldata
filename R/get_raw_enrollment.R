# ==============================================================================
# Raw Enrollment Data Download Functions
# ==============================================================================
#
# This file contains functions for downloading raw enrollment data from the
# New Mexico Public Education Department (PED).
#
# Data comes from the STARS system (Student Teacher Accountability Reporting System):
# https://web.ped.nm.gov/bureaus/information-technology/stars/
#
# The 40-Day enrollment reports contain enrollment counts at state, district,
# and school levels with demographic breakdowns.
#
# Data Availability:
# - Era 1 (2016-2018): Enrollment by district/location/grade only (no subgroups)
#   Files are .xls format with total enrollment counts by grade
# - Era 2 (2019-2025): Subgroup enrollment with demographics
#   Files are .xlsx format with race/ethnicity and special population breakdowns
#
# Note: 2024 (SY 2023-24) only has 80D data available; 40D subgroup file not published
#
# ==============================================================================

#' Download raw enrollment data from NM PED
#'
#' Downloads the 40-Day enrollment report Excel file from NM PED's website.
#' NM PED publishes enrollment counts annually with breakdowns by demographics.
#'
#' @param end_year School year end (2023-24 = 2024)
#' @return Data frame with enrollment data
#' @keywords internal
get_raw_enr <- function(end_year) {

  valid_years <- get_valid_years()
  if (!end_year %in% valid_years) {
    stop(paste0(
      "end_year must be between ", min(valid_years), " and ", max(valid_years), ".\n",
      "Note: Year refers to the spring semester (e.g., 2024 = 2023-24 school year)"
    ))
  }

  message(paste("Downloading NM PED enrollment data for", end_year, "..."))

  # Download the Excel file
  excel_path <- download_enrollment_file(end_year)

  if (is.null(excel_path)) {
    stop(paste("Failed to download enrollment data for year", end_year))
  }

  # Read and parse the Excel file based on format era
  if (end_year <= 2018) {
    result <- parse_era1_excel(excel_path, end_year)
  } else {
    result <- parse_era2_excel(excel_path, end_year)
  }

  # Clean up temp file
  unlink(excel_path)

  result
}


#' Download enrollment Excel file from NM PED
#'
#' Tries multiple URL patterns to find the enrollment file.
#' NM PED uses different naming conventions across years.
#'
#' @param end_year School year end
#' @return Path to downloaded temp file, or NULL if download failed
#' @keywords internal
download_enrollment_file <- function(end_year) {

  # Build potential URLs based on observed patterns
  urls <- build_enrollment_urls(end_year)

  # Determine file extension based on era
  file_ext <- if (end_year <= 2018) ".xls" else ".xlsx"

  # Try each URL until one works
  tname <- tempfile(
    pattern = paste0("nm_enr_", end_year, "_"),
    tmpdir = tempdir(),
    fileext = file_ext
  )

  for (url in urls) {
    message(paste("  Trying:", url))

    tryCatch({
      response <- httr::GET(
        url,
        httr::write_disk(tname, overwrite = TRUE),
        httr::timeout(120),
        httr::user_agent("Mozilla/5.0 (compatible; nmschooldata R package)")
      )

      # Check for successful download
      if (!httr::http_error(response)) {
        # Verify it's actually an Excel file (check file size)
        file_info <- file.info(tname)
        if (file_info$size > 1000) {
          # Check if it's a valid Excel file by trying to read sheet names
          sheets <- tryCatch(
            readxl::excel_sheets(tname),
            error = function(e) NULL
          )

          if (!is.null(sheets) && length(sheets) > 0) {
            message(paste("  Downloaded successfully:", basename(url)))
            message(paste("  Sheets found:", paste(sheets, collapse = ", ")))
            return(tname)
          }
        }
      }
    }, error = function(e) {
      # Continue to next URL
    })
  }

  # If all URLs failed, return NULL
  message("  All download attempts failed")
  NULL
}


#' Build potential enrollment file URLs
#'
#' NM PED uses various URL patterns across years. This function generates
#' a list of URLs to try based on observed patterns.
#'
#' URL patterns discovered through research:
#'
#' Era 1 (2016-2018) - Enrollment by district/location/grade only:
#' - 2016: /wp-content/uploads/2018/01/2015-2016-Enrollment-by-district-by-location-by-grade-1.xls
#' - 2017: /wp-content/uploads/2018/01/2016-2017-Enrollment-by-district-by-location-by-grade-1.xls
#' - 2018: /wp-content/uploads/2018/01/2017-2018-Enrollment-by-district-by-location-by-grade-1.xls
#'
#' Era 2 (2019-2025) - Subgroup enrollment with demographics:
#' - 2019: /wp-content/uploads/2025/01/SY2018-2019-Subgroup-Enrollment-by-Grade-40D-by-district-by-school-final-1.xlsx
#' - 2020: /wp-content/uploads/2025/01/SY2019-2020-Subgroup-Enrollment-by-Grade-40D-by-district-by-school-final-1.xlsx
#' - 2021: /wp-content/uploads/2025/01/SY2020-2021-Subgroup-Enrollment-by-Grade-40D-by-district-by-school-final-1.xlsx
#' - 2022: /wp-content/uploads/2025/01/SY2021-2022-Subgroup-Enrollment-by-Grade-40D-by-district-by-school_Final.xlsx
#' - 2023: /wp-content/uploads/2025/01/SY-22-23-40D-Enrollment-Subgroup-Percentages-with-Averages-Certified.xlsx
#' - 2024: /wp-content/uploads/2025/01/SY2023_2024_80D_Enrollment_By_District_by_Grade_Certified_20240605.xlsx (80D only)
#' - 2025: /wp-content/uploads/_legacy/2025/02/SY2024_2025_40D_Enrollment_Subgroup_Percentages.xlsx
#'
#' @param end_year School year end
#' @return Character vector of URLs to try
#' @keywords internal
build_enrollment_urls <- function(end_year) {

  # Base URLs for NM PED
  base_new <- "https://web.ped.nm.gov/wp-content/uploads"
  base_old <- "https://webnew.ped.state.nm.us/wp-content/uploads"

  # School year notation
  sy_dash <- paste0(end_year - 1, "-", end_year)
  sy_short_dash <- paste0(substr(as.character(end_year - 1), 3, 4), "-", substr(as.character(end_year), 3, 4))

  urls <- c()

  if (end_year <= 2018) {
    # ===========================================================================
    # Era 1: Enrollment by district/location/grade only (2016-2018)
    # These are .xls files without demographic subgroups
    # ===========================================================================
    urls <- c(
      # Primary location on new site
      paste0(base_new, "/2025/01/", end_year - 1, "-", end_year, "-Enrollment-by-district-by-location-by-grade-1.xls"),

      # Older location on webnew site
      paste0(base_old, "/2018/01/", end_year - 1, "-", end_year, "-Enrollment-by-district-by-location-by-grade-1.xls"),
      paste0(base_old, "/2018/01/", end_year - 1, "-", end_year, "-Enrollment-by-district-by-location-by-grade.xls"),

      # Alternative patterns
      paste0(base_new, "/2018/01/", end_year - 1, "-", end_year, "-Enrollment-by-district-by-location-by-grade-1.xls")
    )

  } else if (end_year == 2024) {
    # ===========================================================================
    # Special case: 2024 (SY 2023-24)
    # Only 80D data is available; 40D subgroup file not published
    # ===========================================================================
    urls <- c(
      # 80D enrollment data (what's available)
      paste0(base_new, "/2025/01/SY2023_2024_80D_Enrollment_By_District_by_Grade_Certified_20240605.xlsx"),

      # Try potential 40D patterns in case they're published later
      paste0(base_new, "/2025/01/SY2023-2024-Subgroup-Enrollment-by-Grade-40D-by-district-by-school-final-1.xlsx"),
      paste0(base_new, "/2025/01/SY2023_2024_40D_Enrollment_Subgroup_Percentages.xlsx"),
      paste0(base_new, "/_legacy/2025/02/SY2023_2024_40D_Enrollment_Subgroup_Percentages.xlsx")
    )

  } else if (end_year == 2025) {
    # ===========================================================================
    # 2025 (SY 2024-25) - Most recent data
    # ===========================================================================
    urls <- c(
      paste0(base_new, "/_legacy/2025/02/SY2024_2025_40D_Enrollment_Subgroup_Percentages.xlsx"),
      paste0(base_new, "/2025/02/SY2024_2025_40D_Enrollment_Subgroup_Percentages.xlsx"),
      paste0(base_new, "/2025/01/SY2024_2025_40D_Enrollment_Subgroup_Percentages.xlsx"),
      paste0(base_new, "/2025/01/SY2024-2025-Subgroup-Enrollment-by-Grade-40D-by-district-by-school-final-1.xlsx")
    )

  } else if (end_year == 2023) {
    # ===========================================================================
    # 2023 (SY 2022-23) - Different naming pattern
    # ===========================================================================
    urls <- c(
      paste0(base_new, "/2025/01/SY-22-23-40D-Enrollment-Subgroup-Percentages-with-Averages-Certified.xlsx"),
      paste0(base_old, "/2022/12/SY-22-23-40D-Enrollment-Subgroup-Percentages-with-Averages-Certified.xlsx"),
      paste0(base_new, "/2025/01/SY2022-2023-Subgroup-Enrollment-by-Grade-40D-by-district-by-school-final-1.xlsx")
    )

  } else {
    # ===========================================================================
    # Era 2: Subgroup enrollment with demographics (2019-2022)
    # ===========================================================================
    urls <- c(
      # Primary patterns on new site
      paste0(base_new, "/2025/01/SY", end_year - 1, "-", end_year, "-Subgroup-Enrollment-by-Grade-40D-by-district-by-school-final-1.xlsx"),
      paste0(base_new, "/2025/01/SY", end_year - 1, "-", end_year, "-Subgroup-Enrollment-by-Grade-40D-by-district-by-school_Final.xlsx"),

      # Older patterns on webnew site
      paste0(base_old, "/2022/05/SY", end_year - 1, "-", end_year, "-Subgroup-Enrollment-by-Grade-40D-by-district-by-school-final-1.xlsx"),
      paste0(base_old, "/2022/02/SY", end_year - 1, "-", end_year, "-Subgroup-Enrollment-by-Grade-40D-by-district-by-school_Final.xlsx"),

      # Alternative patterns
      paste0(base_new, "/2025/01/SY", end_year - 1, "_", end_year, "_40D_Enrollment_Subgroup_Percentages.xlsx")
    )
  }

  urls
}


#' Parse Era 1 Excel file (2016-2018)
#'
#' Older NM PED enrollment files contain only total enrollment by grade,
#' without demographic subgroup breakdowns. These files have 8 header rows
#' that need to be skipped.
#'
#' @param excel_path Path to downloaded Excel file
#' @param end_year School year end
#' @return Data frame with enrollment data
#' @keywords internal
parse_era1_excel <- function(excel_path, end_year) {

  sheets <- readxl::excel_sheets(excel_path)
  message(paste("  Available sheets:", paste(sheets, collapse = ", ")))

  # Era 1 files have 8 header rows to skip
  # Row 9 contains the actual column headers
  data <- readxl::read_excel(
    excel_path,
    sheet = 1,
    skip = 8,
    col_types = "text",
    .name_repair = "unique_quiet"
  )

  # Clean column names
  names(data) <- clean_names(names(data))

  # Add end_year and mark as era 1 (no subgroups)
  data$end_year <- end_year
  data$data_era <- 1L

  data
}


#' Parse Era 2 Excel file (2019-present)
#'
#' Newer NM PED enrollment files include demographic subgroup breakdowns
#' by race/ethnicity and special populations. Different year files have
#' different structures:
#' - 2019-2023: Have 10 header rows to skip, single sheet named "Masked"
#' - 2024: 80-Day file with NO header rows, district-level only, grade columns
#' - 2025: NO header rows, three sheets (by School, by District, State)
#'
#' @param excel_path Path to downloaded Excel file
#' @param end_year School year end
#' @return Data frame with enrollment data
#' @keywords internal
parse_era2_excel <- function(excel_path, end_year) {

  sheets <- readxl::excel_sheets(excel_path)
  message(paste("  Available sheets:", paste(sheets, collapse = ", ")))

  if (end_year == 2025) {
    # 2025 has three sheets: "SY25 by School", "SY25 by District", "SY25 State"
    # Read all three and combine
    data <- parse_2025_excel(excel_path, sheets)
  } else if (end_year == 2024) {
    # 2024 80-Day file has NO header rows, column names in first row
    data <- readxl::read_excel(
      excel_path,
      sheet = 1,
      col_types = "text",
      .name_repair = "unique_quiet"
    )
  } else {
    # 2019-2023: Have 10 header rows to skip
    data <- readxl::read_excel(
      excel_path,
      sheet = 1,
      skip = 10,
      col_types = "text",
      .name_repair = "unique_quiet"
    )
  }

  # Clean column names
  names(data) <- clean_names(names(data))

  # Add end_year and mark as era 2 (with subgroups)
  data$end_year <- end_year
  data$data_era <- 2L

  data
}


#' Parse 2025 Excel file with multiple sheets
#'
#' The 2025 file has three sheets that need to be combined:
#' - "SY25 by School": School-level data
#' - "SY25 by District": District-level aggregates
#' - "SY25 State": State-level aggregate
#'
#' @param excel_path Path to downloaded Excel file
#' @param sheets Vector of sheet names
#' @return Combined data frame
#' @keywords internal
parse_2025_excel <- function(excel_path, sheets) {

  # Read each sheet
  school_sheet <- grep("by School", sheets, value = TRUE, ignore.case = TRUE)
  district_sheet <- grep("by District", sheets, value = TRUE, ignore.case = TRUE)
  state_sheet <- grep("State", sheets, value = TRUE, ignore.case = TRUE)

  result <- list()

  if (length(school_sheet) > 0) {
    school_data <- readxl::read_excel(
      excel_path,
      sheet = school_sheet[1],
      col_types = "text",
      .name_repair = "unique_quiet"
    )
    school_data$sheet_type <- "School"
    result <- c(result, list(school_data))
  }

  if (length(district_sheet) > 0) {
    district_data <- readxl::read_excel(
      excel_path,
      sheet = district_sheet[1],
      col_types = "text",
      .name_repair = "unique_quiet"
    )
    district_data$sheet_type <- "District"
    result <- c(result, list(district_data))
  }

  if (length(state_sheet) > 0) {
    state_data <- readxl::read_excel(
      excel_path,
      sheet = state_sheet[1],
      col_types = "text",
      .name_repair = "unique_quiet"
    )
    state_data$sheet_type <- "State"
    result <- c(result, list(state_data))
  }

  # Combine all sheets
  if (length(result) > 0) {
    dplyr::bind_rows(result)
  } else {
    # Fallback: read first sheet
    readxl::read_excel(
      excel_path,
      sheet = 1,
      col_types = "text",
      .name_repair = "unique_quiet"
    )
  }
}


#' Get column mapping for NM PED enrollment data
#'
#' Returns mappings from NM PED column names to standardized names.
#' NM PED uses various column naming conventions across years.
#'
#' @return Named list of column mappings
#' @keywords internal
get_nm_column_map <- function() {
  list(
    # ID columns
    district_code = c("district_code", "districtcode", "dist_code", "district"),
    district_name = c("district_name", "districtname", "dist_name", "district"),
    school_code = c("school_code", "schoolcode", "location_code", "locationcode",
                    "school", "location"),
    school_name = c("school_name", "schoolname", "location_name", "locationname",
                    "school", "location"),

    # Total enrollment
    total = c("total", "total_enrollment", "totalenrollment", "enrollment",
              "all_students", "allstudents"),

    # Demographics - Ethnicity (NM PED naming)
    white = c("white", "caucasian", "white_not_hispanic"),
    black = c("black", "african_american", "black_or_african_american",
              "african_american_not_hispanic"),
    hispanic = c("hispanic", "hispanic_latino", "hispanic_or_latino"),
    asian = c("asian", "asian_not_hispanic"),
    native_american = c("american_indian", "native_american", "american_indian_alaska_native",
                        "american_indian_or_alaska_native", "am_indian_alaskan"),
    pacific_islander = c("pacific_islander", "native_hawaiian_pacific_islander",
                         "native_hawaiian_or_other_pacific_islander", "hawaiian_pac_isl"),
    multiracial = c("two_or_more", "two_or_more_races", "multiracial", "multi_racial"),

    # Demographics - Gender
    male = c("male", "males", "m"),
    female = c("female", "females", "f"),

    # Special populations
    econ_disadv = c("economically_disadvantaged", "econ_disadv", "frl",
                    "free_reduced_lunch", "low_income"),
    ell = c("el", "ell", "english_learner", "english_learners",
            "limited_english_proficient", "english_language_learner"),
    special_ed = c("sped", "special_ed", "special_education",
                   "students_with_disabilities", "swd", "iep"),

    # Grade levels (NM uses PK through 12)
    grade_pk = c("pk", "prek", "pre_k", "preschool", "pre_kindergarten"),
    grade_k = c("k", "kg", "kindergarten", "kinder"),
    grade_01 = c("1", "01", "grade_1", "grade_01", "1st", "g01"),
    grade_02 = c("2", "02", "grade_2", "grade_02", "2nd", "g02"),
    grade_03 = c("3", "03", "grade_3", "grade_03", "3rd", "g03"),
    grade_04 = c("4", "04", "grade_4", "grade_04", "4th", "g04"),
    grade_05 = c("5", "05", "grade_5", "grade_05", "5th", "g05"),
    grade_06 = c("6", "06", "grade_6", "grade_06", "6th", "g06"),
    grade_07 = c("7", "07", "grade_7", "grade_07", "7th", "g07"),
    grade_08 = c("8", "08", "grade_8", "grade_08", "8th", "g08"),
    grade_09 = c("9", "09", "grade_9", "grade_09", "9th", "g09"),
    grade_10 = c("10", "grade_10", "10th", "g10"),
    grade_11 = c("11", "grade_11", "11th", "g11"),
    grade_12 = c("12", "grade_12", "12th", "g12")
  )
}
