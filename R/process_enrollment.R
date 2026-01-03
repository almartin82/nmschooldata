# ==============================================================================
# Enrollment Data Processing Functions
# ==============================================================================
#
# This file contains functions for processing raw NM PED enrollment data into a
# clean, standardized format.
#
# ==============================================================================

#' Process raw NM PED enrollment data
#'
#' Transforms raw NM PED data into a standardized schema.
#'
#' @param raw_data Data frame from get_raw_enr
#' @param end_year School year end
#' @return Processed data frame with standardized columns
#' @keywords internal
process_enr <- function(raw_data, end_year) {

  # Determine era and process accordingly
  if (end_year <= 2018) {
    processed <- process_era1_enr(raw_data, end_year)
  } else {
    processed <- process_era2_enr(raw_data, end_year)
  }

  processed
}


#' Process Era 1 enrollment data (2016-2018)
#'
#' Era 1 data only has enrollment by grade, without demographic subgroups.
#' The raw data contains both school-level rows and district "Total" rows.
#' District rows have DISTRICT CODE = "Total".
#'
#' @param df Raw data frame
#' @param end_year School year end
#' @return Processed data frame
#' @keywords internal
process_era1_enr <- function(df, end_year) {

  cols <- names(df)

  # Helper to find column by pattern
  find_col <- function(patterns) {
    for (pattern in patterns) {
      matched <- grep(paste0("^", pattern, "$"), cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
      matched <- grep(pattern, cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Find district code column to identify district vs school rows
  dist_code_col <- find_col(c("district_code", "districtcode", "dist_code", "distcode"))

  # Era 1 files have "Total" in the DISTRICT CODE column for district-level rows
  # School rows have numeric district codes
  if (!is.null(dist_code_col)) {
    is_district_row <- df[[dist_code_col]] == "Total"
    is_school_row <- !is_district_row & !is.na(df[[dist_code_col]])
  } else {
    # Fallback: treat all as schools
    is_district_row <- rep(FALSE, nrow(df))
    is_school_row <- rep(TRUE, nrow(df))
  }

  # Find the district name column
  dist_name_col <- find_col(c("district_name", "districtname", "dist_name", "distname", "^district$"))

  # For district rows, the district name is in the previous row's district_name column
  # We need to propagate district names from school rows to their totals
  if (!is.null(dist_name_col)) {
    district_names <- df[[dist_name_col]]
    # Fill NA values forward
    for (i in seq_along(district_names)) {
      if (is.na(district_names[i]) && i > 1) {
        district_names[i] <- district_names[i - 1]
      }
    }
    df$district_name_filled <- district_names
  }

  # Process school rows
  school_df <- df[is_school_row, ]
  n_school_rows <- nrow(school_df)

  # Process district rows
  district_df <- df[is_district_row, ]
  n_district_rows <- nrow(district_df)

  # Build school result
  school_result <- NULL
  if (n_school_rows > 0) {
    school_result <- process_era1_rows(school_df, end_year, "School", find_col)
  }

  # Build district result
  district_result <- NULL
  if (n_district_rows > 0) {
    district_result <- process_era1_rows(district_df, end_year, "District", find_col)
    # For district rows, we need to get district name from the filled column
    if (!is.null(dist_name_col) && "district_name_filled" %in% names(df)) {
      district_result$district_name <- trimws(df[is_district_row, ]$district_name_filled)
    }
  }

  # Combine
  result <- dplyr::bind_rows(district_result, school_result)

  # Remove empty rows
  if ("row_total" %in% names(result)) {
    result <- result[!is.na(result$row_total) & result$row_total > 0, ]
  }

  # Create state aggregate from district rows only
  if (!is.null(district_result) && nrow(district_result) > 0) {
    state_agg <- create_state_aggregate(district_result, end_year)
  } else {
    state_agg <- create_state_aggregate(result, end_year)
  }
  result <- dplyr::bind_rows(state_agg, result)

  result
}


#' Process Era 1 rows into standard format
#'
#' @param df Data frame of rows to process
#' @param end_year School year end
#' @param row_type "School" or "District"
#' @param find_col Function to find columns by pattern
#' @return Processed data frame
#' @keywords internal
process_era1_rows <- function(df, end_year, row_type, find_col) {

  n_rows <- nrow(df)
  cols <- names(df)

  result <- data.frame(
    end_year = rep(end_year, n_rows),
    type = rep(row_type, n_rows),
    stringsAsFactors = FALSE
  )

  # District code and name
  dist_code_col <- find_col(c("district_code", "districtcode", "dist_code", "distcode"))
  if (!is.null(dist_code_col)) {
    result$district_code <- trimws(df[[dist_code_col]])
    # Convert "Total" to NA for district rows
    result$district_code[result$district_code == "Total"] <- NA_character_
  }

  dist_name_col <- find_col(c("district_name", "districtname", "dist_name", "distname", "^district$"))
  if (!is.null(dist_name_col)) {
    result$district_name <- trimws(df[[dist_name_col]])
  }

  # School/location code and name
  school_code_col <- find_col(c("location_id", "locationid", "location_code", "locationcode", "school_code", "schoolcode"))
  if (!is.null(school_code_col)) {
    result$school_code <- trimws(df[[school_code_col]])
  }

  school_name_col <- find_col(c("location_name", "locationname", "school_name", "schoolname"))
  if (!is.null(school_name_col)) {
    result$school_name <- trimws(df[[school_name_col]])
  }

  # Grade-level enrollment
  grade_patterns <- list(
    grade_pk = c("grade_pk", "gradepk", "pk", "prek", "pre_k"),
    grade_k = c("grade_kf", "grade_kn", "gradekf", "gradekn", "^k$", "^kg$", "kindergarten"),
    grade_01 = c("grade_01", "grade01", "^1$", "^01$"),
    grade_02 = c("grade_02", "grade02", "^2$", "^02$"),
    grade_03 = c("grade_03", "grade03", "^3$", "^03$"),
    grade_04 = c("grade_04", "grade04", "^4$", "^04$"),
    grade_05 = c("grade_05", "grade05", "^5$", "^05$"),
    grade_06 = c("grade_06", "grade06", "^6$", "^06$"),
    grade_07 = c("grade_07", "grade07", "^7$", "^07$"),
    grade_08 = c("grade_08", "grade08", "^8$", "^08$"),
    grade_09 = c("grade_09", "grade09", "^9$", "^09$"),
    grade_10 = c("grade_10", "grade10", "^10$"),
    grade_11 = c("grade_11", "grade11", "^11$"),
    grade_12 = c("grade_12", "grade12", "^12$")
  )

  for (grade_name in names(grade_patterns)) {
    col <- find_col(grade_patterns[[grade_name]])
    if (!is.null(col)) {
      result[[grade_name]] <- safe_numeric(df[[col]])
    }
  }

  # Total enrollment
  total_col <- find_col(c("total_enrollment", "totalenrollment", "total", "enrollment"))
  if (!is.null(total_col)) {
    result$row_total <- safe_numeric(df[[total_col]])
  } else {
    # Calculate total from grade columns if not present
    grade_cols <- grep("^grade_", names(result), value = TRUE)
    if (length(grade_cols) > 0) {
      result$row_total <- rowSums(result[, grade_cols, drop = FALSE], na.rm = TRUE)
    }
  }

  result
}


#' Process Era 2 enrollment data (2019-present)
#'
#' Era 2 data includes demographic subgroups by race/ethnicity and
#' special populations. Handles multiple year formats:
#' - 2019-2023: 40D subgroup data
#' - 2024: 80D grade counts only (no subgroups)
#' - 2025: 40D subgroup data with new column naming
#'
#' @param df Raw data frame
#' @param end_year School year end
#' @return Processed data frame
#' @keywords internal
process_era2_enr <- function(df, end_year) {

  cols <- names(df)
  n_rows <- nrow(df)

  # Helper to find column by pattern (searches column names)
  find_col <- function(patterns) {
    for (pattern in patterns) {
      # First try exact match (case insensitive)
      matched <- grep(paste0("^", pattern, "$"), cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
      # Then try partial match
      matched <- grep(pattern, cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Build result dataframe
  result <- data.frame(
    end_year = rep(end_year, n_rows),
    stringsAsFactors = FALSE
  )

  # District code and name
  dist_code_col <- find_col(c("district_code", "districtcode", "dist_code", "distcode"))
  if (!is.null(dist_code_col)) {
    result$district_code <- trimws(df[[dist_code_col]])
  }

  dist_name_col <- find_col(c("district_name", "districtname", "dist_name", "distname", "^district$"))
  if (!is.null(dist_name_col)) {
    result$district_name <- trimws(df[[dist_name_col]])
  }

  # School/location code and name
  school_code_col <- find_col(c("location_id", "locationid", "location_code", "locationcode", "school_code", "schoolcode", "loc_code", "loccode"))
  if (!is.null(school_code_col)) {
    result$school_code <- trimws(df[[school_code_col]])
  }

  school_name_col <- find_col(c("location_name", "locationname", "school_name", "schoolname", "loc_name", "locname"))
  if (!is.null(school_name_col)) {
    result$school_name <- trimws(df[[school_name_col]])
  }

  # Determine type based on school_code or sheet_type (for 2025)
  if ("sheet_type" %in% names(df)) {
    # 2025 format has sheet_type column
    result$type <- df$sheet_type
  } else if ("school_code" %in% names(result)) {
    result$type <- ifelse(
      is.na(result$school_code) | result$school_code == "" | result$school_code == "000",
      "District",
      "School"
    )
  } else {
    result$type <- rep("District", n_rows)
  }

  # Total enrollment - multiple naming patterns
  total_col <- find_col(c(
    "^all_students$", "^allstudents$",
    "^totalenrollment$", "^total_enrollment$", "^total$",
    "^enrollment$"
  ))
  if (!is.null(total_col)) {
    result$row_total <- safe_numeric(df[[total_col]])
  }

  # Demographics - Ethnicity (expanded patterns for 2025 format)
  demo_patterns <- list(
    white = c("caucasian", "caucasian_count", "white", "white_count"),
    black = c("black", "black_count", "african_american", "blackorafricanamerican"),
    hispanic = c("hispanic", "hispaniclatino", "hispaniclatino_count",
                 "hispaniclatinoethnicity", "hispaniclatinoethnicity_count"),
    asian = c("asian", "asian_count"),
    native_american = c("american_indian", "americanindian", "americanindianalaskannative",
                        "american_indian_alaskan_native_count", "indian"),
    pacific_islander = c("pacific", "pacific_islander", "nativehawaiian",
                         "nativehawaiianorotherpacificislander", "hawaiian"),
    multiracial = c("multiracial", "multi_racial", "multi_race", "multirace",
                    "two_or_more", "twoormoreracec")
  )

  for (name in names(demo_patterns)) {
    col <- find_col(demo_patterns[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  # Gender
  male_col <- find_col(c("male", "males", "male_count", "^m$"))
  if (!is.null(male_col)) {
    result$male <- safe_numeric(df[[male_col]])
  }

  female_col <- find_col(c("female", "females", "female_count", "^f$"))
  if (!is.null(female_col)) {
    result$female <- safe_numeric(df[[female_col]])
  }

  # Special populations
  special_patterns <- list(
    econ_disadv = c("economically_disadvantaged", "economicallydisadvantaged",
                    "economically_disadvantaged_count", "frl", "free_reduced"),
    ell = c("^ell$", "ell_count", "^el$", "english_learner", "englishlearner"),
    special_ed = c("sped", "special_ed", "disability", "disability_count",
                   "students_with_disabilities", "iep")
  )

  for (name in names(special_patterns)) {
    col <- find_col(special_patterns[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  # Grade-level enrollment (only for 2024 80D format and older years)
  grade_patterns <- list(
    grade_pk = c("grade_pk", "gradepk", "pk", "prek", "pre_k"),
    grade_k = c("grade_kf", "grade_kn", "grade_k", "gradek", "^k$", "^kg$", "kindergarten", "kinder"),
    grade_01 = c("grade_01", "grade01", "^1$", "^01$", "grade_1", "grade1"),
    grade_02 = c("grade_02", "grade02", "^2$", "^02$", "grade_2", "grade2"),
    grade_03 = c("grade_03", "grade03", "^3$", "^03$", "grade_3", "grade3"),
    grade_04 = c("grade_04", "grade04", "^4$", "^04$", "grade_4", "grade4"),
    grade_05 = c("grade_05", "grade05", "^5$", "^05$", "grade_5", "grade5"),
    grade_06 = c("grade_06", "grade06", "^6$", "^06$", "grade_6", "grade6"),
    grade_07 = c("grade_07", "grade07", "^7$", "^07$", "grade_7", "grade7"),
    grade_08 = c("grade_08", "grade08", "^8$", "^08$", "grade_8", "grade8"),
    grade_09 = c("grade_09", "grade09", "^9$", "^09$", "grade_9", "grade9"),
    grade_10 = c("grade_10", "grade10", "^10$"),
    grade_11 = c("grade_11", "grade11", "^11$"),
    grade_12 = c("grade_12", "grade12", "^12$")
  )

  for (grade_name in names(grade_patterns)) {
    col <- find_col(grade_patterns[[grade_name]])
    if (!is.null(col)) {
      result[[grade_name]] <- safe_numeric(df[[col]])
    }
  }

  # For 2024, calculate row_total from grade columns if not present
  if (end_year == 2024 && (!"row_total" %in% names(result) || all(is.na(result$row_total)))) {
    grade_cols <- grep("^grade_", names(result), value = TRUE)
    if (length(grade_cols) > 0) {
      result$row_total <- rowSums(result[, grade_cols, drop = FALSE], na.rm = TRUE)
    }
  }

  # Remove empty rows
  if ("row_total" %in% names(result)) {
    result <- result[!is.na(result$row_total) & result$row_total > 0, ]
  } else if ("district_code" %in% names(result)) {
    result <- result[!is.na(result$district_code), ]
  }

  # Create state aggregate (skip for 2025 which already has state row)
  if (end_year == 2025 && "State" %in% result$type) {
    # 2025 already has a state row from the State sheet
    # No need to create one
  } else {
    state_agg <- create_state_aggregate(result, end_year)
    result <- dplyr::bind_rows(state_agg, result)
  }

  result
}


#' Create state-level aggregate from processed data
#'
#' Aggregates data to state level. Uses district rows if available (to avoid
#' double-counting), otherwise uses school rows.
#'
#' @param df Processed data frame
#' @param end_year School year end
#' @return Single-row data frame with state totals
#' @keywords internal
create_state_aggregate <- function(df, end_year) {

  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(
      end_year = end_year,
      type = "State",
      district_code = NA_character_,
      school_code = NA_character_,
      district_name = NA_character_,
      school_name = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  # For state aggregate, prefer district-level rows (to avoid double-counting)
  # But only if there are enough district rows to represent all districts
  agg_df <- df
  if ("type" %in% names(df)) {
    district_df <- df[df$type == "District", ]
    school_df <- df[df$type == "School", ]

    # Use district rows only if there are a reasonable number of them
    # (at least 50, since NM has ~150 districts)
    if (nrow(district_df) >= 50) {
      agg_df <- district_df
    } else if (nrow(school_df) > 0) {
      # Otherwise use school rows
      agg_df <- school_df
    }
  }

  # Columns to sum
  sum_cols <- c(
    "row_total",
    "white", "black", "hispanic", "asian",
    "pacific_islander", "native_american", "multiracial",
    "male", "female",
    "econ_disadv", "ell", "special_ed",
    "grade_pk", "grade_k",
    "grade_01", "grade_02", "grade_03", "grade_04",
    "grade_05", "grade_06", "grade_07", "grade_08",
    "grade_09", "grade_10", "grade_11", "grade_12"
  )

  # Filter to columns that exist
  sum_cols <- sum_cols[sum_cols %in% names(agg_df)]

  # Create state row
  state_row <- data.frame(
    end_year = end_year,
    type = "State",
    district_code = NA_character_,
    school_code = NA_character_,
    district_name = NA_character_,
    school_name = NA_character_,
    stringsAsFactors = FALSE
  )

  # Sum each column
  for (col in sum_cols) {
    state_row[[col]] <- sum(agg_df[[col]], na.rm = TRUE)
  }

  state_row
}
