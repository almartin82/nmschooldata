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
#'
#' @param df Raw data frame
#' @param end_year School year end
#' @return Processed data frame
#' @keywords internal
process_era1_enr <- function(df, end_year) {

  cols <- names(df)
  n_rows <- nrow(df)

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

  # Get column mappings
  col_map <- get_nm_column_map()

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

  dist_name_col <- find_col(c("district_name", "districtname", "dist_name", "distname", "district"))
  if (!is.null(dist_name_col)) {
    result$district_name <- trimws(df[[dist_name_col]])
  }

  # School/location code and name
  school_code_col <- find_col(c("location_code", "locationcode", "school_code", "schoolcode", "loc_code", "loccode"))
  if (!is.null(school_code_col)) {
    result$school_code <- trimws(df[[school_code_col]])
  }

  school_name_col <- find_col(c("location_name", "locationname", "school_name", "schoolname", "loc_name", "locname", "location", "school"))
  if (!is.null(school_name_col)) {
    result$school_name <- trimws(df[[school_name_col]])
  }

  # Determine type based on school_code presence
  if ("school_code" %in% names(result)) {
    result$type <- ifelse(
      is.na(result$school_code) | result$school_code == "" | result$school_code == "000",
      "District",
      "School"
    )
  } else {
    result$type <- rep("District", n_rows)
  }

  # Grade-level enrollment (Era 1 typically has grade columns)
  grade_patterns <- list(
    grade_pk = c("pk", "prek", "pre_k", "pre-k"),
    grade_k = c("^k$", "^kg$", "kindergarten", "kinder"),
    grade_01 = c("^1$", "^01$", "grade_1", "grade1", "gr1", "g01"),
    grade_02 = c("^2$", "^02$", "grade_2", "grade2", "gr2", "g02"),
    grade_03 = c("^3$", "^03$", "grade_3", "grade3", "gr3", "g03"),
    grade_04 = c("^4$", "^04$", "grade_4", "grade4", "gr4", "g04"),
    grade_05 = c("^5$", "^05$", "grade_5", "grade5", "gr5", "g05"),
    grade_06 = c("^6$", "^06$", "grade_6", "grade6", "gr6", "g06"),
    grade_07 = c("^7$", "^07$", "grade_7", "grade7", "gr7", "g07"),
    grade_08 = c("^8$", "^08$", "grade_8", "grade8", "gr8", "g08"),
    grade_09 = c("^9$", "^09$", "grade_9", "grade9", "gr9", "g09"),
    grade_10 = c("^10$", "grade_10", "grade10", "gr10", "g10"),
    grade_11 = c("^11$", "grade_11", "grade11", "gr11", "g11"),
    grade_12 = c("^12$", "grade_12", "grade12", "gr12", "g12")
  )

  for (grade_name in names(grade_patterns)) {
    col <- find_col(grade_patterns[[grade_name]])
    if (!is.null(col)) {
      result[[grade_name]] <- safe_numeric(df[[col]])
    }
  }

  # Total enrollment
  total_col <- find_col(c("total", "total_enrollment", "enrollment", "all"))
  if (!is.null(total_col)) {
    result$row_total <- safe_numeric(df[[total_col]])
  } else {
    # Calculate total from grade columns if not present
    grade_cols <- grep("^grade_", names(result), value = TRUE)
    if (length(grade_cols) > 0) {
      result$row_total <- rowSums(result[, grade_cols, drop = FALSE], na.rm = TRUE)
    }
  }

  # Remove empty rows
  if ("row_total" %in% names(result)) {
    result <- result[!is.na(result$row_total) & result$row_total > 0, ]
  }

  # Create state aggregate
  state_agg <- create_state_aggregate(result, end_year)
  result <- dplyr::bind_rows(state_agg, result)

  result
}


#' Process Era 2 enrollment data (2019-present)
#'
#' Era 2 data includes demographic subgroups by race/ethnicity and
#' special populations.
#'
#' @param df Raw data frame
#' @param end_year School year end
#' @return Processed data frame
#' @keywords internal
process_era2_enr <- function(df, end_year) {

  cols <- names(df)
  n_rows <- nrow(df)

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

  # Get column mappings
  col_map <- get_nm_column_map()

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

  dist_name_col <- find_col(c("district_name", "districtname", "dist_name", "distname", "district"))
  if (!is.null(dist_name_col)) {
    result$district_name <- trimws(df[[dist_name_col]])
  }

  # School/location code and name
  school_code_col <- find_col(c("location_code", "locationcode", "school_code", "schoolcode", "loc_code", "loccode"))
  if (!is.null(school_code_col)) {
    result$school_code <- trimws(df[[school_code_col]])
  }

  school_name_col <- find_col(c("location_name", "locationname", "school_name", "schoolname", "loc_name", "locname"))
  if (!is.null(school_name_col)) {
    result$school_name <- trimws(df[[school_name_col]])
  }

  # Determine type based on school_code presence
  if ("school_code" %in% names(result)) {
    result$type <- ifelse(
      is.na(result$school_code) | result$school_code == "" | result$school_code == "000",
      "District",
      "School"
    )
  } else {
    result$type <- rep("District", n_rows)
  }

  # Total enrollment
  total_col <- find_col(col_map$total)
  if (!is.null(total_col)) {
    result$row_total <- safe_numeric(df[[total_col]])
  }

  # Demographics - Ethnicity
  demo_map <- list(
    white = col_map$white,
    black = col_map$black,
    hispanic = col_map$hispanic,
    asian = col_map$asian,
    native_american = col_map$native_american,
    pacific_islander = col_map$pacific_islander,
    multiracial = col_map$multiracial
  )

  for (name in names(demo_map)) {
    col <- find_col(demo_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  # Gender
  male_col <- find_col(col_map$male)
  if (!is.null(male_col)) {
    result$male <- safe_numeric(df[[male_col]])
  }

  female_col <- find_col(col_map$female)
  if (!is.null(female_col)) {
    result$female <- safe_numeric(df[[female_col]])
  }

  # Special populations
  special_map <- list(
    econ_disadv = col_map$econ_disadv,
    ell = col_map$ell,
    special_ed = col_map$special_ed
  )

  for (name in names(special_map)) {
    col <- find_col(special_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  # Grade-level enrollment
  grade_patterns <- list(
    grade_pk = c("pk", "prek", "pre_k"),
    grade_k = c("^k$", "^kg$", "kindergarten", "kinder"),
    grade_01 = c("^1$", "^01$", "grade_1", "grade1"),
    grade_02 = c("^2$", "^02$", "grade_2", "grade2"),
    grade_03 = c("^3$", "^03$", "grade_3", "grade3"),
    grade_04 = c("^4$", "^04$", "grade_4", "grade4"),
    grade_05 = c("^5$", "^05$", "grade_5", "grade5"),
    grade_06 = c("^6$", "^06$", "grade_6", "grade6"),
    grade_07 = c("^7$", "^07$", "grade_7", "grade7"),
    grade_08 = c("^8$", "^08$", "grade_8", "grade8"),
    grade_09 = c("^9$", "^09$", "grade_9", "grade9"),
    grade_10 = c("^10$", "grade_10", "grade10"),
    grade_11 = c("^11$", "grade_11", "grade11"),
    grade_12 = c("^12$", "grade_12", "grade12")
  )

  for (grade_name in names(grade_patterns)) {
    col <- find_col(grade_patterns[[grade_name]])
    if (!is.null(col)) {
      result[[grade_name]] <- safe_numeric(df[[col]])
    }
  }

  # Remove empty rows
  if ("row_total" %in% names(result)) {
    result <- result[!is.na(result$row_total) & result$row_total > 0, ]
  } else if ("district_code" %in% names(result)) {
    result <- result[!is.na(result$district_code), ]
  }

  # Create state aggregate
  state_agg <- create_state_aggregate(result, end_year)
  result <- dplyr::bind_rows(state_agg, result)

  result
}


#' Create state-level aggregate from processed data
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

  # For state aggregate, sum only district-level rows (not schools)
  # to avoid double-counting
  if ("type" %in% names(df)) {
    district_df <- df[df$type == "District", ]
    if (nrow(district_df) == 0) {
      # If no district rows, aggregate schools
      district_df <- df[df$type == "School", ]
    }
  } else {
    district_df <- df
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
  sum_cols <- sum_cols[sum_cols %in% names(district_df)]

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
    state_row[[col]] <- sum(district_df[[col]], na.rm = TRUE)
  }

  state_row
}
