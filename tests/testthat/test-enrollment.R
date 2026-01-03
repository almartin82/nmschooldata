# ==============================================================================
# Comprehensive Tests for nmschooldata Enrollment Functions
# ==============================================================================
#
# These tests verify:
# 1. All years from 2016-2025 can be fetched
# 2. Data structure is correct for each year
# 3. State totals are within reasonable ranges
# 4. Subgroups are present for years with demographic data
# 5. Tidy output maintains fidelity to raw source files
#
# CRITICAL REQUIREMENT: The tidy=TRUE version MUST maintain fidelity to the
# raw, unprocessed source file. Tests verify this for key values.
#
# ==============================================================================

test_that("get_available_years returns correct information", {
  years <- get_available_years()

  expect_type(years, "list")
  expect_equal(years$min_year, 2016)
  expect_equal(years$max_year, 2025)
  expect_true(nchar(years$description) > 50)
})


# ==============================================================================
# Era 1 Tests (2016-2018): Enrollment by grade only, no subgroups
# ==============================================================================

test_that("fetch_enr works for Era 1 years (2016-2018)", {
  skip_on_cran()
  skip_if_offline()

  for (yr in 2016:2018) {
    enr <- fetch_enr(yr, tidy = FALSE, use_cache = FALSE)

    expect_s3_class(enr, "data.frame")
    expect_true(nrow(enr) > 100, info = paste("Year", yr, "should have >100 rows"))
    expect_true("end_year" %in% names(enr))
    expect_true("type" %in% names(enr))
    expect_true("row_total" %in% names(enr))
    expect_true("district_name" %in% names(enr))

    # Should have grade columns
    grade_cols <- grep("^grade_", names(enr), value = TRUE)
    expect_true(length(grade_cols) > 5, info = paste("Year", yr, "should have grade columns"))

    # State row should exist
    state_rows <- enr[enr$type == "State", ]
    expect_equal(nrow(state_rows), 1)

    # State total should be reasonable (300k-400k for NM)
    state_total <- state_rows$row_total
    expect_true(state_total > 300000,
                info = paste("Year", yr, "state total", state_total, "too low"))
    expect_true(state_total < 400000,
                info = paste("Year", yr, "state total", state_total, "too high"))
  }
})


# ==============================================================================
# Era 2a Tests (2019-2023): 40-Day subgroup data
# ==============================================================================

test_that("fetch_enr works for Era 2a years (2019-2023) with subgroups", {
  skip_on_cran()
  skip_if_offline()

  for (yr in 2019:2023) {
    enr <- fetch_enr(yr, tidy = FALSE, use_cache = FALSE)

    expect_s3_class(enr, "data.frame")
    expect_true(nrow(enr) > 100, info = paste("Year", yr, "should have >100 rows"))

    # Should have demographic subgroup columns
    expect_true("hispanic" %in% names(enr),
                info = paste("Year", yr, "should have hispanic column"))
    expect_true("white" %in% names(enr),
                info = paste("Year", yr, "should have white column"))
    expect_true("native_american" %in% names(enr),
                info = paste("Year", yr, "should have native_american column"))
    expect_true("male" %in% names(enr),
                info = paste("Year", yr, "should have male column"))
    expect_true("female" %in% names(enr),
                info = paste("Year", yr, "should have female column"))

    # State total should be reasonable
    state_rows <- enr[enr$type == "State", ]
    expect_equal(nrow(state_rows), 1)
    state_total <- state_rows$row_total
    expect_true(state_total > 300000,
                info = paste("Year", yr, "state total", state_total, "too low"))
    expect_true(state_total < 400000,
                info = paste("Year", yr, "state total", state_total, "too high"))
  }
})


# ==============================================================================
# Era 2b Tests (2024): 80-Day data only, no subgroups
# ==============================================================================

test_that("fetch_enr works for 2024 (80-Day data)", {
  skip_on_cran()
  skip_if_offline()

  # Should produce a message about 80-Day data
  expect_message(
    enr <- fetch_enr(2024, tidy = FALSE, use_cache = FALSE),
    "80-Day"
  )

  expect_s3_class(enr, "data.frame")
  expect_true(nrow(enr) > 100)

  # 2024 80-Day should have grade columns
  grade_cols <- grep("^grade_", names(enr), value = TRUE)
  expect_true(length(grade_cols) > 5)

  # 2024 80-Day should NOT have full demographic subgroups
  # (may have male/female but not race/ethnicity)

  # State total should be reasonable
  state_rows <- enr[enr$type == "State", ]
  expect_equal(nrow(state_rows), 1)
  state_total <- state_rows$row_total
  expect_true(state_total > 280000, info = "2024 state total too low")
  expect_true(state_total < 350000, info = "2024 state total too high")
})


# ==============================================================================
# Era 2c Tests (2025): 40-Day subgroup data with new format
# ==============================================================================

test_that("fetch_enr works for 2025 with new format", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2025, tidy = FALSE, use_cache = FALSE)

  expect_s3_class(enr, "data.frame")
  expect_true(nrow(enr) > 100)

  # Should have all three types from three sheets
  expect_true("State" %in% enr$type)
  expect_true("District" %in% enr$type)
  expect_true("School" %in% enr$type)

  # Should have demographic subgroup columns
  expect_true("hispanic" %in% names(enr))
  expect_true("white" %in% names(enr))
  expect_true("native_american" %in% names(enr))
  expect_true("econ_disadv" %in% names(enr))
  expect_true("ell" %in% names(enr))

  # State total should be reasonable
  state_rows <- enr[enr$type == "State", ]
  expect_equal(nrow(state_rows), 1)
  state_total <- state_rows$row_total
  expect_true(state_total > 280000, info = "2025 state total too low")
  expect_true(state_total < 350000, info = "2025 state total too high")
})


# ==============================================================================
# Tidy Format Tests
# ==============================================================================

test_that("tidy format creates proper structure", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2025, tidy = TRUE, use_cache = FALSE)

  expect_s3_class(enr, "data.frame")

  # Should have required columns
  expect_true("subgroup" %in% names(enr))
  expect_true("grade_level" %in% names(enr))
  expect_true("n_students" %in% names(enr))
  expect_true("pct" %in% names(enr))
  expect_true("is_state" %in% names(enr))
  expect_true("is_district" %in% names(enr))
  expect_true("is_school" %in% names(enr))

  # Subgroup values should include expected categories
  subgroups <- unique(enr$subgroup)
  expect_true("total" %in% subgroups)
  expect_true("hispanic" %in% subgroups)
  expect_true("white" %in% subgroups)
  expect_true("male" %in% subgroups)
  expect_true("female" %in% subgroups)

  # Grade levels should exist
  grade_levels <- unique(enr$grade_level)
  expect_true("TOTAL" %in% grade_levels)
})


# ==============================================================================
# Fidelity Tests: Verify tidy output matches raw source values
# ==============================================================================

test_that("2025 tidy output maintains fidelity to raw source", {
  skip_on_cran()
  skip_if_offline()

  # These values are known from the raw 2025 Excel file
  # SY2024_2025_40D_Enrollment_Subgroup_Percentages.xlsx
  expected_state_total <- 306686
  expected_abq_total <- 75040
  expected_state_male <- 156988
  expected_state_hispanic <- 194595

  enr <- fetch_enr(2025, tidy = TRUE, use_cache = FALSE)

  # Check state total
  state_total <- enr[enr$is_state & enr$subgroup == "total" & enr$grade_level == "TOTAL", ]
  expect_equal(state_total$n_students, expected_state_total,
               info = "State total should match raw file exactly")

  # Check Albuquerque (district code 001)
  abq_total <- enr[enr$district_code == "001" & enr$is_district &
                     enr$subgroup == "total" & enr$grade_level == "TOTAL", ]
  expect_equal(abq_total$n_students, expected_abq_total,
               info = "Albuquerque total should match raw file exactly")

  # Check state male count
  state_male <- enr[enr$is_state & enr$subgroup == "male" & enr$grade_level == "TOTAL", ]
  expect_equal(state_male$n_students, expected_state_male,
               info = "State male count should match raw file exactly")

  # Check state Hispanic count
  state_hispanic <- enr[enr$is_state & enr$subgroup == "hispanic" & enr$grade_level == "TOTAL", ]
  expect_equal(state_hispanic$n_students, expected_state_hispanic,
               info = "State Hispanic count should match raw file exactly")
})


test_that("2024 80-Day data maintains fidelity to raw source", {
  skip_on_cran()
  skip_if_offline()

  # These values are known from the raw 2024 80-Day Excel file
  # SY2023_2024_80D_Enrollment_By_District_by_Grade_Certified_20240605.xlsx
  expected_state_total <- 308913  # Sum of all district totals

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = FALSE)

  # Check state total
  state_total <- enr[enr$is_state & enr$subgroup == "total" & enr$grade_level == "TOTAL", ]
  expect_equal(state_total$n_students, expected_state_total,
               info = "2024 state total should match raw file exactly")
})


test_that("2017 Era 1 data maintains fidelity to raw source", {
  skip_on_cran()
  skip_if_offline()

  # Expected state total from 2016-2017 Era 1 file
  expected_state_total <- 338307

  enr <- fetch_enr(2017, tidy = TRUE, use_cache = FALSE)

  # Check state total
  state_total <- enr[enr$is_state & enr$subgroup == "total" & enr$grade_level == "TOTAL", ]
  expect_equal(state_total$n_students, expected_state_total,
               info = "2017 state total should match raw file exactly")
})


# ==============================================================================
# Multi-Year Tests
# ==============================================================================

test_that("fetch_enr_multi works for multiple years", {
  skip_on_cran()
  skip_if_offline()

  enr_multi <- fetch_enr_multi(c(2023, 2024, 2025), tidy = TRUE, use_cache = FALSE)

  expect_s3_class(enr_multi, "data.frame")

  # Should have all requested years
  years <- unique(enr_multi$end_year)
  expect_true(2023 %in% years)
  expect_true(2024 %in% years)
  expect_true(2025 %in% years)

  # Each year should have state data
  for (yr in c(2023, 2024, 2025)) {
    state_rows <- enr_multi[enr_multi$end_year == yr & enr_multi$is_state, ]
    expect_true(nrow(state_rows) > 0,
                info = paste("Year", yr, "should have state rows"))
  }
})


# ==============================================================================
# Data Quality Tests: Check for impossible/improbable values
# ==============================================================================

test_that("no zeros where real data should exist for 2025", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2025, tidy = TRUE, use_cache = FALSE)

  # State-level totals should never be zero
  state_total <- enr[enr$is_state & enr$subgroup == "total" & enr$grade_level == "TOTAL", ]
  expect_true(state_total$n_students > 0, info = "State total should not be zero")

  # Large districts (APS, Las Cruces) should have non-zero totals
  # APS (001), Las Cruces (017)
  for (dc in c("001", "017")) {
    district_total <- enr[enr$district_code == dc & enr$is_district &
                            enr$subgroup == "total" & enr$grade_level == "TOTAL", ]
    expect_true(nrow(district_total) > 0,
                info = paste("District", dc, "should exist"))
    if (nrow(district_total) > 0) {
      expect_true(district_total$n_students > 1000,
                  info = paste("District", dc, "should have >1000 students"))
    }
  }

  # Hispanic enrollment should be substantial in NM (>50% of state)
  state_hispanic <- enr[enr$is_state & enr$subgroup == "hispanic" & enr$grade_level == "TOTAL", ]
  expect_true(state_hispanic$pct > 0.5,
              info = "Hispanic students should be >50% of NM enrollment")
})


test_that("no impossible values in enrollment data", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2025, tidy = TRUE, use_cache = FALSE)

  # No negative enrollment counts
  expect_true(all(enr$n_students >= 0 | is.na(enr$n_students)),
              info = "No negative enrollment counts allowed")

  # Percentages should be between 0 and 1
  valid_pcts <- enr$pct[!is.na(enr$pct)]
  expect_true(all(valid_pcts >= 0 & valid_pcts <= 1),
              info = "Percentages should be between 0 and 1")

  # No individual school should have >50,000 students
  school_totals <- enr[enr$is_school & enr$subgroup == "total" & enr$grade_level == "TOTAL", ]
  expect_true(all(school_totals$n_students < 50000, na.rm = TRUE),
              info = "No school should have >50,000 students")

  # Male + Female should approximately equal Total for state
  state_rows <- enr[enr$is_state & enr$grade_level == "TOTAL", ]
  total <- state_rows[state_rows$subgroup == "total", "n_students"]
  male <- state_rows[state_rows$subgroup == "male", "n_students"]
  female <- state_rows[state_rows$subgroup == "female", "n_students"]

  if (!is.na(male) && !is.na(female)) {
    gender_sum <- male + female
    # Allow 1% tolerance for rounding
    expect_true(abs(gender_sum - total) / total < 0.01,
                info = "Male + Female should equal Total within 1%")
  }
})


# ==============================================================================
# Year Validation Tests
# ==============================================================================

test_that("invalid years produce errors", {
  expect_error(fetch_enr(2010), "must be between")
  expect_error(fetch_enr(2030), "must be between")
  # Note: R coerces "2024" to numeric 2024, so no error expected for string years
})


test_that("fetch_enr_multi validates years", {
  expect_error(fetch_enr_multi(c(2023, 2010)), "Invalid years")
})


# ==============================================================================
# Comprehensive Fidelity Tests for Each Year
# ==============================================================================
#
# These tests verify that the package output EXACTLY matches values from the
# raw source files. Values are extracted manually from the source Excel files.
#
# ==============================================================================

# --- 2016 Era 1 Fidelity Test ---
test_that("2016 data maintains fidelity to raw source", {
  skip_on_cran()
  skip_if_offline()

  # Values from 2015-2016-Enrollment-by-district-by-location-by-grade-1.xls
  expected_state_total <- 339613

  enr <- fetch_enr(2016, tidy = TRUE, use_cache = FALSE)

  state_total <- enr[enr$is_state & enr$subgroup == "total" & enr$grade_level == "TOTAL", ]
  expect_equal(state_total$n_students, expected_state_total,
               info = "2016 state total should match raw file exactly")

  # Era 1 should only have 'total' subgroup (no demographics)
  subgroups <- unique(enr$subgroup)
  expect_equal(subgroups, "total",
               info = "2016 should only have 'total' subgroup (no demographics)")
})


# --- 2018 Era 1 Fidelity Test ---
test_that("2018 data maintains fidelity to raw source", {
  skip_on_cran()
  skip_if_offline()

  # Values from 2017-2018-Enrollment-by-district-by-location-by-grade-1.xls
  expected_state_total <- 337847

  enr <- fetch_enr(2018, tidy = TRUE, use_cache = FALSE)

  state_total <- enr[enr$is_state & enr$subgroup == "total" & enr$grade_level == "TOTAL", ]
  expect_equal(state_total$n_students, expected_state_total,
               info = "2018 state total should match raw file exactly")
})


# --- 2019 Era 2 Fidelity Test ---
test_that("2019 data maintains fidelity to raw source", {
  skip_on_cran()
  skip_if_offline()

  # Values from SY2018-2019-Subgroup-Enrollment-by-Grade-40D-by-district-by-school-final-1.xlsx
  expected_state_total <- 335131

  enr <- fetch_enr(2019, tidy = TRUE, use_cache = FALSE)

  state_total <- enr[enr$is_state & enr$subgroup == "total" & enr$grade_level == "TOTAL", ]
  expect_equal(state_total$n_students, expected_state_total,
               info = "2019 state total should match raw file exactly")

  # 2019+ should have demographic subgroups
  subgroups <- unique(enr$subgroup)
  expect_true("hispanic" %in% subgroups, info = "2019 should have hispanic subgroup")
  expect_true("male" %in% subgroups, info = "2019 should have male subgroup")
  expect_true("econ_disadv" %in% subgroups, info = "2019 should have econ_disadv subgroup")
})


# --- 2020 Era 2 Fidelity Test ---
test_that("2020 data maintains fidelity to raw source", {
  skip_on_cran()
  skip_if_offline()

  # Values from SY2019-2020-Subgroup-Enrollment-by-Grade-40D-by-district-by-school-final-1.xlsx
  expected_state_total <- 332672

  enr <- fetch_enr(2020, tidy = TRUE, use_cache = FALSE)

  state_total <- enr[enr$is_state & enr$subgroup == "total" & enr$grade_level == "TOTAL", ]
  expect_equal(state_total$n_students, expected_state_total,
               info = "2020 state total should match raw file exactly")
})


# --- 2021 Era 2 Fidelity Test ---
test_that("2021 data maintains fidelity to raw source", {
  skip_on_cran()
  skip_if_offline()

  # Values from SY2020-2021-Subgroup-Enrollment-by-Grade-40D-by-district-by-school-final-1.xlsx
  expected_state_total <- 318349

  enr <- fetch_enr(2021, tidy = TRUE, use_cache = FALSE)

  state_total <- enr[enr$is_state & enr$subgroup == "total" & enr$grade_level == "TOTAL", ]
  expect_equal(state_total$n_students, expected_state_total,
               info = "2021 state total should match raw file exactly")
})


# --- 2022 Era 2 Fidelity Test ---
test_that("2022 data maintains fidelity to raw source", {
  skip_on_cran()
  skip_if_offline()

  # Values from SY2021-2022-Subgroup-Enrollment-by-Grade-40D-by-district-by-school_Final.xlsx
  expected_state_total <- 318353

  enr <- fetch_enr(2022, tidy = TRUE, use_cache = FALSE)

  state_total <- enr[enr$is_state & enr$subgroup == "total" & enr$grade_level == "TOTAL", ]
  expect_equal(state_total$n_students, expected_state_total,
               info = "2022 state total should match raw file exactly")
})


# --- 2023 Era 2 Fidelity Test ---
test_that("2023 data maintains fidelity to raw source", {
  skip_on_cran()
  skip_if_offline()

  # Values from SY-22-23-40D-Enrollment-Subgroup-Percentages-with-Averages-Certified.xlsx
  expected_state_total <- 316478

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = FALSE)

  state_total <- enr[enr$is_state & enr$subgroup == "total" & enr$grade_level == "TOTAL", ]
  expect_equal(state_total$n_students, expected_state_total,
               info = "2023 state total should match raw file exactly")
})


# ==============================================================================
# Subgroup Coverage Tests
# ==============================================================================

test_that("all 13 subgroups are present for 2025", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2025, tidy = TRUE, use_cache = FALSE)
  subgroups <- unique(enr$subgroup)

  # Expected subgroups
  expected_subgroups <- c(
    "total",
    "white", "black", "hispanic", "asian",
    "native_american", "pacific_islander", "multiracial",
    "male", "female",
    "special_ed", "ell", "econ_disadv"
  )

  for (sg in expected_subgroups) {
    expect_true(sg %in% subgroups,
                info = paste("2025 should have", sg, "subgroup"))
  }

  # Exactly 13 subgroups
  expect_equal(length(subgroups), 13,
               info = "2025 should have exactly 13 subgroups")
})


test_that("grade levels vary by data source type", {
  skip_on_cran()
  skip_if_offline()

  all_grades <- c("TOTAL", "PK", "K", "01", "02", "03", "04", "05",
                  "06", "07", "08", "09", "10", "11", "12")

  # 2017 Era 1: has individual grade data
  enr_2017 <- fetch_enr(2017, tidy = TRUE, use_cache = FALSE)
  grades_2017 <- unique(enr_2017$grade_level)
  for (g in all_grades) {
    expect_true(g %in% grades_2017,
                info = paste("Era 1 (2017) should have grade level", g))
  }

  # 2024 80-Day: has individual grade data (district level only)
  enr_2024 <- fetch_enr(2024, tidy = TRUE, use_cache = FALSE)
  grades_2024 <- unique(enr_2024$grade_level)
  for (g in all_grades) {
    expect_true(g %in% grades_2024,
                info = paste("80-Day (2024) should have grade level", g))
  }

  # 2025 40-Day subgroup: only has TOTAL (no individual grade breakdown)
  enr_2025 <- fetch_enr(2025, tidy = TRUE, use_cache = FALSE)
  grades_2025 <- unique(enr_2025$grade_level)
  expect_equal(grades_2025, "TOTAL",
               info = "2025 40-Day subgroup data only has TOTAL grade level")
})


# ==============================================================================
# Data Availability Documentation Tests
# ==============================================================================

test_that("2024 limitation is properly documented", {
  skip_on_cran()
  skip_if_offline()

  # 2024 should only have 'total' subgroup (80-Day data has no demographics)
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = FALSE)
  subgroups <- unique(enr$subgroup)

  expect_equal(subgroups, "total",
               info = "2024 (80-Day) should only have 'total' subgroup")

  # Message should warn about 80-Day limitation
  expect_message(fetch_enr(2024, use_cache = FALSE), "80-Day")
})


test_that("2019-2023 have school data but limited district data", {
  skip_on_cran()
  skip_if_offline()

  # These years have school-level data but no district aggregates in the raw files
  for (yr in c(2019, 2020, 2021, 2022, 2023)) {
    enr <- fetch_enr(yr, tidy = TRUE, use_cache = FALSE)

    school_count <- sum(enr$is_school & enr$subgroup == "total" & enr$grade_level == "TOTAL")
    expect_true(school_count > 500,
                info = paste("Year", yr, "should have >500 schools"))
  }
})


test_that("2016-2018 have school and district data", {
  skip_on_cran()
  skip_if_offline()

  for (yr in 2016:2018) {
    enr <- fetch_enr(yr, tidy = TRUE, use_cache = FALSE)

    district_count <- sum(enr$is_district & enr$subgroup == "total" & enr$grade_level == "TOTAL")
    school_count <- sum(enr$is_school & enr$subgroup == "total" & enr$grade_level == "TOTAL")

    expect_true(district_count > 100,
                info = paste("Year", yr, "should have >100 districts"))
    expect_true(school_count > 500,
                info = paste("Year", yr, "should have >500 schools"))
  }
})
