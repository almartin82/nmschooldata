# Fetch New Mexico enrollment data

Downloads and processes enrollment data from the New Mexico Public
Education Department's 40-Day enrollment reports.

## Usage

``` r
fetch_enr(end_year, tidy = TRUE, use_cache = TRUE)
```

## Arguments

- end_year:

  A school year. Year is the end of the academic year - eg 2023-24
  school year is year '2024'. Valid values are 2016-2025.

- tidy:

  If TRUE (default), returns data in long (tidy) format with subgroup
  column. If FALSE, returns wide format as downloaded.

- use_cache:

  If TRUE (default), uses locally cached data when available. Set to
  FALSE to force re-download from NM PED.

## Value

Data frame with enrollment data. Wide format includes columns for
district_code, school_code, names, and enrollment counts by
demographic/grade. Tidy format pivots these counts into subgroup and
n_students columns.

## Data Availability

- 2016-2018:

  Enrollment by grade only (no demographic subgroups)

- 2019-2023, 2025:

  Full demographic subgroups available

- 2024:

  Only 80-Day enrollment data available; 40-Day subgroup file not
  published

## Examples

``` r
if (FALSE) { # \dontrun{
# Get 2024 enrollment data (2023-24 school year)
enr_2024 <- fetch_enr(2024)

# Get wide format
enr_wide <- fetch_enr(2024, tidy = FALSE)

# Force fresh download (ignore cache)
enr_fresh <- fetch_enr(2024, use_cache = FALSE)

# Filter to specific district
albuquerque <- enr_2024 %>%
  dplyr::filter(grepl("Albuquerque", district_name, ignore.case = TRUE))
} # }
```
