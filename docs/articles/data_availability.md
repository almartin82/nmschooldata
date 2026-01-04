# Understanding NM PED Data Availability

``` r
library(nmschooldata)
library(dplyr)
```

## Data Sources Overview

The `nmschooldata` package fetches enrollment data from the New Mexico
Public Education Department (NM PED). The data comes from two main
sources with different structures:

1.  **40-Day Enrollment Reports**: Collected on the second Wednesday of
    October (approximately 40 school days into the year)
2.  **80-Day Enrollment Reports**: Collected on December 1st
    (approximately 80 school days into the year)

## Data Availability by Year

### Era 1: 2016-2018 (Enrollment by Grade)

These older files contain **enrollment by grade only** (no demographic
subgroups):

- District-level and school-level data
- Individual grade breakdowns (PK through 12)
- Total enrollment
- NO demographic subgroups (race, gender, special populations)

``` r
# Era 1 data structure
enr_2017 <- fetch_enr(2017)

# Only 'total' subgroup available
unique(enr_2017$subgroup)
#> [1] "total"

# But all grade levels are present
unique(enr_2017$grade_level)
#> [1] "TOTAL" "PK" "K" "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12"
```

### Era 2a: 2019-2023 (40-Day Subgroup Data)

These files contain **demographic subgroups** at the school level:

- School-level data only (no district aggregates in raw files)
- 13 demographic subgroups
- TOTAL enrollment only (no individual grade breakdowns)
- State totals are calculated by the package from school-level data

``` r
# Era 2a data structure
enr_2023 <- fetch_enr(2023)

# All 13 subgroups available
unique(enr_2023$subgroup)
#>  [1] "total" "white" "black" "hispanic" "asian" "native_american"
#>  [7] "pacific_islander" "multiracial" "male" "female" "special_ed"
#> [13] "ell" "econ_disadv"

# Only TOTAL grade level
unique(enr_2023$grade_level)
#> [1] "TOTAL"
```

### Era 2b: 2024 (80-Day Data Only)

**Important**: The 40-Day subgroup file for 2023-24 has not been
published by NM PED. Only 80-Day data is available:

- District-level data only (no school breakdowns)
- Individual grade breakdowns (PK through 12)
- NO demographic subgroups
- The package displays a warning when fetching 2024 data

``` r
# 2024 data structure (shows a warning message)
enr_2024 <- fetch_enr(2024)
#> Note: 2024 (SY 2023-24) only has 80-Day enrollment data available.
#>       40-Day subgroup data has not been published by NM PED.

# Only 'total' subgroup available
unique(enr_2024$subgroup)
#> [1] "total"

# But all grade levels are present
unique(enr_2024$grade_level)
#> [1] "TOTAL" "PK" "K" "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12"
```

### Era 2c: 2025 (40-Day Subgroup Data, New Format)

The most recent data is similar to 2019-2023 but with a new file format:

- Three sheets: State, District, and School level data
- 13 demographic subgroups
- TOTAL enrollment only (no individual grade breakdowns)

``` r
# 2025 data structure
enr_2025 <- fetch_enr(2025)

# All three levels available
table(enr_2025$type[enr_2025$subgroup == "total" & enr_2025$grade_level == "TOTAL"])
#> District   School    State
#>      155      876        1

# All 13 subgroups available
length(unique(enr_2025$subgroup))
#> [1] 13
```

## Summary Table

| Years     | Source            | Aggregation Levels      | Demographics | Grade Breakdown |
|-----------|-------------------|-------------------------|--------------|-----------------|
| 2016-2018 | 40-Day enrollment | State, District, School | None         | PK through 12   |
| 2019-2023 | 40-Day subgroup   | State, School           | 13 subgroups | TOTAL only      |
| 2024      | 80-Day only       | State, District         | None         | PK through 12   |
| 2025      | 40-Day subgroup   | State, District, School | 13 subgroups | TOTAL only      |

## Subgroups Available (2019-2023, 2025)

When demographic data is available, the following 13 subgroups are
included:

**Race/Ethnicity:** - `white` - Caucasian/White students - `black` -
Black or African American students - `hispanic` - Hispanic/Latino
students - `asian` - Asian students - `native_american` - American
Indian/Alaska Native students - `pacific_islander` - Native
Hawaiian/Pacific Islander students - `multiracial` - Two or more races

**Gender:** - `male` - `female`

**Special Populations:** - `special_ed` - Students with disabilities
(IEP) - `ell` - English Language Learners - `econ_disadv` - Economically
Disadvantaged (Free/Reduced Lunch eligible)

## Tips for Working with the Data

### Getting State Totals Across Years

``` r
# For consistent state totals, use years with 40-Day data
enr <- fetch_enr_multi(c(2019:2023, 2025))

state_totals <- enr |>
  filter(is_state, subgroup == "total", grade_level == "TOTAL") |>
  select(end_year, n_students)

state_totals
```

### Getting Demographic Trends (Years with Subgroups)

``` r
# Demographics only available for 2019-2023, 2025
enr <- fetch_enr_multi(c(2019:2023, 2025))

hispanic_trend <- enr |>
  filter(is_state, subgroup == "hispanic", grade_level == "TOTAL") |>
  select(end_year, n_students, pct)

hispanic_trend
```

### Getting Grade-Level Data (Era 1 and 2024)

``` r
# Grade breakdowns only for Era 1 (2016-2018) and 2024
enr_2017 <- fetch_enr(2017)

kindergarten_by_district <- enr_2017 |>
  filter(is_district, subgroup == "total", grade_level == "K") |>
  select(district_name, n_students) |>
  arrange(desc(n_students))

kindergarten_by_district
```

## Why This Matters

Understanding data availability is crucial for:

1.  **Trend Analysis**: Only compare years with similar data structures
2.  **Demographic Analysis**: Must use 2019-2023 or 2025 for
    race/ethnicity data
3.  **Grade Analysis**: Must use 2016-2018 or 2024 for individual grade
    data
4.  **Research Planning**: Know what questions can be answered with
    available data

## Data Source

All data comes from the New Mexico Public Education Department (NM PED):

- [STARS System
  Overview](https://web.ped.nm.gov/bureaus/information-technology/stars/)
- [NM Vistas](https://nmvistas.org/)
