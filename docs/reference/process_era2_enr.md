# Process Era 2 enrollment data (2019-present)

Era 2 data includes demographic subgroups by race/ethnicity and special
populations. Handles multiple year formats:

- 2019-2023: 40D subgroup data

- 2024: 80D grade counts only (no subgroups)

- 2025: 40D subgroup data with new column naming

## Usage

``` r
process_era2_enr(df, end_year)
```

## Arguments

- df:

  Raw data frame

- end_year:

  School year end

## Value

Processed data frame
