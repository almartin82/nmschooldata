# Parse Era 2 Excel file (2019-present)

Newer NM PED enrollment files include demographic subgroup breakdowns by
race/ethnicity and special populations. Different year files have
different structures:

- 2019-2023: Have 10 header rows to skip, single sheet named "Masked"

- 2024: 80-Day file with NO header rows, district-level only, grade
  columns

- 2025: NO header rows, three sheets (by School, by District, State)

## Usage

``` r
parse_era2_excel(excel_path, end_year)
```

## Arguments

- excel_path:

  Path to downloaded Excel file

- end_year:

  School year end

## Value

Data frame with enrollment data
