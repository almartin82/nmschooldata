# Parse 2025 Excel file with multiple sheets

The 2025 file has three sheets that need to be combined:

- "SY25 by School": School-level data

- "SY25 by District": District-level aggregates

- "SY25 State": State-level aggregate

## Usage

``` r
parse_2025_excel(excel_path, sheets)
```

## Arguments

- excel_path:

  Path to downloaded Excel file

- sheets:

  Vector of sheet names

## Value

Combined data frame
