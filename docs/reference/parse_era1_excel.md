# Parse Era 1 Excel file (2016-2018)

Older NM PED enrollment files contain only total enrollment by grade,
without demographic subgroup breakdowns. These files have 8 header rows
that need to be skipped.

## Usage

``` r
parse_era1_excel(excel_path, end_year)
```

## Arguments

- excel_path:

  Path to downloaded Excel file

- end_year:

  School year end

## Value

Data frame with enrollment data
