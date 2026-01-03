# Process Era 1 enrollment data (2016-2018)

Era 1 data only has enrollment by grade, without demographic subgroups.
The raw data contains both school-level rows and district "Total" rows.
District rows have DISTRICT CODE = "Total".

## Usage

``` r
process_era1_enr(df, end_year)
```

## Arguments

- df:

  Raw data frame

- end_year:

  School year end

## Value

Processed data frame
