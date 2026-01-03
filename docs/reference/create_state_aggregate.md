# Create state-level aggregate from processed data

Aggregates data to state level. Uses district rows if available (to
avoid double-counting), otherwise uses school rows.

## Usage

``` r
create_state_aggregate(df, end_year)
```

## Arguments

- df:

  Processed data frame

- end_year:

  School year end

## Value

Single-row data frame with state totals
