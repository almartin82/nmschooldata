# Download enrollment Excel file from NM PED

Tries multiple URL patterns to find the enrollment file. NM PED uses
different naming conventions across years.

## Usage

``` r
download_enrollment_file(end_year)
```

## Arguments

- end_year:

  School year end

## Value

Path to downloaded temp file, or NULL if download failed
