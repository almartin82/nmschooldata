# Build potential enrollment file URLs

NM PED uses various URL patterns across years. This function generates a
list of URLs to try based on observed patterns.

## Usage

``` r
build_enrollment_urls(end_year)
```

## Arguments

- end_year:

  School year end

## Value

Character vector of URLs to try

## Details

URL patterns discovered through research:

Era 1 (2016-2018) - Enrollment by district/location/grade only:

- 2016:
  /wp-content/uploads/2018/01/2015-2016-Enrollment-by-district-by-location-by-grade-1.xls

- 2017:
  /wp-content/uploads/2018/01/2016-2017-Enrollment-by-district-by-location-by-grade-1.xls

- 2018:
  /wp-content/uploads/2018/01/2017-2018-Enrollment-by-district-by-location-by-grade-1.xls

Era 2 (2019-2025) - Subgroup enrollment with demographics:

- 2019:
  /wp-content/uploads/2025/01/SY2018-2019-Subgroup-Enrollment-by-Grade-40D-by-district-by-school-final-1.xlsx

- 2020:
  /wp-content/uploads/2025/01/SY2019-2020-Subgroup-Enrollment-by-Grade-40D-by-district-by-school-final-1.xlsx

- 2021:
  /wp-content/uploads/2025/01/SY2020-2021-Subgroup-Enrollment-by-Grade-40D-by-district-by-school-final-1.xlsx

- 2022:
  /wp-content/uploads/2025/01/SY2021-2022-Subgroup-Enrollment-by-Grade-40D-by-district-by-school_Final.xlsx

- 2023:
  /wp-content/uploads/2025/01/SY-22-23-40D-Enrollment-Subgroup-Percentages-with-Averages-Certified.xlsx

- 2024:
  /wp-content/uploads/2025/01/SY2023_2024_80D_Enrollment_By_District_by_Grade_Certified_20240605.xlsx
  (80D only)

- 2025:
  /wp-content/uploads/\_legacy/2025/02/SY2024_2025_40D_Enrollment_Subgroup_Percentages.xlsx
