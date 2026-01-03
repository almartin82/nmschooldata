# nmschooldata: Fetch and Process New Mexico School Data

Downloads and processes school data from the New Mexico Public Education
Department (PED). Provides functions for fetching enrollment data from
the STARS (Student Teacher Accountability Reporting System) and
transforming it into tidy format for analysis.

## Main functions

- [`fetch_enr`](https://almartin82.github.io/nmschooldata/reference/fetch_enr.md):

  Fetch enrollment data for a school year

- [`fetch_enr_multi`](https://almartin82.github.io/nmschooldata/reference/fetch_enr_multi.md):

  Fetch enrollment data for multiple years

- [`tidy_enr`](https://almartin82.github.io/nmschooldata/reference/tidy_enr.md):

  Transform wide data to tidy (long) format

- [`id_enr_aggs`](https://almartin82.github.io/nmschooldata/reference/id_enr_aggs.md):

  Add aggregation level flags

- [`enr_grade_aggs`](https://almartin82.github.io/nmschooldata/reference/enr_grade_aggs.md):

  Create grade-level aggregations

## Cache functions

- [`cache_status`](https://almartin82.github.io/nmschooldata/reference/cache_status.md):

  View cached data files

- [`clear_cache`](https://almartin82.github.io/nmschooldata/reference/clear_cache.md):

  Remove cached data files

## ID System

New Mexico uses a hierarchical ID system:

- District codes: 3 digits (e.g., 001 = Albuquerque Public Schools)

- School codes: 3 digits within each district

- Full IDs combine district and school codes

## Data Sources

Data is sourced from the New Mexico Public Education Department:

- PED Main: <https://webnew.ped.state.nm.us/>

- STARS: <https://web.ped.nm.gov/bureaus/information-technology/stars/>

- NM Vistas: <https://nmvistas.org/>

## Data Availability

Enrollment data is available from 2016-2025:

- 2016-2018: Enrollment by grade only (no demographic subgroups)

- 2019-2023, 2025: Full 40-Day (40D) subgroup enrollment with
  demographics

- 2024: Only 80-Day data available (40D subgroup file not yet published)

Data includes state, district, and school level enrollment counts.

## New Mexico Context

New Mexico has approximately 89 school districts. The state has a
majority-minority student population with Hispanic students representing
the largest demographic group. The state also has significant Native
American student enrollment, particularly in districts near tribal
lands.

## See also

Useful links:

- <https://almartin82.github.io/nmschooldata>

- <https://github.com/almartin82/nmschooldata>

- Report bugs at <https://github.com/almartin82/nmschooldata/issues>

## Author

**Maintainer**: Al Martin <almartin@example.com>
