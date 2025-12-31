# nmschooldata

Fetch and analyze New Mexico public school enrollment data from the New Mexico Public Education Department (PED).

## What can you find with nmschooldata?

**10 years of enrollment data (2016-2025).** Around 330,000 students. 89 school districts. Here are ten stories hiding in the numbers:

---

### 1. A majority-minority state

New Mexico is one of the most diverse states in the nation. Hispanic students make up over 60% of enrollment, with significant Native American representation.

```r
library(nmschooldata)
library(dplyr)

enr <- fetch_enr(2023)

enr %>%
  filter(is_state, grade_level == "TOTAL", subgroup != "total") %>%
  filter(subgroup %in% c("hispanic", "white", "native_american", "black", "asian")) %>%
  mutate(pct = pct * 100) %>%
  select(subgroup, n_students, pct) %>%
  arrange(desc(n_students))
```

---

### 2. The Native American education landscape

New Mexico has the third-highest Native American student population in the country, concentrated in districts near the Navajo Nation, Zuni, and numerous Pueblos.

```r
enr <- fetch_enr(2023)

enr %>%
  filter(is_district, grade_level == "TOTAL", subgroup == "native_american") %>%
  arrange(desc(n_students)) %>%
  head(10) %>%
  select(district_name, n_students, pct)
```

---

### 3. Enrollment has been declining

Like many western states, New Mexico's K-12 enrollment peaked and has been slowly declining.

```r
enr <- fetch_enr_multi(2019:2023)

enr %>%
  filter(is_state, grade_level == "TOTAL", subgroup == "total") %>%
  select(end_year, n_students) %>%
  mutate(change = n_students - lag(n_students))
```

---

### 4. COVID hit New Mexico schools hard

The pandemic caused notable enrollment drops, especially in early elementary grades where families delayed kindergarten entry.

```r
enr <- fetch_enr_multi(2019:2023)

enr %>%
  filter(is_state, subgroup == "total",
         grade_level %in% c("K", "01", "06", "09")) %>%
  select(end_year, grade_level, n_students) %>%
  tidyr::pivot_wider(names_from = grade_level, values_from = n_students)
```

---

### 5. Albuquerque dominates, but is shrinking

Albuquerque Public Schools serves roughly one-quarter of all New Mexico students, but the district has been losing enrollment faster than the state average.

```r
enr <- fetch_enr_multi(2019:2023)

enr %>%
  filter(is_district, grade_level == "TOTAL", subgroup == "total",
         grepl("Albuquerque", district_name)) %>%
  select(end_year, district_name, n_students) %>%
  mutate(
    change_pct = (n_students / first(n_students) - 1) * 100
  )
```

---

### 6. Rural and urban divide

New Mexico's schools range from large urban districts like Albuquerque and Las Cruces to tiny rural districts serving fewer than 100 students.

```r
enr <- fetch_enr(2023)

# District size distribution
enr %>%
  filter(is_district, grade_level == "TOTAL", subgroup == "total") %>%
  mutate(size_category = case_when(
    n_students >= 10000 ~ "Large (10k+)",
    n_students >= 1000 ~ "Medium (1k-10k)",
    n_students >= 100 ~ "Small (100-1k)",
    TRUE ~ "Tiny (<100)"
  )) %>%
  group_by(size_category) %>%
  summarize(
    n_districts = n(),
    total_students = sum(n_students)
  )
```

---

### 7. High economic disadvantage rates

New Mexico has one of the highest child poverty rates in the country. The economically disadvantaged student data tells this story.

```r
enr <- fetch_enr(2023)

enr %>%
  filter(is_state, grade_level == "TOTAL", subgroup == "econ_disadv") %>%
  select(n_students, pct)
```

---

### 8. English Learners in a bilingual state

New Mexico's border location and cultural heritage means a significant English Learner population.

```r
enr <- fetch_enr(2023)

# State-level EL enrollment
enr %>%
  filter(is_state, grade_level == "TOTAL", subgroup == "ell") %>%
  select(n_students, pct)

# Districts with highest EL populations
enr %>%
  filter(is_district, grade_level == "TOTAL", subgroup == "ell") %>%
  arrange(desc(n_students)) %>%
  head(10) %>%
  select(district_name, n_students, pct)
```

---

### 9. Grade-level patterns

New Mexico follows national patterns with smaller kindergarten cohorts than upper grades, a demographic shift that will work its way through the system.

```r
enr <- fetch_enr(2023)

enr %>%
  filter(is_state, subgroup == "total",
         grade_level %in% c("PK", "K", "01", "05", "08", "12")) %>%
  select(grade_level, n_students) %>%
  arrange(match(grade_level, c("PK", "K", "01", "05", "08", "12")))
```

---

### 10. The charter school landscape

New Mexico has a significant charter school sector. Compare district-run schools versus charter schools across the state.

```r
enr <- fetch_enr(2023)

# Schools by type
enr %>%
  filter(is_school, grade_level == "TOTAL", subgroup == "total") %>%
  mutate(is_charter = grepl("Charter|Academy", school_name, ignore.case = TRUE)) %>%
  group_by(is_charter) %>%
  summarize(
    n_schools = n(),
    total_enrollment = sum(n_students)
  )
```

---

## Installation

```r
# install.packages("remotes")
remotes::install_github("almartin82/state-schooldata", subdir = "nmschooldata")
```

## Quick start

```r
library(nmschooldata)
library(dplyr)

# Fetch one year
enr_2023 <- fetch_enr(2023)

# Fetch multiple years
enr_multi <- fetch_enr_multi(2019:2023)

# State totals
enr_2023 %>%
  filter(is_state, subgroup == "total", grade_level == "TOTAL")

# District breakdown
enr_2023 %>%
  filter(is_district, subgroup == "total", grade_level == "TOTAL") %>%
  arrange(desc(n_students))

# Demographics statewide
enr_2023 %>%
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("hispanic", "white", "native_american", "black", "asian")) %>%
  select(subgroup, n_students, pct)
```

## Data availability

| Years | Source | Aggregation Levels | Demographics | Notes |
|-------|--------|-------------------|--------------|-------|
| **2019-2023, 2025** | 40-Day subgroup files | State, District, School | Race, Gender, Special Populations (EL, Econ Disadv, SpEd) | Full demographic detail |
| **2024** | 80-Day file only | State, District, School | Limited | 40D subgroup file not yet published |
| **2016-2018** | 40-Day enrollment | State, District, School | Grade-level only | No demographic subgroups |

### What's available by year range

- **Demographics**: Race/ethnicity, gender, and special populations (ELL, economically disadvantaged, special education) available 2019+. Only grade-level counts for 2016-2018.
- **Aggregation**: Package computes state aggregates from district data. District and school levels included in raw data.
- **40-Day vs 80-Day**: New Mexico reports enrollment at 40-day and 80-day counts. This package uses 40-day data when available (the standard snapshot used for funding).

## Data source

New Mexico Public Education Department:
- [PED Main](https://webnew.ped.state.nm.us/)
- [STARS System](https://web.ped.nm.gov/bureaus/information-technology/stars/)
- [NM Vistas](https://nmvistas.org/)

## Part of the 50 State Schooldata Family

This package is part of a family of R packages providing school enrollment data for all 50 US states. Each package fetches data directly from the state's Department of Education.

**See also:** [njschooldata](https://github.com/almartin82/njschooldata) - The original state schooldata package for New Jersey.

**All packages:** [github.com/almartin82](https://github.com/almartin82?tab=repositories&q=schooldata)

## Author

[Andy Martin](https://github.com/almartin82) (almartin@gmail.com)

## License

MIT
