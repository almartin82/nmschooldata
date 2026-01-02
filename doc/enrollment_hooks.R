## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 8,
  fig.height = 5
)

## ----load-packages------------------------------------------------------------
library(nmschooldata)
library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(theme_minimal(base_size = 14))

## ----statewide-trend----------------------------------------------------------
# Note: 2024 excluded as only 80-Day data is available (no 40-Day subgroup file)
enr <- fetch_enr_multi(c(2019:2023, 2025))

state_totals <- enr |>
  filter(is_state, subgroup == "total", grade_level == "TOTAL") |>
  select(end_year, n_students) |>
  mutate(change = n_students - lag(n_students),
         pct_change = round(change / lag(n_students) * 100, 2))

state_totals

## ----statewide-chart----------------------------------------------------------
ggplot(state_totals, aes(x = end_year, y = n_students)) +
  geom_line(linewidth = 1.2, color = "#C41230") +
  geom_point(size = 3, color = "#C41230") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "New Mexico Public School Enrollment (2019-2023, 2025)",
    subtitle = "Steady decline accelerated by COVID-19",
    x = "School Year (ending)",
    y = "Total Enrollment"
  )

## ----top-districts------------------------------------------------------------
top_districts <- enr |>
  filter(is_district, subgroup == "total", grade_level == "TOTAL",
         end_year == 2025) |>
  arrange(desc(n_students)) |>
  head(10) |>
  select(district_name, n_students)

top_districts

## ----top-districts-chart------------------------------------------------------
top_districts |>
  mutate(district_name = forcats::fct_reorder(district_name, n_students)) |>
  ggplot(aes(x = n_students, y = district_name, fill = district_name)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::comma(n_students)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.2))) +
  scale_fill_viridis_d(option = "turbo") +
  labs(
    title = "New Mexico's Largest School Districts (2025)",
    subtitle = "APS serves about 1 in 4 NM students",
    x = "Total Enrollment",
    y = NULL
  )

## ----demographics-------------------------------------------------------------
enr_2025 <- fetch_enr(2025)

demographics <- enr_2025 |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("hispanic", "white", "native_american", "black", "asian", "multiracial")) |>
  mutate(pct = round(pct * 100, 1)) |>
  select(subgroup, n_students, pct) |>
  arrange(desc(n_students))

demographics

## ----demographics-chart-------------------------------------------------------
demographics |>
  mutate(subgroup = forcats::fct_reorder(subgroup, n_students)) |>
  ggplot(aes(x = n_students, y = subgroup, fill = subgroup)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "New Mexico Student Demographics (2025)",
    subtitle = "Hispanic students are the clear majority",
    x = "Number of Students",
    y = NULL
  )

## ----native-american----------------------------------------------------------
native_am <- enr_2025 |>
  filter(is_district, subgroup == "native_american", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  head(10) |>
  select(district_name, n_students, pct) |>
  mutate(pct = round(pct * 100, 1))

native_am

## ----covid-k------------------------------------------------------------------
covid_grades <- enr |>
  filter(is_state, subgroup == "total",
         grade_level %in% c("K", "01", "06", "09"),
         end_year %in% c(2019:2023, 2025)) |>
  select(end_year, grade_level, n_students) |>
  pivot_wider(names_from = grade_level, values_from = n_students)

covid_grades

## ----growth-chart-------------------------------------------------------------
enr |>
  filter(is_state, subgroup == "total",
         grade_level %in% c("K", "01", "06", "09")) |>
  ggplot(aes(x = end_year, y = n_students, color = grade_level)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Enrollment by Grade Level Over Time",
    subtitle = "Kindergarten shows steepest COVID-era decline",
    x = "School Year",
    y = "Students",
    color = "Grade"
  )

## ----abq-lc-------------------------------------------------------------------
abq_lc <- enr |>
  filter(is_district, subgroup == "total", grade_level == "TOTAL",
         grepl("Albuquerque|Las Cruces", district_name)) |>
  select(end_year, district_name, n_students) |>
  pivot_wider(names_from = end_year, values_from = n_students)

abq_lc

## ----abq-lc-chart-------------------------------------------------------------
enr |>
  filter(is_district, subgroup == "total", grade_level == "TOTAL",
         grepl("Albuquerque|Las Cruces", district_name)) |>
  ggplot(aes(x = end_year, y = n_students, color = district_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Albuquerque vs Las Cruces Enrollment",
    subtitle = "The state's two largest districts",
    x = "School Year",
    y = "Enrollment",
    color = "District"
  )

