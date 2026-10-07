# Group Project R Code and Report


Loading in data and setting up environment

``` r
rm(list = ls()) # clears environment

library(readr)
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.2.1     ✔ purrr     1.2.2
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   4.0.3     ✔ tibble    3.3.0
    ✔ lubridate 1.9.4     ✔ tidyr     1.3.2
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
data <- read_csv("health_fitness_dataset.csv") 
```

    Rows: 687701 Columns: 23
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (6): date, gender, activity_type, intensity, smoking_status, health_con...
    dbl (17): participant_id, age, height_cm, weight_kg, bmi, duration_minutes, ...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Filtering for rows that

``` r
diabetes_data <- data %>%
  filter(health_condition == "Diabetes")
```
