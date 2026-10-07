# Group Project R Code


# health and Fitness dataset

[Link to
Dataset](https://www.kaggle.com/datasets/evan65549/health-and-fitness-dataset/data)

Loading in data and setting up environment

``` r
rm(list = ls()) # clears environment

library(readr)
library(tidyverse)
library(knitr)

data <- read_csv("health_fitness_dataset.csv") 
```

## Looking at all particiapnts

``` r
data %>%
  # count observations per participant (cluster)
  group_by(participant_id) %>%
  summarize(n_obs = n(), .groups = "drop") %>%
  summarize(
    n_clusters = n(),                     # number of unique participants
    avg_obs_per_cluster = mean(n_obs)
  ) %>%
  kable() # outputs into nice table :D
```

| n_clusters | avg_obs_per_cluster |
|-----------:|--------------------:|
|       3000 |            229.2337 |

In the entire dataset, there are 3000 participants. With 229
observations on average per participant.

## Looking at participants with diabetes

Filtering for rows where `health_condition` = Diabetes

``` r
diabetes_data <- data %>%
  filter(health_condition == "Diabetes")
```

``` r
diabetes_data %>%
  # count observations per participant (cluster)
  group_by(participant_id) %>%
  summarize(n_obs = n(), .groups = "drop") %>%
  summarize(
    n_clusters = n(),                     # number of unique participants
    avg_obs_per_cluster = mean(n_obs)
  ) %>%
  kable()
```

| n_clusters | avg_obs_per_cluster |
|-----------:|--------------------:|
|        283 |            228.8127 |

There are 283 participants with diabetes in this dataset. With 229
observations on average per participant.

# Chronic Disease EHR Dataset

[Link to
Dataset](https://www.kaggle.com/datasets/zara2099/chronic-disease-ehr-dataset?resource=download)

``` r
chronic_data <- read_csv("Chronic_Disease_EHR_Dataset.csv") 
```

``` r
chronic_data %>%
  group_by(Patient_ID) %>%
  summarize(n_obs = n(), .groups = "drop") %>%
  summarize(
    n_clusters = n(),                     # number of unique participants
    avg_obs_per_cluster = mean(n_obs)
  ) %>%
  kable()
```

| n_clusters | avg_obs_per_cluster |
|-----------:|--------------------:|
|       1750 |                   4 |

There are 1750 participants with diabetes in this dataset. With 4
observations on average per participant.
