# Group Project R Code


# Chronic Disease EHR Dataset

[Link to
Dataset](https://www.kaggle.com/datasets/zara2099/chronic-disease-ehr-dataset?resource=download)

``` r
library(readr)
library(tidyverse)
library(knitr)
library(ggplot2)

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
  kable() # outputs into nice table :D
```

| n_clusters | avg_obs_per_cluster |
|-----------:|--------------------:|
|       1750 |                   4 |

There are 1750 participants in this data set. With 4 observations on
average per participant.

# Exploratory Data Analysis

Our Primary outcome of interest is `Estimated_GFR`. or estimated
glomerular filtration rate (eGFR), a continuous measure of kidney
function.

``` r
# Taking a sample of the data so that it doesn't look so cluttered in the plot
set.seed(123)

n_subjects <- 200

sampled_ids <- chronic_data %>%
  distinct(Patient_ID) %>%
  slice_sample(n = n_subjects)

sample_data <- chronic_data %>%
  semi_join(sampled_ids, by = "Patient_ID")


# Spaghetti Plot
ggplot(sample_data, aes(x= Visit_Number, y = Estimated_GFR, group = Patient_ID )) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.4) +
  facet_wrap(~ Diabetes_Status)
```

<div id="fig-spaghettiplot-GFR-Vist">

![](README_files/figure-commonmark/fig-spaghettiplot-GFR-Vist-1.png)

Figure 1: Spaghetti Plot of Estimated GFR by Visit Number

</div>
