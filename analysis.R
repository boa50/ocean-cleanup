library(readr)
library(dplyr)
library(janitor)

### UNDERSTANDING THE DATA
df <- read_csv("data/ocean-cleanup.csv") %>%
  clean_names()

summary(df)

unique(df$country)

unique(df$cleanup_type)

# It's a mess
unique(df$group_name)

### CLEANING THE DATA
df <- df %>%
  mutate(
    is_group_activity = !is.na(group_name)
  )
