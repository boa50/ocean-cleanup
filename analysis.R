library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(ggplot2)

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
    is_group_activity = !is.na(group_name),
    cleanup_date = as.Date(cleanup_date, "%d-%b-%y")
  ) %>%
  filter(year(cleanup_date) <= 2020) %>%
  filter(year(cleanup_date) >= 2015)

# Verifying the number of records per date
df %>%
  filter(year(cleanup_date) <= 2020) %>%
  filter(year(cleanup_date) >= 2015) %>%
  group_by(cleanup_date) %>%
  summarise(qty = n()) %>%
  ggplot(aes(x = cleanup_date, y = qty)) +
  geom_line()

### CHARTS
# Put some KPIs
# Pounds collected until now
# Number of volunteers
# Miles travels (comparing with travels around the world)
#

# Cleanups qtd
# Cleanups pounds
# Location heatmap
# Where are the groups
# What are the most common things
# Which cleanup type is the most efficient

### GETTING ENOUGH INFORMATION
# Date
df %>%
  select(id, cleanup_date) %>%
  write_csv("date.csv")

# Pounds
df %>%
  select(id, pounds) %>%
  write_csv("pounds.csv")
