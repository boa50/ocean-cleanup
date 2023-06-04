library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(lubridate)
library(ggplot2)
library(stringr)

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

# Cleanups qty
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

# Miles
df %>%
  select(id, miles) %>%
  write_csv("miles.csv")

# Volunteers
df %>%
  select(id, adults, children) %>%
  write_csv("volunteers.csv")

# Locations
df %>%
  mutate(geo_info = paste(gps_lat, gps_long, sep = ","),
         city = gsub("^(.*),.*", "\\1", zone),
         state = gsub(",.*", "", state)) %>%
  select(id, geo_info, city, state) %>%
  write_csv("locations.csv")

# Common objects
df %>%
  select(c(1,15:61)) %>%
  pivot_longer(-1, names_to = "object", values_to = "quantity") %>%
  filter(quantity > 0) %>%
  mutate(object = str_replace_all(object, "_", " ") %>% str_to_sentence) %>%
  write_csv("objects.csv")
