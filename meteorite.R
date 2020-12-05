library(tidyverse)
library(janitor)

meteorite_data <-read_csv(("meteorite_landings.csv"))

meteorite_clean <- janitor::clean_names(meteorite_data)

meteorite_clean


meteorite_clean <- meteorite_clean %>%
  separate(
  col = geo_location,
  into = c("latitude", "longitude"),
  sep = "\\,"
  )

meteorite_clean

meteorite_cleans <- meteorite_clean %>%
  mutate(
  longitude = str_remove(longitude, pattern = "[)]"))


meteorite_cleans <- meteorite_clean %>%
  mutate(
    latitude  = str_remove(latitude, pattern = "[(]"))

meteorite_cleans

meteorite_cleans %>%
  summarise( na_latitude = sum(is.na(latitude)),
             na_longitude = sum(is.na(longitude)))

meteorite_cleans %>%
  filterdsads
