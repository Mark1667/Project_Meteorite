library(tidyverse)
library(janitor)
library(assertr)


meteorite_data <-read_csv("Projects/meteorite/meteorite_landings.csv")


head(meteorite_data)

stopifnot(
  names(meteorite_data) %in% c("id", "name", "mass (g)", "fall", "year", "GeoLocation")
)

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
  longitude = str_remove(longitude, pattern = "[)]")) %>% 
  mutate(
    latitude  = str_remove(latitude, pattern = "[(]"))




meteorite_cleans_numeric <- meteorite_cleans%>%
  mutate(
    longitude = as.numeric(longitude)) %>% 
  mutate(
    latitude  = as.numeric(latitude))



  meteorite_cleans_numeric %>% 
  summarise(across(.fns = ~sum(is.na(.x))))

  meteorite_cleans_numeric <- meteorite_cleans_numeric %>% 
  replace(is.na(.), 0)
 
  
 #------------------------------------------------------------
  # put in the checks on the data first 
  meteorite_cleans_numeric %>% 
    verify(latitude >= -90 & latitude <=90) %>% 
    verify(longitude >= -180 & longitude <= 180)
  
  
  
  
  
  meteorite_cleans_numeric %>% 
    summarise(across(.fns = ~sum(is.na(.x))))
  
  
  meteorite_greater_than_999 <- meteorite_cleans_numeric%>% 
    filter(mass_g >= 1000) %>% 
    arrange((desc(year)))
  
  view(meteorite_greater_than_999)
  
  