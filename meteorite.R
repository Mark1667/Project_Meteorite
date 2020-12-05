library(tidyverse)
library(janitor)



meteorite_data <-read_csv("meteorite_landings.csv")


head(meteorite_data)

stopifnot(
  names(meteorite_data) %in% c("id", "name", "mass (g)", "fall", "year", "GeoLocation")
)


Lat_long_check <- function(meteorite_data check){
  
      # put in the checks on the data first 
  meteorite_data_check %>% 
      verify(latitude >= -90 & latitude <=90) %>% 
      verify(longitude >= -180 & longitude <= 180)
    
  returnValue()
  
}





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
 
  
  meteorite_cleans_numeric %>% 
    summarise(across(.fns = ~sum(is.na(.x))))
  
  
  meteorite_greater_than_999 <- meteorite_cleans_numeric%>% 
    filter(mass_g >= 1000) %>% 
    arrange((desc(year)))
  
  view(meteorite_greater_than_999)
  
  
  install.packages("assertr")
  
  
  library(assertr)
  