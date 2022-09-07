
# Initialising -----------------------------------------------------------

install.packages("remotes")
remotes::install_github("ropengov/fmi2")

library(tidyverse)


station_data <- fmi2::fmi_stations()

# Plotting the map -----------------------------------------------------------

station_data <- station_data %>% 
  filter(type %in% c("Automaattinen sääasema",
                     "IL:n hallinnoima lentosääasema",
                     "Sadeasema"))

# Get data for Tulliniemi only
tulliniemi_station <- station_data %>% 
  dplyr::filter(fmisid == 100946)

# Plot on a map using leaflet
leaflet::leaflet(station_data) %>% 
  leaflet::setView(lng = tulliniemi_station$lon, 
                   lat = tulliniemi_station$lat, 
                   zoom = 5) %>% 
  leaflet::addTiles() %>%  
  leaflet::addMarkers(~lon, 
                      ~lat, popup = ~name, 
                      label = ~as.character(fmisid))



# Getting the weather from FMI API----------------------------------------------------

# pitää muuttaa näistä oma funktio

start_date <- lubridate::ymd("2022.02.01")
end_date <- lubridate::ymd("2022.02.28")

fmi_data <- function(fmisid, start_date, end_date){
  try( # yanky but works
  fmi2::obs_weather_daily(starttime = start_date,
                          endtime = end_date,
                          fmisid = fmisid) %>% 
    mutate(fmisid = fmisid) %>% 
    as_tibble(),
  silent = T) 
}


data <- fmi_data(100946, start_date, end_date)
data


station_data_list <- lapply(station_data$fmisid, fmi_data, 
                            start_date = start_date, 
                            end_date = end_date)

elimination <- sapply(station_data_list, is_tibble)
data <- bind_rows(station_data_list[elimination])

data %>% 
  group_by(variable) %>% #again a yank solution
  mutate(row = row_number()) %>%
  pivot_wider(names_from = variable,
              values_from = value) %>% 
  select(-row) %>%  #again a yank solution
  write_csv(file = 'weather.csv')
