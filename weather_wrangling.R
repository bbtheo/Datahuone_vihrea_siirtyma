
# Initialising -----------------------------------------------------------

#install if needed
#install.packages("remotes")
#remotes::install_github("ropengov/fmi2")

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

start_date <- lubridate::ymd("2022.09.01")
end_date <- lubridate::ymd("2022.09.08")

fmi_stations_data_daily <- function(fmisid, start_date, end_date){
  
  
  # function that calls fmi API but fails silently have to be handled in 
  # following functions. returns daily observations of a weather station
  
  
  try( # yanky but works
    fmi2::obs_weather_daily(starttime = start_date,
                            endtime = end_date,
                            fmisid = fmisid) %>% 
      mutate(fmisid = fmisid) %>% 
      as_tibble(),
    silent = T
    ) 
}

fmi_stations_data_hourly <- function(fmisid, start_date, end_date){
  
  # function that calls fmi API but fails silently have to be handled in 
  # following functions. returns hourly observations of a weather station
  
  
  
  try( # yanky but works
    fmi2::obs_weather_daily(starttime = start_date,
                            endtime = end_date,
                            fmisid = fmisid) %>% 
      mutate(fmisid = fmisid) %>% 
      as_tibble(),
    silent = T
  ) 
}

fmi_data<- function(start_date, end_date, station_data = NULL, daily = T){
  
  if (!is.logical(daily)){ 
    stop("daily is not boolean")
  }
  
  if(is.null(station_data)) {
    station_data <- fmi2::fmi_stations() %>% 
      filter(type %in% c("Automaattinen sääasema",
                         "IL:n hallinnoima lentosääasema",
                         "Sadeasema")) %>% 
      select(fmisid) %>% 
      pull()
      
  }
  
  
  if(daily){
    
    station_data_list <- lapply(station_data, fmi_stations_data_daily, 
                                start_date = start_date, 
                                end_date = end_date)
    
  } else {
    station_data_list <- lapply(station_data, fmi_stations_data_hourly, 
                                start_date = start_date, 
                                end_date = end_date)
  }
  
  
  
  elimination <- sapply(station_data_list, is_tibble)
  data <- bind_rows(station_data_list[elimination])
  
  data %>% 
    filter(!is.na(value)) %>% 
    pivot_wider(names_from = variable,
                values_from = value, 
                values_fill = NA)
}



vuosi_data <- fmi_data("2022-01-01", "2022-09-09") 

# Nearest station ------------------------------------------------

dist_mat <- station_data %>% 
  select(lat, lon) %>% 
  dist(upper = F)

dist_mat <- as.matrix(dist_mat)  
colnames(dist_mat) <- station_data$fmisid
rownames(dist_mat) <- station_data$fmisid

diag(dist_mat) <- NA # jotta asema itse ei ole lähin


nearest_station <- function(station_id){
  which(dist_mat[station_id,] == min(dist_mat[station_id,], 
                                   na.rm = T)) %>%  
    names()  
}

lapply(station_data$fmisid, nearest_station)
