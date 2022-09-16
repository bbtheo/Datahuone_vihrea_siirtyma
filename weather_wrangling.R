
# Initialising -----------------------------------------------------------------

#install if needed
#install.packages("remotes")
#remotes::install_github("ropengov/fmi2")

library(tidyverse)


station_data <- fmi2::fmi_stations()

# Plotting the map -------------------------------------------------------------

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



# Getting the weather from FMI API----------------------------------------------

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
  
  #Hourly api only works for max 30 day request the function has to be converted
  #to a recursive or parallel to work with larger requests
  
  try( # yanky but works
    fmi2::obs_weather_hourly(starttime = start_date,
                            endtime = end_date,
                            fmisid = fmisid) %>% 
      mutate(fmisid = fmisid) %>% 
      as_tibble(),
    silent = T
  ) 
}

fmi_data <- function(start_date, end_date, station_data = NULL, daily = T){
  
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
    
    if(end_date - start_date > lubridate::year(1)){
      Print("too long of a time")
      break
    }
    
    station_data_list <- lapply(station_data, fmi_stations_data_daily, 
                                start_date = start_date, 
                                end_date = end_date)
    
  } else {
    
    if(end_date - start_date > lubridate::month(1)){
      Print("too long of a time")
      break
    }
    
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



# Nearest station --------------------------------------------------------------

dist_mat <- station_data %>% 
  select(lat, lon) %>% 
  dist(upper = F)

dist_mat <- as.matrix(dist_mat)  
colnames(dist_mat) <- station_data$fmisid
rownames(dist_mat) <- station_data$fmisid

diag(dist_mat) <- NA # jotta asema itse ei ole lähin


nearest_station <- function(station_id, filtered = NULL, dist_mat = NULL){
  #Function that returns the closest station id
  
  if(is.null(dist_mat)){
    
    dist_mat <- fmi2::fmi_stations() %>% 
      select(lat, lon) %>% 
      dist(upper = F)
    
    dist_mat <- as.matrix(dist_mat)  
    colnames(dist_mat) <- station_data$fmisid
    rownames(dist_mat) <- station_data$fmisid
    
    diag(dist_mat) <- NA # jotta asema itse ei ole lähin
    
  }
  
  dist_2 <- dist_mat[, !colnames(dist_mat) %in% filtered]
  
  which(dist_2[station_id,] == min(dist_2[station_id,], 
                                   na.rm = T)) %>%  
    names()  
}

nearest_station('101784', c("101776","101785"))

lapply(station_data$fmisid, nearest_station)

# Function to find the data that is missing and nearest neighbor station  ------


# Todella harvoista havainnoista puuttuu molemmat sekä sade että lämpötila 
# näin ollen 

nearest_station("101784")

station_data

neighbours <- vuosi_data %>% 
  distinct(fmisid) %>% 
  rowwise() %>% 
  mutate(nearest = nearest_station(fmisid, 
                                   dist_mat = dist_mat),
         nearest_2 = nearest_station(fmisid, 
                                     nearest, 
                                     dist_mat = dist_mat),
         nearest_3 = nearest_station(fmisid,
                                     c(nearest, nearest_2), 
                                     dist_mat = dist_mat),
         nearest_4 = nearest_station(fmisid,
                                     c(nearest, nearest_2, nearest_3), 
                                     dist_mat = dist_mat),
         nearest_5 = nearest_station(fmisid,
                                     c(nearest, nearest_2, nearest_3, nearest_4), 
                                     dist_mat = dist_mat)
         )


vuosi_data %>% left_join(neighbours)

daily_data <- fmi_data("2022-09-01", "2022-09-02", daily = F)
