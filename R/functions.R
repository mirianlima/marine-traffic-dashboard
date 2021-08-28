
load_data <- function(ship_name){
  fread('data/ships_clean.csv') %>% 
    filter(shipname == ship_name) %>% 
    unique() %>% 
    as_tibble()
}

calculate_max_distance <- function(data){

  indexes <-
    data %>%
    slice_max(distance) %>%
    slice_max(datetime) %>%
    transmute(
      max = ID,
      previous = max-1
    ) %>% 
    as_tibble()
  
  #cols <- c("ID","shipname", "ship_type", "datetime","lat", "lon", "distance", "port", "speed")

  data[data$ID == indexes$previous | data$ID == indexes$max, ]
}

create_map <- function(data){
  
  
  popup <- paste0("<strong>", str_to_upper(data$shipname),"</strong> <br>",
                  "Distance: ",data$distance %>% round()," meters", "<br>",
                  "Date & Time: ", data$datetime, "<br>",
                  "Port at the time: ", data$port, "<br>",
                  "Latitude: ", data$lat, "<br>", 
                  "Longitude: ", data$lon, "<br>")
  
  icons <- awesomeIcons(icon = 'ship',
                        iconColor = "yellow",
                        library = "fa",
                        markerColor = "black")
  
  map <- leaflet() %>%
    addTiles() %>%
    setView(lat = data$lat[[1]],
            lng = data$lon[[1]],
            zoom = 14) %>%
    addAwesomeMarkers(data,
                      lng = data$lon,
                      lat = data$lat,
                      popup = popup,
                      icon = icons) %>%
    addPolylines(data,
                 lng = data$lon,
                 lat = data$lat,
                 color = "yellow")
  
  map 
  
}