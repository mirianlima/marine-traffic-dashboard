# Install/Load Libraries
pacman::p_load(c(
  "tidyverse",
  "dtplyr",
  "lubridate",
  "data.table",
  "utils",
  "geosphere",
  "leaflet"),
  character.only = TRUE,
  install = TRUE)

unzip("../data/ships_04112020.zip", exdir = "../data")

ships_raw <- read_csv("../data/ships.csv") 

names(ships_raw) <- str_to_lower(names(ships_raw))

ships_clean <- ships_raw %>% 
  select(-19) %>% # remove duplicated column
  select(
    shipname,
    ship_type,
    width,
    flag,
    datetime,
    lat,
    lon,
    speed,
    port
  ) %>% # keep only relevant vars
  mutate(
    datetime = ymd_hms(datetime),
    shipname = str_to_title(shipname, locale = "en"),
    port = str_to_title(port, locale = "en")
  ) %>% 
  # Compute distances
  arrange(shipname, ship_type, datetime, port) %>% 
  mutate(ID = rownames(ships_raw)) %>% 
  group_by(shipname) %>% 
  mutate(
    distance = distHaversine(cbind(lon, lat),cbind(lag(lon),lag(lat))),
    distance = replace_na(distance, 0)
  ) %>%
  ungroup()

write_csv(ships_clean, "../data/ships_clean.csv")

vessels <- ships_clean %>% 
  select(port, ship_type, shipname) %>% 
  unique()

write_csv(vessels, "../data/vessels.csv")
  