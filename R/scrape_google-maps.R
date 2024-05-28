library(googleway)
library(keyring)
library(tidyverse)

airports = read_csv("C:/Users/OzanO/Downloads/airports.csv", name_repair = janitor::make_clean_names)

map = google_map(
  data = tram_stops
)

map |>
  add_markers(
    lat = "stop_lat",
    lon = "stop_lon"
  )
