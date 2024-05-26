library(sf)
library(tidyverse)

nc = system.file("gpkg/nc.gpkg", package = "sf") |>
  read_sf()

nc_32119 = st_transform(nc, "EPSG:32119")

nc_32119 |>
  select(BIR74) |>
  plot(graticule = TRUE, axes = TRUE)
