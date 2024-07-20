# Initialize
library(DBI)
library(duckdb)
library(tidyverse)
library(zipcodeR)
library(jsonlite)

# Create DB
con = dbConnect(duckdb(), "wv-tourism.duckdb")

dbListTables(con)

# Tables
zip_codes = search_state("WV") |> 
  select(zipcode, major_city, cities = common_city_list, county) |>
  mutate(cities = cities |> map(parse_gzjson_raw)) |> # Thanks to https://stackoverflow.com/a/77285606/26439015
  unnest_longer(cities) |> 
  summarize(
    .by = c(zipcode, major_city, county), 
    cities = str_c(cities, collapse = "|"))

dbWriteTable(con, "zip_codes", zip_codes)

# Close Connection
dbDisconnect(con)