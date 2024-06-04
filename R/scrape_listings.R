# Initialize ----
library(tidyverse) # Load the Tidyverse
library(httr2) # HTTP Requests
library(rvest) # Parse HTML
library(janitor) # Cleaning Dirty Data
library(keyring) # Access to OS keyring
source("R/functions.R") # Project-wide functions
source("R/variables.R") # Project-wide variables

# Scrape ----
urls = c(
  "https://wvtourism.com/things-to-do/outdoor-adventures/",
  "https://wvtourism.com/things-to-do/luxury-relaxation/",
  "https://wvtourism.com/things-to-do/arts-culture-history/",
  "https://wvtourism.com/places-to-stay/",
  "https://wvtourism.com/places-to-go/")

links_responses = urls |>
  map(get_request) |>
  req_perform_sequential(on_error = "continue")

links = resps_successes(links_responses) |>
  map(extract_links) |>
  list_rbind() |>
  slice_head(n = 1, by = url) |> # Some Categories are repeated across website sections
  filter(
    category != "Stargazing", # Section of website unfinished as of 2024-06-03
    # The following are big/different enough to need their own scripts:
    str_detect(url, "/state-parks/", negate = TRUE),    # scrape_state-parks.R
    str_detect(url, "/scenic-routes/", negate = TRUE),  # scrape_routes.R
    str_detect(url, "/travel-regions/", negate = TRUE)) # scrape_cities.R

listings_responses = links |>
  pull(url) |>
  map(get_request) |>
  req_perform_sequential(on_error = "continue")

listings = list(category = links$category, resp = listings_responses) |>
  pmap(extract_listings) |>
  list_rbind()

# Clean ----
# There is redundancy in scrape data collection, this is on purpose.
# The scrape collects the HTML data regardless, so even though I'm using my custom data to fill,
# the scrape data is there as a back up if needed.

data = listings |>
  mutate(
    across(everything(), \(col) if_else(col == "", NA_character_, str_squish(col))),
    region = if_else(region == "Array", NA_character_, region),
    zip = if_else(
      str_sub(address, -5L, -5L) == "-",
      str_sub(address, -10L, -6L),
      str_sub(address, -5L, -1L))) |>
  left_join(zips |> select(-state, -country, -irs_estimated_population), join_by(zip)) |>
  mutate(
    city = coalesce(city.y, city.x),
    county = coalesce(county.y, county.x),
    .keep = "unused") |>
  left_join(travel_regions, join_by(county)) |>
  mutate(region = coalesce(region.y, region.x), .keep = "unused") |>
  select(category, company, address, city, county, region, zip, url_wvtourism)

companies = data |>
  select(-category) |>
  distinct() |>
  arrange(company) |>
  slice_head(n = 1, by = company) # There were 3 duplicates

categories = data |>
  select(category, url_wvtourism) |>
  inner_join(companies |> select(url_wvtourism), join_by(url_wvtourism))

write_rds(companies, "Pipeline/companies.rds")
write_rds(categories, "Pipeline/categories.rds")

# Google Maps Data ----
google_details_responses = companies |>
  mutate(search_text = str_c(company, address, sep = " ")) |>
  pull(search_text) |>
  map(get_google_details) |>
  req_perform_sequential(on_error = "continue")

google_details = list(url_wvtourism = companies$url_wvtourism, resp = google_responses) |>
  pmap(extract_google_details) |>
  list_rbind()
