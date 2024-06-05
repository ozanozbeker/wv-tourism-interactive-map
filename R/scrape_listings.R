# Initialize ----
library(tidyverse) # Load the Tidyverse
library(httr2) # HTTP Requests
library(rvest) # Parse HTML
library(janitor) # Cleaning Dirty Data
library(keyring) # Access to OS keyring
source("R/functions.R") # Project-wide functions
source("R/variables.R") # Project-wide variables

# Scrape | Listings ----
# First we are going to start with the listings.
# There are about 2200 companies on the website, but only about 1200 are featured.
urls = c(
  "https://wvtourism.com/things-to-do/outdoor-adventures/",
  "https://wvtourism.com/things-to-do/luxury-relaxation/",
  "https://wvtourism.com/things-to-do/arts-culture-history/",
  "https://wvtourism.com/places-to-stay/",
  "https://wvtourism.com/places-to-go/")

# Each of the URLs above act as a base for links to different Categories of Companies
links_responses = urls |>
  map(get_request) |>
  req_perform_sequential(on_error = "continue")

links = resps_successes(links_responses) |>
  map(extract_links) |>
  list_rbind() |>
  filter(
    category != "Stargazing", # Section of website unfinished as of 2024-06-03
    # The following are big/different enough to need their own scripts:
    str_detect(url, "/state-parks/", negate = TRUE),    # scrape_state-parks.R
    str_detect(url, "/scenic-routes/", negate = TRUE),  # scrape_routes.R
    str_detect(url, "/travel-regions/", negate = TRUE)) # scrape_cities.R

# Within each Category page, there are listings of different Companies
listings_responses = links |>
  pull(url) |>
  map(get_request) |>
  req_perform_sequential(on_error = "continue")

listings = list(category = links$category, resp = listings_responses) |>
  pmap(extract_listings) |>
  list_rbind()

# There is redundancy in scrape data collection, this is on purpose.
# The scrape collects the HTML data regardless, so even though I'm using my custom region data,
# the scrape data is there as a back up if needed.
data = listings |>
  mutate(
    across(everything(), \(col) if_else(col == "", NA_character_, str_squish(col))),
    company = coalesce(company, pull_company_from_url(url_wvtourism)),
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

tags = categories |>
  summarise(tags = str_c(category, collapse = ", "), .by = url_wvtourism)

# Scrape | Google Maps Data ----
# This is technically an API call not a web scrape, but cleaning the JSON response
# is similar to extracting HTML elements and it fits the naming scheme.
google_details_responses = companies |>
  mutate(search_text = str_c(company, address, sep = " ")) |>
  pull(search_text) |>
  map(get_google_details) |>
  req_perform_sequential(on_error = "continue")

google_details = list(url_wvtourism = companies$url_wvtourism, resp = google_details_responses) |>
  pmap(extract_google_details, .progress = TRUE) |>
  list_rbind()

# Scrape | Company Pages ----
# Now that the companies are cleaned up, we will scrape their individual pages to gather some more info.
# Some of this info will be cross checked with info from the Google Maps Data above, but again the
# redundancy is for data quality.

# Also, We are going to filter to what matched with the Google API. # Not everything is perfect, and
# also the WV Tourism website has a lot of redundancy in my opinion with 2-3 parts of a business
# listed # even though it's the same thing.

# For perfection, I would need to get paid :)

company_pages_responses = google_details |>
  pull(url_wvtourism) |>
  map(get_request) |>
  req_perform_sequential(on_error = "continue")

company_pages = resps_successes(company_pages_responses) |>
  map(extract_company_info, .progress = TRUE) |>
  list_rbind()

# Final Data ----
# Now we combine everything for a master data table of company information.

companies_all = google_details |>
  left_join(companies, join_by(url_wvtourism)) |>
  left_join(company_pages, join_by(url_wvtourism)) |>
  mutate(
    url_company = coalesce(url_company.y, url_company.x),
    company = if_else(pmax(str_length(company), str_length(name)) == str_length(company), company, name),
    address = coalesce(address.x, address.y),
    .keep = "unused") |>
  select(
    # Company Details
    company, description, rating, phone, email, address, region,
    # Website
    starts_with("url"),
    # Travel Details
    is_wheelchair_accessible, starts_with("accepts"), starts_with("hours"),
    # Mapping Utilities
    latitude, longitude, google_id = id, city, county, zip) |>
  left_join(tags, join_by(url_wvtourism))

# Export ----
write_rds(companies_all, "Pipeline/companies.rds")
write_csv(companies_all, "Output/companies.csv")

write_rds(categories, "Pipeline/categories.rds")
write_csv(categories, "Output/categories.csv")
