# Initialize ----
library(tidyverse) # Load the Tidyverse
library(httr2) # HTTP Requests
library(rvest) # Parse HTML
library(janitor) # Cleaning Dirty Data
source("R/functions.R") # Project-wide functions

# Functions ----
extract_links = function(resp) {
  resp_body_html(resp) |>
  html_element("ul.ttd") |>
  html_children() |>
  html_children() |>
  html_attrs() |>
  as_tibble_col() |>
  unnest_wider(value) |>
  clean_names() |>
  filter(str_detect(href, "things-to-do")) |>
  filter(is.na(class)) |>
  select(category = title, url = href, url_bg = data_bg)
}

extract_listing = function(category, url) {
  resp = get_request(url) |> req_perform()

  listings = resp |>
    resp_body_html() |>
    html_element(".listings-wall") |>
    html_children()

  listings_details_1 = listings |>
    html_attrs() |>
    as_tibble_col() |>
    unnest_wider(value) |>
    clean_names() |>
    select(-class) |>
    remove_empty("rows") |>
    rename(city = data_city, region = data_region, county = data_county) |>
    mutate(across(everything(), str_to_title))

  listings_details_2 = listings |>
    html_children() |>
    html_children() |>
    html_elements("div.meta-top") |>
    html_children() |>
    html_attrs() |>
    as_tibble_col() |>
    unnest_wider(value) |>
    clean_names() |>
    select(title, href) |>
    mutate(
      source = if_else(str_detect(href, "wvtourism"), "wvtourism", "google"),
      title = str_remove_all(title, "LIST: |MAPS: ")) |>
    pivot_wider(
      names_from = source,
      names_prefix = "url_",
      values_from = href,
      values_fn = list) |>
    unnest_longer(col = everything()) |>
    rename(company = title) |>
    mutate(
      address = str_sub(url_google, 28L, -1L),
      url_google = URLencode(url_google))

  # Extracting the phone number is easy, but the inconsistency of the phone number
  # for the same company across different listings is not worth the hassel.
  # Also phone numbers are very liable to change anyway.
  # listings_details_3 = listings |>
  #   html_children() |>
  #   html_children() |>
  #   html_elements("div.meta-bottom") |>
  #   html_children() |>
  #   html_elements('a') |>
  #   html_text2() |>
  #   as_tibble_col() |>
  #   mutate(phone = str_sub(value, 8L, -1L), .keep = "none")

  data = listings_details_2 |>
    bind_cols(listings_details_1) |>
    # bind_cols(listings_details_3) |>
    mutate(category = category, .before = company)

  return(data)
}

# Scrapes ----
# The steps are pretty much the same for the most part but wanted to
# separate as they are different calls and have some minor variation.

# Zip Codes
zips = read_html("https://www.zipcodestogo.com/West%20Virginia/") |>
  html_element("table.inner_table") |>
  html_table() |>
  select(zip = X1, city = X2, county = X3) |>
  mutate(zip = parse_number(zip)) |>
  filter(!is.na(zip)) |>
  suppressWarnings() # intentional warning parsing the first two rows

# Outdoor Adventures
resp_outdoor = get_request("https://wvtourism.com/things-to-do/outdoor-adventures/") |> req_perform()
resp_outdoor

links_outdoor = extract_links(resp_outdoor)

outdoor = links_outdoor |>
  select(category, url) |>
  pmap(extract_listing, .progress = TRUE) |>
  list_rbind() |>
  mutate(group = "Outdoor Adventures", .before = category)

# Luxury Relaxation
resp_luxury = get_request("https://wvtourism.com/things-to-do/luxury-relaxation/") |> req_perform()
resp_luxury

links_luxury = extract_links(resp_luxury)

luxury = links_luxury |>
  select(category, url) |>
  pmap(extract_listing, .progress = TRUE) |>
  list_rbind() |>
  mutate(group = "Luxury Relaxation", .before = category)

# Arts, Culture, & History
resp_arts = get_request("https://wvtourism.com/things-to-do/arts-culture-history/") |> req_perform()
resp_arts

links_arts = extract_links(resp_arts)

arts = links_arts |>
  # Scenic Routes are non-standard and will in a different script
  filter(url != "https://wvtourism.com/things-to-do/arts-culture-history/scenic-routes/") |>
  select(category, url) |>
  pmap(extract_listing, .progress = TRUE) |>
  list_rbind() |>
  mutate(group = "Arts, Culture, & History", .before = category)

# Combine & Clean Data ----
# There is redundancy in data collection, this is on purpose.
# The scrape collects the HTML data regardless, so even though I'm using my custom data
# to fill, the scrape data is there as a back up if needed.

data = outdoor |>
  bind_rows(luxury) |>
  bind_rows(arts) |>
  mutate(
    across(everything(), \(col) if_else(col == "", NA_character_, str_squish(col))),
    region = if_else(region == "Array", NA_character_, region),
    zip = if_else(str_sub(address, -5L, -5L) == "-", str_sub(address, -10L, -6L), str_sub(address, -5L, -1L))) |>
  left_join(zips, join_by(zip)) |>
  mutate(
    city = coalesce(city.y, city.x),
    county = coalesce(county.y, county.x),
    .keep = "unused") |>
  left_join(travel_regions, join_by(county)) |>
  mutate(region = coalesce(region.y, region.x), .keep = "unused") |>
  select(
    group, category, company, phone, address, city, county,
    region, state, country, zip, url_wvtourism, url_google
  )

companies = data |>
  select(-group, -category) |>
  distinct() |>
  arrange(company) |>
  slice_head(n = 1, by = company) # There were only 3 duplicates anyway

tags = data |>
  select(group, category, url_wvtourism) |>
  inner_join(companies |> select(url_wvtourism), join_by(url_wvtourism)) |>
  arrange(url_wvtourism) |>
  mutate(topic = "Things To Do", .before = group)

# Export ----
write_csv(companies, "Output/companies.csv")
write_csv(tags, "Output/tags.csv")
