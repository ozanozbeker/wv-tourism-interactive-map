# Introduction ----
# This scripts contains all the code for the project in one R script for easy sharing.
# I chose not to include the comments from the notebooks as this is a more streamlined version.
# You can find the full project with code at https://github.com/ozanozbeker/wv-tourism-interactive-map

# Initialize ----
library(DBI)
library(duckdb)
library(tidyverse)
library(zipcodeR)
library(jsonlite)

## Functions ----
get_request = function(url) {
  url |>
    request() |>
    req_method("GET") |>
    req_throttle(rate = 2) |> # 2 requests per second
    req_user_agent("Ozan Ozbeker (https://github.com/ozanozbeker/wv-tourism-interactive-map)")
}

extract_homepage_info = function(resp) {

  page = resp |> resp_body_html()

  title = page |>
    html_element("h1.title") |>
    html_text2()

  description = page |>
    html_element(".wvto-hero div.copy") |>
    html_text2() |>
    str_remove(fixed(title)) |>
    str_remove("\\n")

  results = tibble(
    title = title,
    description = description
  )

  return(results)
}

extract_categories_urls = function(resp) {

  page = resp |> resp_body_html()

  title = page |> html_element("h1.title") |> html_text2()

  results = resp |>
    resp_body_html() |>
    html_element("ul.ttd") |>
    html_children() |>
    html_children() |>
    html_attrs() |>
    as_tibble_col() |>
    unnest_wider(value) |>
    clean_names() |>
    filter(is.na(class)) |>
    select(category = title, url = href) |>
    mutate(group = title, .before = 1)

  return(results)
}

extract_destinations_urls = function(resp) {

  page = resp |> resp_body_html()

  title = page |> html_element("h1.title") |> html_text2()

  results = page |>
    html_elements(".listings-wall div.meta-top") |>
    html_children() |>
    html_attrs() |>
    as_tibble_col() |>
    unnest_wider(value) |>
    clean_names() |>
    select(destination = title, url = href) |>
    filter(str_detect(destination, "LIST:")) |>
    mutate(destination = str_remove(destination, fixed("LIST: "))) |>
    mutate(category = title, .before = 1)

  return(results)
}

extract_destination_info = function(resp) {

  page = resp_body_html(resp)

  url = resp_url(resp)

  destination = page |>
    html_elements("h1") |>
    html_text2()

  description = page |>
    html_element(".desc") |>
    html_elements("p") |>
    html_text2() |>
    str_c(collapse = " ") |>
    str_replace_all('”', '"') |>
    str_replace_all('“', '"')

  website = page |>
    html_element(".meta_block--website a") |>
    html_attr("href")

  phone = page |>
    html_element('span[itemprop="telephone"]') |>
    html_text2()

  email = page |>
    html_element('a[itemprop="email"]') |>
    html_text2()

  address = page |>
    html_element('a[class="address"]') |>
    html_text2()

  results = tibble(
    url = url,
    destination = destination,
    description = description,
    website = website,
    phone = phone,
    email = email,
    address = address
  )

  return(results)
}

# Build DuckDB ----
con = dbConnect(duckdb(), "duck.db")

## Zip Codes ----
zip_codes = search_state("WV") |>
  select(zipcode, major_city, cities = common_city_list, county) |>
  mutate(cities = map(cities, parse_gzjson_raw)) |>
  unnest_longer(cities) |>
  summarize(
    .by = c(zipcode, major_city, county),
    cities = str_c(cities, collapse = "|"))

dbWriteTable(con, "zip_codes", zip_codes)

## Travel Regions ----
travel_regions = tribble(
  ~region,                         ~county,
  "Eastern Panhandle",             "Berkeley County",
  "Eastern Panhandle",             "Jefferson County",
  "Eastern Panhandle",             "Morgan County",
  "Hatfield-McCoy Mountains",      "Boone County",
  "Hatfield-McCoy Mountains",      "Lincoln County",
  "Hatfield-McCoy Mountains",      "Logan County",
  "Hatfield-McCoy Mountains",      "McDowell County",
  "Hatfield-McCoy Mountains",      "Mercer County",
  "Hatfield-McCoy Mountains",      "Mingo County",
  "Hatfield-McCoy Mountains",      "Wayne County",
  "Hatfield-McCoy Mountains",      "Wyoming County",
  "Metro Valley",                  "Cabell County",
  "Metro Valley",                  "Kanawha County",
  "Metro Valley",                  "Mason County",
  "Metro Valley",                  "Putnam County",
  "Mid-Ohio Valley",               "Calhoun County",
  "Mid-Ohio Valley",               "Jackson County",
  "Mid-Ohio Valley",               "Pleasants County",
  "Mid-Ohio Valley",               "Ritchie County",
  "Mid-Ohio Valley",               "Roane County",
  "Mid-Ohio Valley",               "Wirt County",
  "Mid-Ohio Valley",               "Wood County",
  "Mountain Lakes",                "Braxton County",
  "Mountain Lakes",                "Clay County",
  "Mountain Lakes",                "Gilmer County",
  "Mountain Lakes",                "Lewis County",
  "Mountain Lakes",                "Nicholas County",
  "Mountain Lakes",                "Upshur County",
  "Mountain Lakes",                "Webster County",
  "Mountaineer Country",           "Barbour County",
  "Mountaineer Country",           "Doddridge County",
  "Mountaineer Country",           "Harrison County",
  "Mountaineer Country",           "Marion County",
  "Mountaineer Country",           "Monongalia County",
  "Mountaineer Country",           "Preston County",
  "Mountaineer Country",           "Taylor County",
  "New River - Greenbrier Valley", "Fayette County",
  "New River - Greenbrier Valley", "Greenbrier County",
  "New River - Greenbrier Valley", "Monroe County",
  "New River - Greenbrier Valley", "Raleigh County",
  "New River - Greenbrier Valley", "Summers County",
  "Northern Panhandle",            "Brooke County",
  "Northern Panhandle",            "Hancock County",
  "Northern Panhandle",            "Marshall County",
  "Northern Panhandle",            "Ohio County",
  "Northern Panhandle",            "Tyler County",
  "Northern Panhandle",            "Wetzel County",
  "Potomac Highlands",             "Grant County",
  "Potomac Highlands",             "Hampshire County",
  "Potomac Highlands",             "Hardy County",
  "Potomac Highlands",             "Mineral County",
  "Potomac Highlands",             "Pendleton County",
  "Potomac Highlands",             "Pocahontas County",
  "Potomac Highlands",             "Randolph County",
  "Potomac Highlands",             "Tucker County"
)

dbWriteTable(con, "travel_regions", travel_regions)

# Scrape Destinations ----
## Groups ----
groups_urls = tribble(
  ~ group, ~ url,
  "Outdoor Adventures", "https://wvtourism.com/things-to-do/outdoor-adventures/",
  "Luxury & Relaxation", "https://wvtourism.com/things-to-do/luxury-relaxation/",
  "Culture & Lifestyle", "https://wvtourism.com/things-to-do/arts-culture-history/",
  "Places to Stay", "https://wvtourism.com/places-to-stay/",
  "Places to Go", "https://wvtourism.com/places-to-go/"
)

groups_reqs = groups_urls |> pull(url) |> map(get_request)
groups_resps = groups_reqs |> req_perform_sequential(on_error = "continue")
groups_resps_failures = resps_failures(groups_resps) |> print()
groups_resps_successes = resps_successes(groups_resps) |> print()

groups = groups_resps_successes |>
  map(extract_homepage_info) |>
  list_rbind() |>
  rename(group = title) |>
  left_join(groups_urls, join_by(group))

dbWriteTable(con, "groups", groups)

## Categories ----
categories_urls = groups_resps_successes |>
  map(extract_categories_urls) |>
  list_rbind() |>
  filter(
    category != "Stargazing", # Section of website unfinished as of 2024-08-14
    # The following are big/different enough to need their own scripts:
    str_detect(url, "/state-parks/", negate = TRUE),    # scrape_state-parks.R
    str_detect(url, "/scenic-routes/", negate = TRUE),  # scrape_routes.R
    str_detect(url, "/travel-regions/", negate = TRUE)) # scrape_cities.R

categories_reqs = categories_urls |> pull(url) |> map(get_request)
categories_resps = categories_reqs |> req_perform_sequential(on_error = "continue")
categories_resps_failures = resps_failures(categories_resps)
categories_resps_successes = resps_successes(categories_resps)

categories = categories_resps_successes |>
  map(extract_homepage_info) |>
  list_rbind() |>
  rename(category = title) |>
  left_join(categories_urls, join_by(category))

dbWriteTable(con, "categories", categories)

## Destinations ----
destinations_urls = categories_resps_successes |>
  map(extract_destinations_urls, .progress = TRUE) |>
  list_rbind()

destinations_requests = destinations_urls |> distinct(url) |> pull(url) |> map(get_request)
destinations_resps = destinations_requests |> req_perform_sequential(on_error = "continue")
destinations_resps_successes = resps_successes(destinations_resps)
destinations_resps_failures = resps_failures(destinations_resps)

destination_category_cross = destinations_urls |>
  distinct(url, category) |>
  arrange(url)

dbWriteTable(con, "destination_category_cross", destination_category_cross)

destinations = destinations_resps_successes |>
  map(extract_destination_info, .progress = TRUE) |>
  list_rbind() |>
  mutate(
    destination = coalesce(destination, pull_destination_from_url(url)),
    zip = if_else(
      str_sub(address, -5L, -5L) == "-",
      str_sub(address, -10L, -6L),
      str_sub(address, -5L, -1L)),
    across(where(is.character), \(x) if_else(x == "", NA_character_, str_squish(x)))) |>
  distinct()

dbWriteTable(con, "destinations", destinations)
