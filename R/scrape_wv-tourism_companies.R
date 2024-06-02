# Initialize ----
library(tidyverse) # Load the Tidyverse
library(googleway) # Google Maps API
library(httr2) # HTTP Requests
library(xml2)  # Parse XML
library(rvest) # Parse HTML
source("R/functions.R") # Project-wide functions
source("R/variables.R") # Project-wide variables

# Functions ----
get_request = function(url) {
  request(url) |>
    req_method("GET") |>
    req_throttle(rate = 30 / 60) |> # 30 requests per minute
    req_user_agent("Ozan Ozbeker (https://github.com/ozanozbeker/wv-tourism)")
}

extract_sitemap = function(resp) {
  # There might be a cleaner way of doing this with the
  # actual {xml2} functions but {tidyr} go burrr.
  resp_body_xml(resp) |>
  as_list() |>
  list_flatten() |>
  list_flatten() |>
  list_flatten() |>
  discard(\(content) str_detect(content, "wp-content")) |>
  as_tibble(.name_repair = janitor::make_clean_names) |>
  rename(
    urlset_url_loc_1 = urlset_url_loc,
    urlset_url_lastmod_1 = urlset_url_lastmod) |>
  pivot_longer(
    cols = everything(),
    names_to = c(".value", "row"),
    names_pattern = "urlset_url_(.*)_(.*)") |>
  mutate(
    url = loc,
    last_modified = ymd_hms(lastmod, tz = "UTC"),
    .keep = "none")
}

extract_info = function(resp) {
  # Read Page
  page = resp_body_html(resp)

  # Extract data
  name = page |> html_element("h1") |> html_text2()
  description = page |> html_element(".desc") |> html_text2() |> str_replace_all("\n", " ") |> str_replace_all("  ", " ")
  website = page |> html_element(".meta_block--website a") |> html_attr("href")
  phone = page |> html_element('span[itemprop="telephone"]') |> html_text2()
  email = page |> html_element('a[itemprop="email"]') |> html_text2()
  address = page |> html_element(".address_cont a") |> html_text2()

  # Return tibble
  data = tibble(
    url = resp_url(resp),
    name = name,
    description = description,
    website = website,
    phone = phone,
    email = email,
    address = address)

  return(data)
}

extract_after_last_comma = function(address_clean) {
  last_comma = address_clean |>
    str_locate_all(",") |>
    as_tibble(.name_repair = janitor::make_clean_names) |>
    slice_tail(n = 1) |>
    pull(1)

  tryCatch({
    text = address_clean |>
    str_sub(last_comma[[1]] + 2, -1) |>
    str_squish()

    return(text)

  }, error = function(e) {

    return(NA_character_)

  })
}

extract_before_last_comma = function(address_clean) {
  last_comma = address_clean |>
    str_locate_all(",") |>
    as_tibble(.name_repair = janitor::make_clean_names) |>
    slice_tail(n = 1) |>
    pull(1)

  tryCatch({
    text = address_clean |>
    str_sub(1, last_comma[[1]] - 1) |>
    str_squish()

    return(text)

  }, error = function(e) {

    return(address_clean)

  })
}

extract_google_geocode = function(url, input, address, latitude, longitude) {

  # API Call & Log
  if (input == "geo") {
    resp = google_reverse_geocode(c(latitude, longitude))
    write_to_log("Logs/google_geocode.txt", str_trunc(str_c("Pulling: ", latitude, " & ", longitude), 80, "right"))
  } else {
    resp = google_geocode(address)
    write_to_log("Logs/google_geocode.txt", str_trunc(str_c("Pulling: ", address), 40, "right"))
  }

  Sys.sleep(3)

  address_components = geocode_address_components(resp)

  # Break if null
  if (is.null(address_components)) {
    data = tibble(
      url = url,
      address = address,
      street = NA_character_,
      city = NA_character_,
      county = NA_character_,
      state = NA_character_,
      zip = NA_character_,
      latitude = latitude,
      longitude = latitude
    )

    return(data)
  }

  # Extract function
  extract_component = function(address_components, type) {
    result = address_components |>
      filter(map_lgl(types, \(types) {type %in% types})) |>
      pull(long_name)

    if (length(result) == 0) {result = ""}
    return(result)
  }

  # Extract relevant info
  street_number = extract_component(address_components, "street_number")
  route = extract_component(address_components, "route")
  street = str_c(street_number, route, sep = " ") |> str_squish()
  city = extract_component(address_components, "locality")
  county = extract_component(address_components, "administrative_area_level_2")
  zip = extract_component(address_components, "postal_code")
  address_clean = str_c(street, city, county, "WV", zip, sep = ", ") |>
    str_squish() |>
    str_replace_all(", ,", ", ") |> # leading commas get taken care of below
    str_replace("^, ", "") |>
    str_replace("^, ", "") |> # if doubled
    str_replace(",$", "") |>
    str_squish()

  data = tibble(
    url = url,
    address = address_clean,
    street = street,
    city = city,
    county = county,
    state = "WV",
    zip = zip,
    latitude = geocode_coordinates(resp)[["lat"]][[1]],
    longitude = geocode_coordinates(resp)[["lng"]][[1]]
  ) |>
    mutate(across(where(is_character), \(col) if_else(col == "", NA_character_, col)))

  return(data)
}

# Data ----
history = read_rds("Pipeline/companies.rds")

urls = c(
  "https://wvtourism.com/company-sitemap.xml",
  "https://wvtourism.com/company-sitemap2.xml",
  "https://wvtourism.com/company-sitemap3.xml")

# Scrape ----
# Get the current list of companies
sitemap_responses = urls |>
  map(get_request) |>
  req_perform_sequential(on_error = "continue")

sitemap_failures = resps_failures(sitemap_responses) # Not used anywhere

sitemap = resps_successes(sitemap_responses) |>
  map(extract_sitemap, .progress = TRUE) |>
  list_rbind() |>
  filter(url != "https://wvtourism.com/company/")

# Get new company pages only if they haven't been updated
info_responses = sitemap |>
  anti_join(history, join_by(url, last_modified)) |>
  arrange(url) |>
  pull(url) |>
  map(get_request) |>
  req_perform_sequential(on_error = "continue")

info_failures = resps_failures(info_responses) # Not used anywhere

info = resps_successes(info_responses) |>
  map(extract_info, .progress = TRUE) |>
  list_rbind() |>
  left_join(sitemap, join_by(url)) |>
  mutate(
    across(where(is.character), \(col) if_else(col == "", NA, col)),
    across(where(is.character), \(col) if_else(str_to_upper(col) == "NA|N/A", NA, col)))

# Tidy the data ----
info_address = info |>
  filter(!is.na(address), address |> str_remove_all("[:punct:]") |> str_detect("[:alpha:]")) |>
  mutate(input = "address", latitude = NA_real_, longitude = NA_real_) |>
  select(url, input, address, latitude, longitude)

info_geo = info |>
  filter(!is.na(address), address |> str_remove_all("[:punct:]") |> str_detect("[:alpha:]", negate = TRUE)) |>
  rowwise() |>
  mutate(
    latitude = extract_before_last_comma(address) |> parse_number(),
    longitude = extract_after_last_comma(address) |> parse_number()) |>
  ungroup() |>
  mutate(input = "geo", address = NA_character_) |>
  select(url, input, address, latitude, longitude)

info_google = union(info_address, info_geo) |>
  pmap(safely(extract_google_geocode), .progress = TRUE)

# Safely stuff goes here

info_full = info_google |>
  left_join(info |> select(-address))

# Save Results ----
companies = history |>
  anti_join(info_full, join_by(url)) |>
  union(info_full)

write_rds(companies, "Pipeline/companies.rds")
write_csv(companies, "Output/companies.csv", na = "")
