# Initialize ----
library(tidyverse) # Load the Tidyverse
library(httr2) # HTTP Requests
library(xml2)  # Parse XML
library(rvest) # Parse HTML
library(googleway) # Google Maps API

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

address_cleaner = function(address) {
  address |>
    str_to_upper() |>
    str_replace_all(",,", ",") |>
    str_remove_all("[.]") |>
    str_remove(" WV,| WEST VIRGINIA,") |>
    str_squish()
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

# Data ----
history = read_rds("Pipeline/companies.rds")

# Table of zip codes and details based on USPS standards.
# Population from 2020 census.
# From https://www.unitedstateszipcodes.org/zip-code-database/
wv_zips = read_csv("Data/zip_code_database.csv", show_col_types = FALSE) |>
  filter(state == "WV") |>
  select(zip, primary_city, acceptable_cities, county, population = irs_estimated_population)

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

# Need to separate the instances where it's actually the latitude and longitude
# Other filters: no commas (row 233)
# Do something so if there is no street, city doesn't get removed
#   This could also be fixed by ignoring and filling in city via zipcode

# Maybe add one final human check on bad city and street names
# - streets with more than 1 comma (297)
# - cities with nonstandard characters (297)

# Maybe do something about PO boxes?

# Check to see where zipcode is NA (362)
# Get a list of all WV zipcodes to cross reference

test = info |>
  select(address) |>
  filter(!is.na(address)) |>
  mutate(
    address_clean = address_cleaner(address),
    state = "WV",
    zipcode = address_clean |> str_extract("\\d{5}$|\\d{5}-\\d{4}$") |> str_sub(1, 5),
    address_clean = address_clean |> str_remove(", \\d{5}$|, \\d{5}-\\d{4}$") |> str_squish()) |>
  rowwise() |>
  mutate(
    city = extract_after_last_comma(address_clean),
    street = extract_before_last_comma(address_clean)) |>
  ungroup() |>
  select(-address_clean) |>
  mutate(across(c(street, city), str_to_title))

info_address = info |>
  select(url, name, address) |>
  filter(address |> str_remove_all("[:punct:]") |> str_detect("[:alpha:]", negate = FALSE))

info_geo = info |>
  select(url, name, address) |>
  filter(address |> str_remove_all("[:punct:]") |> str_detect("[:alpha:]", negate = TRUE)) |>
  rowwise() |>
  mutate(
    latitude = extract_before_last_comma(address) |> parse_number(),
    longitude = extract_after_last_comma(address) |> parse_number()
  )

# Save Results ----
companies = history |>
  anti_join(info, join_by(url)) |>
  union(info)

write_rds(companies, "Pipeline/companies.rds")
write_csv(companies, "Output/companies.csv", na = "")

# Do something for failures if it becomes an issue
