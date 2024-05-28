# Initialize ----
library(tidyverse) # Load the Tidyverse
library(httr2) # HTTP Requests
library(xml2)  # Parse XML
library(rvest) # Parse HTML

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

# Data ----
history = read_rds("Pipeline/companies.rds")

urls = c(
  "https://wvtourism.com/company-sitemap.xml",
  "https://wvtourism.com/company-sitemap2.xml",
  "https://wvtourism.com/company-sitemap3.xml")

# Scrape ----
# Get the current list of companies
sitemap_responses = map(urls, get_request) |>
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

# Save Results ----
companies = history |>
  anti_join(info, join_by(url)) |>
  union(info)

write_rds(companies, "Pipeline/companies.rds")
write_csv(companies, "Output/companies.csv", na = "")

# Do something for failures if it becomes an issue
