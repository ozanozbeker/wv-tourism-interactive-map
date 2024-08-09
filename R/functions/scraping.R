# Exploration
find_attributes = function(page, attribute) {

  page |>
    html_elements("*") |>
    html_attr(attribute) |>
    as_tibble_col("attribute") |>
    remove_missing()
}

# Scrape
get_request = function(url) {
  url |>
    request() |>
    req_method("GET") |>
    req_throttle(rate = 2) |> # 30 requests per minute
    req_user_agent("Ozan Ozbeker (https://github.com/ozanozbeker/wv-tourism-interactive-map)")
}

pull_destination_from_url = function(url_wvtourism) {
  location_tibble = url_wvtourism |>
    str_locate_all("/") |>
    as_tibble_col() |>
    unnest("value") |>
    slice_tail(n = 2)

  last_index = location_tibble |>
    slice_tail(n = 1) |>
    pluck(1) |>
    pluck(1)

  second_last_index = location_tibble |>
    slice_head(n = 1) |>
    pluck(1) |>
    pluck(1)

  company = url_wvtourism |> str_sub(second_last_index, last_index) |>
    str_replace_all("-", " ") |>
    str_remove_all("/") |>
    str_to_title()

  return(company)
}

get_coords = function(url_wvtourism, address) {

  coords = mb_geocode(
    search_text = address,
    search_within = c(-82.644739, 37.201483, -77.719519, 40.638801),
    language = "en",
    country = "US"
  )

  data = tibble(
    url_wvtourism = url_wvtourism,
    lat = coords[[1]],
    lon = coords[[2]]
  )

  return(data)
}

get_address = function(url_wvtourism, lat, lon) {

  address = mb_reverse_geocode(
    coordinates = c(lat, lon)
  )

  data = tibble(
    url_wvtourism = url_wvtourism,
    address = address
  )

  return(data)
}
