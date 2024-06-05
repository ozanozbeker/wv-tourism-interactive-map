# httr2 ----
get_request = function(url) {
  require(httr2)

  request(url) |>
    req_method("GET") |>
    req_throttle(rate = 30 / 60) |> # 30 requests per minute
    req_user_agent("Ozan Ozbeker (https://github.com/ozanozbeker/wv-tourism)")
}

extract_links = function(resp) {
  resp_body_html(resp) |>
    html_element("ul.ttd") |>
    html_children() |>
    html_children() |>
    html_attrs() |>
    as_tibble_col() |>
    unnest_wider(value) |>
    clean_names() |>
    filter(is.na(class)) |>
    select(category = title, url = href, url_bg = data_bg)
}

extract_listings = function(category, resp) {

  base = resp |>
    resp_body_html() |>
    html_element(".listings-wall") |>
    html_children()

  data_1 = base |>
    html_attrs() |>
    as_tibble_col() |>
    unnest_wider(value) |>
    clean_names() |>
    select(-class) |>
    remove_empty("rows") |>
    rename(city = data_city, region = data_region, county = data_county) |>
    mutate(across(everything(), str_to_title))

  data_2 = base |>
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
    mutate(address = str_sub(url_google, 28L, -1L)) |>
    select(-url_google)

  data = data_2 |>
    bind_cols(data_1) |>
    mutate(category = category, .before = company)

  return(data)
}

get_google_details = function(search_text) {
  require(httr2)
  require(keyring)

  request("https://places.googleapis.com/v1/places:searchText") |>
    req_method("POST") |>
    req_throttle(rate = 30 / 60) |> # limit 30 requests per minute
    req_user_agent("Ozan Ozbeker (https://github.com/ozanozbeker/wv-tourism)") |>
    req_headers(
      "Content-Type" = "application/json",
      "X-Goog-Api-Key" = key_get("Google Maps API Key"),
      "X-Goog-FieldMask" = str_c(
        "places.id",
        "places.nationalPhoneNumber",
        "places.formattedAddress",
        "places.location",
        "places.rating",
        "places.googleMapsUri",
        "places.websiteUri",
        "places.regularOpeningHours",
        "places.displayName",
        "places.paymentOptions",
        "places.accessibilityOptions",
        sep = ",")) |>
    req_body_json(list(textQuery = search_text))
}

extract_google_details = function(url_wvtourism, resp) {
  base = resp_body_json(resp) |>
    list_flatten() |>
    list_flatten()

  data = tibble(
    id = base |> pluck("places_id"),
    name = base |> pluck("places_displayName", "text"),
    description = base |> pluck("places_primaryTypeDisplayName", "text"),
    summary = base |> pluck("places_generativeSummary", "overview", "text"),
    phone = base |> pluck("places_nationalPhoneNumber"),
    address = base |> pluck("places_formattedAddress"),
    latitude = base |> pluck("places_location", "latitude"),
    longitude = base |> pluck("places_location", "longitude"),
    rating = base |> pluck("places_rating"),
    url_googlemaps = base |> pluck("places_googleMapsUri"),
    url_company = base |> pluck("places_websiteUri"),
    is_wheelchair_accessible = base |> pluck("places_accessibilityOptions", "wheelchairAccessibleParking"))

  if (!is_null(base |> pluck("places_paymentOptions"))) {
    payment = base |>
      pluck("places_paymentOptions") |>
      list_transpose() |>
      as_tibble_col() |>
      unnest_wider(value) |>
      clean_names()
  }

  if (!is_null(base |> pluck("places_regularOpeningHours", "weekdayDescriptions"))) {
    hours = base |>
      pluck("places_regularOpeningHours", "weekdayDescriptions") |>
      as_tibble_col("hours") |>
      unnest(hours) |>
      mutate(
        day = str_extract(hours, "[:alpha:]*"),
        hours = str_extract(hours, "\\d.*")) |>
      pivot_wider(names_from = day, names_prefix = "hours_", values_from = hours) |>
      clean_names()
  }

  data_all = data |>
    bind_cols(payment) |>
    bind_cols(hours) |>
    mutate(url_wvtourism)

  return(data_all)
}

extract_company_info = function(resp) {
  # Read Page
  page = resp_body_html(resp)

  # Extract data
  description = page |> html_element(".desc") |> html_text2() |> str_replace_all("\n", " ") |> str_replace_all("  ", " ")
  website = page |> html_element(".meta_block--website a") |> html_attr("href")
  phone = page |> html_element('span[itemprop="telephone"]') |> html_text2()
  url_company = page |> html_element('a[itemprop="email"]') |> html_text2()

  # Return tibble
  data = tibble(
    url_wvtourism = resp_url(resp),
    description = description,
    website = website,
    phone = phone,
    url_company = url_company)

  return(data)
}

# Tidy ----
extract_after_last_comma = function(address_clean) {
  require(tidyverse)
  require(janitor)

  last_comma = address_clean |>
    str_locate_all(",") |>
    as_tibble(.name_repair = make_clean_names) |>
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
  require(tidyverse)
  require(janitor)

  last_comma = address_clean |>
    str_locate_all(",") |>
    as_tibble(.name_repair = make_clean_names) |>
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

pull_company_from_url = function(url_wvtourism) {
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
