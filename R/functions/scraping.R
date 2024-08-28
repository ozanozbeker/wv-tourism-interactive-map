# General) ----
get_request = function(url) {
  url |>
    request() |>
    req_method("GET") |>
    req_throttle(rate = 5) |> # 5 requests per second
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

# 02_scrape_destinations ----
extract_categories_urls = function(resp) {

  page = resp |> resp_body_html()

  title = page |> html_element("h1.title") |> html_text2()

  results = page |>
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

# 03_scrape_cities ----
extract_cities_urls = function(resp) {

  page = resp |> resp_body_html()

  title = page |> html_element("h1.title") |> html_text2()

  results = page |>
    html_element("div.wvto-featured-pages") |>
    html_children() |>
    html_children() |>
    html_children() |>
    html_children() |>
    html_attrs() |>
    as_tibble_col("url") |>
    unnest(url) |>
    distinct() |>
    mutate(
      travel_region = title,
      city = url |>
        str_remove("https://wvtourism.com/") |>
        str_remove_all("/") |>
        str_replace_all("-", " ") |>
        str_to_title(),
      .before = 1)

  return(results)
}

extract_city_info = function(resp) {

  page = resp |> resp_body_html()

  header_count = page |>
    html_elements("h3.wp-block-heading") |>
    html_text2() |>
    as_tibble_col() |>
    filter(value != "") |>
    NROW()

  if (header_count == 0) {
    title = page |>
      html_element("h1") |>
      html_text()

    description = page |>
      html_element("div.desc") |>
      html_element("p") |>
      html_text2()

    categories = NULL
  }

  if (header_count > 0) {
    title = page |>
      html_element("h1.title") |>
      html_text2()

    description = page |>
      html_element(".wvto-hero") |>
      html_children() |>
      html_children() |>
      html_children() |>
      html_text2()

    # Shepherdstown
    categories = page |>
      html_elements(".wp-block-group") |>
      html_children() |>
      html_children() |>
      html_text2() |>
      as_tibble_col("category") |>
      filter(category != "") |>
      slice_head(n = 2 * header_count) |>
      mutate(
        row_number = row_number(),
        description = lead(category)) |>
      filter(row_number %% 2 == 1) |>
      select(-row_number) |>
      pivot_wider(
        names_from = category,
        values_from = description)

    if (ncol(categories) != header_count) {
      # Charleston
      categories = page |>
        html_elements(".wp-block-columns")  |>
        html_children() |>
        html_children() |>
        html_text2() |>
        as_tibble_col("category") |>
        filter(category != "") |>
        slice_head(n = 2 * header_count) |>
        mutate(
          row_number = row_number(),
          description = lead(category)) |>
        filter(row_number %% 2 == 1) |>
        select(-row_number) |>
        pivot_wider(
          names_from = category,
          values_from = description)
    }
  }

  df = tibble(
    city = title,
    description = description) |>
    bind_cols(categories)

  return(df)
}

# 04_scrape_routes ----
extract_mountain_ride_info = function(resp) {

  page = resp |> resp_body_html()

  title = page |> html_elements("h1.title") |> html_text2()

  description = page |>
    html_element("div.cell.copy-cell") |>
    html_element("div.copy") |>
    html_text2() |>
    str_remove_all("\\n") |>
    str_remove(fixed(title))

  details = page |>
    html_element(xpath = "//div[contains(@class, 'gb-block-layout-column-inner')][h2]") |>
    html_elements("p") |>
    html_text2() |>
    str_c(collapse = " ") |>
    str_remove(fixed(" Ready to hit the road? Use the button below to view the detailed directions and to send this itinerary directly to Google Maps on your smartphone. Click Here To Download This Route"))

  results = tibble(
    road_trip = title,
    description = description,
    details = details
  )

  return(results)
}

extract_mountain_ride_route = function(resp) {

  page = resp |> resp_body_html()

  title = page |> html_elements("h1.title") |> html_text2()

  destinations = page |>
    html_element("div.wp-block-group.content") |>
    html_elements("h2") |>
    html_text2() |>
    as_tibble_col("destination") |>
    mutate(destination = str_replace_all(destination, "and", "&")) |>
    filter(destination != "")

  descriptions = page |>
    html_element("div.wp-block-group.content") |>
    html_elements("p.has-text-align-center") |>
    html_text2() |>
    as_tibble_col("description") |>
    filter(description != "Explore More Mountain Rides")

  results = destinations |>
    bind_cols(descriptions) |>
    mutate(
      road_trip = title,
      route_stop = row_number(),
      .before = 1)

  return(results)
}

extract_road_trip_info = function(resp) {

  page = resp |> resp_body_html()

  url = resp$url

  details = page |>
    html_element("p.content") |>
    html_text2()

  results = tibble(
    details = details,
    url = url
  )

  return(results)
}

extract_road_trip_route = function(resp) {

  page = resp |> resp_body_html()

  url = resp$url

  destinations = page |>
    html_elements("div.wp-block-genesis-blocks-gb-column.gb-is-vertically-aligned-center") |>
    html_children() |>
    html_elements("h3") |>
    html_text2() |>
    as_tibble_col("destination") |>
    mutate(destination = str_replace_all(destination, "and", "&")) |>
    filter(destination != "")

  descriptions = page |>
    html_elements("div.wp-block-genesis-blocks-gb-column.gb-is-vertically-aligned-center") |>
    html_children() |>
    html_elements("p") |>
    html_text2() |>
    as_tibble_col("description")

  # pull_destination_from_grid()
  destinations_urls = page |>
    html_elements("div.grid-x.card") |>
    html_elements("div.cell.small-12") |>
    html_elements("div.meta-top") |>
    html_elements(xpath = "a[not(@target)]") |>
    html_attrs() |>
    as_tibble_col() |>
    unnest_wider(value) |>
    select(destination = title, url_destination = href) |>
    mutate(destination = str_remove(destination, fixed("LIST: ")))

  results = destinations |>
    bind_cols(descriptions) |>
    mutate(
      url = url,
      route_stop = row_number(),
      .before = 1) |>
    left_join(destinations_urls, join_by(destination))

  return(results)
}
