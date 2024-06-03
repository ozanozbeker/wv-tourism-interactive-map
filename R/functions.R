# Import ----
## Logging ----
write_log_line = function(path_log) {
  write_file( # I like the lines, sue me
    str_c('|------------------------------------------------------------------------|\n'),
    path_log,
    append = TRUE
  )
}

write_to_log = function(path_log, text) {
  write_file(
    str_c(str_sub(now(), 1, 23),' | ', text, '\n'),
    path_log,
    append = TRUE
  )
}

## Scraping ----
get_request = function(url) {
  request(url) |>
    req_method("GET") |>
    req_throttle(rate = 30 / 60) |> # 30 requests per minute
    req_user_agent("Ozan Ozbeker (https://github.com/ozanozbeker/wv-tourism)")
}

extract_sitemap_index = function(resp_sitemap_index) {
  # There might be a cleaner way of doing this with the
  # actual xml2 functions but {tidyr} go burrr.
  resp_body_xml(resp_companies_index) |>
  xml2::as_list() |>
  as_list() |>
  list_flatten() |>
  list_flatten() |>
  list_flatten() |>
  as_tibble(.name_repair = janitor::make_clean_names) |>
  rename(
    sitemapindex_sitemap_loc_1 = sitemapindex_sitemap_loc,
    sitemapindex_sitemap_lastmod_1 = sitemapindex_sitemap_lastmod) |>
  pivot_longer(
    cols = everything(),
    names_to = c(".value", "row"),
    names_pattern = "sitemapindex_sitemap_(.*)_(.*)") |>
  mutate(
    url = loc,
    last_modified = ymd_hms(lastmod, tz = "UTC"),
    .keep = "none")
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
