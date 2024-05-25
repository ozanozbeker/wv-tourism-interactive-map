# Import ----
extract_sitemap_table = function(sitemap_url) {
  # There might be a cleaner way of doing this with the
  # actual xml2 functions but {tidyr} go burrr.

  type = case_when(
    str_detect(sitemap_url, "index")   ~ "index",
    str_detect(sitemap_url, "page")    ~ "page",
    str_detect(sitemap_url, "company") ~ "company"
  )

  if (type == "index") {

    table = sitemap_url |>
      read_xml() |>
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

  } else if (type == "page" | type == "company") {

    table = sitemap_url |>
      read_xml() |>
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

  } else {
    print("Bad input")
    return(NULL)
  }

  return(table)
}
