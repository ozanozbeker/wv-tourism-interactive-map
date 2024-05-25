# Import ----
extract_sitemap_table = function(xml) {
  # There might be a cleaner way of doing this with the
  # actual xml2 functions but {tidyr} go burrrr.
  xml |>
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
