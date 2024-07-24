get_request = function(url) {
  url |>
    request() |>
    req_method("GET") |>
    req_throttle(rate = 2) |> # 30 requests per minute
    req_user_agent("Ozan Ozbeker (https://github.com/ozanozbeker/wv-tourism-interactive-map)")
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
