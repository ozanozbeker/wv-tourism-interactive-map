# Initialize ----
library(tidyverse) # Load the Tidyverse
library(xml2) # Parse XML
library(rvest) # Easily Harvest (Scrape) Web Pages
source("R/functions.R") # Custom Functions

# Website Map ----
# URLs
url_page = "https://wvtourism.com/page-sitemap.xml"
url_company = c(
  "https://wvtourism.com/company-sitemap.xml",
  "https://wvtourism.com/company-sitemap2.xml",
  "https://wvtourism.com/company-sitemap3.xml")

directories = extract_sitemap_table(url_page)
companies = map(url_company, extract_sitemap_table) |> list_rbind()

# Places to Go ----
places_to_go = directories |> filter(str_detect(url, "places-to-go"))
