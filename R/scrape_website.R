# Initialize ----
library(tidyverse) # Load the Tidyverse
library(xml2) # Parse XML
library(rvest) # Easily Harvest (Scrape) Web Pages
source("R/functions.R") # Custom Functions

# Website Map ----
# Base URL
url_base = "https://wvtourism.com"
url_sitemap = "/sitemap_index.xml"
url_page = "/"

sitemap_xml = read_xml(str_c(url_base, url_sitemap))
sitemap = extract_sitemap(sitemap_xml)

