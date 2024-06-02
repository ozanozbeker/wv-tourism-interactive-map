# WV Tourism Map

This projects aims to showcase the culmination of my R skills across different techniques (data wrangling, web scraping, database design, dashboarding, etc.) by creating an interactive map of my home state, West Virginia. I will populate most of the information from the official tourism website, <https://wvtourism.com/>. The end goal is to create an interactive dashboard that anybody can use to plan their ultimate WV vacation.

This repo will consist most of development updates and I will be creating a blog post that dives deeper into the creation process.

# Process Flow

For this project, I'm going to be *loosely* following the data science workflow introduced in [R for Data Science (2e)](https://r4ds.hadley.nz/whole-game#fig-ds-whole-game).

## Import

Most of the data will come scraping the WV Tourism website, with the help of {rvest} and maybe {httr2}. I'm going to be creating a website map by exploration, but the goal is to scrape the content with code so that if there are updates to the website in the future, the map can be updated systematically.

I will need to get geographical data, which I have not explored as of this writing.

## Tidy

The website is categorized pretty well, and I will use its structure for my data. In smaller projects, I like to just save RDS files as they contain all the information of the R object, but I would like this source code to be as accessible as possible, so the final product might save CSVs or a different common format. As most of this is text data, I'm not sure if much tidying will be involved outside of data formatting.

## Transform/Model

I'm not sure how much Transformation and/or Modeling will be involved in this project, but this is still WIP.

## Visualize

I will be using {ggplot2} as the basis of exploration and testing, but I will need to use other packages in conjunction or in-place of because the end goal is to have an interactive map with filters, selections, etc.

## Communicate

The interactivity of the map will be powered with Tableau Public (as of now).
