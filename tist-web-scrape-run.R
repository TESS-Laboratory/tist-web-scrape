# This is the main script to run the web-scraping of TIST data.

# --- libraries required to run this web-scraping pipeline ---
library(rvest)
library(dplyr)
library(tibble)
library(stringr)
library(tibble)
library(purrr)
library(janitor)
library(measurements)
library(glue)
library(sf)

# options(warn = 0)

# --- read the functions that are in the R directory ---
purrr::walk(
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  source
)

# --- This is the pipeline for Uganda ---
# first get the url links for all projects in the specified country
ug_proj_urls <- project_urls("uganda")
# then get the links to all the clusters in each of those projects
ug_cluster_urls <- cluster_urls(ug_proj_urls)
# then the url links for the groups in each cluster
ug_group_urls <- group_urls(ug_cluster_urls)

# Scrape the data from each cluster
# (this takes a long time ~2hrs for Uganda)
ug_all <- get_all_group_tabs(ug_group_urls)
# Finally save those outputs.
ug_files <- save_group_tabs(ug_all)

# view polygons and points on a map.
# we must clean the spatial data to view on a leaflet map otherwise things crash.
ug_polys <- clean_up_spat(read_sf("TIST-data/uganda_grove_polys.gpkg"))
ug_points <- clean_up_spat(read_sf("TIST-data/uganda_grove_points.gpkg"))

mapview::mapview(ug_polys) +
  mapview::mapview(ug_points)
