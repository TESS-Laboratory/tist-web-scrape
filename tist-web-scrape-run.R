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

options(warn = 2)

# --- read the functions that are in the R directory ---
purrr::walk(
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  source
)

# --- This is the pipeline for Uganda ---
# first get the url links for all projects in a country
ug_proj_urls <- project_urls("uganda")
# then get the links to all the clusters in each of those projects
ug_cluster_urls <- cluster_urls(ug_proj_urls)
# then the url links for the groups in each cluster
ug_group_urls <- group_urls(ug_cluster_urls)

## --- for testing specific issue: ---
# ug_group_urls_test <- ug_group_urls[1:100]
# # ug_group_urls_test <- "https://www.tist.org/i2//detgroup.php?groupid=16396"
# attr(ug_group_urls_test, "country") <- "uganda"
# ug_all_test <- get_all_group_tabs(ug_group_urls_test)
# ug_all_test[[2]]$geometry
# ply_test <- clean_up_spat(ug_all_test[[2]])
# pnt_test <- clean_up_spat(ug_all_test[[1]])

# mapview::mapview(ply_test) +
#   mapview::mapview(pnt_test)


# Scrape the data from each cluster
# (this takes a long time ~2hrs for Uganda)
ug_all <- get_all_group_tabs(ug_group_urls)
# Finally save those outputs.
ug_files <- save_group_tabs(ug_all)
