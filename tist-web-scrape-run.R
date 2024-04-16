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

# --- read the functions that are in the R directory ---
source("R/tist-web-scrape-functions.R")
source("R/build_tables.R")

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
