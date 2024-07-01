# The targets script to run the TIST web scraping pipeline.
# to run this pipeline use any of:
# `targets::tar_make()`, source("targets-run.R"), or `make`.
# `make` is preferred as this method will run the pipeline in the
# background and therefore can be left to run after disconnecting from ssh.

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.
library(tibble)
# Set target options:
tar_option_set(
  # Packages for the pipeline
  packages = c(
    "tibble", "rvest", "dplyr", "tibble", "stringr", "purrr",
    "janitor", "measurements", "glue", "sf", "cli"
  ),
  format = "qs", # use qs
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()


values <- tibble(
  countries = c("uganda", "kenya", "tanzania", "india")
)


# Replace the target list below with your own:
list(
  tar_map(
    values,
    tar_target(
      get_proj_urls,
      project_urls(countries)
    ),
    tar_target(
      get_cluster_urls,
      cluster_urls(get_proj_urls)
    ),
    tar_target(
      get_group_urls,
      group_urls(get_cluster_urls)
    ),
    tar_target(
      group_tabs,
      get_group_tables(get_group_urls, countries),
      pattern = map(get_group_urls),
      iteration = "list"
    ),
    tar_target(
      combined_group_tabs,
      collect_group_tabs(group_tabs, countries)
    ),
    tar_target(
      save_tabs,
      save_group_tabs(combined_group_tabs, countries)
    )
  )
)
