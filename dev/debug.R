## --- for testing specific issue: ---

library(targets)
tar_manifest()

tar_read(get_group_urls_uganda)

tar_make(combined_group_tabs_tanzania)

tar_renv(extras = c(
  "bslib", "crew", "gt", "markdown", "pingr", "rstudioapi", "shiny",
  "shinybusy", "shinyWidgets", "visNetwork", "tibble", "httpgd",
  "languageserver"
))


renv::init()
renv::snapshot()
renv::restore()


tar_make(combined_group_tabs_tanzania)

tar_source()

tz <- tar_read(get_group_urls_tanzania)[1:10]
attr(tz, "country") <- "tanzania"

get_group_tables(tz, "tanzania")

usethis::use_git_ignore(c("targets-run.log"))

gitcreds::gitcreds_set()
