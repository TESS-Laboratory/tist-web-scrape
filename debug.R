library(rvest)



ug_group_urls[1]
copuntry <- "uganda"

nodeset <- insis_read_html(ug_group_urls[1])

x <- nodeset |>
  rvest::html_elements("table") |>
  rvest::html_elements("table") |>
  rvest::html_elements("table") |>
  rvest::html_elements("table")



gu <- grove_poly_urls(x[3]) |>
  dplyr::bind_rows()

gu

x <- get_grove_polygon("https://www.tist.org/i2//bin/tractplotgoogle.php?locationid=144568", "lala")

plot(x)
mapview:mapview(x)
