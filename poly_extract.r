library(rvest)

#' function to retrieve the polygon of a grove from the TIST website
#' @param grove_url the url of the grove
#' @return a simple feature (sf) of the polygon
get_grove_polygon <- function(grove_url) {
  map_html <- rvest::read_html(grove_url)
  script_node <- map_html |>
    rvest::html_elements(xpath = "/html/head/script[2]") |>
    html_text2() |>
    # with stringr get text after the pattern "polyline.getPath();"
    stringr::str_extract("(?<=polyline.getPath\\(\\);)(.*)") |>
    # now extract vlues contained between the LatLng and ;
    stringr::str_extract_all("(?<=LatLng\\()(.*?)(?=;)") |>
    unlist() |>
    stringr::str_replace_all("\\)", "")

  coords <- script_node |>
    stringr::str_split(",") |>
    unlist() |>
    as.numeric() |>
    matrix(ncol = 2, byrow = TRUE)


  poly <- wk::wk_handle(
    wk::xy(x = coords[, 2], y = coords[, 1]),
    wk::wk_polygon_filter(
      wk::sfc_writer()
    )
  ) |>
    sf::st_as_sf(crs = "EPSG:4326") |>
    sf::st_make_valid()

  sf::st_geometry(poly) <- "geometry"
  return(poly)
}

p <- get_grove_polygon("https://www.tist.org/i2/bin/tractplotgoogle.php?locationid=407")

mapview::mapview(p)
