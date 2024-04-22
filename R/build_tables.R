#' Build the info table from a group page
#' @param ginfo A character vector with group information in html format
#' @param country The country of the group
#' @param group_url The URL of the group page
#' @return A tibble with group information
build_info_tab <- function(ginfo, country, group_url) {
  tibble::tibble(
    tist_num = stringr::str_extract(ginfo, "(?<=TIST Number: )\\w+"),
    group_name = stringr::str_extract(ginfo, "(?<=Group Name: ).*?(?=\\n)"),
    cluster = stringr::str_extract(ginfo, "(?<=Cluster: ).*?(?=Village:)"),
    village = stringr::str_extract(ginfo, "(?<=Village: ).*?(?=Group Center:)"),
    group_center = stringr::str_extract(
      ginfo, "(?<=Group Center: ).*?(?=Group Number:)"
    ),
    group_num = as.integer(stringr::str_extract(
      ginfo, "(?<=Group Number: ).*?(?=Area:)"
    )),
    area = stringr::str_extract(ginfo, "(?<=Area: ).*?(?=\n\nDate of Audit:)"),
    audit_date = stringr::str_extract(
      ginfo, "(?<=Date of Audit: ).*?(?=Quantifiers:)"
    ),
    quantifiers = stringr::str_extract(ginfo, "(?<=Quantifiers: ).*")
  ) |>
    dplyr::mutate(
      audit_date = lubridate::dmy(audit_date),
      country = stringr::str_to_title(country),
      group_url = group_url
    ) |>
    dplyr::relocate(tist_num, country, area, cluster, village)
}

#' convert degrees lon lat to decimal
#' @param x A character vector with degrees, minutes, and seconds
#' @return A numeric vector with decimal degrees
process_coords <- function(x) {
  coord <- x |>
    stringr::str_replace_all("[A-Za-z]", "") |>
    stringr::str_trim() |>
    measurements::conv_unit(from = "deg_dec_min", to = "dec_deg") |>
    as.numeric()

  purrr::map2_dbl(
    coord,
    stringr::str_detect(x, "[WwSs]"),
    ~ if_else(.y, -.x, .x)
  )
}

#' Tidy up an html table from a group page on TIST
#' @param x A tibble created from an html table
#' @return A tibble with cleaned up column names and data
html_tab_tidy <- function(x) {
  suppressWarnings(x |>
    dplyr::select(dplyr::where(~ !all(is.na(.)))) |>
    setNames(x[1, ]) |>
    dplyr::slice(-1) |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ replace(., . %in% c("", "-"), NA)
    )) |>
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.))) |>
    janitor::clean_names() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c(
          "age", "average_circumference_cm",
          "age_years", "trees_present",
          "grove_area_in_ha", "trees",
          "number_present"
        )),
        as.numeric
      ),
      dplyr::across(
        dplyr::any_of("date"), lubridate::dmy
      ),
      dplyr::across(
        dplyr::any_of("name"),
        # function ro replace double space with singel space
        ~ stringr::str_replace_all(., "  ", " ")
      )
    ))
}

#' Build a tibble with grove information
#' @param ginfo A table with grove information
#' @param group_info A tibble with group information
#' @return A tibble with grove information
build_grove_tab <- function(ginfo, group_info) {
  # browser()
  df <- html_tab_tidy(ginfo)

  suppressWarnings({
    df <- df |>
      dplyr::mutate(
        longitude = process_coords(longitude),
        latitude = process_coords(latitude),
      ) |>
      sf::st_as_sf(
        coords = c("longitude", "latitude"),
        crs = 4326, na.fail = FALSE
      )
  })

  dplyr::cross_join(df, group_info) |>
    dplyr::relocate(colnames(df[0]), .after = dplyr::last_col()) |>
    dplyr::mutate(name = make.unique(name))
}

#' General function to clean a table and join it to the info
#' table from a group page
#' @param ginfo A table with general information
#' @param group_info A tibble with group information
build_tab_general <- function(ginfo, group_info) {
  html_tab_tidy(ginfo) |>
    dplyr::cross_join(group_info)
}

#' Get the tables from a group page
#' @param g_url The URL of the group page
#' @param country The country of the group
#' @return A list of tibbles with group information
#' @description The returned list comprises 4 tibbles: grove_tab, seed_det_tab,
#' tree_det_tab, and tree_circ_tab.
get_group_tables <- function(g_url, country) {
  x <- insis_read_html(g_url)

  elements <- x |>
    rvest::html_elements("table") |>
    rvest::html_elements("table") |>
    rvest::html_elements("table") |>
    rvest::html_elements("table")

  el_tabs <- elements |>
    rvest::html_table()

  el_chrs <- elements |>
    rvest::html_text2()

  info_tab <- build_info_tab(
    el_chrs[[2]],
    country = country,
    group_url = g_url
  )

  grove_tab <- build_grove_tab(el_tabs[[3]], group_info = info_tab)

  gu <- grove_poly_urls(elements[3]) |>
    dplyr::bind_rows() |>
    dplyr::mutate(name = make.unique(
      stringr::str_replace_all(name, "  ", " ")
    ))

  # browser()
  grove_tab_poly <- sf::st_drop_geometry(grove_tab) |>
    dplyr::left_join(gu, by = "name") |> # multiple = "first"
    sf::st_as_sf()

  seed_det_tab <- build_tab_general(el_tabs[[4]], group_info = info_tab)

  tree_det_tab <- build_tab_general(el_tabs[[6]], group_info = info_tab)

  tree_circ_tab <- build_tab_general(el_tabs[[7]], group_info = info_tab)

  return(list(grove_tab, grove_tab_poly, seed_det_tab, tree_det_tab, tree_circ_tab))
}

#' Get all the tables from a list of group URLs
#' @param group_urls_vec A character vector with group URLs
#' @return A list of tibbles with group information
#' @description The returned list comprises 4 tibbles: grove_tab, seed_det_tab,
#' tree_det_tab, and tree_circ_tab.
get_all_group_tabs <- function(group_urls_vec) {
  country <- attr(group_urls_vec, "country")
  df_list <- purrr::map(group_urls_vec, get_group_tables, country, .progress = TRUE) |>
    purrr::transpose() |>
    purrr::map(dplyr::bind_rows)
  attr(df_list, "country") <- country
  return(df_list)
}

#' Save the group tables to disk
#' @param group_tab_list A list of tibbles with group information from
#' `get_all_group_tabs()`
#' @param country The country of the group
#' @param parent_dir The parent directory to save the files to
#' @param grove_filename The filename for the grove table
#' @param seed_det_filename The filename for the seed detail table
#' @param tree_det_filename The filename for the tree detail table
#' @param tree_circ_filename The filename for the tree circumference table
#' @return A list with the file paths for the saved tables
save_group_tabs <- function(
    group_tab_list,
    country = attributes(group_tab_list)$country,
    parent_dir = "TIST-data",
    grove_filename = "{country}_grove_tab.gpkg",
    seed_det_filename = "{country}_seed_det_tab.csv",
    tree_det_filename = "{country}_tree_det_tab.csv",
    tree_circ_filename = "{country}_tree_circ_tab.csv") {
  if (!dir.exists(parent_dir)) {
    dir.create(parent_dir)
  }

  sf::write_sf(
    group_tab_list[[1]],
    file.path(parent_dir, glue::glue(grove_filename)),
    delete_dsn = TRUE
  )

  purrr::walk2(
    group_tab_list[2:4],
    c(seed_det_filename, tree_det_filename, tree_circ_filename),
    ~ vroom::vroom_write(
      .x,
      file.path(parent_dir, glue::glue(.y))
    )
  )
  return(list(
    grove = file.path(parent_dir, glue::glue(grove_filename)),
    seed_det = file.path(parent_dir, glue::glue(seed_det_filename)),
    tree_det = file.path(parent_dir, glue::glue(tree_det_filename)),
    tree_circ = file.path(parent_dir, glue::glue(tree_circ_filename))
  ))
}


#' function to retrieve the polygon of a grove from the TIST website
#' @param grove_url the url of the grove
#' @return a simple feature (sf) of the polygon
get_grove_polygon <- function(grove_url, grove_name) {
  map_html <- rvest::read_html(grove_url)
  script_node <- map_html |>
    rvest::html_elements(xpath = "/html/head/script[2]") |>
    rvest::html_text2() |>
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
  poly$name <- grove_name
  return(poly)
}

grove_poly_urls <- function(ns) {
  x <- ns |>
    rvest::html_elements("p") |>
    rvest::html_elements("a")

  links <- x |>
    rvest::html_attr("href")

  if (length(links) == 0) {
    return(
      tibble::tibble(name = character(), geometry = sf::st_sfc()) |>
        sf::st_as_sf() |>
        sf::st_set_crs(4326)
    )
  }

  paste_base_url(links) |>
    purrr::map(get_gmap_url) |>
    paste_base_url(sep = "bin/") |>
    purrr::set_names(rvest::html_text2(x)) |>
    purrr::imap(~ get_grove_polygon(.x, .y))
}


get_gmap_url <- function(img_link) {
  poly_img_page <- insis_read_html(img_link)

  poly_img_page |>
    # get the body
    rvest::html_elements("table") |>
    rvest::html_elements("span") |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")
}
