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
    grove_point_filename = "{country}_grove_points.gpkg",
    grove_poly_filename = "{country}_grove_polys.gpkg",
    seed_det_filename = "{country}_seed_det_tab.csv",
    tree_det_filename = "{country}_tree_det_tab.csv",
    tree_circ_filename = "{country}_tree_circ_tab.csv") {
  if (!dir.exists(parent_dir)) {
    dir.create(parent_dir)
  }

  gpkg_writer <- function(x, y) {
    sf::st_write(
      x,
      file.path(parent_dir, glue::glue(y)),
      delete_dsn = TRUE
    )
  }

  gpkg_writer(group_tab_list[[1]], grove_point_filename)
  gpkg_writer(group_tab_list[[2]], grove_poly_filename)

  purrr::walk2(
    group_tab_list[3:5],
    c(seed_det_filename, tree_det_filename, tree_circ_filename),
    ~ vroom::vroom_write(
      .x,
      file.path(parent_dir, glue::glue(.y))
    )
  )
  return(list(
    grove_points = file.path(parent_dir, glue::glue(grove_point_filename)),
    grove_polys = file.path(parent_dir, glue::glue(grove_poly_filename)),
    seed_det = file.path(parent_dir, glue::glue(seed_det_filename)),
    tree_det = file.path(parent_dir, glue::glue(tree_det_filename)),
    tree_circ = file.path(parent_dir, glue::glue(tree_circ_filename))
  ))
}
