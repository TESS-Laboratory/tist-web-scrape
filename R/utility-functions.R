clean_up_spat <- function(x) {
  geom_type <- sf::st_geometry_type(x) |>
    table() |>
    which.max() |>
    names()

  x_filt <- filter(x, !sf::st_is_empty(x))

  switch(geom_type,
    "POINT" = sf::st_cast(x_filt, "POINT"),
    "POLYGON" = sf::st_cast(x_filt, "MULTIPOLYGON"),
    "MULTIPOLYGON" = sf::st_cast(x_filt, "MULTIPOLYGON"),
    cli::cli_abort("Something is off with the geometry type.")
  )
}
