clean_up_spat <- function(x) {
  filter(x, !sf::st_is_empty(x))
}
