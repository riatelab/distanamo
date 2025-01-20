#' dc_combine_bbox
#'
#' Takes a list of sf objects and compute the bounding box
#' that covers them all.
#' @param list_layers A list of sf objects
#' @return An sf bounding box is returned.
#' @export
#' @examples
#' library(sf)
#' start <- st_read(
#'   dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
#'   layer = "start", quiet = TRUE
#' )
#' points <- st_read(
#'   dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
#'   layer = "points", quiet = TRUE
#' )
#' center <- st_read(
#'   dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
#'   layer = "center", quiet = TRUE
#' )
#'
#' bbox <- dc_combine_bbox(list(start, points, center))
#' bbox
dc_combine_bbox <- function(list_layers) {
  combined_bbox <- Reduce(function(b1, b2) {
    c(
      xmin = min(b1["xmin"], b2["xmin"]),
      ymin = min(b1["ymin"], b2["ymin"]),
      xmax = max(b1["xmax"], b2["xmax"]),
      ymax = max(b1["ymax"], b2["ymax"])
    )
  }, lapply(list_layers, sf::st_bbox))

  combined_bbox <- sf::st_set_crs(
    sf::st_bbox(combined_bbox),
    sf::st_crs(list_layers[[1]])
  )
  return(combined_bbox)
}
