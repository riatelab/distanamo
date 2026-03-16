#' @title Compute the bounding box that covers all the layers
#' @description Takes a list of sf objects and compute the bounding box
#' that covers them all.
#' @param list_layers A list of sf objects
#' @return An sf bounding box is returned.
#' @export
#' @examples
#' library(sf)
#'
#' # Read source points
#' source_pts <- st_read(
#'   dsn = system.file("gpkg/data-prefecture.gpkg", package = "distanamo"),
#'   layer = "prefecture", quiet = TRUE
#' )
#'
#' # Read the background layer to deform
#' background_layer <- st_read(
#'   dsn = system.file("gpkg/data-prefecture.gpkg", package = "distanamo"),
#'   layer = "departement", quiet = TRUE
#' )
#'
#' bbox <- dc_combine_bbox(list(source_pts, background_layer))
#' bbox
dc_combine_bbox <- function(list_layers) {
  combined_bbox <- Reduce(function(b1, b2) {
    c(
      xmin = min(b1["xmin"], b2["xmin"], na.rm = TRUE),
      ymin = min(b1["ymin"], b2["ymin"], na.rm = TRUE),
      xmax = max(b1["xmax"], b2["xmax"], na.rm = TRUE),
      ymax = max(b1["ymax"], b2["ymax"], na.rm = TRUE)
    )
  }, lapply(list_layers, sf::st_bbox))

  combined_bbox <- sf::st_set_crs(
    sf::st_bbox(combined_bbox),
    sf::st_crs(list_layers[[1]])
  )
  return(combined_bbox)
}
