#' dc_adjust
#'
#' @export
dc_adjust <- function(
    source_points,
    image_points,
    adjustment_type = "euclidean") {
  if (adjustment_type != "euclidean" && adjustment_type != "affine") {
    stop("The adjustment type must be either 'euclidean' or 'affine'")
  }
  adjustment_type <- ifelse(adjustment_type == "euclidean", 0, 1)
  li <- .Call(
    savvy_adjust__impl,
    sf::st_as_binary(sf::st_geometry(source_points)),
    sf::st_as_binary(sf::st_geometry(image_points)),
    adjustment_type
  )
  source_crs <- sf::st_crs(source_points)
  layer <- sf::st_sf(source_points)
  sf::st_geometry(layer) <- sf::st_as_sfc(li$image_points)
  li$source_points <- source_points
  li$image_points <- sf::st_set_crs(layer, source_crs)
  class(li) <- "adjustment_result"
  return(li)
}
