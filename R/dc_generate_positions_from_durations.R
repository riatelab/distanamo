#' dc_generate_positions_from_durations
#'
#' Generate positions from durations matrix, using PCoA to find the relative
#' positions between points, then perform a procrustes analysis to find the
#' best fit between the source points and the image points.
#' @param durations The durations matrix
#' @param source_points The source points, an sf POINT object
#' @return A list object with the source points and the image points, ready
#' to be used with the dc_create function
#' @export
dc_generate_positions_from_durations <- function(durations, source_points) {
  li <- .Call(
    savvy_generate_positions_from_durations_matrix__impl,
    sf::st_as_binary(sf::st_geometry(source_points)),
    as.matrix(durations)
  )
  source_crs <- sf::st_crs(source_points)
  layer <- sf::st_sf(source_points)
  sf::st_geometry(layer) <- sf::st_as_sfc(li$image_points)
  li$source_points <- source_points
  li$image_points <- sf::st_set_crs(layer, source_crs)
  class(li) <- "distanamo_multipolar_displacement_result"
  return(li)
}
