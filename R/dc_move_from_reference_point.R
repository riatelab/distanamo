#' dc_move_from_reference_point
#'
#' Move points from a reference point using durations between the reference point
#' and all the other points.
#' @param reference_point The point from which the other points will be moved,
#' an sf POINT object
#' @param other_points The other points to move, an sf POINT object
#' @param duration_col_name The name of the column containing the durations
#' in the other_points sf object
#' @param factor The factor of displacement (default: 1)
#' @return A list object with the source points and the image points, ready
#' to be used with the dc_create function
#' @export
#' @examples
#' library(sf)
#' start <- st_read(dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
#'                  layer = "start", quiet = TRUE)
#' points <-  st_read(dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
#'                    layer = "points", quiet = TRUE)
#' center <-  st_read(dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
#'                    layer = "center", quiet = TRUE)
#'
#' pos_result <- dc_move_from_reference_point(
#'   reference_point = start,
#'   other_points = points,
#'   duration_col_name = "durations",
#'   factor = 1
#' )
#'
#' plot(st_geometry(pos_result$source_points), pch = 4, cex = .5)
#' plot(st_geometry(pos_result$image_points), add = TRUE, pch = 4, cex = .5, col = "red")
#' plot(st_geometry(center), add = TRUE)
dc_move_from_reference_point <- function(reference_point, other_points, duration_col_name, factor) {
  if (missing(factor)) {
    factor <- 1
  }
  if (factor <= 0) {
    stop('Factor must be a non-null positive value')
  }
  if (length(sf::st_geometry(reference_point)) != 1) {
    stop('Reference point must be a single point')
  }
  if (length(sf::st_geometry(other_points)) == 0) {
    stop('No other points to move')
  }
  if (sf::st_crs(reference_point) != sf::st_crs(other_points)) {
    stop('The reference point and the other points must have the same CRS')
  }

  points <- sf::st_set_crs(
    sf::st_sf(geometry=c(sf::st_geometry(reference_point), sf::st_geometry(other_points))),
    sf::st_crs(reference_point)
  )

  durations <- c(0, other_points[[duration_col_name]])

  new_points <- .Call(
    savvy_move_points_from_durations__impl,
    sf::st_as_binary(sf::st_geometry(points)),
    durations,
    factor
  )

  source_crs <- sf::st_crs(points)

  layer <- sf::st_sf(points)
  sf::st_geometry(layer) <- sf::st_as_sfc(new_points)
  new_points <- sf::st_set_crs(layer, source_crs)
  li <- list(
    reference_point = reference_point,
    source_points = points,
    image_points = new_points
  )
  class(li) <- "distanamo_unipolar_displacement_result"

  return(li)
}
