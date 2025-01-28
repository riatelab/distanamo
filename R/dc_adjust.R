#' @title Adjusts image points to source points
#' @description Computes the positions of the adjusted points by fitting image points to source points.
#' @param source_points The source point layer, sf POINT object
#' @param image_points The layer of point to be adjusted to fit `source_points`,
#' sf POINT object
#' @param adjustment_type The adjustment type to use, either "euclidean" or "affine"
#' @return A list object with the transformation matrix, various metrics and
#' the adjusted points
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
#' # Read non adjusted image points
#' image_pts_not_adj <- st_read(
#'   dsn = system.file("gpkg/data-prefecture.gpkg", package = "distanamo"),
#'   layer = "image-points-not-adjusted", quiet = TRUE
#' )
#'
#' # Read the background layer to deform
#' background_layer <- st_read(
#'   dsn = system.file("gpkg/data-prefecture.gpkg", package = "distanamo"),
#'   layer = "departement", quiet = TRUE
#' )
#'
#' # Adjust image points to source points
#' adj_result <- dc_adjust(
#'   source_points = source_pts,
#'   image_points = image_pts_not_adj,
#'   "euclidean"
#' )
#'
#' # Use adjusted points to create the interpolation grid
#' igrid <- dc_create(
#'   source_points = source_pts,
#'   image_points = adj_result$image_points,
#'   precision = 2,
#'   bbox = st_bbox(background_layer)
#' )
#'
#' # Deform the target layer
#' background_deformed <- dc_interpolate(
#'   interpolation_grid = igrid,
#'   layer_to_deform = background_layer
#' )
#'
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
