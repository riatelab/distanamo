#' dc_interpolate_parallel
#'
#' Interpolate a list of sf layers using the interpolation grid.
#' @param interpolation_grid The interpolation grid
#' @param layers_to_deform A list of sf layers to interpolate
#' @return The sf layers deformed by the interpolation grid
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
#' background_deformed <- dc_interpolate_parallel(
#'   interpolation_grid = igrid,
#'   layers_to_deform = list(background_layer = background_layer, source_pts = source_pts)
#' )
#'
dc_interpolate_parallel <- function(
    interpolation_grid,
    layers_to_deform) {
  l <- lapply(layers_to_deform, function(layer) sf::st_as_binary(sf::st_geometry(layer)))
  res <- .Call(
    savvy_InterpolationGrid_transform_layers_parallel__impl,
    interpolation_grid$.ptr,
    l
  )
  for (i in seq_along(layers_to_deform)) {
    layer <- layers_to_deform[[i]]
    source_crs <- sf::st_crs(layer)
    sf::st_geometry(layer) <- sf::st_as_sfc(res[[i]])
    l[[i]] <- sf::st_set_crs(layer, source_crs)
  }
  return(l)
}
