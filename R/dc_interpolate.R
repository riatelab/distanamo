#' dc_interpolate
#'
#' Interpolate a sf layer using the interpolation grid.
#' @param interpolation_grid The interpolation grid
#' @param layer_to_deform The sf layer to interpolate
#' @return The sf layer deformed by the interpolation grid
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
#' # Read image points
#' image_pts <- st_read(
#'   dsn = system.file("gpkg/data-prefecture.gpkg", package = "distanamo"),
#'   layer = "image-points", quiet = TRUE
#' )
#'
#' # Read the background layer to deform
#' background_layer <- st_read(
#'   dsn = system.file("gpkg/data-prefecture.gpkg", package = "distanamo"),
#'   layer = "departement", quiet = TRUE
#' )
#'
#' # Create the interpolation grid
#' igrid <- dc_create(
#'   source_points = source_pts,
#'   image_points = image_pts,
#'   precision = 2,
#'   bbox = st_bbox(background_layer)
#' )
#'
#' # Plot various depictions of the interpolation grid
#' plot(igrid)
#'
#' # Useful information about the interpolation grid
#' summary(igrid)
#'
#' # Deform the target layer
#' background_deformed <- dc_interpolate(
#'   interpolation_grid = igrid,
#'   layer_to_deform = background_layer
#' )
#'
#' plot(st_geometry(background_deformed))
#'
dc_interpolate <- function(
    interpolation_grid,
    layer_to_deform) {
  source_crs <- sf::st_crs(layer_to_deform)
  res <- .Call(
    savvy_InterpolationGrid_transform_layer__impl,
    interpolation_grid$.ptr,
    sf::st_as_binary(sf::st_geometry(layer_to_deform))
  )
  layer <- sf::st_sf(layer_to_deform)
  sf::st_geometry(layer) <- sf::st_as_sfc(res)
  return(sf::st_set_crs(layer, source_crs))
}
