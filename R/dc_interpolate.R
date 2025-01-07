#' dc_interpolate
#'
#' Interpolate a sf layer using the interpolation grid.
#' @param interpolation_grid The interpolation grid
#' @param layer_to_deform The sf layer to interpolate
#' @return The sf layer deformed by the interpolation grid
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
#' bbox <- dc_combine_bbox(list_layers = list(points, center))
#'
#' # Create the interpolation grid
#' igrid <- dc_create(
#'   source_points = pos_result$source_points,
#'   image_points = pos_result$image_points,
#'   precision = 2,
#'   bbox = bbox,
#' )
#'
#' # Deform the target layer
#' center_deform <- dc_interpolate(interpolation_grid = igrid,
#'                                 layer_to_deform = center)
#'
#' plot(st_geometry(igrid$interpolated_grid), col = NA)
#' plot(st_geometry(center_deform), add = TRUE)
dc_interpolate <- function (
  interpolation_grid,
  layer_to_deform
) {
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