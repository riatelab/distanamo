#' dc_generate_positions_from_durations
#'
#' Generate positions from durations matrix, using PCoA to find the relative
#' positions between points, then perform a procrustes analysis to find the
#' best fit between the source points and the image points.
#' @param durations The durations matrix
#' @param source_points The source points, an sf POINT object
#' @param adjustment_type The adjustment type to use, either "euclidean" or "affine"
#' @return A list object with the source points and the image points, ready
#' to be used with the dc_create function
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
#' # Read durations matrix
#' durations_mat <- read.csv(system.file("csv/mat.csv", package = "distanamo"), row.names = 1)
#'
#' # Generate the positions from the whole duration matrix and
#' # adjust the result to the source points
#' pos_result <- dc_generate_positions_from_durations(
#'   durations = durations_mat,
#'   source_points = source_pts,
#'   adjustment_type = "euclidean"
#' )
#'
#' # Display and plot useful information about the positioning step
#' plot(pos_result)
#' summary(pos_result)
#'
#' # Use the result of the positioning to create the interpolation grid
#' igrid <- dc_create(
#'   source_points = pos_result$source_points,
#'   image_points = pos_result$image_points,
#'   precision = 2.0,
#'   bbox = st_bbox(background_layer)
#' )
#'
#' # Deform the background layer
#' background_deformed <- dc_interpolate(
#'   interpolation_grid = igrid,
#'   layer_to_deform = background_layer
#' )
#'
dc_generate_positions_from_durations <- function(durations, source_points, adjustment_type = "euclidean") {
  if (adjustment_type != "euclidean" && adjustment_type != "affine") {
    stop("The adjustment type must be either 'euclidean' or 'affine'")
  }
  adjustment_type <- ifelse(adjustment_type == "euclidean", 0, 1)
  li <- .Call(
    savvy_generate_positions_from_durations_matrix__impl,
    sf::st_as_binary(sf::st_geometry(source_points)),
    as.matrix(durations),
    adjustment_type
  )
  source_crs <- sf::st_crs(source_points)
  layer <- sf::st_sf(source_points)
  sf::st_geometry(layer) <- sf::st_as_sfc(li$image_points)
  li$source_points <- source_points
  li$image_points <- sf::st_set_crs(layer, source_crs)
  class(li) <- "multipolar_displacement_result"
  return(li)
}
