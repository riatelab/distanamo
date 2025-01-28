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
#' durations_mat <- read.csv(system.file("csv/mat.csv", package = "distanamo"), row.names = 1)
#' dur <- durations_mat["CAEN", ]
#'
#' source_pts$durations <- as.double(dur)
#'
#' ref_point <- subset(source_pts, source_pts$NOM_COM == "CAEN")
#' other_points <- subset(source_pts, !source_pts$NOM_COM == "CAEN")
#'
#' # Generate position from durations between the reference point
#' # and the other points
#' positioning_result <- dc_move_from_reference_point(
#'   reference_point = ref_point,
#'   other_points = other_points,
#'   duration_col_name = "durations",
#'   factor = 1
#' )
#'
#' # Display and plot useful information about the positioning step
#' plot(positioning_result)
#' summary(positioning_result)
#'
#' # Create the interpolation grid
#' igrid <- dc_create(
#'   source_points = positioning_result$source_points,
#'   image_points = positioning_result$image_points,
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
dc_move_from_reference_point <- function(reference_point, other_points, duration_col_name, factor) {
  if (missing(factor)) {
    factor <- 1
  }
  if (factor <= 0) {
    stop("Factor must be a non-null positive value")
  }
  if (length(sf::st_geometry(reference_point)) != 1) {
    stop("Reference point must be a single point")
  }
  if (length(sf::st_geometry(other_points)) == 0) {
    stop("No other points to move")
  }
  if (sf::st_crs(reference_point) != sf::st_crs(other_points)) {
    stop("The reference point and the other points must have the same CRS")
  }

  points <- sf::st_set_crs(
    sf::st_sf(geometry = c(sf::st_geometry(reference_point), sf::st_geometry(other_points))),
    sf::st_crs(reference_point)
  )

  durations <- c(0, other_points[[duration_col_name]])

  pos_res <- .Call(
    savvy_move_points_from_durations__impl,
    sf::st_as_binary(sf::st_geometry(points)),
    durations,
    factor
  )

  source_crs <- sf::st_crs(points)

  layer <- sf::st_sf(points)
  sf::st_geometry(layer) <- sf::st_as_sfc(pos_res$image_points)
  pos_res$image_points <- sf::st_set_crs(layer, source_crs)
  li <- list(
    reference_point = reference_point,
    source_points = points,
    image_points = pos_res$image_points,
    reference_speed = pos_res$reference_speed
  )
  class(li) <- "unipolar_displacement_result"

  return(li)
}
