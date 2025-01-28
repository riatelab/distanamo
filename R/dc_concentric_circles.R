#' @title Create concentric circles around the reference point
#' @description Create concentric circles around the reference point, using the results of the
#' `dc_move_from_reference_point`.
#' @param positioning_result The object returned by the `dc_move_from_reference_point`
#' function.
#' @param steps The steps (in the unit of the durations used ine the
#' `dc_move_from_reference_point` function) at which creating the circles.
#' @return A sf LINESTRING object
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
#' # Create the interpolation grid
#' igrid <- dc_create(
#'   source_points = positioning_result$source_points,
#'   image_points = positioning_result$image_points,
#'   precision = 2.0,
#'   bbox = st_bbox(background_layer)
#' )
#'
#' # Use the positioning result to create concentric circles
#' # every 60-minutes
#' circles <- dc_concentric_circles(
#'   positioning_result,
#'   c(60, 120, 180, 240, 300, 360, 420, 480, 540, 600, 660, 720)
#' )
#'
#' # Deform the background layer
#' background_deformed <- dc_interpolate(
#'   interpolation_grid = igrid,
#'   layer_to_deform = background_layer
#' )
#'
dc_concentric_circles <- function(
    positioning_result,
    steps = list(30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360)) {
  res <- lapply(
    steps,
    function(s) {
      geom <- sf::st_boundary(sf::st_buffer(positioning_result$reference_point, positioning_result$reference_speed * s))$geom
      return(sf::st_sf(geometry = geom, step = s))
    }
  )
  combined_res <- do.call(rbind, res)
  return(combined_res)
}
