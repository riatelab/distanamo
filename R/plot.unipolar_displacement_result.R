#' plot unipolar_displacement_result object
#'
#' Plot the result of dc_move_from_reference_point
#'
#' @param x object of class unipolar_displacement_result
#' @param ... further specifications, see \link{plot} for details
#' @importFrom graphics legend
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
#' # Plot result of the positioning step
#' plot(positioning_result)
#'
#' # Summary statistics of the positioning step
#' summary(positioning_result)
#'
plot.unipolar_displacement_result <- function(x, ...) {
  if (!inherits(x, "unipolar_displacement_result")) stop("Not a unipolar_displacement_result object")
  plot(sf::st_geometry(x$source_points), col = "blue", main = "Source points to image points displacement")
  plot(sf::st_geometry(x$image_points), col = "red", add = TRUE)
  c_source <- sf::st_coordinates(x$source_points)
  c_image <- sf::st_coordinates(x$image_points)
  better_arrow(c_source, c_image)
  legend("topleft",
    legend = c("Source points", "Image points"),
    col = c("blue", "red"), pt.lwd = 1, bty = "n", cex = 0.8, pch = 1
  )
}
