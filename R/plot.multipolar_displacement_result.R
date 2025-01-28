#' plot multipolar_displacement_result object
#'
#' Plot the result of dc_generate_positions_from_durations
#'
#' @param x object of class multipolar_displacement_result
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
#' # Plot result of the positioning step
#' plot(pos_result)
#'
#' # Summary statistics of the positioning step
#' summary(pos_result)
#'
plot.multipolar_displacement_result <- function(x, ...) {
  if (!inherits(x, "multipolar_displacement_result")) stop("Not a multipolar_displacement_result object")
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
