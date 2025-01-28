#' plot adjustment_result object
#'
#' Plot the result of dc_adjust
#'
#' @param x object of class adjustment_result
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
#' # Plot result of the adjustment step
#' plot(adj_result)
#'
#' # Summary statistics of the adjustment step
#' summary(adj_result)
#'
plot.adjustment_result <- function(x, ...) {
  if (!inherits(x, "adjustment_result")) stop("Not a adjustment_result object")
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
