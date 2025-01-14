#' @export
plot.multipolar_displacement_result <- function(x, ...) {
  if (!inherits(x, "multipolar_displacement_result")) stop("Not a multipolar_displacement_result object")
  plot(sf::st_geometry(x$source_points), col = "blue", main = "Source points to image points displacement")
  plot(sf::st_geometry(x$image_points), col = "red", add = TRUE)
  c_source <- sf::st_coordinates(x$source_points)
  c_image <- sf::st_coordinates(x$image_points)
  better_arrow(c_source, c_image)
  legend("topleft", legend = c("Source points", "Image points"),
         col = c("blue", "red"), pt.lwd = 1, bty = "n", cex = 0.8, pch = 1)
}
