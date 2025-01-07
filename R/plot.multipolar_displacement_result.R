#' @export
plot.distanamo_multipolar_displacement_result <- function(x, ...) {
  if (!inherits(x, "distanamo_multipolar_displacement_result")) stop("Not a distanamo_multipolar_displacement_result object")
  plot(sf::st_geometry(x$source_points), col = "blue", main = "Source points to image points displacement")
  plot(sf::st_geometry(x$image_points), col = "red", add = TRUE)
  c_source <- sf::st_coordinates(x$source_points)
  c_image <- sf::st_coordinates(x$image_points)
  identical_pts <-identical(c_source, c_image)
  cc_source <- c_source[!identical_pts,]
  cc_image <- c_image[!identical_pts,]
  arrows(
    x0 = cc_source[,1],
    y0 = cc_source[,2],
    x1 = cc_image[,1],
    y1 = cc_image[,2],
    length = 0.075
  )
  legend("topleft", legend = c("Source points", "Image points"),
         col = c("blue", "red"), pt.lwd = 1, bty = "n", cex = 0.8, pch = 1)
}
