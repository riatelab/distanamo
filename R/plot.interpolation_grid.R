#' @importFrom grDevices devAskNewPage
#' @importFrom graphics arrows legend
#' @export
plot.interpolation_grid <- function(
  x,
  which = 1:3,
  ask = interactive(),
  caption = list("Source grid", "Interpolated grid", "Image to interpolated points"),
  ...
) {
  if (!inherits(x, "interpolation_grid")) stop("Not a interpolation_grid object")
  if (!is.numeric(which) || any(which < 1) || any(which > 3)) stop("'which' must be in 1:3")

  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }

  plots <- list(
    source_grid = function () {
      plot(sf::st_geometry(x$source_grid), main = caption[1], col = "green")
    },
    interpolated_grid = function () {
      plot(sf::st_geometry(x$interpolated_grid), main = caption[2], col = "pink")
    },
    image_to_interpolated_points = function () {
      plot(sf::st_geometry(x$image_points), main = caption[3], col = "blue")
      plot(sf::st_geometry(x$interpolated_points), col = "red", add = TRUE)
      c_image <- sf::st_coordinates(x$image_points)
      c_interpolated <- sf::st_coordinates(x$interpolated_points)
      # We dont want to draw arrows if the point in c_image is the same
      # as the point in c_interpolated
      better_arrow(c_image, c_interpolated)
      legend("topleft", legend = c("Image points", "Interpolated points"),
             col = c("blue", "red"), pt.lwd = 1, bty = "n", cex = 0.8, pch = 1)
    }
  )

  plot_names <- c("source_grid", "interpolated_grid", "image_to_interpolated_points")

  for (i in which) plots[[plot_names[i]]]()
}
