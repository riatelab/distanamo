#' plot interpolation_grid object
#'
#' Plot the interpolation grid, resulting from dc_create
#'
#' @param x object of class interpolation_grid
#' @param which which plot to display, a subset of 1:4 (the default)
#' @param ask logical; if TRUE, the user is asked before each plot
#' @param caption captions to appear above the plots; character vector
#' or list of valid graphics annotations
#' @param ... further specifications, see \link{plot} for details
#' @importFrom grDevices devAskNewPage
#' @importFrom graphics arrows legend
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
#' # Read image points
#' image_pts <- st_read(
#'   dsn = system.file("gpkg/data-prefecture.gpkg", package = "distanamo"),
#'   layer = "image-points", quiet = TRUE
#' )
#'
#' # Read the background layer to deform
#' background_layer <- st_read(
#'   dsn = system.file("gpkg/data-prefecture.gpkg", package = "distanamo"),
#'   layer = "departement", quiet = TRUE
#' )
#'
#' # Create the interpolation grid
#' igrid <- dc_create(
#'   source_points = source_pts,
#'   image_points = image_pts,
#'   precision = 2,
#'   bbox = st_bbox(background_layer)
#' )
#'
#' # Plot various depictions of the interpolation grid
#' plot(igrid)
#'
#' # Useful information about the interpolation grid
#' summary(igrid)
#'
plot.interpolation_grid <- function(
    x,
    which = 1:4,
    ask = interactive(),
    caption = list("Source grid", "Interpolated grid", "Image to interpolated points", "Deformation strength"),
    ...) {
  if (!inherits(x, "interpolation_grid")) stop("Not a interpolation_grid object")
  if (!is.numeric(which) || any(which < 1) || any(which > 4)) stop("'which' must be in 1:4")

  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }

  plots <- list(
    source_grid = function() {
      plot(sf::st_geometry(x$source_grid), main = caption[1], col = "green")
    },
    interpolated_grid = function() {
      plot(sf::st_geometry(x$interpolated_grid), main = caption[2], col = "pink")
    },
    image_to_interpolated_points = function() {
      plot(sf::st_geometry(x$image_points), main = caption[3], col = "blue")
      plot(sf::st_geometry(x$interpolated_points), col = "red", add = TRUE)
      c_image <- sf::st_coordinates(x$image_points)
      c_interpolated <- sf::st_coordinates(x$interpolated_points)
      # We dont want to draw arrows if the point in c_image is the same
      # as the point in c_interpolated
      better_arrow(c_image, c_interpolated)
      legend("topleft",
        legend = c("Image points", "Interpolated points"),
        col = c("blue", "red"), pt.lwd = 1, bty = "n", cex = 0.8, pch = 1
      )
    },
    deformation_strength = function() {
      mat <- .Call(savvy_InterpolationGrid_get_deformation_data__impl, x$.ptr)
      raster <- terra::rast(t(mat))
      terra::ext(raster) <- terra::ext(x$bbox[1], x$bbox[3], x$bbox[2], x$bbox[4])
      t_raster <- terra::rast(nrows = nrow(mat) * 10, ncols = ncol(mat) * 10, extent = terra::ext(raster))
      raster_resampled <- terra::resample(raster, t_raster, method = "cubicspline")
      pal <- get_continuous_pal(
        c(min(mat), 1, max(mat)),
        c("#4575b4", "#ffffbf", "#a50026")
      )
      terra::plot(raster_resampled, main = caption[4], col = pal)
    }
  )

  plot_names <- c("source_grid", "interpolated_grid", "image_to_interpolated_points", "deformation_strength")

  for (i in which) plots[[plot_names[i]]]()
}
