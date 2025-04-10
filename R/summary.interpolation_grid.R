#' summary interpolation_grid object
#'
#' Compute summary statistics for interpolation_grid
#'
#' @param object object of class interpolation_grid
#' @param ... further specifications, see \link{summary} for details
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
summary.interpolation_grid <- function(object, ...) {
  if (!inherits(object, "interpolation_grid")) stop("Not a interpolation_grid object")

  rmse_interp_image <- .Call(savvy_InterpolationGrid_rmse_interp_image__impl, object$.ptr)
  rmse_interp_source <- .Call(savvy_InterpolationGrid_rmse_interp_source__impl, object$.ptr)

  summary_obj <- list(
    deformation_strength = .Call(savvy_InterpolationGrid_deformation_strength__impl, object$.ptr),
    mae = .Call(savvy_InterpolationGrid_mae__impl, object$.ptr),
    n_cells = length(sf::st_geometry(object$source_grid)),
    precision = object$precision,
    resolution = object$resolution,
    rmse_interp_image = rmse_interp_image[[1]],
    rmse_x_interp_image = rmse_interp_image[[2]],
    rmse_y_interp_image = rmse_interp_image[[3]],
    rmse_interp_source = rmse_interp_source[[1]],
    rmse_x_interp_source = rmse_interp_source[[2]],
    rmse_y_interp_source = rmse_interp_source[[3]],
    r_squared = .Call(savvy_InterpolationGrid_r_squared__impl, object$.ptr)
  )
  cat("Summary of the interpolation grid:\n")
  cat("Number of cells:", summary_obj$n_cells, "\n")
  cat("Precision:", summary_obj$resolution, paste0("(\u03b1 = ", summary_obj$precision, ")"), "\n")
  cat("Deformation strength:", summary_obj$deformation_strength, "\n")
  cat("Mean absolute error:", summary_obj$mae, "\n")
  cat("RMSE (interp - image):", summary_obj$rmse_interp_image, "\n")
  cat("RMSE x (interp - image):", summary_obj$rmse_x_interp_image, "\n")
  cat("RMSE y (interp - image):", summary_obj$rmse_y_interp_image, "\n")
  cat("RMSE (interp - source):", summary_obj$rmse_interp_source, "\n")
  cat("RMSE x (interp - source):", summary_obj$rmse_x_interp_source, "\n")
  cat("RMSE y (interp - source):", summary_obj$rmse_y_interp_source, "\n")
  cat("R squared:", summary_obj$r_squared, "\n")
  return(invisible(summary_obj))
}
