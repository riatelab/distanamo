#' summary multipolar_displacement_result object
#'
#' Compute summary statistics for multipolar_displacement_result
#'
#' @param object object of class multipolar_displacement_result
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
summary.multipolar_displacement_result <- function(object, ...) {
  if (!inherits(object, "multipolar_displacement_result")) stop("Not a multipolar_displacement_result object")
  dists <- sf::st_distance(object$source_points, object$image_points, by_element = TRUE)
  summary_obj <- list(
    error = object$error,
    min_displacement = min(dists),
    mean_displacement = mean(dists),
    max_displacement = max(dists)
  )
  cat("Summary of the multipolar displacement result:\n")
  cat("Min displacement:", summary_obj$min_displacement, paste0("[", units::deparse_unit(summary_obj$min_displacement), "]"), "\n")
  cat("Mean displacement:", summary_obj$mean_displacement, paste0("[", units::deparse_unit(summary_obj$mean_displacement), "]"), "\n")
  cat("Max displacement:", summary_obj$max_displacement, paste0("[", units::deparse_unit(summary_obj$max_displacement), "]"), "\n")
  cat("Transformation matrix:\n")
  cat("    ", object$a11, object$a12, object$a13, "\n    ", object$a21, object$a22, object$a23, "\n")
  cat("Scale:", object$scale, "\n")
  cat("RMSE:", object$rmse, "\n")
  cat("RMSE x:", object$rmse_x, "\n")
  cat("RMSE y:", object$rmse_y, "\n")
  return(invisible(summary_obj))
}
