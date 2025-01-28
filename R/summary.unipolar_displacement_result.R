#' summary unipolar_displacement_result object
#'
#' Compute summary statistics for unipolar_displacement_result
#'
#' @param object object of class unipolar_displacement_result
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
summary.unipolar_displacement_result <- function(object, ...) {
  if (!inherits(object, "unipolar_displacement_result")) stop("Not a unipolar_displacement_result object")
  # We don't take into account the first point as it is the reference point and
  # it was not moved
  dists <- sf::st_distance(object$source_points[-1, ], object$image_points[-1, ], by_element = TRUE)
  summary_obj <- list(
    min_displacement = min(dists),
    mean_displacement = mean(dists),
    max_displacement = max(dists)
  )
  cat("Summary of the unipolar displacement result:\n")
  cat("Min displacement:", summary_obj$min_displacement, paste0("[", units::deparse_unit(summary_obj$min_displacement), "]"), "\n")
  cat("Mean displacement:", summary_obj$mean_displacement, paste0("[", units::deparse_unit(summary_obj$mean_displacement), "]"), "\n")
  cat("Max displacement:", summary_obj$max_displacement, paste0("[", units::deparse_unit(summary_obj$max_displacement), "]"), "\n")
  return(invisible(summary_obj))
}
