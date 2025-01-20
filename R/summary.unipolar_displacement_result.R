#' @export
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
