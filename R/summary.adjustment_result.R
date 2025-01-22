#' @export
summary.adjustment_result <- function(object, ...) {
  if (!inherits(object, "adjustment_result")) stop("Not a adjustment_result object")
  dists <- sf::st_distance(object$source_points, object$image_points, by_element = TRUE)
  summary_obj <- list(
    error = object$error,
    min_displacement = min(dists),
    mean_displacement = mean(dists),
    max_displacement = max(dists)
  )
  cat("Summary of the adjustment:\n")
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
