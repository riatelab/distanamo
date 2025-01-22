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
  return(invisible(summary_obj))
}
