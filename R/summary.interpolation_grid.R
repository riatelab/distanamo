#' @export
summary.distanamo_interpolation_grid <- function(object, ...) {
  if (!inherits(object, "distanamo_interpolation_grid")) stop("Not a distanamo_interpolation_grid object")
  summary_obj <- list(
    deformation_strength = .Call(savvy_InterpolationGrid_deformation_strength__impl, object$.ptr),
    mae = .Call(savvy_InterpolationGrid_mae__impl, object$.ptr),
    n_cells = length(sf::st_geometry(object$source_grid)),
    precision = object$precision,
    resolution = object$resolution,
    rmse = .Call(savvy_InterpolationGrid_rmse__impl, object$.ptr),
    r_squared = .Call(savvy_InterpolationGrid_r_squared__impl, object$.ptr)
  )
  cat("Summary of the interpolation grid:\n")
  cat("Number of cells:", summary_obj$n_cells, "\n")
  cat("Precision:", summary_obj$resolution, paste0("(\u03b1 = ", summary_obj$precision, ")"), "\n")
  cat("Deformation strength:", summary_obj$deformation_strength,"\n")
  cat("Mean absolute error:", summary_obj$mae, "\n")
  cat("Root mean squared error:", summary_obj$rmse, "\n")
  cat("R squared:", summary_obj$r_squared, "\n")
  return(invisible(summary_obj))
}
