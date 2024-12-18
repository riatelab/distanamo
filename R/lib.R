#' @useDynLib distanamo, .registration = TRUE
#' @keywords internal
NULL

#' @export
`$<-.distanamo_interpolation_grid` <- function(x, name, value) {
  stop("distanamo_interpolation_grid cannot be modified", call. = FALSE)
}

#' @export
`[[<-.distanamo_interpolation_grid` <- function(x, i, value) {
  stop("distanamo_interpolation_grid cannot be modified", call. = FALSE)
}

#' @export
summary.distanamo_interpolation_grid <- function(object, ...) {
  summary_obj <- list(
    n_cells = length(sf::st_geometry(object$source_grid)),
    deformation_strength = .Call(savvy_InterpolationGrid_deformation_strength__impl, object$.ptr),
    precision = object$precision,
    resolution = object$resolution
  )
  cat("Summary of the interpolation grid:\n")
  cat("Number of cells:", summary_obj$n_cells, "\n")
  cat("Precision:", summary_obj$resolution, paste0("(\u03b1 = ", summary_obj$precision, ")"), "\n")
  cat("Deformation strength:", summary_obj$deformation_strength,"\n")
  return(invisible(summary_obj))
}

#' dc_create
#'
#' Create a new interpolation grid which covers the source points and with a cell size
#' deduced from the precision.
#'
#' The grid is then interpolated to match the
#' image points. This then allows one to interpolate any point on the grid
#' (enabling the deformation of geometries such as background layers)
#' and to retrieve useful metrics about the deformation.
#'
#' If the bbox provided does not cover all the source points, the grid will
#' be extended to cover all the source points.
#'
#' The precision controls the size of the grid cells (higher is more precise,
#' for example 0.5 generally gives a coarse result, 2 a satisfactory result
#' and 4 a particularly fine result). A precision of 2 is usually a good
#' default value.
#'
#' The number of iterations controls the number of iterations for the
#' interpolation. It is generally 4 times the square root of the number of
#' points (and this is the default value it the niter parameter is not
#' provided.
#'
#' Note that the number of source points must be equal to the number of
#' image points, and either they must be supplied in the same order
#' (as they are homologous points), or the name of a field containing
#' an identifier must be supplied to enable them to be sorted in the
#' same order.
#' @param source_points The source point layer
#' @param image_points The image point layer
#' @param precision The precision of the grid to be created
#' (a higher value means a higher precision - 0.5 gives usually a coarse result,
#' 2 is good default, 4 is very detailed).
#' @param bbox The bounding box of the grid to be created
#' @param niter The number of iterations (using `round(4 * sqrt(length(source_points)))`
#' is a good default for this value)
#' @param sort_by The field to sort the source and image points by
#' @return An interpolation grid to be used to transform the layers
#' @export
dc_create <- function (
  source_points,
  image_points,
  precision,
  bbox,
  niter,
  sort_by
) {
  # The CRS must be the same for the source and image points
  # but we allow the CRS to be missing for both the source and image points
  if (
    !(is.na(sf::st_crs(source_points)) && is.na(sf::st_crs(image_points)))
    && sf::st_crs(source_points) != sf::st_crs(image_points)
  ) {
    stop("The source and image point layers must have the same CRS")
  }
  # The source_points and image_points must have the same number of points
  if (length(sf::st_geometry(source_points)) != length(sf::st_geometry(image_points))) {
    stop("The source and image point layers must have the same number of points")
  }
  # If the sort_by argument is given, we sort the source and image points
  # by the field given in the sort_by argument
  if (!missing(sort_by)) {
    # Source_points and image_points must have this field
    if (!sort_by %in% names(source_points) || !sort_by %in% names(image_points)) {
      stop("The source and image point layers must have the field given in the sort_by argument")
    }
    source_points <- source_points[order(source_points[[sort_by]]),]
    image_points <- image_points[order(image_points[[sort_by]]),]
  }
  # If the niter argument is missing or is not a positive integer, we
  # compute the number of iterations as 4 times the square root of the number
  if (missing(niter) || niter <= 0) {
    niter <- round(4 * sqrt(length(source_points)))
  }
  e <- new.env(parent = emptyenv())
  e$.ptr <- .Call(
    savvy_InterpolationGrid_new__impl,
    sf::st_as_binary(sf::st_geometry(source_points)),
    sf::st_as_binary(sf::st_geometry(image_points)),
    precision,
    niter,
    bbox
  )
  e$crs <- sf::st_crs(source_points)
  e$source_grid <- sf::st_set_crs(.source_grid(e$.ptr), e$crs)
  e$interpolated_grid <- sf::st_set_crs(.interpolated_grid(e$.ptr), e$crs)
  e$precision <- precision
  e$resolution <- .Call(savvy_InterpolationGrid_resolution__impl, e$.ptr)
  e$bbox <- .Call(savvy_InterpolationGrid_bbox__impl, e$.ptr)
  names(e$bbox) <- c('xmin', 'ymin', 'xmax', 'ymax')
  class(e) <- "distanamo_interpolation_grid"
  return(e)
}

#' dc_interpolate
#'
#' Interpolate a sf layer using the interpolation grid.
#' @param interpolation_grid The interpolation grid
#' @param layer_to_deform The sf layer to interpolate
#' @return The sf layer deformed by the interpolation grid
#' @export
dc_interpolate <- function (
  interpolation_grid,
  layer_to_deform
) {
  source_crs <- sf::st_crs(layer_to_deform)
  res <- .Call(
    savvy_InterpolationGrid_transform_layer__impl,
    interpolation_grid$.ptr,
    sf::st_as_binary(sf::st_geometry(layer_to_deform))
  )
  layer <- sf::st_sf(layer_to_deform)
  sf::st_geometry(layer) <- sf::st_as_sfc(res)
  return(sf::st_set_crs(layer, source_crs))
}

#' dc_combine_bbox
#' 
#' Takes a list of sf layers and compute the bounding box
#' that covers them all.
#' @param list_layers A list of sf layers
#' @return The bounding box as a vector of doubles (as sf::st_bbox)
#' @export
dc_combine_bbox <- function(list_layers) {
  combined_bbox <- Reduce(function(b1, b2) c(
    xmin = min(b1["xmin"], b2["xmin"]),
    ymin = min(b1["ymin"], b2["ymin"]),
    xmax = max(b1["xmax"], b2["xmax"]),
    ymax = max(b1["ymax"], b2["ymax"])
  ), lapply(list_layers, sf::st_bbox))
  return(combined_bbox)
}


#' dc_move_points
#'
#' Move points.
#' @param points The points to be moved
#' @param times The times between the points (note that)
#' the reference point should have a time of 0 and that times
#' and points should be in the same order.
#' @param factor The factor of displacement (default: 1)
#' @return The moved points
#' @export
dc_move_points <- function(points, times, factor) {
  if (missing(factor)) {
    factor <- 1
  }
  if (factor <= 0) {
    stop('Factor must be a non-null positive value')
  }
  new_points <- .Call(
    savvy_move_points_from_times__impl,
    sf::st_as_binary(sf::st_geometry(points)),
    times,
    factor
  )
  source_crs <- sf::st_crs(points)

  layer <- sf::st_sf(points)
  sf::st_geometry(layer) <- sf::st_as_sfc(new_points)
  return(sf::st_set_crs(layer, source_crs))
}

#' .source_grid
#'
#' Retrieve the source grid as an sf layer
#' @param interpolation_grid The interpolation grid
#' @return The source grid as an sf layer
#' @noRd
.source_grid <- function(ptr) {
  grid <- sf::st_as_sfc(
    .Call(savvy_InterpolationGrid_get_source_grid__impl, ptr)
  )
  return(sf::st_sf(geometry=grid))
}

#' .interpolated_grid
#'
#' Retrieve the interpolated grid as an sf layer
#' @param interpolation_grid The interpolation grid
#' @return The interpolated grid as an sf layer
#' @noRd
.interpolated_grid <- function(ptr) {
  grid <- sf::st_as_sfc(
    .Call(savvy_InterpolationGrid_get_interpolated_grid__impl, ptr)
  )
  return(sf::st_sf(geometry=grid))
}
