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
`$<-.distanamo_multipolar_displacement_result` <- function(x, name, value) {
  stop("distanamo_multipolar_displacement_result cannot be modified", call. = FALSE)
}

#' @export
`[[<-.distanamo_multipolar_displacement_result` <- function(x, i, value) {
  stop("distanamo_multipolar_displacement_result cannot be modified", call. = FALSE)
}

#' @export
`$<-.distanamo_unipolar_displacement_result` <- function(x, name, value) {
  stop("distanamo_unipolar_displacement_result cannot be modified", call. = FALSE)
}

#' @export
`[[<-.distanamo_unipolar_displacement_result` <- function(x, i, value) {
  stop("distanamo_unipolar_displacement_result cannot be modified", call. = FALSE)
}


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

#' @export
plot.distanamo_interpolation_grid <- function(
  x,
  which = 1:3,
  ask = interactive(),
  caption = list("Source grid", "Interpolated grid", "Image to interpolated points"),
  ...
) {
  if (!inherits(x, "distanamo_interpolation_grid")) stop("Not a distanamo_interpolation_grid object")
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
      identical_pts <-identical(c_image, c_interpolated)
      cc_image <- c_image[!identical_pts,]
      cc_interpolated <- c_interpolated[!identical_pts,]
      arrows(
        x0 = cc_image[,1],
        y0 = cc_image[,2],
        x1 = cc_interpolated[,1],
        y1 = cc_interpolated[,2],
        length = 0.075
      )
      legend("topleft", legend = c("Image points", "Interpolated points"),
             col = c("blue", "red"), pt.lwd = 1, bty = "n", cex = 0.8, pch = 1)
    }
  )

  plot_names <- c("source_grid", "interpolated_grid", "image_to_interpolated_points")

  for (i in which) plots[[plot_names[i]]]()
}

#' @export
summary.distanamo_multipolar_displacement_result <- function(object, ...) {
  if (!inherits(object, "distanamo_multipolar_displacement_result")) stop("Not a distanamo_multipolar_displacement_result object")
  dists <- sf::st_distance(object$source_points, object$image_points, by_element = TRUE)
  summary_obj <- list(
    error = object$error,
    min_displacement = min(dists),
    mean_displacement = mean(dists),
    max_displacement = max(dists)
  )
  cat("Summary of the multipolar displacement result:\n")
  cat("Error (procrustes distance):", summary_obj$error, "\n")
  cat("Min displacement:", summary_obj$min_displacement, paste0("[", units::deparse_unit(summary_obj$min_displacement), "]"), "\n")
  cat("Mean displacement:", summary_obj$mean_displacement, paste0("[", units::deparse_unit(summary_obj$mean_displacement), "]"), "\n")
  cat("Max displacement:", summary_obj$max_displacement, paste0("[", units::deparse_unit(summary_obj$max_displacement), "]"), "\n")
  return(invisible(summary_obj))
}

#' @export
plot.distanamo_multipolar_displacement_result <- function(x, ...) {
  if (!inherits(x, "distanamo_multipolar_displacement_result")) stop("Not a distanamo_multipolar_displacement_result object")
  plot(sf::st_geometry(x$source_points), col = "blue", main = "Source points to image points displacement")
  plot(sf::st_geometry(x$image_points), col = "red", add = TRUE)
  c_source <- sf::st_coordinates(x$source_points)
  c_image <- sf::st_coordinates(x$image_points)
  identical_pts <-identical(c_source, c_image)
  cc_source <- c_source[!identical_pts,]
  cc_image <- c_image[!identical_pts,]
  arrows(
      x0 = cc_source[,1],
      y0 = cc_source[,2],
      x1 = cc_image[,1],
      y1 = cc_image[,2],
      length = 0.075
  )
  legend("topleft", legend = c("Source points", "Image points"),
         col = c("blue", "red"), pt.lwd = 1, bty = "n", cex = 0.8, pch = 1)
}

#' @export
summary.distanamo_unipolar_displacement_result <- function(object, ...) {
  if (!inherits(object, "distanamo_unipolar_displacement_result")) stop("Not a distanamo_unipolar_displacement_result object")
  # We don't take into account the first point as it is the reference point and
  # it was not moved
  dists <- sf::st_distance(object$source_points[-1,], object$image_points[-1,], by_element = TRUE)
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

#' @export
plot.distanamo_unipolar_displacement_result <- function(x, ...) {
  if (!inherits(x, "distanamo_unipolar_displacement_result")) stop("Not a distanamo_unipolar_displacement_result object")
  plot(sf::st_geometry(x$source_points), col = "blue", main = "Source points to image points displacement")
  plot(sf::st_geometry(x$image_points), col = "red", add = TRUE)
  c_source <- sf::st_coordinates(x$source_points)
  c_image <- sf::st_coordinates(x$image_points)
  identical_pts <-identical(c_source, c_image)
  cc_source <- c_source[!identical_pts,]
  cc_image <- c_image[!identical_pts,]
  arrows(
    x0 = cc_source[,1],
    y0 = cc_source[,2],
    x1 = cc_image[,1],
    y1 = cc_image[,2],
    length = 0.075
  )
  legend("topleft", legend = c("Source points", "Image points"),
         col = c("blue", "red"), pt.lwd = 1, bty = "n", cex = 0.8, pch = 1)
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
#' @param source_points The source point layer, sf POINT object
#' @param image_points The image point layer, sf POINT object
#' @param precision The precision of the grid to be created
#' (a higher value means a higher precision - 0.5 gives usually a coarse result,
#' 2 is good default, 4 is very detailed).
#' @param bbox The bounding box of the grid to be created
#' @param niter The number of iterations
#' (default is  `round(4 * sqrt(length(source_points)))`)
#' @param sort_by The field to sort the source and image points by
#' @return An interpolation grid to be used to transform the layers
#' @export
#' @examples
#' library(sf)
#' start <- st_read(dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
#'                  layer = "start", quiet = TRUE)
#' points <-  st_read(dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
#'                    layer = "points", quiet = TRUE)
#' center <-  st_read(dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
#'                    layer = "center", quiet = TRUE)
#'
#' pos_result <- dc_move_from_reference_point(
#'   reference_point = start,
#'   other_points = points,
#'   duration_col_name = "durations",
#'   factor = 1
#' )
#'
#' bbox <- dc_combine_bbox(list_layers = list(points, center))
#'
#' # Create the interpolation grid
#' igrid <- dc_create(
#'   source_points = pos_result$source_points,
#'   image_points = pos_result$image_points,
#'   precision = 2,
#'   bbox = bbox,
#' )
#'
#' # Deform the target layer
#' center_deform <- dc_interpolate(interpolation_grid = igrid,
#'                                 layer_to_deform = center)
#'
#' plot(st_geometry(igrid$interpolated_grid), col = NA)
#' plot(st_geometry(center_deform), add = TRUE)
#'
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
  e$source_points <- source_points
  e$image_points <- image_points
  e$interpolated_points <- sf::st_set_crs(.interpolated_points(e$.ptr), e$crs)
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
#' @examples
#' library(sf)
#' start <- st_read(dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
#'                  layer = "start", quiet = TRUE)
#' points <-  st_read(dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
#'                    layer = "points", quiet = TRUE)
#' center <-  st_read(dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
#'                    layer = "center", quiet = TRUE)
#'
#' pos_result <- dc_move_from_reference_point(
#'   reference_point = start,
#'   other_points = points,
#'   duration_col_name = "durations",
#'   factor = 1
#' )
#'
#' bbox <- dc_combine_bbox(list_layers = list(points, center))
#'
#' # Create the interpolation grid
#' igrid <- dc_create(
#'   source_points = pos_result$source_points,
#'   image_points = pos_result$image_points,
#'   precision = 2,
#'   bbox = bbox,
#' )
#'
#' # Deform the target layer
#' center_deform <- dc_interpolate(interpolation_grid = igrid,
#'                                 layer_to_deform = center)
#'
#' plot(st_geometry(igrid$interpolated_grid), col = NA)
#' plot(st_geometry(center_deform), add = TRUE)
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

#' dc_interpolate_parallel
#'
#' Interpolate a list of sf layers using the interpolation grid.
#' @param interpolation_grid The interpolation grid
#' @param layers_to_deform A list of sf layers to interpolate
#' @return The sf layers deformed by the interpolation grid
#' @export
dc_interpolate_parallel <- function (
  interpolation_grid,
  layers_to_deform
) {
  l <- lapply(layers_to_deform, function(layer) sf::st_as_binary(sf::st_geometry(layer)))
  res <- .Call(
    savvy_InterpolationGrid_transform_layers_parallel__impl,
    interpolation_grid$.ptr,
    l
  )
  for (i in seq_along(layers_to_deform)) {
    layer <- layers_to_deform[[i]]
    source_crs <- sf::st_crs(layer)
    sf::st_geometry(layer) <- sf::st_as_sfc(res[[i]])
    l[[i]] <- sf::st_set_crs(layer, source_crs)
  }
  return(l)
}


#' dc_combine_bbox
#'
#' Takes a list of sf objects and compute the bounding box
#' that covers them all.
#' @param list_layers A list of sf objects
#' @return An sf bounding box is returned.
#' @export
#' @examples
#' library(sf)
#' start <- st_read(dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
#'                  layer = "start", quiet = TRUE)
#' points <-  st_read(dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
#'                    layer = "points", quiet = TRUE)
#' center <-  st_read(dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
#'                    layer = "center", quiet = TRUE)
#'
#' bbox <- dc_combine_bbox(list(start, points, center))
#' bbox
dc_combine_bbox <- function(list_layers) {
  combined_bbox <- Reduce(function(b1, b2) c(
    xmin = min(b1["xmin"], b2["xmin"]),
    ymin = min(b1["ymin"], b2["ymin"]),
    xmax = max(b1["xmax"], b2["xmax"]),
    ymax = max(b1["ymax"], b2["ymax"])
  ), lapply(list_layers, sf::st_bbox))

  combined_bbox <- sf::st_set_crs(sf::st_bbox(combined_bbox),
                                  sf::st_crs(list_layers[[1]]))
  return(combined_bbox)
}


#' dc_move_from_reference_point
#'
#' Move points from a reference point using durations between the reference point
#' and all the other points.
#' @param reference_point The point from which the other points will be moved,
#' an sf POINT object
#' @param other_points The other points to move, an sf POINT object
#' @param duration_col_name The name of the column containing the durations
#' in the other_points sf object
#' @param factor The factor of displacement (default: 1)
#' @return A list object with the source points and the image points, ready
#' to be used with the dc_create function
#' @export
#' @examples
#' library(sf)
#' start <- st_read(dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
#'                  layer = "start", quiet = TRUE)
#' points <-  st_read(dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
#'                    layer = "points", quiet = TRUE)
#' center <-  st_read(dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
#'                    layer = "center", quiet = TRUE)
#'
#' pos_result <- dc_move_from_reference_point(
#'   reference_point = start,
#'   other_points = points,
#'   duration_col_name = "durations",
#'   factor = 1
#' )
#'
#' plot(st_geometry(pos_result$source_points), pch = 4, cex = .5)
#' plot(st_geometry(pos_result$image_points), add = TRUE, pch = 4, cex = .5, col = "red")
#' plot(st_geometry(center), add = TRUE)
dc_move_from_reference_point <- function(reference_point, other_points, duration_col_name, factor) {
  if (missing(factor)) {
    factor <- 1
  }
  if (factor <= 0) {
    stop('Factor must be a non-null positive value')
  }
  if (length(sf::st_geometry(reference_point)) != 1) {
      stop('Reference point must be a single point')
  }
  if (length(sf::st_geometry(other_points)) == 0) {
      stop('No other points to move')
  }
  if (sf::st_crs(reference_point) != sf::st_crs(other_points)) {
    stop('The reference point and the other points must have the same CRS')
  }

  points <- sf::st_set_crs(
    sf::st_sf(geometry=c(sf::st_geometry(reference_point), sf::st_geometry(other_points))),
    sf::st_crs(reference_point)
  )

  durations <- c(0, other_points[[duration_col_name]])

  new_points <- .Call(
    savvy_move_points_from_durations__impl,
    sf::st_as_binary(sf::st_geometry(points)),
    durations,
    factor
  )

  source_crs <- sf::st_crs(points)

  layer <- sf::st_sf(points)
  sf::st_geometry(layer) <- sf::st_as_sfc(new_points)
  new_points <- sf::st_set_crs(layer, source_crs)
  li <- list(
    reference_point = reference_point,
    source_points = points,
    image_points = new_points
  )
  class(li) <- "distanamo_unipolar_displacement_result"

  return(li)
}

#' dc_generate_positions_from_durations
#'
#' Generate positions from durations matrix, using PCoA to find the relative
#' positions between points, then perform a procrustes analysis to find the
#' best fit between the source points and the image points.
#' @param durations The durations matrix
#' @param source_points The source points, an sf POINT object
#' @return A list object with the source points and the image points, ready
#' to be used with the dc_create function
#' @export
dc_generate_positions_from_durations <- function(durations, source_points) {
  li <- .Call(
    savvy_generate_positions_from_durations_matrix__impl,
    sf::st_as_binary(sf::st_geometry(source_points)),
    as.matrix(durations)
  )
  source_crs <- sf::st_crs(source_points)
  layer <- sf::st_sf(source_points)
  sf::st_geometry(layer) <- sf::st_as_sfc(li$image_points)
  li$source_points <- source_points
  li$image_points <- sf::st_set_crs(layer, source_crs)
  class(li) <- "distanamo_multipolar_displacement_result"
  return(li)
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

#' .interpolated_points
#'
#' Retrieve the interpolated points as an sf layer
#' @param interpolation_grid The interpolation grid
#' @return The interpolated points as an sf layer
#' @noRd
.interpolated_points <- function(ptr) {
  pts <- sf::st_as_sfc(
    .Call(savvy_InterpolationGrid_interpolated_points__impl, ptr)
  )
  return(sf::st_sf(geometry=pts))
}
