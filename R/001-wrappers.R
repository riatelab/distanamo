#' @export
`$<-.distanamo_interpolation_grid` <- function(x, name, value) {
  stop("distanamo_interpolation_grid cannot be modified", call. = FALSE)
}

#' @export
`[[<-.distanamo_interpolation_grid` <- function(x, i, value) {
  stop("distanamo_interpolation_grid cannot be modified", call. = FALSE)
}

#' @export
summary.distanamo_interpolation_grid <- function(interpolation_grid, ...) {
  summary_obj <- list(
    n_cells = length(interpolation_grid$source_grid),
    deformation_strength = interpolation_grid$.inner$deformation_strength(),
    precision = interpolation_grid$precision,
    resolution = interpolation_grid$resolution
  )
  cat("Summary of the interpolation grid:\n")
  cat("Number of cells:", summary_obj$n_cells, "\n")
  cat("Precision:", summary_obj$resolution, paste0("(α = ", summary_obj$precision, ")"), "\n")
  cat("Deformation strength:", summary_obj$deformation_strength,"\n")
  return(summary_obj)
}

#' dc_create
#'
#' Create the interpolation grid.
#' @param source_points The source point layer
#' @param image_points The image point layer
#' @param precision The precision of the grid to be created
#' (a higher value means a higher precision - 0.5 gives usually a coarse result,
#' 2 is good default, 4 is very detailed).
#' @param bbox The bounding box of the grid to be created
#' @param niter The number of iterations (using `round(4 * sqrt(length(source_points)))`
#' is a good default for this value)
#' @return An interpolation grid to be used to transform the layers
#' @export
dc_create <- function (
  source_points,
  image_points,
  precision,
  bbox,
  niter
) {
  if (
    !is.na(sf::st_crs(source_points) != sf::st_crs(image_points))
    && sf::st_crs(source_points) != sf::st_crs(image_points)
  ) {
    stop("The source and image point layers must have the same CRS")
  }
  if (missing(niter) || niter <= 0) {
    niter <- round(4 * sqrt(length(source_points)))
  }
  e <- new.env(parent = emptyenv())
  e$.inner <- InterpolationGrid$new(
    sf::st_as_binary(sf::st_geometry(source_points)),
    sf::st_as_binary(sf::st_geometry(image_points)),
    precision,
    niter,
    bbox
  )
  e$crs <- sf::st_crs(source_points)
  e$source_grid <- sf::st_set_crs(.source_grid(e$.inner), e$crs)
  e$interpolated_grid <- sf::st_set_crs(.interpolated_grid(e$.inner), e$crs)
  e$precision <- precision
  e$resolution <- e$.inner$resolution()
  e$bbox <- e$.inner$bbox()
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
  res <- interpolation_grid$.inner$transform_layer(sf::st_as_binary(sf::st_geometry(layer_to_deform)))
  
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

#' .source_grid
#'
#' Retrieve the source grid as an sf layer
#' @param interpolation_grid The interpolation grid
#' @return The source grid as an sf layer
#' @noRd
.source_grid <- function(interpolation_grid) {
  grid <- sf::st_as_sfc(interpolation_grid$get_source_grid())
  return(sf::st_sf(geometry=grid))
}

#' .interpolated_grid
#'
#' Retrieve the interpolated grid as an sf layer
#' @param interpolation_grid The interpolation grid
#' @return The interpolated grid as an sf layer
#' @noRd
.interpolated_grid <- function(interpolation_grid) {
  grid <- sf::st_as_sfc(interpolation_grid$get_interpolated_grid())
  return(sf::st_sf(geometry=grid))
}
