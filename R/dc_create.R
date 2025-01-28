#' @title Create an interpolation grid
#' @description Create a new interpolation grid which covers the source points and with a cell size
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
#' 2 is good default, 4 is very detailed)
#' @param bbox The bounding box of the grid to be created
#' @param niter The number of iterations
#' (default is `floor(4 * sqrt(length(source_points)))`)
#' @param sort_by The field to sort the source and image points by
#' @return An interpolation grid to be used to transform the layers
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
#' # Deform the target layer
#' background_deformed <- dc_interpolate(
#'   interpolation_grid = igrid,
#'   layer_to_deform = background_layer
#' )
#'
#' plot(st_geometry(background_deformed))
#'
dc_create <- function(
    source_points,
    image_points,
    precision,
    bbox,
    niter,
    sort_by) {
  # The CRS must be the same for the source and image points
  # but we allow the CRS to be missing for both the source and image points
  if (
    !(is.na(sf::st_crs(source_points)) && is.na(sf::st_crs(image_points))) &&
      sf::st_crs(source_points) != sf::st_crs(image_points)
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
    source_points <- source_points[order(source_points[[sort_by]]), ]
    image_points <- image_points[order(image_points[[sort_by]]), ]
  }
  # If the niter argument is missing or is not a positive integer, we
  # compute the number of iterations as 4 times the square root of the number
  if (missing(niter) || niter <= 0) {
    niter <- floor(4 * sqrt(length(source_points)))
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
  names(e$bbox) <- c("xmin", "ymin", "xmax", "ymax")
  class(e) <- "interpolation_grid"
  return(e)
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
  return(sf::st_sf(geometry = grid))
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
  return(sf::st_sf(geometry = grid))
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
  return(sf::st_sf(geometry = pts))
}
