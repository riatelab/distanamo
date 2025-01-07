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
