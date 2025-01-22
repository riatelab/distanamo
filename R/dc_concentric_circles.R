#' dc_concentric_circles
#'
#' Create concentric circles around the reference point, using the results of the
#' `dc_move_from_reference_point`.
#' @param positioning_result The object returned by the `dc_move_from_reference_point`
#' function.
#' @param steps The steps (in the unit of the durations used ine the
#' `dc_move_from_reference_point` function) at which creating the circles.
#' @return A sf LINESTRING object
#' @export
dc_concentric_circles <- function(
    positioning_result,
    steps = list(30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360)) {
  res <- lapply(
    steps,
    function(s) {
      geom <- sf::st_boundary(sf::st_buffer(positioning_result$reference_point, positioning_result$reference_speed * s))$geom
      return(sf::st_sf(geometry = geom, step = s))
    }
  )
  combined_res <- do.call(rbind, res)
  return(combined_res)
}
