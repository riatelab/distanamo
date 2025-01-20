#' @export
dc_concentric_circle <- function(
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
