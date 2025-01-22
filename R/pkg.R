#' @title Create Distance Cartograms
#' @name distanamo
#' @rdname distanamo
#' @description
#' This package enables the creation of what is often defined as a distance
#' cartogram. Distance cartograms are a type of cartogram that deforms the
#' layers of a map according to the distances between a set of source points
#' and a set of image points.
#' This is done by extending (by interpolation) to the layer(s) of the study
#' area (territorial divisions, network...) the local displacement between the
#' source coordinates and the image coordinates, derived from the distances
#' between each pair of homologous points (source / image points).
#' @docType package
"_PACKAGE"

#' @useDynLib distanamo, .registration = TRUE
#' @keywords internal
NULL

#' @export
`$<-.interpolation_grid` <- function(x, name, value) {
  stop("interpolation_grid cannot be modified", call. = FALSE)
}

#' @export
`[[<-.interpolation_grid` <- function(x, i, value) {
  stop("interpolation_grid cannot be modified", call. = FALSE)
}

#' @export
`$<-.multipolar_displacement_result` <- function(x, name, value) {
  stop("multipolar_displacement_result cannot be modified", call. = FALSE)
}

#' @export
`[[<-.multipolar_displacement_result` <- function(x, i, value) {
  stop("multipolar_displacement_result cannot be modified", call. = FALSE)
}

#' @export
`$<-.unipolar_displacement_result` <- function(x, name, value) {
  stop("unipolar_displacement_result cannot be modified", call. = FALSE)
}

#' @export
`[[<-.unipolar_displacement_result` <- function(x, i, value) {
  stop("unipolar_displacement_result cannot be modified", call. = FALSE)
}

#' @export
`$<-.adjustment_result` <- function(x, name, value) {
  stop("adjustment_result cannot be modified", call. = FALSE)
}

#' @export
`[[<-.adjustment_result` <- function(x, i, value) {
  stop("adjustment_result cannot be modified", call. = FALSE)
}
