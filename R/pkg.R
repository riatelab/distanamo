#' @title Create Distance Cartograms
#' @name distanamo
#' @rdname distanamo
#' @description
#' This package enables the creation of what is often defined as a distance
#' cartogram. This is done by showing (on background(s) layer(s), such as the
#' territorial divisions of the study zone) the local deformations (calculated
#' using Waldo Tobler's bidimensional regression) to fit image points to
#' source points.
#' @docType package
"_PACKAGE"

#' @useDynLib distanamo, .registration = TRUE
#' @keywords internal
NULL

#' @export
`$<-.interpolation_grid` <- function(x, name, value) {
  stop("distanamo_interpolation_grid cannot be modified", call. = FALSE)
}

#' @export
`[[<-.interpolation_grid` <- function(x, i, value) {
  stop("distanamo_interpolation_grid cannot be modified", call. = FALSE)
}

#' @export
`$<-.multipolar_displacement_result` <- function(x, name, value) {
  stop("distanamo_multipolar_displacement_result cannot be modified", call. = FALSE)
}

#' @export
`[[<-.multipolar_displacement_result` <- function(x, i, value) {
  stop("distanamo_multipolar_displacement_result cannot be modified", call. = FALSE)
}

#' @export
`$<-.unipolar_displacement_result` <- function(x, name, value) {
  stop("distanamo_unipolar_displacement_result cannot be modified", call. = FALSE)
}

#' @export
`[[<-.unipolar_displacement_result` <- function(x, i, value) {
  stop("distanamo_unipolar_displacement_result cannot be modified", call. = FALSE)
}
