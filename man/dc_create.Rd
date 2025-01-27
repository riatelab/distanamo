% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dc_create.R
\name{dc_create}
\alias{dc_create}
\title{dc_create}
\usage{
dc_create(source_points, image_points, precision, bbox, niter, sort_by)
}
\arguments{
\item{source_points}{The source point layer, sf POINT object}

\item{image_points}{The image point layer, sf POINT object}

\item{precision}{The precision of the grid to be created
(a higher value means a higher precision - 0.5 gives usually a coarse result,
2 is good default, 4 is very detailed)}

\item{bbox}{The bounding box of the grid to be created}

\item{niter}{The number of iterations
(default is  \code{round(4 * sqrt(length(source_points)))})}

\item{sort_by}{The field to sort the source and image points by}
}
\value{
An interpolation grid to be used to transform the layers
}
\description{
Create a new interpolation grid which covers the source points and with a cell size
deduced from the precision.
}
\details{
The grid is then interpolated to match the
image points. This then allows one to interpolate any point on the grid
(enabling the deformation of geometries such as background layers)
and to retrieve useful metrics about the deformation.

If the bbox provided does not cover all the source points, the grid will
be extended to cover all the source points.

The precision controls the size of the grid cells (higher is more precise,
for example 0.5 generally gives a coarse result, 2 a satisfactory result
and 4 a particularly fine result). A precision of 2 is usually a good
default value.

The number of iterations controls the number of iterations for the
interpolation. It is generally 4 times the square root of the number of
points (and this is the default value it the niter parameter is not
provided.

Note that the number of source points must be equal to the number of
image points, and either they must be supplied in the same order
(as they are homologous points), or the name of a field containing
an identifier must be supplied to enable them to be sorted in the
same order.
}
\examples{
library(sf)
start <- st_read(
  dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
  layer = "start", quiet = TRUE
)
points <- st_read(
  dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
  layer = "points", quiet = TRUE
)
center <- st_read(
  dsn = system.file("gpkg/pit.gpkg", package = "distanamo"),
  layer = "center", quiet = TRUE
)

pos_result <- dc_move_from_reference_point(
  reference_point = start,
  other_points = points,
  duration_col_name = "durations",
  factor = 1
)

bbox <- dc_combine_bbox(list_layers = list(points, center))

# Create the interpolation grid
igrid <- dc_create(
  source_points = pos_result$source_points,
  image_points = pos_result$image_points,
  precision = 2,
  bbox = bbox,
)

# Deform the target layer
center_deform <- dc_interpolate(
  interpolation_grid = igrid,
  layer_to_deform = center
)

plot(st_geometry(igrid$interpolated_grid), col = NA)
plot(st_geometry(center_deform), add = TRUE)

}
