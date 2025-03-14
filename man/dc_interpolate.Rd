% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dc_interpolate.R
\name{dc_interpolate}
\alias{dc_interpolate}
\title{Interpolate a sf layer using the interpolation grid}
\usage{
dc_interpolate(interpolation_grid, layer_to_deform)
}
\arguments{
\item{interpolation_grid}{The interpolation grid}

\item{layer_to_deform}{The sf layer to interpolate}
}
\value{
The sf layer deformed by the interpolation grid
}
\description{
Interpolate a sf layer using the interpolation grid.
}
\examples{
library(sf)

# Read source points
source_pts <- st_read(
  dsn = system.file("gpkg/data-prefecture.gpkg", package = "distanamo"),
  layer = "prefecture", quiet = TRUE
)

# Read image points
image_pts <- st_read(
  dsn = system.file("gpkg/data-prefecture.gpkg", package = "distanamo"),
  layer = "image-points", quiet = TRUE
)

# Read the background layer to deform
background_layer <- st_read(
  dsn = system.file("gpkg/data-prefecture.gpkg", package = "distanamo"),
  layer = "departement", quiet = TRUE
)

# Create the interpolation grid
igrid <- dc_create(
  source_points = source_pts,
  image_points = image_pts,
  precision = 2,
  bbox = st_bbox(background_layer)
)

# Plot various depictions of the interpolation grid
plot(igrid)

# Useful information about the interpolation grid
summary(igrid)

# Deform the target layer
background_deformed <- dc_interpolate(
  interpolation_grid = igrid,
  layer_to_deform = background_layer
)

plot(st_geometry(background_deformed))

}
