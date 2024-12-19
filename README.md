# Distanamo

This package allows you to create distance cartograms (or distance anamorphoses, hence the name).

## Installation

You can install the development version of `distanamo` from GitHub with:

```R
# install.packages("remotes")
remotes::install_github("riatelab/distanamo")
```

You will need the [Rust toolchain](https://rustup.rs/) to compile the Rust code.
The Minimum Supported Rust Version (MSRV) is 1.82.0.

The package is not yet on CRAN or on R-universe.

## Usage

To use this package you need to provide two sets of homologous points : *source points* and *image points*.

```R
# Read source points, image points and the background layer to deform
source_pts <- sf::st_read('data-source-point.geojson')
image_pts <- sf::st_read('data-image-point.geojson')
background_layer <- sf::st_read('background.geojson')
bbox <- sf::st_bbox(background_layer)

# Create the interpolation grid
igrid <- dc_create(source_pts, image_pts, 2.0, bbox)

# Use it to deform our layer of interest
deformed_background <- dc_interpolate(igrid, background_layer)

# Access the underlying grids (source and interpolated)
# as sf objects:
plot(igrid$source_grid)
plot(igrid$interpolated_grid)

# Display useful information
summary(igrid)
```

Optionally you can provide a layer of source points and matrix of duration between
the points.

This duration matrix will be used to extract the duration between a reference points
and all the other points, allowing to move closer / farther points from the reference point
depending on if they can be reached faster or slower of the mean speed (between the reference point
and all the others).

```R
# Read source points and layer to be deformed
source_pts <- sf::st_read('data-source-point.geojson')
background_layer <- sf::st_read('background.geojson')
bbox <- sf::st_bbox(background_layer)

# Read duration between points
d <-read.csv('mat.csv', row.names = 1)
# The CSV is a time matrix structured as follow
#           AGEN   BORDEAUX   GRENOBLE etc.
# AGEN      0.0    111.2      200.3
# BORDEAUX  112.3  0.0        300.1
# GRENOBLE  199.4  301.1      0.0
# etc.
dv <- d['GRENOBLE', ]
# So we have only the duration between GRENOBLE and all the other points
#          AGEN   BORDEAUX   GRENOBLE etc.
# GRENOBLE 199.4  301.1      0.0

# Move points to create the image points layer
image_pts <- dc_move_points(source_pts, as.double(dv))

# Create the interpolation grid
igrid <- dc_create(source_pts, image_pts, 2.0, bbox)

# Deform the target layer
deformed_background <- dc_interpolate(igrid, background_layer)

plot(deformed_background)
```

## Example

![Example of distance cartogram](./man/figures/ex-1.png)
*Map made with [`mapsf`](https://github.com/riatelab/mapsf).*

## More information about the method

- This is a port of the **[Darcy](https://thema.univ-fcomte.fr/productions/software/darcy/)** standalone software regarding the bidimensional regression and the backgrounds layers deformation.  
All credits for the contribution of the method goes to **Colette Cauvin** *(Théma - Univ. Franche-Comté)* and for the reference Java implementation goes to **Gilles Vuidel** *(Théma - Univ. Franche-Comté)*.

- This method is also available as a **QGIS plugin** ([GitHub repository](https://github.com/mthh/QgisDistanceCartogramPlugin) / [QGIS plugin repository](https://plugins.qgis.org/plugins/dist_cartogram/)).

- This R package is a wrapper around the Rust library [`distance-cartogram-rs`](https://github.com/mthh/distance-cartogram-rs)
  which is well documented and can be used directly from Rust.

## License

**GPL-3.0**