# Distanamo

This package allows you to create distance cartograms (or distance anamorphoses, hence the name).

## Installation

You can install the development version of `gepafer` from GitHub with:

```R
# install.packages("remotes")
remotes::install_github("riatelab/distanamo")
```

You will need the [Rust toolchain](https://rustup.rs/) to compile the Rust code.

The package is not yet on CRAN.

## Usage

To use this package you need to provide to sets of homologuous points : *source points* and *image points*.

```R
source_pts <- sf::st_read('ata-source-point.geojson')
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

## License

GPL-3.