#' @importFrom grDevices devAskNewPage
#' @importFrom graphics arrows legend
#' @export
plot.interpolation_grid <- function(
  x,
  which = 1:4,
  ask = interactive(),
  caption = list("Source grid", "Interpolated grid", "Image to interpolated points", "Deformation strength"),
  ...
) {
  if (!inherits(x, "interpolation_grid")) stop("Not a interpolation_grid object")
  if (!is.numeric(which) || any(which < 1) || any(which > 4)) stop("'which' must be in 1:4")

  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }

  plots <- list(
    source_grid = function () {
      plot(sf::st_geometry(x$source_grid), main = caption[1], col = "green")
    },
    interpolated_grid = function () {
      plot(sf::st_geometry(x$interpolated_grid), main = caption[2], col = "pink")
    },
    image_to_interpolated_points = function () {
      plot(sf::st_geometry(x$image_points), main = caption[3], col = "blue")
      plot(sf::st_geometry(x$interpolated_points), col = "red", add = TRUE)
      c_image <- sf::st_coordinates(x$image_points)
      c_interpolated <- sf::st_coordinates(x$interpolated_points)
      # We dont want to draw arrows if the point in c_image is the same
      # as the point in c_interpolated
      better_arrow(c_image, c_interpolated)
      legend("topleft", legend = c("Image points", "Interpolated points"),
             col = c("blue", "red"), pt.lwd = 1, bty = "n", cex = 0.8, pch = 1)
    },
    deformation_strength = function () {
      mat <- .Call(savvy_InterpolationGrid_get_deformation_data__impl, x$.ptr)
      raster <- terra::rast(t(mat))
      terra::ext(raster) <- terra::ext(igrid$bbox[1], igrid$bbox[3], igrid$bbox[2], igrid$bbox[4])
      t_raster <- terra::rast(nrow = nrow(mat) * 10, ncol = ncol(mat) * 10, ext = terra::ext(raster))
      raster_resampled <- terra::resample(raster, t_raster, method = "cubicspline")
      pal <- get_continuous_pal(
        c(min(mat), 1, max(mat)),
        c("#4575b4", "#ffffbf", "#a50026"),
        1
      )
      terra::plot(raster_resampled, main = caption[4], col = pal)
    }
  )

  plot_names <- c("source_grid", "interpolated_grid", "image_to_interpolated_points", "deformation_strength")

  for (i in which) plots[[plot_names[i]]]()
}

#' Code from https://github.com/riatelab/mapsf/blob/dev/R/mf_map_utils.R
get_hex_pal <- function(pal, alpha) {
  pal <- grDevices::col2rgb(pal, alpha = FALSE)
  ffun <- function(x) {
    grDevices::rgb(pal[1, x],
                   pal[2, x],
                   pal[3, x],
                   maxColorValue = 255
    )
  }
  paste0(sapply(seq_len(ncol(pal)), ffun), get_alpha(alpha))
}

#' Code from https://github.com/riatelab/mapsf/blob/dev/R/mf_map_utils.R
get_alpha <- function(alpha) {
  if (alpha < 0) {
    alpha <- 0
  }
  if (alpha > 1) {
    alpha <- 1
  }
  sprintf("%02X", as.integer(255.999 * alpha))
}

#' Code from https://github.com/riatelab/mapsf/blob/dev/R/mf_raster_utils.R
get_continuous_pal <- function(breaks, pal, alpha) {
  etendu <- max(breaks) - min(breaks)
  lb <- length(breaks)
  dd <- data.frame(from = breaks[1:(lb - 1)], to = breaks[2:lb])
  dd$diff <- dd$to - dd$from
  dd$ncol <- round(dd$diff * 1000 / etendu)
  dd$colfrom <- pal[1:(lb - 1)]
  dd$colto <- pal[2:lb]
  l <- list()
  for (i in 1:(lb - 1)) {
    l[[i]] <- colorRampPalette(c(dd$colfrom[i], dd$colto[i]), alpha = TRUE)(dd$ncol[i])
  }
  p <- do.call(c, l)
  if (!is.null(alpha)) {
    p <- get_hex_pal(p, alpha)
  }
  p
}
