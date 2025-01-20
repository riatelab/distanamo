#' @importFrom graphics arrows
better_arrow <- function(src, img, col = "black"){
  ahs <- arrow_has_length(src, img)
  arrows(
    x0 = src[!ahs, 1],
    y0 = src[!ahs, 2],
    x1 = img[!ahs, 1],
    y1 = img[!ahs, 2],
    length = 0,
    col = col
  )
  arrows(
    x0 = src[ahs, 1],
    y0 = src[ahs, 2],
    x1 = img[ahs, 1],
    y1 = img[ahs, 2],
    length = 0.075,
    col = col
  )

}

#' @importFrom graphics par
arrow_has_length <- function(x, y){
  xti <- par("pin")[1]/diff(par("usr")[1:2])
  yti <- par("pin")[2]/diff(par("usr")[3:4])
  d <- apply(cbind(x, y), 1, pythagore, xti, yti)
  return(d >= 1/1000)
}

pythagore <- function(x, xti, yti) {
  sqrt((xti*x[1] - xti*x[3])^2 + (yti*x[2] - yti*x[4])^2)
}

#' Code copied and slightly adapted (to remove support for customizing alpha channel)
#' from https://github.com/riatelab/mapsf/blob/dev/R/mf_raster_utils.R
#' @noRd
#' @importFrom grDevices colorRampPalette
get_continuous_pal <- function(breaks, pal) {
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
  return(p)
}
