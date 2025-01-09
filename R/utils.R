#' @importFrom graphics arrows
better_arrow <- function(src, img){
  ahs <- arrow_has_length(src, img)
  arrows(
    x0 = src[!ahs, 1],
    y0 = src[!ahs, 2],
    x1 = img[!ahs, 1],
    y1 = img[!ahs, 2],
    length = 0
  )
  arrows(
    x0 = src[ahs, 1],
    y0 = src[ahs, 2],
    x1 = img[ahs, 1],
    y1 = img[ahs, 2],
    length = 0.075
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
