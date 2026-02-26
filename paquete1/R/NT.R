#' Calculo de noches tropicales para un SpatRaster
#'
#' \code{mean_nt} devuelve las noches tropicales promedio anuales
#'
#' @param raster_entrada un objeto de la clase SpatRaster con las temperaturas mínimas por dia
#' @param year_inicio año de inicio del periodo que se desea calcular
#' @param year_fin año de inicio del periodo que se desea calcular
#'
#'
#' @return devuelve un objeto de la clase SpatRaster con la media anual de noches tropicales por pixel
#'
#'
#'@import terra
#'@export
#'
#' @examples
#' \dontrun{
#'library(terra)
#'r_base <- rast(nrows = 10, ncols = 10,
#'             xmin = 0, xmax = 10,
#'             ymin = 0, ymax = 10)
#'
#' r1 <- rast(r_base)
#' r2 <- rast(r_base)
#'
#' values(r1) <- runif(ncell(r1), min = 10, max = 30)
#' values(r2) <- runif(ncell(r2), min = 10, max = 30)
#'
#' tm_min <- c(r1, r2)
#'
#' time(tm_min) <- as.Date(c("2001-01-01", "2002-01-01"))
#'
#' tm_min_mean <- mean_nt(tm_min,2001,2002)}
#'


mean_nt <- function(raster_entrada,year_inicio,year_fin){

  tm_periodo <- subset(raster_entrada,format(time(raster_entrada), "%Y") >= year_inicio & format(time(raster_entrada), "%Y") <= year_fin)

  nt_periodo <- ifel(tm_periodo >= 20, 1, 0)

  tiempo <- format(time(nt_periodo), "%Y")

  nt_anuales <- tapp(nt_periodo, index = tiempo, fun = sum)

  NT_MEAN <- mean(nt_anuales)

  return(NT_MEAN)}
