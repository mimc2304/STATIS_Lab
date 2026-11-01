# =============================================================================
# CAPA 3 - GEOMETRÍA: las coordenadas que se dibujan.
#
# El compromiso de ade4 se representa con marcadores de filas y de columnas
# escalados ambos por los valores singulares (li = UD, co = VD), es decir, con
# la geometría del HJ-Biplot (Galindo-Villardón, 1986): la misma calidad de
# representación para las dos nubes. Aquí van las operaciones geométricas
# comunes a todos los métodos; la primera es «Voltear».
# =============================================================================

#' Voltear ejes de un resultado
#'
#' Cambia el signo de los ejes indicados en todas las coordenadas del espacio
#' del compromiso (individuos, variables y trayectorias) a la vez. Los
#' productos escalares, las distancias y la inercia no cambian: solo la
#' orientación del dibujo (el signo de un eje es arbitrario).
#' @param res objeto `statislab`.
#' @param ejes números de los ejes a voltear.
#' @return el mismo resultado con los ejes volteados (se anotan en
#'   `res$volteados`).
#' @export
voltear_resultado <- function(res, ejes = integer()) {
  ejes <- as.integer(ejes)
  if (!length(ejes)) return(res)
  voltear <- function(m) {
    if (is.null(m)) return(m)
    cols <- intersect(paste("Eje", ejes), colnames(m))
    for (cl in cols) m[[cl]] <- -m[[cl]]
    m
  }
  voltear_m <- function(m) {
    if (is.null(m)) return(m)
    k <- ejes[ejes <= ncol(m)]
    m[, k] <- -m[, k]
    m
  }
  for (s in c("filas", "variables")) res[[s]] <- lapply(res[[s]], voltear_m)
  for (s in c("trayectorias", "trayectorias_variables")) res[[s]] <- lapply(res[[s]], voltear)
  res$volteados <- sort(unique(c(res$volteados, ejes)))
  res
}

#' Etiqueta de un eje con su porcentaje de inercia
#' @param res objeto `statislab`.
#' @param eje número de eje.
#' @export
etiqueta_eje <- function(res, eje) {
  p <- if (!is.null(res$inercia) && eje <= nrow(res$inercia)) res$inercia[[3]][eje] else NA
  if (is.na(p)) paste("Eje", eje) else paste0("Eje ", eje, " (", pct(p), ")")
}
