# =============================================================================
# CAPA 4 - DIAGNÓSTICOS: cuánto explica cada eje, cuánto se parecen las tablas,
# qué tablas pesan en el compromiso, cuán fuerte es la co-estructura y si es
# significativa. Funciones comunes a todos los métodos.
# =============================================================================

#' Tabla de valores propios e inercia
#' @param eig valores propios.
#' @param etiqueta nombre de la columna del porcentaje.
#' @export
tabla_inercia <- function(eig, etiqueta = "% Inercia") {
  eig <- as.numeric(eig)
  eig <- eig[eig > max(eig) * 1e-10]
  df <- data.frame(Eje = paste("Eje", seq_along(eig)), `Valor propio` = eig,
                   p = 100 * eig / sum(eig), `% Acumulado` = cumsum(100 * eig / sum(eig)),
                   check.names = FALSE)
  names(df)[3] <- etiqueta
  df
}

#' Interestructura de un PTA / STATIS de ade4
#'
#' Reúne la matriz RV entre ocasiones, los pesos del compromiso (primer vector
#' propio de la matriz RV, todos del mismo signo por Perron-Frobenius), la
#' calidad (cos²) con que el compromiso representa a cada ocasión y las
#' coordenadas de las ocasiones en el mapa de la interestructura.
#' @param p objeto `pta` de ade4.
#' @param niveles nombres de las ocasiones.
#' @return lista con `RV`, `ocasiones` (data.frame), `coordenadas` y `inercia`.
#' @export
interestructura_pta <- function(p, niveles = p$tab.names) {
  RV <- as.matrix(p$RV)
  dimnames(RV) <- list(niveles, niveles)
  pesos <- abs(as.numeric(p$tabw))
  coord <- como_coordenadas(p$RV.coo, 2)
  if (sum(coord[, 1]) < 0) coord[, 1] <- -coord[, 1]   # ocasiones del lado positivo del eje 1
  rownames(coord) <- niveles
  list(
    RV = RV,
    ocasiones = data.frame(Ocasión = niveles, `Peso (vector propio)` = pesos,
                           `Peso (suma 1)` = pesos / sum(pesos),
                           `cos²` = as.numeric(p$cos2), check.names = FALSE, row.names = NULL),
    coordenadas = coord,
    inercia = tabla_inercia(p$RV.eig)
  )
}

#' Matriz RV como tabla
#' @param RV matriz cuadrada de coeficientes RV.
#' @export
tabla_rv <- function(RV) {
  df <- data.frame(Ocasión = rownames(RV), RV, check.names = FALSE, row.names = NULL)
  df
}

#' Resultado de un test de permutación de ade4
#' @param rt objeto `randtest`.
#' @param estadistico nombre del estadístico.
#' @export
prueba_permutacion <- function(rt, estadistico = "RV") {
  list(estadistico = estadistico, observado = rt$obs, p = rt$pvalue, simulados = rt$sim,
       repeticiones = rt$rep, esperado = rt$expvar[["Expectation"]],
       hipotesis = "H0: no existe co-estructura entre X e Y")
}

#' Descomposición de la co-inercia (lo que muestra `summary()` de ade4)
#'
#' Para cada eje: valor propio (co-inercia), covarianza (su raíz), desviación
#' de las coordenadas de X y de Y y su correlación. Además, la inercia que
#' recogen los ejes de co-inercia en cada tabla frente a la máxima posible (la
#' de los ejes de su análisis por separado): una razón cerca de 1 indica que
#' la co-inercia no sacrificó la estructura propia de la tabla.
#' @param co objeto `coinertia` de ade4.
#' @param dX,dY los `dudi` de cada tabla.
#' @param nombres nombres de las tablas.
#' @return lista con `ejes`, `inercia_X`, `inercia_Y` y `RV`.
#' @export
descomposicion_coinercia <- function(co, dX, dY, nombres = c(X = "X", Y = "Y")) {
  nf <- co$nf
  norma <- function(M, w) apply(as.matrix(M), 2, function(v) sqrt(sum(v * v * w) / sum(w)))
  eig <- co$eig[seq_len(nf)]
  covar <- sqrt(eig)
  sdX <- norma(co$lX, dX$lw)
  sdY <- norma(co$lY, dX$lw)
  total <- sum(co$eig)
  ejes <- data.frame(Eje = paste("Eje", seq_len(nf)), `Co-inercia` = eig,
                     `% Co-inercia` = 100 * eig / total, `% Acumulado` = cumsum(100 * eig / total),
                     Covarianza = covar, `Desv. X` = sdX, `Desv. Y` = sdY,
                     Correlación = covar / sdX / sdY, check.names = FALSE, row.names = NULL)
  razon <- function(sd, d) {
    data.frame(Ejes = vapply(seq_len(nf), function(k) paste(seq_len(k), collapse = "-"), ""),
               `Inercia proyectada` = cumsum(sd^2), `Inercia máxima` = cumsum(d$eig[seq_len(nf)]),
               Razón = cumsum(sd^2) / cumsum(d$eig[seq_len(nf)]), check.names = FALSE)
  }
  list(ejes = ejes, inercia_X = razon(sdX, dX), inercia_Y = razon(sdY, dY),
       RV = sum(co$eig) / sqrt(sum(dX$eig^2)) / sqrt(sum(dY$eig^2)))
}

#' Longitud de las flechas de co-estructura en un plano
#'
#' Distancia entre la posición de cada individuo según X y según Y (en
#' coordenadas normadas). Flecha corta: X e Y describen igual a ese individuo
#' (co-estructura fuerte); flecha larga: co-estructura débil.
#' @param res resultado con `filas$X` y `filas$Y`.
#' @param ejes plano.
#' @export
longitud_flechas <- function(res, ejes = c(1, 2)) {
  d <- res$filas$X[, ejes, drop = FALSE] - res$filas$Y[, ejes, drop = FALSE]
  stats::setNames(sqrt(rowSums(d^2)), rownames(res$filas$X))
}

# Texto con decimales en el idioma de la sesión (lectura guiada)
num <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = marca_decimal())
pct <- function(x, d = 2) paste0(num(x, d), " %")
