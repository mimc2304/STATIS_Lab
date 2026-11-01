# =============================================================================
# STATICO (Simier et al., 1999; Thioulouse, Simier y Chessel, 2004).
#
# Una secuencia de pares de K tablas (X_k, Y_k): en cada ocasión k, X_k e Y_k
# describen a los mismos individuos; las ocasiones pueden tener individuos
# distintos, pero siempre las mismas variables.
#   1. Co-estructura de cada ocasión:  Z_k = X_k' D_k Y_k        (p × q)
#   2. PTA (X-STATIS) de la serie {Z_k}:
#        interestructura  RV(k, l) entre las tablas cruzadas,
#        pesos            α = primer vector propio de la matriz RV,
#        compromiso       Z+ = Σ α_k Z_k  y su SVD (plano de variables X e Y).
#   3. Intraestructura: los individuos de cada ocasión (filas de X_k y de Y_k)
#      se proyectan sobre los ejes del compromiso -> trayectorias.
# Responde: ¿en qué ocasiones la relación X-Y es estable (la parte común)?
#
# Motor: ade4 (ktab.match2ktabs + pta, lo mismo que ade4::statico() pero con
# el número de ejes elegido). Validado con ade4::statico() y la base meau de
# Thioulouse (2011) (tests/testthat/test-statico.R).
# =============================================================================

#' STATICO: STATIS sobre la co-estructura de pares de K tablas
#'
#' @inheritParams par_ktablas
#' @param preparacion_x,preparacion_y códigos de [PREPARACIONES_KTABLA].
#' @param ejes número de ejes del compromiso.
#' @param nombre_x,nombre_y nombres de las tablas.
#' @return objeto `statislab`.
#' @export
#' @examples
#' meau <- ejemplo_statis("meau")
#' b <- BLOQUES_EJEMPLO$meau
#' res <- statico(meau, b$x, b$y, ocasion = "Estacion", individuo = "Sitio",
#'                preparacion_x = "total", preparacion_y = "centrar")
#' res
statico <- function(datos, variables_x, variables_y, ocasion, individuo = NULL,
                    preparacion_x = "total", preparacion_y = "centrar", ejes = 2,
                    nombre_x = "X", nombre_y = "Y") {
  pk <- par_ktablas(datos, variables_x, variables_y, ocasion, individuo)
  ejes <- max(2L, as.integer(ejes))
  px <- preparar_ktabla(pk$X, preparacion_x, ejes)
  py <- preparar_ktabla(pk$Y, preparacion_y, ejes)
  kcoi <- ktab.match2ktabs(px$ktab, py$ktab)
  st <- pta(kcoi, scannf = FALSE, nf = ejes)
  nf <- st$nf
  if (nf < 2) stop("El compromiso tiene un solo eje: no se puede dibujar un plano.", call. = FALSE)
  nombres <- c(X = nombre_x, Y = nombre_y)
  occ <- pk$X$ocasion; ind <- pk$X$individuo

  tv <- function(m, etiquetas) {
    df <- como_trayectorias(m, as.character(etiquetas[[2]]), as.character(etiquetas[[1]]), nf, "Variable")
    df
  }
  inter <- interestructura_pta(st, pk$X$niveles)
  res <- nuevo_resultado(
    "statico", "STATICO", nombres,
    preparacion = list(X = etiqueta_preparacion(preparacion_x), Y = etiqueta_preparacion(preparacion_y)),
    inercia = tabla_inercia(st$eig),
    interestructura = list(XY = inter),
    variables = list(X = como_coordenadas(st$li, nf), Y = como_coordenadas(st$co, nf)),
    trayectorias = list(X = como_trayectorias(st$supIX, ind, occ, nf),
                        Y = como_trayectorias(st$supIY, ind, occ, nf)),
    trayectorias_variables = list(X = tv(st$Tli, st$TL), Y = tv(st$Tco, st$TC)),
    motor = st,
    llamada = list(variables_x = variables_x, variables_y = variables_y, ocasion = ocasion,
                   individuo = individuo, preparacion_x = preparacion_x,
                   preparacion_y = preparacion_y, ejes = ejes, nombre_x = nombre_x,
                   nombre_y = nombre_y))
  res$ktablas <- resumen_ktabla(pk)
  res$tablas <- tablas_statico(res)
  res
}

tablas_statico <- function(res) {
  n <- res$nombres
  inter <- res$interestructura$XY
  tablas <- list(
    res$ktablas, tabla_rv(inter$RV), inter$ocasiones, inter$inercia, res$inercia,
    coordenadas_a_tabla(res$variables$X, "Variable"),
    coordenadas_a_tabla(res$variables$Y, "Variable"),
    res$trayectorias$X, res$trayectorias$Y)
  names(tablas) <- c("Ocasiones", "Interestructura (RV)", "Pesos y cos²",
                     "Valores propios de la interestructura", "Valores propios del compromiso",
                     paste("Compromiso:", n[["X"]]), paste("Compromiso:", n[["Y"]]),
                     paste("Trayectorias:", n[["X"]]), paste("Trayectorias:", n[["Y"]]))
  tablas
}

# Frases comunes de la interestructura (STATICO y CoSTATIS)
lectura_interestructura <- function(inter, que) {
  oc <- inter$ocasiones
  RV <- inter$RV
  diag(RV) <- NA
  i <- which(RV == max(RV, na.rm = TRUE), arr.ind = TRUE)[1, ]
  j <- which(RV == min(RV, na.rm = TRUE), arr.ind = TRUE)[1, ]
  mejor <- oc$Ocasión[which.max(oc$`cos²`)]
  peor <- oc$Ocasión[which.min(oc$`cos²`)]
  c(
    sprintf("Interestructura de %s: el primer eje recoge el %s de la semejanza entre ocasiones.",
            que, pct(inter$inercia[[3]][1])),
    sprintf("Las ocasiones más parecidas son %s y %s (RV = %s); las más distintas, %s y %s (RV = %s).",
            rownames(RV)[i[1]], colnames(RV)[i[2]], num(max(RV, na.rm = TRUE), 3),
            rownames(RV)[j[1]], colnames(RV)[j[2]], num(min(RV, na.rm = TRUE), 3)),
    sprintf("%s es la ocasión mejor representada por el compromiso (cos² = %s) y %s la peor (cos² = %s): lo que el compromiso muestra vale sobre todo para las ocasiones con cos² alto.",
            mejor, num(max(oc$`cos²`), 3), peor, num(min(oc$`cos²`), 3)),
    sprintf("Pesos en el compromiso (suma 1): %s.",
            paste(sprintf("%s %s", oc$Ocasión, num(oc$`Peso (suma 1)`, 3)), collapse = ", ")))
}

# Variables con más peso en un eje (las de mayor coordenada absoluta)
destacadas <- function(m, eje = 1, k = 3) {
  v <- m[, eje]
  o <- order(-abs(v))[seq_len(min(k, length(v)))]
  paste(sprintf("%s (%s)", rownames(m)[o], num(v[o])), collapse = ", ")
}

#' Lectura guiada de un STATICO
#' @param res resultado de [statico()].
#' @param ejes plano que se interpreta.
#' @export
lectura_statico <- function(res, ejes = c(1, 2)) {
  n <- res$nombres
  inr <- res$inercia
  c(
    lectura_interestructura(res$interestructura$XY,
                            sprintf("las co-estructuras %s-%s", n[["X"]], n[["Y"]])),
    sprintf("El plano %d-%d del compromiso recoge el %s de la co-estructura estable.",
            ejes[1], ejes[2], pct(sum(inr[[3]][ejes]))),
    sprintf("Eje %d: en %s destacan %s; en %s, %s.", ejes[1], n[["X"]],
            destacadas(res$variables$X, ejes[1]), n[["Y"]], destacadas(res$variables$Y, ejes[1])),
    sprintf("Una variable de %s y una de %s en la misma dirección están asociadas en forma estable en todas las ocasiones; en direcciones opuestas, la asociación es inversa.",
            n[["X"]], n[["Y"]]),
    "En «Trayectorias» se ve cómo se ubica cada individuo en cada ocasión respecto a esa estructura común: los cambios grandes de posición indican ocasiones en que el individuo se aparta del patrón estable.",
    "El signo de los ejes es arbitrario: voltear el gráfico no cambia ninguna conclusión.")
}
