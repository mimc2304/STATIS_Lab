# =============================================================================
# CoSTATIS (Thioulouse, 2011): Co-inercia de los compromisos de dos análisis
# K-tablas. Es el orden inverso de STATICO: primero la parte estable de cada
# serie y después su relación.
#   1. PTA de la serie X = {X_k} -> compromiso X+ (n × p).
#   2. PTA de la serie Y = {Y_k} -> compromiso Y+ (n × q).
#   3. Co-inercia(X+, Y+): ejes de máxima covarianza entre los dos compromisos.
#   4. Cada ocasión de X y de Y se proyecta como suplementaria -> trayectorias.
#   5. Test de permutación sobre la co-inercia de los compromisos.
# Exige los mismos individuos en todas las ocasiones (cada PTA los alinea).
# Responde: ¿cómo se relacionan las estructuras estables de X y de Y?
#
# Motor: ade4 (pta + dudi.pca + coinertia, los mismos pasos que
# ade4::costatis() pero con el número de ejes elegido). Validado con
# ade4::costatis() y la base meau (tests/testthat/test-costatis.R).
# =============================================================================

#' CoSTATIS: Co-inercia de los compromisos de dos K-tablas
#'
#' @inheritParams statico
#' @param permutaciones repeticiones del test (0 = sin test).
#' @param semilla semilla del test.
#' @return objeto `statislab`.
#' @export
#' @examples
#' meau <- ejemplo_statis("meau")
#' b <- BLOQUES_EJEMPLO$meau
#' res <- costatis(meau, b$x, b$y, ocasion = "Estacion", individuo = "Sitio",
#'                 preparacion_x = "total", preparacion_y = "centrar")
#' res
costatis <- function(datos, variables_x, variables_y, ocasion, individuo,
                     preparacion_x = "total", preparacion_y = "centrar", ejes = 2,
                     permutaciones = 999, semilla = 123, nombre_x = "X", nombre_y = "Y") {
  pk <- par_ktablas(datos, variables_x, variables_y, ocasion, individuo, mismos_individuos = TRUE)
  ejes <- max(2L, as.integer(ejes))
  px <- preparar_ktabla(pk$X, preparacion_x, ejes)
  py <- preparar_ktabla(pk$Y, preparacion_y, ejes)

  ptaX <- pta(px$ktab, scannf = FALSE, nf = ejes)
  ptaY <- pta(py$ktab, scannf = FALSE, nf = ejes)
  dudiX <- dudi.pca(t(ptaX$tab), center = FALSE, scannf = FALSE, nf = ptaX$nf)
  dudiY <- dudi.pca(t(ptaY$tab), center = FALSE, scannf = FALSE, nf = ptaY$nf)
  co <- coinertia(dudiX, dudiY, scannf = FALSE, nf = ejes)
  nf <- co$nf
  if (nf < 2) stop("La co-estructura tiene un solo eje: no se puede dibujar un plano.", call. = FALSE)

  # Ocasiones como suplementarias (igual que ade4::costatis)
  suplementarias <- function(kt, pesos_ejes, lw) {
    U <- as.matrix(pesos_ejes) * unlist(lw)
    do.call(rbind, lapply(seq_along(kt$blo), function(i) {
      S <- t(as.matrix(kt[[i]])) %*% U
      sweep(S, 2, sqrt(colSums(S^2 * dudiX$lw)), "/")
    }))
  }
  supX <- suplementarias(px$ktab, co$c1, co$cw)
  supY <- suplementarias(py$ktab, co$l1, co$lw)

  test <- NULL
  if (permutaciones > 0) {
    if (!is.null(semilla)) set.seed(semilla)
    test <- prueba_permutacion(randtest(co, nrepet = permutaciones, output = "full"))
  }

  nombres <- c(X = nombre_x, Y = nombre_y)
  occ <- pk$X$ocasion; ind <- pk$X$individuo
  individuos <- ind[occ == pk$X$niveles[1]]
  filas <- list(X = como_coordenadas(co$mX, nf), Y = como_coordenadas(co$mY, nf))
  rownames(filas$X) <- rownames(filas$Y) <- individuos
  inter <- list(X = interestructura_pta(ptaX, pk$X$niveles),
                Y = interestructura_pta(ptaY, pk$X$niveles))
  names(inter) <- c("X", "Y")

  res <- nuevo_resultado(
    "costatis", "CoSTATIS", nombres,
    preparacion = list(X = etiqueta_preparacion(preparacion_x), Y = etiqueta_preparacion(preparacion_y)),
    inercia = tabla_inercia(co$eig, "% Co-inercia"),
    interestructura = inter, filas = filas,
    variables = list(X = como_coordenadas(co$co, nf), Y = como_coordenadas(co$li, nf)),
    trayectorias = list(X = como_trayectorias(supX, ind, occ, nf),
                        Y = como_trayectorias(supY, ind, occ, nf)),
    test = test, motor = list(coinercia = co, pta_X = ptaX, pta_Y = ptaY),
    llamada = list(variables_x = variables_x, variables_y = variables_y, ocasion = ocasion,
                   individuo = individuo, preparacion_x = preparacion_x,
                   preparacion_y = preparacion_y, ejes = ejes, permutaciones = permutaciones,
                   semilla = semilla, nombre_x = nombre_x, nombre_y = nombre_y))
  res$diagnosticos <- descomposicion_coinercia(co, dudiX, dudiY, nombres)
  res$ktablas <- resumen_ktabla(pk)
  res$tablas <- tablas_costatis(res)
  res
}

tablas_costatis <- function(res) {
  n <- res$nombres
  base <- tablas_coinercia(res)
  base <- base[names(base) != "Valores propios"]
  extra <- list(res$ktablas,
                tabla_rv(res$interestructura$X$RV), res$interestructura$X$ocasiones,
                tabla_rv(res$interestructura$Y$RV), res$interestructura$Y$ocasiones,
                res$trayectorias$X, res$trayectorias$Y)
  names(extra) <- c("Ocasiones", paste("RV entre ocasiones:", n[["X"]]), paste("Pesos y cos²:", n[["X"]]),
                    paste("RV entre ocasiones:", n[["Y"]]), paste("Pesos y cos²:", n[["Y"]]),
                    paste("Trayectorias:", n[["X"]]), paste("Trayectorias:", n[["Y"]]))
  c(base, extra)
}

#' Lectura guiada de un CoSTATIS
#' @param res resultado de [costatis()].
#' @param ejes plano que se interpreta.
#' @export
lectura_costatis <- function(res, ejes = c(1, 2)) {
  n <- res$nombres
  coin <- lectura_coinercia(res, ejes)
  coin[1] <- sprintf("El plano %d-%d recoge el %s de la co-inercia entre el compromiso de %s y el de %s.",
                     ejes[1], ejes[2], pct(sum(res$inercia[[3]][ejes])), n[["X"]], n[["Y"]])
  c(lectura_interestructura(res$interestructura$X, n[["X"]]),
    lectura_interestructura(res$interestructura$Y, n[["Y"]])[1:3],
    coin,
    "Las trayectorias muestran cada ocasión proyectada sobre los ejes de co-inercia de los compromisos: una ocasión lejos del resto es un momento en que la estructura se aparta de la parte estable.")
}
