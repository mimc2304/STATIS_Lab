# =============================================================================
# Co-inercia (Dolédec y Chessel, 1994; Dray, Chessel y Thioulouse, 2003).
#
# Dos tablas X (n × p) e Y (n × q) sobre los mismos individuos, cada una con
# su preparación (pesos de columnas Q_X, Q_Y) y los mismos pesos de filas D.
#   Tabla cruzada:   Z = Y' D X                       (q × p)
#   Descomposición:  SVD de Q_Y^½ Z Q_X^½ -> ejes que maximizan la covarianza
#                    entre las coordenadas de X y las de Y (la co-inercia).
#   RV = traza(W_X D W_Y D) / raíz(traza((W_X D)²) traza((W_Y D)²)),
#        con W = X Q X' (Robert y Escoufier, 1976).
#   Test: se permutan las filas de una tabla (H0: no hay co-estructura).
#
# Motor: coinertia(), validado con la práctica de los ríos del Doubs y
# la del Banco Mundial (tests/testthat/test-coinercia.R).
# =============================================================================

#' Análisis de Co-inercia
#'
#' @param datos data.frame (individuos en filas; nombres de fila = etiquetas).
#' @param variables_x,variables_y variables de cada tabla.
#' @param preparacion_x,preparacion_y códigos de [PREPARACIONES_TABLA].
#' @param ejes número de ejes de co-inercia a conservar.
#' @param permutaciones repeticiones del test de permutación (0 = sin test).
#' @param semilla semilla del test (para que el p-valor sea reproducible).
#' @param nombre_x,nombre_y nombres de las tablas para tablas y gráficos.
#' @return objeto `statislab`.
#' @export
#' @examples
#' doubs <- ejemplo_statis("doubs")
#' b <- BLOQUES_EJEMPLO$doubs
#' res <- coinercia(doubs, b$x, b$y, preparacion_y = "centrar",
#'                  nombre_x = "Ambiente", nombre_y = "Peces")
#' res
coinercia <- function(datos, variables_x, variables_y, preparacion_x = "estandarizar",
                      preparacion_y = "estandarizar", ejes = 2, permutaciones = 999,
                      semilla = 123, nombre_x = "X", nombre_y = "Y") {
  par <- par_tablas(datos, variables_x, variables_y)
  ejes <- max(2L, as.integer(ejes))
  d <- preparar_par_tablas(par, preparacion_x, preparacion_y, ejes = ejes)
  dudiX <- d$X; dudiY <- d$Y
  co <- coinertia(dudiX, dudiY, scannf = FALSE, nf = ejes)
  if (co$nf < 2) stop("La co-estructura tiene un solo eje: no se puede dibujar un plano.", call. = FALSE)
  nf <- co$nf
  nombres <- c(X = nombre_x, Y = nombre_y)
  desc <- descomposicion_coinercia(co, dudiX, dudiY, nombres)

  test <- NULL
  if (permutaciones > 0) {
    if (!is.null(semilla)) set.seed(semilla)
    test <- prueba_permutacion(randtest(co, nrepet = permutaciones, output = "full"))
  }

  filas <- list(X = como_coordenadas(co$mX, nf), Y = como_coordenadas(co$mY, nf))
  rownames(filas$X) <- rownames(filas$Y) <- rownames(par$X)
  variables <- list(X = como_coordenadas(co$co, nf), Y = como_coordenadas(co$li, nf))

  res <- nuevo_resultado(
    "coinercia", "Co-inercia", nombres,
    preparacion = list(X = etiqueta_preparacion(preparacion_x), Y = etiqueta_preparacion(preparacion_y)),
    inercia = tabla_inercia(co$eig, "% Co-inercia"),
    filas = filas, variables = variables, test = test, motor = co,
    llamada = list(variables_x = variables_x, variables_y = variables_y,
                   preparacion_x = preparacion_x, preparacion_y = preparacion_y, ejes = ejes,
                   permutaciones = permutaciones, semilla = semilla,
                   nombre_x = nombre_x, nombre_y = nombre_y))
  res$diagnosticos <- desc
  res$tablas <- tablas_coinercia(res)
  res
}

tablas_coinercia <- function(res, ejes = c(1, 2)) {
  n <- res$nombres
  d <- res$diagnosticos
  lon <- longitud_flechas(res, ejes)
  ind <- data.frame(Individuo = rownames(res$filas$X),
                    stats::setNames(as.data.frame(res$filas$X), paste(n[["X"]], colnames(res$filas$X))),
                    stats::setNames(as.data.frame(res$filas$Y), paste(n[["Y"]], colnames(res$filas$Y))),
                    `Longitud de la flecha` = lon, check.names = FALSE, row.names = NULL)
  tablas <- list(
    d$ejes, res$inercia, d$inercia_X, d$inercia_Y,
    ind[order(ind$`Longitud de la flecha`), ],
    coordenadas_a_tabla(res$variables$X, "Variable"),
    coordenadas_a_tabla(res$variables$Y, "Variable"),
    data.frame(Estadístico = "RV", Valor = d$RV, `p-valor` = res$test$p %||% NA_real_,
               Permutaciones = res$test$repeticiones %||% NA_integer_, check.names = FALSE))
  names(tablas) <- c("Co-inercia por eje", "Valores propios", paste("Inercia de", n[["X"]]),
                     paste("Inercia de", n[["Y"]]), "Individuos", paste("Variables de", n[["X"]]),
                     paste("Variables de", n[["Y"]]), "Coeficiente RV")
  tablas
}

#' Lectura guiada de una Co-inercia
#' @param res resultado de [coinercia()].
#' @param ejes plano que se interpreta.
#' @export
lectura_coinercia <- function(res, ejes = c(1, 2)) {
  n <- res$nombres
  d <- res$diagnosticos
  inr <- res$inercia
  plano <- sum(inr[[3]][ejes])
  lon <- sort(longitud_flechas(res, ejes))
  frases <- c(
    sprintf("El plano %d-%d recoge el %s de la co-inercia entre %s y %s (eje %d: %s).",
            ejes[1], ejes[2], pct(plano), n[["X"]], n[["Y"]], ejes[1], pct(inr[[3]][ejes[1]])),
    sprintf("En el eje 1, las coordenadas de los individuos según %s y según %s tienen una correlación de %s.",
            n[["X"]], n[["Y"]], num(d$ejes$Correlación[1])),
    sprintf("El eje 1 de co-inercia conserva el %s de la inercia que tendría el primer eje del análisis de %s por separado, y el %s en %s.",
            pct(100 * d$inercia_X$Razón[1], 1), n[["X"]], pct(100 * d$inercia_Y$Razón[1], 1), n[["Y"]]),
    sprintf("El coeficiente RV entre las dos tablas es %s (0 = sin relación, 1 = misma estructura).", num(d$RV, 3))
  )
  if (!is.null(res$test)) {
    frases <- c(frases, if (res$test$p < 0.05) {
      sprintf("Test de permutación (%d repeticiones): p = %s < 0,05. Se rechaza H0: existe co-estructura entre %s y %s.",
              res$test$repeticiones, num(res$test$p, 3), n[["X"]], n[["Y"]])
    } else {
      sprintf("Test de permutación (%d repeticiones): p = %s ≥ 0,05. No hay evidencia de co-estructura. Con pocos individuos el test tiene poca potencia: el plano sigue siendo descriptivo.",
              res$test$repeticiones, num(res$test$p, 3))
    })
  }
  frases <- c(frases,
    sprintf("Co-estructura más fuerte (flechas más cortas): %s.",
            paste(sprintf("%s (%s)", names(lon)[1:min(3, length(lon))], num(lon[1:min(3, length(lon))])), collapse = ", ")),
    sprintf("Co-estructura más débil (flechas más largas): %s.",
            paste(sprintf("%s (%s)", rev(names(lon))[1:min(3, length(lon))], num(rev(lon)[1:min(3, length(lon))])), collapse = ", ")),
    "El signo de los ejes es arbitrario: voltear el gráfico no cambia ninguna conclusión.")
  frases
}
