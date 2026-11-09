# =============================================================================
# STATIS (directo) y STATIS neutrosófico.
#
# STATIS (Escoufier, 1973; L'Hermier des Plantes, 1976; Lavit et al., 1994;
# Abdi et al., 2012): K tablas medidas sobre LOS MISMOS INDIVIDUOS; las variables
# pueden coincidir o no entre ocasiones.
#   1. Cada tabla se prepara y da su matriz entre individuos
#        W_k = X_k Q_k X_k'        (n × n)  y su operador  W_k D.
#   2. Interestructura: RV entre los operadores; pesos α = primer vector propio.
#   3. Compromiso: W+ = Σ α_k W_k / ||W_k||  y su descomposición espectral:
#      los individuos del compromiso (G = V Λ^(1/2)).
#   4. Intraestructura: cada ocasión proyectada sobre el compromiso
#      (individuos parciales  F_k = s W_k D G Λ^-1, con baricentro G) y las
#      variables de cada ocasión como correlaciones con los ejes del compromiso
#      (círculo de correlaciones).
# Motor: ade4 (ktab.list.dudi() + statis()); validado con ade4::statis() y con
# el cálculo algebraico directo (tests/testthat/test-statis.R).
#
# Es el espejo del STATIS DUAL (calculo_statis_dual_neutro.R): allí se comparan
# las relaciones entre variables; aquí, la configuración de los individuos.
#
# Versión neutrosófica: cada dato llega como una terna (T, I, F) y se resume en
# la puntuación S = wT·T + wI·(1 - I) + wF·(1 - F) de puntuacion_neutrosofica();
# el STATIS se aplica a la K-tabla de puntuaciones (Bustos-Chiliquinga et al.,
# 2026, que lo hacen en su versión DUAL).
# =============================================================================

# ------------------------------------------------------------ Preparación ----

# K-tabla con los mismos individuos en todas las ocasiones, ordenados igual
ktabla_statis <- function(datos, variables, ocasion, individuo) {
  datos <- as.data.frame(datos, check.names = FALSE)
  if (is.null(individuo)) {
    stop("Indica la columna de individuos: STATIS necesita saber qué fila de cada ocasión ",
         "corresponde al mismo individuo.", call. = FALSE)
  }
  datos <- ordenar_individuos_comunes(datos, ocasion, individuo, metodo = "STATIS",
                                      alternativa = "STATIS DUAL")
  ktabla_larga(datos, variables, ocasion, individuo)
}

# Tablas preparadas (una matriz n × p por ocasión), sin usar ade4
# parcial: media y desviación de cada ocasión; total: centra cada ocasión y usa una
# sola desviación para todas; centrar: solo la media de cada ocasión; ninguna: tal
# cual; *_global: media (y desviación) de todas las ocasiones juntas. Las
# desviaciones se calculan con n, como dudi.pca().
tablas_preparadas <- function(kt, preparacion) {
  validas <- c("parcial", "total", "centrar", "ninguna", "centrar_global", "estandarizar_global")
  if (!preparacion %in% validas) stop("Preparación no reconocida: ", preparacion, call. = FALSE)
  X <- kt$X
  occ <- kt$ocasion
  cen <- function(M) sweep(M, 2, colMeans(M))
  esd <- function(M) sqrt(colMeans(M^2))
  if (preparacion == "parcial") {
    for (k in kt$niveles) comprobar_no_constantes(X[occ == k, , drop = FALSE], k)
  } else if (preparacion %in% c("total", "estandarizar_global")) {
    comprobar_no_constantes(X)
  }
  Z <- X
  if (preparacion %in% c("parcial", "total", "centrar")) {
    for (k in kt$niveles) Z[occ == k, ] <- cen(X[occ == k, , drop = FALSE])
  } else if (preparacion %in% c("centrar_global", "estandarizar_global")) {
    Z <- cen(X)
  }
  if (preparacion == "parcial") {
    for (k in kt$niveles) Z[occ == k, ] <- sweep(Z[occ == k, , drop = FALSE], 2, esd(Z[occ == k, , drop = FALSE]), "/")
  } else if (preparacion %in% c("total", "estandarizar_global")) {
    Z <- sweep(Z, 2, esd(Z), "/")
  }
  lapply(stats::setNames(kt$niveles, kt$niveles), function(k) {
    m <- Z[occ == k, , drop = FALSE]
    rownames(m) <- kt$individuo[occ == k]
    m
  })
}

# ------------------------------------------------------------------ Motor ----

# STATIS de una lista de tablas con los mismos individuos (filas en el mismo orden)
motor_statis <- function(tablas, ejes = 2) {
  ejes <- max(2L, as.integer(ejes))
  niveles <- names(tablas)
  ind <- rownames(tablas[[1]])
  K <- length(tablas)
  for (k in seq_len(K)) {
    if (!identical(rownames(tablas[[k]]), ind)) stop("Las tablas deben tener los mismos individuos, en el mismo orden.", call. = FALSE)
    if (all(abs(tablas[[k]]) < 1e-12)) stop("La ocasión «", niveles[k], "» no tiene variación.", call. = FALSE)
  }
  dd <- lapply(tablas, function(m) dudi.pca(as.data.frame(m, check.names = FALSE), center = FALSE,
                                            scale = FALSE, scannf = FALSE, nf = 1))
  kt <- ktab.list.dudi(dd, tabnames = niveles)
  st <- statis(kt, scannf = FALSE, nf = ejes)
  nf <- min(ejes, st$C.nf, ncol(st$C.li))
  if (nf < 2) stop("El compromiso tiene un solo eje: no se puede dibujar un plano.", call. = FALSE)

  lw <- kt$lw
  D <- diag(lw, nrow = length(lw))
  X <- lapply(seq_len(K), function(k) as.matrix(kt[[k]]))
  cw <- split(kt$cw, rep(seq_len(K), kt$blo))
  # Operadores W_k D normalizados como en ade4 (traza de (W_k D)^2 igual a 1)
  W <- lapply(seq_len(K), function(k) {
    m <- X[[k]] %*% (cw[[k]] * t(X[[k]]))
    md <- m %*% D
    m / sqrt(sum(diag(md %*% md)))
  })
  a <- abs(as.numeric(st$RV.tabw))
  Wc <- Reduce(`+`, Map(`*`, W, a))
  dimnames(Wc) <- list(ind, ind)

  G <- as.matrix(st$C.li)[, seq_len(nf), drop = FALSE]
  rownames(G) <- ind
  lam <- st$C.eig[seq_len(nf)]
  total <- diag(Wc)                                       # inercia de cada individuo

  # Individuos de cada ocasión sobre el compromiso (su baricentro, con pesos a/Σa, es G)
  parciales <- lapply(W, function(w) sum(a) * w %*% D %*% G %*% diag(1 / lam, nrow = nf))
  tray_ind <- como_trayectorias(do.call(rbind, parciales), rep(ind, K), rep(niveles, each = length(ind)), nf)

  # Variables de cada ocasión: correlación con los ejes del compromiso
  cor_ej <- lapply(seq_len(K), function(k) {
    r <- vapply(seq_len(nf), function(d) {
      u <- G[, d]
      apply(X[[k]], 2, function(x) sum(lw * x * u) / sqrt(sum(lw * x^2) * sum(lw * u^2)))
    }, numeric(ncol(X[[k]])))
    r <- matrix(r, nrow = ncol(X[[k]]), dimnames = list(colnames(X[[k]]), NULL))
    r[!is.finite(r)] <- 0
    r
  })
  vars <- lapply(cor_ej, rownames)
  tray_var <- como_trayectorias(do.call(rbind, cor_ej), unlist(vars),
                                rep(niveles, vapply(vars, length, 1L)), nf, "Variable")
  mismas <- all(vapply(vars, function(v) setequal(v, vars[[1]]), TRUE))
  if (mismas) {
    pk <- a / sum(a)
    circ <- Reduce(`+`, Map(function(r, w) r[vars[[1]], , drop = FALSE] * w, cor_ej, pk))
  } else {
    circ <- do.call(rbind, Map(function(r, n) { rownames(r) <- paste0(rownames(r), " (", n, ")"); r },
                               cor_ej, niveles))
  }
  circ <- como_coordenadas(circ, nf)

  inter <- interestructura_pta(list(RV = st$RV, tabw = st$RV.tabw, cos2 = st$cos2,
                                    RV.coo = st$RV.coo, RV.eig = st$RV.eig), niveles)
  list(st = st, nf = nf, inter = inter, compromiso = Wc, G = G, lambda = lam, total = total, lw = lw,
       circulo = circ, tray_ind = tray_ind, tray_var = tray_var, individuos = ind, niveles = niveles,
       variables_iguales = mismas,
       baricentro = max(abs(Reduce(`+`, Map(`*`, parciales, a / sum(a))) - G)))
}

# Calidad (‰ de la inercia del individuo) y contribución (‰ del eje)
indicadores_statis <- function(m) {
  cal <- sweep(m$G^2, 1, ifelse(m$total > 0, m$total, NA), "/") * 1000
  ctr <- sweep(m$G^2 * m$lw, 2, m$lambda, "/") * 1000
  colnames(cal) <- paste0("Calidad ", seq_len(ncol(m$G)), " (‰)")
  colnames(ctr) <- paste0("Contribución ", seq_len(ncol(m$G)), " (‰)")
  list(calidad = cal, contribucion = ctr)
}

tabla_individuos_statis <- function(m, ejes = c(1, 2)) {
  ind <- indicadores_statis(m)
  df <- data.frame(Individuo = rownames(m$G), m$G, ind$calidad, ind$contribucion,
                   check.names = FALSE, row.names = NULL)
  names(df)[1 + seq_len(ncol(m$G))] <- paste("Eje", seq_len(ncol(m$G)))
  df[[sprintf("Calidad plano %d-%d (‰)", ejes[1], ejes[2])]] <- rowSums(ind$calidad[, ejes, drop = FALSE])
  df
}

resultado_statis <- function(m, kt, metodo, titulo, preparacion, llamada, nombre = "las tablas") {
  res <- nuevo_resultado(
    metodo, titulo, nombres = c(X = nombre, Y = nombre),
    preparacion = list(X = etiqueta_preparacion(preparacion)),
    inercia = tabla_inercia(m$st$C.eig),
    interestructura = list(X = m$inter),
    filas = list(X = m$G),
    variables = list(X = m$circulo, Y = m$circulo[0, , drop = FALSE]),
    trayectorias = list(X = m$tray_ind),
    trayectorias_variables = list(X = m$tray_var),
    motor = m$st, llamada = llamada)
  res$ktablas <- resumen_ktabla(kt)
  res$compromiso <- m$compromiso
  res$control <- c(baricentro = m$baricentro)
  res$tablas <- list(
    "Ocasiones" = res$ktablas,
    "Interestructura (RV)" = tabla_rv(m$inter$RV),
    "Pesos y cos²" = m$inter$ocasiones,
    "Valores propios de la interestructura" = m$inter$inercia,
    "Valores propios del compromiso" = res$inercia,
    "Compromiso: individuos" = tabla_individuos_statis(m),
    "Compromiso: variables" = coordenadas_a_tabla(m$circulo, "Variable"),
    "Matriz del compromiso" = coordenadas_a_tabla(m$compromiso, "Individuo"),
    "Trayectorias: individuos" = m$tray_ind,
    "Trayectorias: variables" = m$tray_var)
  res
}

# ------------------------------------------------------------- Funciones ----

#' STATIS
#'
#' Los mismos individuos en todas las ocasiones; las variables son las mismas
#' columnas en cada ocasión. Reproduce `ade4::statis()` sobre una tabla por ocasión.
#' Para tablas con variables distintas, usa [statis_bloques()].
#' @param datos data.frame en formato largo (una fila por individuo y ocasión).
#' @param variables variables numéricas.
#' @param ocasion columna de la ocasión.
#' @param individuo columna del individuo (obligatoria: identifica la fila de cada
#'   ocasión que corresponde al mismo individuo).
#' @param preparacion código de [PREPARACIONES_KTABLA]; por defecto `"parcial"`
#'   (cada variable estandarizada dentro de su ocasión).
#' @param ejes ejes del compromiso.
#' @return objeto `statislab`.
#' @export
#' @examples
#' meau <- ejemplo_statis("meau")
#' b <- BLOQUES_EJEMPLO$meau
#' res <- statis_directo(meau, b$x, "Estacion", "Sitio")
#' res$inercia
statis_directo <- function(datos, variables, ocasion, individuo, preparacion = "parcial", ejes = 2) {
  kt <- ktabla_statis(datos, variables, ocasion, individuo)
  m <- motor_statis(tablas_preparadas(kt, preparacion), ejes)
  resultado_statis(m, kt, "statis", "STATIS", preparacion,
                   list(variables = variables, ocasion = ocasion, individuo = individuo,
                        preparacion_x = preparacion, ejes = ejes))
}

#' STATIS con tablas de variables distintas (formato ancho)
#'
#' Cada bloque es un conjunto de columnas de la misma hoja; las filas son los
#' individuos, así que son los mismos en todos los bloques. Los bloques pueden
#' tener distintas variables y distinto número de ellas.
#' @param datos data.frame con una fila por individuo (nombres de fila = individuos).
#' @param bloques lista con nombre: cada elemento son las columnas de una tabla.
#' @param preparacion `"parcial"` (estandarizar cada tabla), `"centrar"` o `"ninguna"`.
#' @param ejes ejes del compromiso.
#' @return objeto `statislab`.
#' @export
#' @examples
#' d <- ejemplo_statis("frutas")
#' res <- statis_bloques(d, list(Descriptores = names(d)[2:6], Jueces = paste0("Juez", 1:6)))
#' res$interestructura$X$ocasiones
statis_bloques <- function(datos, bloques, preparacion = "parcial", ejes = 2) {
  datos <- as.data.frame(datos, check.names = FALSE)
  if (!is.list(bloques) || length(bloques) < 2 || is.null(names(bloques)) || any(!nzchar(names(bloques)))) {
    stop("Indica al menos 2 bloques con nombre: list(Tabla1 = c(...), Tabla2 = c(...)).", call. = FALSE)
  }
  if (!preparacion %in% c("parcial", "centrar", "ninguna")) {
    stop("Con bloques, la preparación puede ser «parcial», «centrar» o «ninguna».", call. = FALSE)
  }
  if (nrow(datos) < 3) stop("Se necesitan al menos 3 individuos.", call. = FALSE)
  tablas <- lapply(bloques, function(v) {
    comprobar_columnas(datos, v)
    if (length(v) < 2) stop("Cada bloque necesita al menos 2 variables.", call. = FALSE)
    M <- matriz_numerica(datos, v)
    if (preparacion != "ninguna") M <- sweep(M, 2, colMeans(M))
    if (preparacion == "parcial") {
      comprobar_no_constantes(M)
      M <- sweep(M, 2, sqrt(colMeans(M^2)), "/")
    }
    rownames(M) <- rownames(datos)
    M
  })
  m <- motor_statis(tablas, ejes)
  kt <- structure(list(niveles = names(bloques), n = stats::setNames(rep(nrow(datos), length(bloques)), names(bloques)),
                       X = matrix(numeric(), 0, 0)), class = "ktabla")
  res <- resultado_statis(m, kt, "statis", "STATIS", preparacion,
                          list(bloques = bloques, preparacion_x = preparacion, ejes = ejes))
  res$ktablas <- data.frame(Ocasión = names(bloques), Individuos = nrow(datos),
                            Variables = vapply(bloques, length, 1L), check.names = FALSE, row.names = NULL)
  res$tablas[["Ocasiones"]] <- res$ktablas
  res
}

#' STATIS sobre puntuaciones neutrosóficas
#'
#' Convierte cada terna (T, I, F) en una puntuación con [puntuacion_neutrosofica()]
#' y aplica [statis_directo()] a la K-tabla de puntuaciones. Compara el resultado con
#' el STATIS de la verdad sola (T), para ver qué cambia al incluir la
#' indeterminación y la falsedad.
#' @inheritParams ternas_ktabla
#' @param pesos pesos de la puntuación (`T`, `I`, `F`).
#' @param preparacion código de [PREPARACIONES_KTABLA].
#' @param ejes ejes del compromiso.
#' @param comparar calcular la comparación con el STATIS de T.
#' @return objeto `statislab` con, además, `avisos`, `comparacion` e `indeterminacion`.
#' @export
statis_neutro <- function(datos, variables, ocasion, individuo,
                          formato = c("ternas", "respuestas"), regla = NULL,
                          pesos = c(T = 1, I = 1, F = 1) / 3, preparacion = "parcial",
                          ejes = 2, comparar = TRUE) {
  formato <- match.arg(formato)
  pesos <- normalizar_pesos_tif(pesos)
  if (is.null(individuo)) {
    stop("Indica la columna de individuos: STATIS necesita saber qué fila de cada ocasión ",
         "corresponde al mismo individuo.", call. = FALSE)
  }
  t <- ternas_ktabla(datos, variables, ocasion, individuo, formato, regla)
  S <- puntuacion_neutrosofica(t$T, t$I, t$F, pesos)
  vars <- colnames(S)
  hoja <- function(M) {
    df <- data.frame(M, check.names = FALSE)
    df$.ocasion <- t$ocasion
    df$.individuo <- t$individuo
    rownames(df) <- NULL
    df
  }
  kt <- ktabla_statis(hoja(S), vars, ".ocasion", ".individuo")
  m <- motor_statis(tablas_preparadas(kt, preparacion), ejes)
  llamada <- list(variables = variables, ocasion = ocasion, individuo = individuo, formato = formato,
                  regla = regla, pesos = pesos, preparacion_x = preparacion, ejes = ejes)
  res <- resultado_statis(m, kt, "statis_neutro", "STATIS neutrosófico", preparacion, llamada,
                          nombre = "las puntuaciones")

  occ <- factor(t$ocasion, levels = unique(t$ocasion))
  Imed <- vapply(vars, function(v) tapply(t$I[, v], occ, mean), numeric(nlevels(occ)))
  Imed <- matrix(Imed, nrow = nlevels(occ), dimnames = list(levels(occ), vars))
  res$indeterminacion <- Imed

  res$avisos <- avisos_ternas(t, pesos)
  res$comparacion <- if (comparar) comparar_statis_con_verdad(kt, m, t, preparacion, ejes) else NULL
  if (comparar && is.null(res$comparacion)) {
    res$avisos <- c(res$avisos, "No se pudo calcular el STATIS de T solo (por ejemplo, T es constante en alguna ocasión); se omite la comparación.")
  }
  pz <- data.frame(Componente = c("Verdad (T)", "Determinación (1 - I)", "No falsedad (1 - F)"),
                   Peso = unname(pesos), check.names = FALSE)
  res$tablas <- c(list("Puntuación: pesos" = pz, "Ternas: resumen" = t$resumen,
                       "Indeterminación media" = coordenadas_a_tabla(Imed, "Ocasión")),
                  res$tablas,
                  if (!is.null(res$comparacion)) list("Comparación con T solo" = res$comparacion$tabla))
  res
}

# STATIS de T solo, para ver qué cambia al agregar I y F
comparar_statis_con_verdad <- function(kt, m, t, preparacion, ejes) {
  kT <- kt
  kT$X <- t$T[match(rownames(kt$X), paste(t$ocasion, t$individuo, sep = ".")), colnames(kt$X), drop = FALSE]
  rownames(kT$X) <- rownames(kt$X)
  mT <- tryCatch(motor_statis(tablas_preparadas(kT, preparacion), ejes), error = function(e) NULL)
  if (is.null(mT)) return(NULL)
  ut <- function(M) M[upper.tri(M)]
  q <- min(3L, m$nf, mT$nf)
  r_rv <- if (length(ut(m$inter$RV)) >= 2) stats::cor(ut(m$inter$RV), ut(mT$inter$RV)) else NA_real_
  rho <- stats::cor(m$inter$ocasiones$`Peso (suma 1)`, mT$inter$ocasiones$`Peso (suma 1)`, method = "spearman")
  pr <- correlacion_procrustes(m$G[, seq_len(q), drop = FALSE], mT$G[, seq_len(q), drop = FALSE])
  tabla <- data.frame(
    Medida = c("Correlación entre las matrices RV (triángulo superior)",
               "Correlación de Spearman entre los pesos",
               sprintf("Correlación de Procrustes de los individuos (%d ejes)", q),
               "% de inercia del plano 1-2: puntuación", "% de inercia del plano 1-2: T solo"),
    Valor = c(r_rv, rho, pr, sum(tabla_inercia(m$st$C.eig)[[3]][1:2]), sum(tabla_inercia(mT$st$C.eig)[[3]][1:2])),
    check.names = FALSE)
  list(tabla = tabla, rv = r_rv, spearman = rho, procrustes = pr)
}

# ----------------------------------------------------------- Lectura guiada --

#' Lectura guiada de un STATIS (clásico o neutrosófico)
#' @param res resultado de [statis_directo()], [statis_bloques()] o [statis_neutro()].
#' @param ejes plano que se interpreta.
#' @export
lectura_statis <- function(res, ejes = c(1, 2)) {
  inr <- res$inercia
  tab <- tabla_individuos_statis_res(res, ejes)
  cal <- tab[[sprintf("Calidad plano %d-%d (‰)", ejes[1], ejes[2])]]
  bien <- tab$Individuo[cal >= 500]; mal <- tab$Individuo[cal < 500]
  recorte <- function(x) if (length(x) > 8) paste0(paste(x[1:8], collapse = ", "), ", ... (", length(x), " en total)") else paste(x, collapse = ", ")
  tr <- res$trayectorias$X
  c(
    if (identical(res$metodo, "statis_neutro")) sprintf(
      "Cada terna se resumió con S = %s·T + %s·(1 - I) + %s·(1 - F) antes del análisis.",
      num(res$llamada$pesos[["T"]], 3), num(res$llamada$pesos[["I"]], 3), num(res$llamada$pesos[["F"]], 3)),
    res$avisos,
    lectura_interestructura(res$interestructura$X, "las ocasiones"),
    sprintf("El plano %d-%d del compromiso recoge el %s de la inercia de la configuración común de los individuos.",
            ejes[1], ejes[2], pct(sum(inr[[3]][ejes]))),
    sprintf("Eje %d: los individuos que más se separan son %s.", ejes[1], destacadas(res$filas$X, ejes[1])),
    sprintf("Eje %d: los individuos que más se separan son %s.", ejes[2], destacadas(res$filas$X, ejes[2])),
    if (length(bien)) sprintf("Individuos bien representados en el plano (calidad ≥ 500 ‰): %s.", recorte(bien)),
    if (length(mal)) sprintf("Individuos con calidad menor que 500 ‰ (no interpretar sus distancias): %s.", recorte(mal)),
    "En el círculo de correlaciones, una variable cuya flecha llega cerca del círculo está bien representada: su flecha apunta hacia los individuos donde toma valores altos; dos flechas con ángulo pequeño se relacionan en forma directa, cerca de 180° en forma inversa.",
    if (!is.null(tr)) "En «Trayectorias» cada individuo une sus posiciones en las distintas ocasiones: un recorrido largo indica un individuo cuya posición cambió mucho respecto al patrón común.",
    if (!is.null(res$comparacion)) sprintf(
      "Frente al STATIS de T solo: correlación entre las matrices RV = %s, Spearman entre los pesos = %s y Procrustes de los individuos = %s. Valores cercanos a 1 indican que I y F cambian poco la estructura.",
      num(res$comparacion$rv, 3), num(res$comparacion$spearman, 3), num(res$comparacion$procrustes, 3)),
    "El signo de los ejes es arbitrario: voltear el gráfico no cambia ninguna conclusión.")
}

tabla_individuos_statis_res <- function(res, ejes) {
  t <- res$tablas[["Compromiso: individuos"]]
  col <- sprintf("Calidad plano %d-%d (‰)", ejes[1], ejes[2])
  if (!col %in% names(t)) t[[col]] <- rowSums(t[paste0("Calidad ", ejes, " (‰)")])
  t
}
