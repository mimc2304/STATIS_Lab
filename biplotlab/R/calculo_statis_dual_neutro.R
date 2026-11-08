# =============================================================================
# STATIS DUAL y STATIS DUAL sobre puntuaciones neutrosóficas.
#
# STATIS DUAL (Lavit, 1988; Abdi et al., 2012): K tablas con LAS MISMAS
# VARIABLES; los individuos pueden coincidir o no entre ocasiones.
#   1. Cada tabla se prepara (por defecto, estandarización parcial: media y
#      desviación de su ocasión) y da su matriz entre variables
#        W_k = X_k' D_k X_k   (p × p; con la parcial, la matriz de correlaciones).
#   2. Interestructura: RV entre las W_k; pesos α = primer vector propio.
#   3. Compromiso: W+ = Σ α_k W_k / ||W_k||  y su descomposición espectral.
#   4. Variables en el compromiso (círculo de correlaciones), individuos de
#      cada ocasión y variables vistas desde cada ocasión (trayectorias).
# Motor: ade4 (preparar_ktabla() + statis()), el mismo de la práctica jv73.
#
# Versión neutrosófica: cada dato llega como una terna (T, I, F) en [0, 1] y
# se convierte en una puntuación
#        S = wT·T + wI·(1 − I) + wF·(1 − F),     wT + wI + wF = 1,
# la deneutrosificación del STATIS Dual neutrosófico de Bustos-Chiliquinga et
# al. (2026). Al centrar, S equivale a wT·T − wI·I − wF·F; con pesos iguales es
# la misma puntuación T − I − F del HJ neutrosófico. Después se aplica el STATIS
# DUAL a la K-tabla de puntuaciones.
#
# Diferencias con el script del artículo, a propósito (ver
# verificacion_neutrosofico/REVISION_DUAL_NEUTROSOFICO.md):
#   - el cálculo es general: las ternas vienen de los datos (columnas _T, _I,
#     _F o respuestas recodificadas); no hay reglas fijas por variable;
#   - las tablas se normalizan como en ade4 y Abdi (2012), así el resultado se
#     reproduce con ade4::statis();
#   - las variables se dibujan en el círculo de correlaciones (escala unitaria);
#   - la calidad se calcula sobre la inercia total de cada variable;
#   - se avisa cuando F = 1 − T (F no aporta información propia).
# =============================================================================

#' Puntuación neutrosófica de una terna (T, I, F)
#'
#' @param T,I,F matrices (o vectores) con valores entre 0 y 1, del mismo tamaño.
#' @param pesos pesos de verdad, determinación (1 − I) y no falsedad (1 − F),
#'   con nombres `T`, `I` y `F`; no negativos. Se normalizan para que sumen 1.
#' @return `wT·T + wI·(1 − I) + wF·(1 − F)`, del mismo tamaño que `T`.
#' @export
#' @examples
#' puntuacion_neutrosofica(0.8, 0.1, 0.2)                      # pesos iguales
#' puntuacion_neutrosofica(0.8, 0.1, 0.2, c(T = .5, I = .3, F = .2))
puntuacion_neutrosofica <- function(T, I, F, pesos = c(T = 1, I = 1, F = 1) / 3) {
  pesos <- normalizar_pesos_tif(pesos)
  comprobar_ternas(T, I, F)
  pesos[["T"]] * T + pesos[["I"]] * (1 - I) + pesos[["F"]] * (1 - F)
}

normalizar_pesos_tif <- function(pesos) {
  if (is.null(names(pesos)) || !setequal(names(pesos), c("T", "I", "F"))) {
    stop("Los pesos de la puntuación deben llamarse T, I y F.", call. = FALSE)
  }
  pesos <- pesos[c("T", "I", "F")]
  if (any(!is.finite(pesos)) || any(pesos < 0) || sum(pesos) <= 0) {
    stop("Los pesos T, I y F deben ser números no negativos y no pueden ser todos 0.", call. = FALSE)
  }
  pesos / sum(pesos)
}

comprobar_ternas <- function(T, I, F) {
  for (n in c("T", "I", "F")) {
    m <- get(n)
    if (!is.numeric(m)) stop("La componente ", n, " debe ser numérica.", call. = FALSE)
    if (anyNA(m)) stop("La componente ", n, " tiene datos faltantes.", call. = FALSE)
    if (any(m < 0 | m > 1)) stop("La componente ", n, " debe estar entre 0 y 1.", call. = FALSE)
  }
  if (!identical(dim(T), dim(I)) || !identical(dim(T), dim(F)) ||
      length(T) != length(I) || length(T) != length(F)) {
    stop("T, I y F deben tener el mismo tamaño.", call. = FALSE)
  }
  invisible(TRUE)
}

# ------------------------------------------------------------- Motor DUAL ----

# STATIS DUAL de una ktabla ya armada. Devuelve las piezas del resultado.
motor_statis_dual <- function(kt, preparacion = "parcial", ejes = 2) {
  ejes <- max(2L, as.integer(ejes))
  p <- preparar_ktabla(kt, preparacion, ejes)
  st <- statis(p$ktab, scannf = FALSE, nf = ejes)
  nf <- min(ejes, st$C.nf %||% ncol(st$C.li), ncol(st$C.li))
  if (nf < 2) stop("El compromiso tiene un solo eje: no se puede dibujar un plano.", call. = FALSE)
  K <- length(p$ktab$blo)

  # Matrices entre variables de cada ocasión, normalizadas como en statis()
  X <- lapply(seq_len(K), function(k) as.matrix(p$ktab[[k]]))
  cw <- split(p$ktab$cw, rep(seq_len(K), p$ktab$blo))
  W <- lapply(seq_len(K), function(k) {
    m <- X[[k]] %*% (cw[[k]] * t(X[[k]]))
    s <- sqrt(sum(m^2))
    if (s == 0) stop("La ocasión «", kt$niveles[k], "» no tiene variación.", call. = FALSE)
    m / s
  })
  a <- abs(as.numeric(st$RV.tabw))
  Wc <- Reduce(`+`, Map(`*`, W, a))
  dimnames(Wc) <- list(colnames(kt$X), colnames(kt$X))

  G <- as.matrix(st$C.li)[, seq_len(nf), drop = FALSE]     # U Λ^(1/2), como ade4
  rownames(G) <- colnames(kt$X)
  lam <- st$C.eig[seq_len(nf)]
  U <- sweep(G, 2, sqrt(lam), "/")
  total <- diag(Wc)                                        # inercia de cada variable

  # Círculo de correlaciones: cada variable con longitud total 1
  circ <- G / sqrt(ifelse(total > 0, total, NA))
  circ[is.na(circ)] <- 0
  # Variables vistas desde cada ocasión: su baricentro (pesos suma 1) es G
  parciales <- lapply(W, function(m) sum(a) * m %*% U %*% diag(1 / sqrt(lam), nrow = nf))
  tv <- do.call(rbind, parciales)
  tray_var <- como_trayectorias(tv, rep(colnames(kt$X), K), rep(kt$niveles, each = ncol(kt$X)), nf, "Variable")
  # Individuos de cada ocasión (columnas de las tablas de ktab.within)
  tray_ind <- como_trayectorias(as.matrix(st$C.Co)[, seq_len(nf), drop = FALSE], kt$individuo,
                                as.character(kt$ocasion), nf)

  inter <- interestructura_pta(list(RV = st$RV, tabw = st$RV.tabw, cos2 = st$cos2,
                                    RV.coo = st$RV.coo, RV.eig = st$RV.eig), kt$niveles)
  list(st = st, nf = nf, inter = inter, compromiso = Wc, G = G, circulo = circ, U = U, lambda = lam,
       total = total, tray_var = tray_var, tray_ind = tray_ind,
       baricentro = max(abs(Reduce(`+`, Map(`*`, parciales, a / sum(a))) - G)))
}

# Calidad (‰ de la inercia de la variable) y contribución (‰ del eje)
indicadores_dual <- function(m) {
  G <- m$G
  cal <- sweep(G^2, 1, ifelse(m$total > 0, m$total, NA), "/") * 1000
  ctr <- sweep(G^2, 2, m$lambda, "/") * 1000
  colnames(cal) <- paste0("Calidad ", seq_len(ncol(G)), " (‰)")
  colnames(ctr) <- paste0("Contribución ", seq_len(ncol(G)), " (‰)")
  list(calidad = cal, contribucion = ctr)
}

tabla_variables_dual <- function(m, ejes = c(1, 2)) {
  ind <- indicadores_dual(m)
  circ <- m$circulo; colnames(circ) <- paste("Círculo", seq_len(ncol(circ)))
  df <- data.frame(Variable = rownames(m$G), m$G, circ, ind$calidad, ind$contribucion,
                   check.names = FALSE, row.names = NULL)
  names(df)[1 + seq_len(ncol(m$G))] <- paste("Eje", seq_len(ncol(m$G)))
  df[[sprintf("Calidad plano %d-%d (‰)", ejes[1], ejes[2])]] <- rowSums(ind$calidad[, ejes, drop = FALSE])
  df
}

#' STATIS DUAL
#'
#' Mismas variables en todas las ocasiones; los individuos pueden coincidir o
#' no. Reproduce `ade4::statis(ktab.within(withinpca(...)))`.
#' @param datos data.frame en formato largo (una fila por individuo y ocasión).
#' @param variables variables numéricas (las mismas en todas las ocasiones).
#' @param ocasion columna de la ocasión.
#' @param individuo columna del individuo, o `NULL` (nombres de fila).
#' @param preparacion código de [PREPARACIONES_KTABLA]; por defecto
#'   `"parcial"` (matrices de correlaciones de cada ocasión).
#' @param ejes ejes del compromiso.
#' @return objeto `statislab`.
#' @export
#' @examples
#' data(jv73, package = "ade4")
#' d <- data.frame(jv73$morpho, Rio = jv73$fac.riv)
#' res <- statis_dual(d, names(jv73$morpho), "Rio")
#' res$inercia
statis_dual <- function(datos, variables, ocasion, individuo = NULL, preparacion = "parcial", ejes = 2) {
  kt <- ktabla_larga(datos, variables, ocasion, individuo)
  m <- motor_statis_dual(kt, preparacion, ejes)
  res <- resultado_dual(m, kt, "statis_dual", "STATIS DUAL", preparacion,
                        list(variables = variables, ocasion = ocasion, individuo = individuo,
                             preparacion_x = preparacion, ejes = ejes))
  res
}

resultado_dual <- function(m, kt, metodo, titulo, preparacion, llamada, nombre = "las tablas") {
  res <- nuevo_resultado(
    metodo, titulo, nombres = c(X = nombre, Y = nombre),
    preparacion = list(X = etiqueta_preparacion(preparacion)),
    inercia = tabla_inercia(m$st$C.eig),
    interestructura = list(X = m$inter),
    variables = list(X = como_coordenadas(m$circulo, m$nf),
                     Y = como_coordenadas(m$circulo, m$nf)[0, , drop = FALSE]),
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
    "Compromiso: variables" = tabla_variables_dual(m),
    "Matriz del compromiso" = coordenadas_a_tabla(m$compromiso, "Variable"),
    "Trayectorias: variables" = m$tray_var,
    "Trayectorias: individuos" = m$tray_ind)
  res
}

# ------------------------------------------------------ Versión neutrosófica --

#' Ternas T, I, F de una hoja en formato largo
#'
#' Lee las ternas como el HJ neutrosófico ([ternas_desde_columnas()] o
#' [ternas_desde_respuestas()]) y conserva la ocasión y el individuo de cada
#' fila que queda.
#' @param datos data.frame en formato largo.
#' @param variables con `formato = "ternas"`, nombres base de las columnas
#'   `nombre_T`, `nombre_I`, `nombre_F`; con `"respuestas"`, las preguntas.
#' @param ocasion,individuo columnas de la ocasión y del individuo (`NULL` =
#'   nombres de fila).
#' @param formato `"ternas"` o `"respuestas"`.
#' @param regla con `"respuestas"`: vector respuesta -> `"T"`, `"I"`, `"F"`,
#'   `"N"` o `"X"`.
#' @return lista con `T`, `I`, `F` (matrices), `ocasion`, `individuo`,
#'   `conservadas` y `resumen`.
#' @export
ternas_ktabla <- function(datos, variables, ocasion, individuo = NULL,
                          formato = c("ternas", "respuestas"), regla = NULL) {
  formato <- match.arg(formato)
  datos <- as.data.frame(datos, check.names = FALSE)
  comprobar_columnas(datos, c(ocasion, individuo))
  t <- if (formato == "ternas") {
    ternas_desde_columnas(datos, variables)
  } else {
    if (is.null(regla)) stop("Falta la regla de recodificación de las respuestas.", call. = FALSE)
    ternas_desde_respuestas(datos, variables, regla)
  }
  comprobar_ternas(t$T, t$I, t$F)
  d <- datos[t$conservadas, , drop = FALSE]
  t$ocasion <- as.character(d[[ocasion]])
  t$individuo <- if (is.null(individuo)) rownames(d) else as.character(d[[individuo]])
  t
}

#' STATIS DUAL sobre puntuaciones neutrosóficas
#'
#' Convierte cada terna (T, I, F) en una puntuación con
#' [puntuacion_neutrosofica()] y aplica [statis_dual()] a la K-tabla de
#' puntuaciones. Además compara el resultado con el STATIS DUAL de la verdad
#' sola (T), para ver qué cambia al incluir la indeterminación y la falsedad.
#' @inheritParams ternas_ktabla
#' @param pesos pesos de la puntuación (`T`, `I`, `F`).
#' @param preparacion código de [PREPARACIONES_KTABLA] (por defecto `"parcial"`).
#' @param ejes ejes del compromiso.
#' @param comparar calcular la comparación con el STATIS DUAL de T.
#' @return objeto `statislab` con, además, `avisos`, `comparacion` y
#'   `indeterminacion`.
#' @export
statis_dual_neutro <- function(datos, variables, ocasion, individuo = NULL,
                               formato = c("ternas", "respuestas"), regla = NULL,
                               pesos = c(T = 1, I = 1, F = 1) / 3, preparacion = "parcial",
                               ejes = 2, comparar = TRUE) {
  formato <- match.arg(formato)
  pesos <- normalizar_pesos_tif(pesos)
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
  kt <- ktabla_larga(hoja(S), vars, ".ocasion", ".individuo")
  m <- motor_statis_dual(kt, preparacion, ejes)
  llamada <- list(variables = variables, ocasion = ocasion, individuo = individuo, formato = formato,
                  regla = regla, pesos = pesos, preparacion_x = preparacion, ejes = ejes)
  res <- resultado_dual(m, kt, "statis_dual_neutro", "STATIS DUAL neutrosófico", preparacion, llamada,
                        nombre = "las puntuaciones")

  # Indeterminación media por variable y ocasión
  occ <- factor(t$ocasion, levels = unique(t$ocasion))
  Imed <- vapply(vars, function(v) tapply(t$I[, v], occ, mean), numeric(nlevels(occ)))
  Imed <- matrix(Imed, nrow = nlevels(occ), dimnames = list(levels(occ), vars))
  res$indeterminacion <- Imed

  res$avisos <- avisos_ternas(t, pesos)
  res$comparacion <- if (comparar) comparar_con_verdad(kt, m, t, preparacion, ejes) else NULL
  if (comparar && is.null(res$comparacion)) {
    res$avisos <- c(res$avisos, "No se pudo calcular el STATIS DUAL de T solo (por ejemplo, T es constante en alguna ocasión); se omite la comparación.")
  }

  pz <- data.frame(Componente = c("Verdad (T)", "Determinación (1 - I)", "No falsedad (1 - F)"),
                   Peso = unname(pesos), check.names = FALSE)
  res$tablas <- c(list("Puntuación: pesos" = pz, "Ternas: resumen" = t$resumen,
                       "Indeterminación media" = coordenadas_a_tabla(Imed, "Ocasión")),
                  res$tablas,
                  if (!is.null(res$comparacion)) list("Comparación con T solo" = res$comparacion$tabla))
  res
}

# Avisos sobre la información de las ternas
avisos_ternas <- function(t, pesos) {
  av <- character()
  if (max(abs(t$T + t$F - 1)) < 1e-8) {
    av <- c(av, sprintf(paste(
      "En estos datos F = 1 - T en todas las celdas: la falsedad no aporta información propia.",
      "La puntuación se reduce a %s·T + %s·(1 - I); solo importa la razón entre esos dos pesos."),
      num(pesos[["T"]] + pesos[["F"]], 3), num(pesos[["I"]], 3)))
  }
  if (max(t$I) == 0) {
    av <- c(av, "I = 0 en todas las celdas: no hay indeterminación y el análisis equivale a un STATIS DUAL de wT·T - wF·F.")
  }
  av
}

# STATIS DUAL de T solo, para ver qué cambia al agregar I y F
comparar_con_verdad <- function(kt, m, t, preparacion, ejes) {
  kT <- kt
  kT$X <- t$T[match(rownames(kt$X), paste(t$ocasion, t$individuo, sep = ".")), colnames(kt$X), drop = FALSE]
  rownames(kT$X) <- rownames(kt$X)
  mT <- tryCatch(motor_statis_dual(kT, preparacion, ejes), error = function(e) NULL)
  if (is.null(mT)) return(NULL)
  ut <- function(M) M[upper.tri(M)]
  q <- min(3L, m$nf, mT$nf)
  r_rv <- if (length(ut(m$inter$RV)) >= 2) stats::cor(ut(m$inter$RV), ut(mT$inter$RV)) else NA_real_
  rho <- stats::cor(m$inter$ocasiones$`Peso (suma 1)`, mT$inter$ocasiones$`Peso (suma 1)`,
                    method = "spearman")
  pr <- correlacion_procrustes(m$circulo[, seq_len(q), drop = FALSE], mT$circulo[, seq_len(q), drop = FALSE])
  tabla <- data.frame(
    Medida = c("Correlación entre las matrices RV (triángulo superior)",
               "Correlación de Spearman entre los pesos",
               sprintf("Correlación de Procrustes de las variables (%d ejes)", q),
               "% de inercia del plano 1-2: puntuación", "% de inercia del plano 1-2: T solo"),
    Valor = c(r_rv, rho, pr, sum(tabla_inercia(m$st$C.eig)[[3]][1:2]),
              sum(tabla_inercia(mT$st$C.eig)[[3]][1:2])),
    check.names = FALSE)
  list(tabla = tabla, rv = r_rv, spearman = rho, procrustes = pr)
}

#' Correlación de Procrustes entre dos configuraciones
#'
#' Centra y normaliza las dos configuraciones y devuelve la suma de los valores
#' singulares de `A'B` (Peres-Neto y Jackson, 2001): 1 = misma forma salvo
#' rotación, reflexión y escala.
#' @param A,B matrices con las mismas filas.
#' @export
correlacion_procrustes <- function(A, B) {
  A <- scale(as.matrix(A), scale = FALSE); B <- scale(as.matrix(B), scale = FALSE)
  na <- sqrt(sum(A^2)); nb <- sqrt(sum(B^2))
  if (na == 0 || nb == 0) return(NA_real_)
  sum(svd(crossprod(A / na, B / nb))$d)
}

# ----------------------------------------------------------- Lectura guiada --

#' Lectura guiada de un STATIS DUAL (clásico o neutrosófico)
#' @param res resultado de [statis_dual()] o [statis_dual_neutro()].
#' @param ejes plano que se interpreta.
#' @export
lectura_statis_dual <- function(res, ejes = c(1, 2)) {
  inr <- res$inercia
  tab <- tabla_variables_dual_res(res, ejes)
  cal <- tab[[sprintf("Calidad plano %d-%d (‰)", ejes[1], ejes[2])]]
  bien <- tab$Variable[cal >= 500]; mal <- tab$Variable[cal < 500]
  c(
    if (identical(res$metodo, "statis_dual_neutro")) sprintf(
      "Cada terna se resumió con S = %s·T + %s·(1 - I) + %s·(1 - F) antes del análisis.",
      num(res$llamada$pesos[["T"]], 3), num(res$llamada$pesos[["I"]], 3), num(res$llamada$pesos[["F"]], 3)),
    res$avisos,
    lectura_interestructura(res$interestructura$X, "las ocasiones"),
    sprintf("El plano %d-%d del compromiso recoge el %s de la inercia de la estructura común entre variables.",
            ejes[1], ejes[2], pct(sum(inr[[3]][ejes]))),
    sprintf("Eje %d: destacan %s.", ejes[1], destacadas(res$variables$X, ejes[1])),
    sprintf("Eje %d: destacan %s.", ejes[2], destacadas(res$variables$X, ejes[2])),
    if (length(bien)) sprintf("Variables bien representadas en el plano (calidad ≥ 500 ‰): %s.", paste(bien, collapse = ", ")),
    if (length(mal)) sprintf("Variables con calidad menor que 500 ‰ en el plano (no interpretar sus ángulos): %s.", paste(mal, collapse = ", ")),
    "En el círculo, dos variables bien representadas con un ángulo pequeño se relacionan en forma directa en el conjunto de las ocasiones; cerca de 180°, en forma inversa; cerca de 90°, sin relación lineal.",
    if (!is.null(res$comparacion)) sprintf(
      "Frente al STATIS DUAL de T solo: correlación entre las matrices RV = %s, Spearman entre los pesos = %s y Procrustes de las variables = %s. Valores cercanos a 1 indican que I y F cambian poco la estructura.",
      num(res$comparacion$rv, 3), num(res$comparacion$spearman, 3), num(res$comparacion$procrustes, 3)),
    "El signo de los ejes es arbitrario: voltear el gráfico no cambia ninguna conclusión.")
}

tabla_variables_dual_res <- function(res, ejes) {
  t <- res$tablas[["Compromiso: variables"]]
  col <- sprintf("Calidad plano %d-%d (‰)", ejes[1], ejes[2])
  if (!col %in% names(t)) {
    cal <- t[paste0("Calidad ", ejes, " (‰)")]
    t[[col]] <- rowSums(cal)
  }
  t
}
