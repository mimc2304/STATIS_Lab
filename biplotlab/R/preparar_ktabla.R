# =============================================================================
# CAPA 1 - PREPROCESAMIENTO: todo lo que cambia la tabla antes de factorizar.
#
# Cada tabla se convierte en un objeto `dudi` de ade4 (el "triplete" tabla,
# pesos de columnas, pesos de filas). La elección queda guardada en el
# resultado para que la Sintaxis R y la lectura guiada la expliquen.
# =============================================================================

#' Preparaciones para una tabla (Co-inercia)
#'
#' Vector con nombre: etiqueta que ve el usuario y código para [preparar_tabla()].
#' @export
PREPARACIONES_TABLA <- c(
  "Sin transformar"                     = "ninguna",
  "Estandarizar columnas (ACP normado)" = "estandarizar",
  "Centrar columnas (ACP centrado)"     = "centrar",
  "Abundancias o frecuencias (AFC)"     = "afc"
)

#' Preparaciones para una K-tabla (STATICO, CoSTATIS)
#'
#' En tres vías no es lo mismo usar la media o la desviación de cada ocasión que
#' las de todas las ocasiones juntas. `"parcial"` y `"total"` son las dos de
#' Bouroche (1975), como `withinpca()` de ade4:
#' - `"ninguna"`: los datos tal como están.
#' - `"centrar"`: se resta la media de la ocasión del dato.
#' - `"centrar_global"`: se resta la media de todas las ocasiones.
#' - `"parcial"`: media y desviación de la ocasión del dato.
#' - `"total"`: media de la ocasión del dato y una sola desviación para todas
#'   las ocasiones, la de la tabla ya centrada por ocasión (conserva las
#'   diferencias de dispersión entre ocasiones).
#' - `"estandarizar_global"`: media y desviación de todas las ocasiones.
#' @export
PREPARACIONES_KTABLA <- c(
  "Sin transformar"                                       = "ninguna",
  "Centrar: media de cada ocasión"                        = "centrar",
  "Centrar: media global"                                 = "centrar_global",
  "Estandarizar: media y desviación de cada ocasión (parcial)" = "parcial",
  "Estandarizar: media de cada ocasión y desviación común (total)" = "total",
  "Estandarizar: media y desviación globales"             = "estandarizar_global"
)

#' Prepara una tabla y la devuelve como `dudi` de ade4
#'
#' @param X matriz numérica (individuos en filas).
#' @param preparacion código de [PREPARACIONES_TABLA].
#' @param pesos_filas pesos de los individuos (los fija la AFC de la otra
#'   tabla); `NULL` da pesos iguales.
#' @param ejes número de ejes que conserva el `dudi`.
#' @export
preparar_tabla <- function(X, preparacion = "estandarizar", pesos_filas = NULL, ejes = 2) {
  df <- as.data.frame(X, check.names = FALSE)
  switch(
    preparacion,
    # center y scale van escritos como TRUE o FALSE: el test de ade4 vuelve a leer
    # la llamada de dudi.pca() y no encuentra una variable local
    ninguna = {
      w <- pesos_filas %||% rep(1 / nrow(df), nrow(df))
      dudi.pca(df, row.w = w, center = FALSE, scale = FALSE, scannf = FALSE, nf = ejes)
    },
    estandarizar = {
      comprobar_no_constantes(X)
      w <- pesos_filas %||% rep(1 / nrow(df), nrow(df))
      dudi.pca(df, row.w = w, scale = TRUE, scannf = FALSE, nf = ejes)
    },
    centrar = {
      w <- pesos_filas %||% rep(1 / nrow(df), nrow(df))
      dudi.pca(df, row.w = w, scale = FALSE, scannf = FALSE, nf = ejes)
    },
    afc = {
      if (any(X < 0)) stop("La AFC necesita valores mayores o iguales que 0 (abundancias o frecuencias).",
                           call. = FALSE)
      vacias <- rownames(X)[rowSums(X) == 0]
      if (length(vacias)) {
        stop("La AFC no admite individuos con todos los valores en 0: ",
             paste(utils::head(vacias, 6), collapse = ", "), ".", call. = FALSE)
      }
      if (any(colSums(X) == 0)) stop("La AFC no admite variables con todos los valores en 0.", call. = FALSE)
      dudi.coa(df, scannf = FALSE, nf = ejes)
    },
    stop("Preparación no reconocida: ", preparacion, call. = FALSE)
  )
}

#' Prepara las dos tablas de una Co-inercia
#'
#' La Co-inercia exige los mismos pesos de filas en las dos tablas. Si una
#' tabla se analiza con AFC, la otra toma sus pesos de filas (como en la
#' práctica de los ríos del Doubs con ade4).
#' @param par objeto de [par_tablas()].
#' @param preparacion_x,preparacion_y códigos de [PREPARACIONES_TABLA].
#' @param ejes ejes de cada `dudi`.
#' @return lista con los `dudi` `X` e `Y`.
#' @export
preparar_par_tablas <- function(par, preparacion_x = "estandarizar", preparacion_y = "estandarizar",
                                ejes = 2) {
  if (preparacion_x == "afc" && preparacion_y == "afc") {
    stop("Las dos tablas no pueden ir con AFC a la vez: cada AFC da pesos distintos a los ",
         "individuos y la Co-inercia necesita los mismos. Analiza una con AFC y la otra con ACP.",
         call. = FALSE)
  }
  if (preparacion_x == "afc") {
    dX <- preparar_tabla(par$X, "afc", ejes = ejes)
    dY <- preparar_tabla(par$Y, preparacion_y, pesos_filas = dX$lw, ejes = ejes)
  } else if (preparacion_y == "afc") {
    dY <- preparar_tabla(par$Y, "afc", ejes = ejes)
    dX <- preparar_tabla(par$X, preparacion_x, pesos_filas = dY$lw, ejes = ejes)
  } else {
    dX <- preparar_tabla(par$X, preparacion_x, ejes = ejes)
    dY <- preparar_tabla(par$Y, preparacion_y, ejes = ejes)
  }
  list(X = dX, Y = dY)
}

#' Prepara una K-tabla y la devuelve como `ktab` de ade4
#'
#' El `ktab` que devuelve `ktab.within()` tiene una tabla por ocasión con las
#' **variables en filas y los individuos en columnas**, que es lo que esperan
#' `statico()` y `costatis()`.
#' @param kt objeto [ktabla_larga()].
#' @param preparacion código de [PREPARACIONES_KTABLA].
#' @param ejes ejes del análisis intra-ocasión.
#' @return lista con `dudi` (el análisis intra-ocasión) y `ktab`.
#' @export
preparar_ktabla <- function(kt, preparacion = "total", ejes = 2) {
  df <- as.data.frame(kt$X, check.names = FALSE)
  # Parcial divide por la desviación dentro de cada ocasión; total, por la global
  if (preparacion == "parcial") {
    for (k in kt$niveles) comprobar_no_constantes(kt$X[kt$ocasion == k, , drop = FALSE], k)
  } else if (preparacion %in% c("total", "estandarizar_global")) {
    comprobar_no_constantes(kt$X)
  }
  intra <- function() wca(dudi.pca(df, scale = FALSE, scannf = FALSE, nf = ejes),
                          kt$ocasion, scannf = FALSE, nf = ejes)
  dudi <- switch(
    preparacion,
    parcial = withinpca(df, kt$ocasion, scaling = "partial", scannf = FALSE, nf = ejes),
    total   = withinpca(df, kt$ocasion, scaling = "total", scannf = FALSE, nf = ejes),
    centrar = intra(),
    # ade4 no trae estas: ktab.within() solo lee tab y los pesos, así que se usa
    # el análisis intra-ocasión como molde y se cambia su tabla
    ninguna = , centrar_global = , estandarizar_global = {
      molde <- intra()
      molde$tab[] <- tabla_sin_centrar_por_ocasion(df, preparacion)
      molde
    },
    stop("Preparación no reconocida: ", preparacion, call. = FALSE)
  )
  list(dudi = dudi, ktab = ktab.within(dudi, colnames = kt$individuo))
}

# Tabla con transformaciones que no usan la media de cada ocasión. La desviación
# se calcula con n, como dudi.pca() y withinpca()
tabla_sin_centrar_por_ocasion <- function(df, preparacion) {
  if (preparacion == "ninguna") return(df)
  centrada <- as.data.frame(scale(df, center = TRUE, scale = FALSE))
  if (preparacion == "centrar_global") return(centrada)
  centrada / rep(sqrt(colMeans(as.matrix(centrada)^2)), each = nrow(centrada))
}

# Texto corto de la preparación (tablas y lectura guiada)
etiqueta_preparacion <- function(codigo) {
  todas <- c(PREPARACIONES_TABLA, PREPARACIONES_KTABLA)
  names(todas)[match(codigo, todas)] %||% codigo
}

comprobar_no_constantes <- function(X, ocasion = NULL) {
  s <- apply(X, 2, stats::sd)
  constantes <- colnames(X)[!is.finite(s) | s == 0]
  if (length(constantes)) {
    stop("Estas variables son constantes", if (!is.null(ocasion)) paste0(" en la ocasión «", ocasion, "»"),
         " y no se pueden estandarizar: ", paste(constantes, collapse = ", "),
         ". Quítalas o elige una opción sin estandarizar.", call. = FALSE)
  }
}
