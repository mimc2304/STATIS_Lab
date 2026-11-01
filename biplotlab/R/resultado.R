# =============================================================================
# EL OBJETO DE RESULTADO COMÚN (el "objeto BGL").
#
# Todos los métodos devuelven una lista de clase `statislab` con las mismas
# secciones. Así los gráficos, las tablas, el volteo y las descargas se
# escriben una sola vez (grafico_ktabla.R, diagnosticos_ktabla.R,
# geometria_ktabla.R) y sirven para cualquier método nuevo del grupo.
#
# Coordenadas: matrices con columnas "Eje 1", "Eje 2", ... y nombres de fila.
# Trayectorias: data.frame con Individuo, Ocasión y "Eje 1", "Eje 2", ...
# =============================================================================

#' Crear un resultado de K-tablas
#'
#' @param metodo id del método (`"coinercia"`, `"statico"`, ...).
#' @param titulo nombre para mostrar.
#' @param nombres nombres de las tablas, `c(X = "Ambiente", Y = "Especies")`.
#' @param preparacion lista con la preparación de cada tabla (texto).
#' @param inercia data.frame de [tabla_inercia()].
#' @param interestructura `NULL` o lista con una entrada por serie de tablas
#'   (cada una de [interestructura_pta()]).
#' @param filas lista `X`, `Y` con las coordenadas de los individuos.
#' @param variables lista `X`, `Y` con las coordenadas de las variables.
#' @param trayectorias lista `X`, `Y` con los individuos de cada ocasión.
#' @param trayectorias_variables lista `X`, `Y` con las variables de cada ocasión.
#' @param tablas lista con nombre de data.frames (pestaña Tablas y Excel).
#' @param test `NULL` o lista de [prueba_permutacion()].
#' @param motor objeto original de ade4, para auditar.
#' @param llamada argumentos usados (para la Sintaxis R).
#' @export
nuevo_resultado <- function(metodo, titulo, nombres = c(X = "X", Y = "Y"), preparacion = list(),
                            inercia = NULL, interestructura = NULL, filas = NULL,
                            variables = NULL, trayectorias = NULL,
                            trayectorias_variables = NULL, tablas = list(), test = NULL,
                            motor = NULL, llamada = list()) {
  structure(list(metodo = metodo, titulo = titulo, nombres = nombres, preparacion = preparacion,
                 inercia = inercia, interestructura = interestructura, filas = filas,
                 variables = variables, trayectorias = trayectorias,
                 trayectorias_variables = trayectorias_variables, tablas = tablas, test = test,
                 motor = motor, llamada = llamada, volteados = integer()),
            class = "statislab")
}

#' @export
print.statislab <- function(x, ...) {
  cat(x$titulo, "\n", sep = "")
  if (length(x$preparacion)) {
    for (n in names(x$preparacion)) cat("  Tabla ", x$nombres[[n]], ": ", x$preparacion[[n]], "\n", sep = "")
  }
  if (!is.null(x$inercia)) {
    cat("\nInercia por eje:\n")
    print(utils::head(x$inercia, 5), row.names = FALSE, digits = 4)
  }
  if (!is.null(x$test)) {
    cat(sprintf("\nTest de permutación: RV = %.4f, p = %.3f (%d permutaciones)\n",
                x$test$observado, x$test$p, x$test$repeticiones))
  }
  cat("\nTablas disponibles: ", paste(names(x$tablas), collapse = "; "), "\n", sep = "")
  invisible(x)
}

# Coordenadas de ade4 -> matriz con columnas "Eje 1", "Eje 2", ...
como_coordenadas <- function(m, ejes = ncol(m)) {
  m <- as.matrix(m)[, seq_len(min(ejes, ncol(m))), drop = FALSE]
  colnames(m) <- paste("Eje", seq_len(ncol(m)))
  m
}

# Coordenadas por ocasión -> data.frame Individuo, Ocasión, Eje 1, ...
como_trayectorias <- function(m, individuo, ocasion, ejes = ncol(m), nombre = "Individuo") {
  m <- como_coordenadas(m, ejes)
  if (nrow(m) != length(individuo)) stop("Trayectorias: número de filas inesperado.", call. = FALSE)
  df <- data.frame(individuo, factor(ocasion, levels = unique(ocasion)), m,
                   check.names = FALSE, row.names = NULL)
  names(df)[1:2] <- c(nombre, "Ocasión")
  df
}

# Matriz de coordenadas -> data.frame con la primera columna de nombres
coordenadas_a_tabla <- function(m, nombre = "Elemento") {
  df <- data.frame(rownames(m), m, check.names = FALSE, row.names = NULL)
  names(df)[1] <- nombre
  df
}
