# =============================================================================
# CAPA 0 - FUNDAMENTOS: el objeto K-tabla.
#
# Los datos llegan en formato largo: una fila por individuo y ocasión, con una
# columna que dice la ocasión (año, estación, evaluador...), otra (opcional)
# con el nombre del individuo y las variables de las tablas X e Y.
#
#   Ocasion  Sitio  Temp  pH ...  |  Eda  Bsp ...
#   spring   S1     ...           |  ...
#   spring   S2     ...           |  ...
#   summer   S1     ...           |  ...
#
# ktabla_larga() corta esa hoja en K tablas (una por ocasión) y par_ktablas()
# arma las dos series X e Y que usan STATICO y CoSTATIS. Todos los métodos
# reciben estos objetos, así las validaciones se escriben una sola vez.
# =============================================================================

#' Construir una K-tabla desde una hoja en formato largo
#'
#' @param datos data.frame con una fila por individuo y ocasión. Los nombres de
#'   fila se usan como nombre del individuo si no se indica `individuo`.
#' @param variables nombres de las variables numéricas de la tabla.
#' @param ocasion columna que identifica la ocasión (la tercera vía). Las
#'   ocasiones se ordenan como aparecen en los datos, no alfabéticamente (así
#'   «primavera, verano, otoño, invierno» conserva su orden).
#' @param individuo columna con el nombre de cada individuo, o `NULL` para usar
#'   los nombres de fila.
#' @param minimo_por_ocasion número mínimo de individuos en cada ocasión.
#' @return objeto de clase `ktabla`: lista con `X` (matriz con las filas
#'   ordenadas por ocasión), `ocasion` (factor), `individuo`, `niveles`, `K` y
#'   `n` (individuos por ocasión).
#' @export
ktabla_larga <- function(datos, variables, ocasion, individuo = NULL,
                         minimo_por_ocasion = 3) {
  datos <- as.data.frame(datos, check.names = FALSE)
  comprobar_columnas(datos, c(variables, ocasion, individuo))
  if (length(variables) < 2) stop("Cada tabla necesita al menos 2 variables.", call. = FALSE)
  if (ocasion %in% variables) {
    stop("La columna de ocasión («", ocasion, "») no puede ser también una variable.", call. = FALSE)
  }
  X <- matriz_numerica(datos, variables)

  occ <- as.character(datos[[ocasion]])
  if (anyNA(occ)) stop("La columna de ocasión «", ocasion, "» tiene valores vacíos.", call. = FALSE)
  niveles <- unique(occ)
  if (length(niveles) < 2) {
    stop("Se necesitan al menos 2 ocasiones; la columna «", ocasion, "» tiene solo una.", call. = FALSE)
  }
  ind <- if (is.null(individuo)) rownames(datos) else as.character(datos[[individuo]])
  if (anyNA(ind)) stop("La columna de individuos tiene valores vacíos.", call. = FALSE)

  # Ordenar por ocasión respetando el orden de aparición dentro de cada una
  orden <- order(match(occ, niveles), seq_along(occ))
  occ <- factor(occ[orden], levels = niveles)
  ind <- ind[orden]
  X <- X[orden, , drop = FALSE]

  n <- table(occ)
  pocos <- names(n)[n < minimo_por_ocasion]
  if (length(pocos)) {
    stop("Cada ocasión necesita al menos ", minimo_por_ocasion, " individuos. Tienen menos: ",
         paste(pocos, collapse = ", "), ".", call. = FALSE)
  }
  repetidos <- unlist(lapply(split(ind, occ), function(v) v[duplicated(v)]))
  if (length(repetidos)) {
    stop("Hay individuos repetidos dentro de una misma ocasión: ",
         paste(utils::head(unique(repetidos), 6), collapse = ", "), ".", call. = FALSE)
  }
  rownames(X) <- paste(occ, ind, sep = ".")
  structure(list(X = X, ocasion = occ, individuo = ind, niveles = niveles,
                 K = length(niveles), n = c(n)),
            class = "ktabla")
}

#' Dos K-tablas emparejadas (X e Y) sobre los mismos individuos
#'
#' En cada ocasión, la tabla X y la tabla Y describen a los mismos individuos
#' (son las mismas filas de la hoja).
#'
#' @inheritParams ktabla_larga
#' @param variables_x,variables_y variables de cada tabla (sin repetir).
#' @param mismos_individuos si es `TRUE` exige que todas las ocasiones tengan
#'   exactamente los mismos individuos (lo necesita CoSTATIS) y los ordena igual
#'   en todas, según el orden de la primera ocasión.
#' @return objeto de clase `par_ktablas` con `X` e `Y` (dos `ktabla`).
#' @export
par_ktablas <- function(datos, variables_x, variables_y, ocasion, individuo = NULL,
                        mismos_individuos = FALSE, minimo_por_ocasion = 3) {
  comunes <- intersect(variables_x, variables_y)
  if (length(comunes)) {
    stop("Una variable no puede estar en X y en Y a la vez: ", paste(comunes, collapse = ", "),
         ".", call. = FALSE)
  }
  datos <- as.data.frame(datos, check.names = FALSE)
  if (mismos_individuos) datos <- ordenar_individuos_comunes(datos, ocasion, individuo)
  X <- ktabla_larga(datos, variables_x, ocasion, individuo, minimo_por_ocasion)
  Y <- ktabla_larga(datos, variables_y, ocasion, individuo, minimo_por_ocasion)
  structure(list(X = X, Y = Y), class = "par_ktablas")
}

# CoSTATIS hace un PTA de cada serie: la fila i de cada ocasión debe ser el
# mismo individuo. Se comprueba y se ordena como en la primera ocasión.
ordenar_individuos_comunes <- function(datos, ocasion, individuo) {
  comprobar_columnas(datos, c(ocasion, individuo))
  occ <- as.character(datos[[ocasion]])
  ind <- if (is.null(individuo)) rownames(datos) else as.character(datos[[individuo]])
  if (is.null(individuo)) {
    stop("Indica la columna de individuos: CoSTATIS necesita saber qué fila de cada ocasión ",
         "corresponde al mismo individuo.", call. = FALSE)
  }
  niveles <- unique(occ)
  base <- ind[occ == niveles[1]]
  for (k in niveles[-1]) {
    otros <- ind[occ == k]
    if (!setequal(base, otros) || length(base) != length(otros)) {
      faltan <- union(setdiff(base, otros), setdiff(otros, base))
      stop("CoSTATIS necesita los mismos individuos en todas las ocasiones. En «", k,
           "» no coinciden: ", paste(utils::head(faltan, 6), collapse = ", "),
           if (length(faltan) > 6) ", ..." else "", ". Usa STATICO, que admite individuos distintos.",
           call. = FALSE)
    }
  }
  orden <- order(match(occ, niveles), match(ind, base))
  datos[orden, , drop = FALSE]
}

#' Dos tablas sobre los mismos individuos (Co-inercia)
#'
#' @param datos data.frame (nombres de fila = individuos).
#' @param variables_x,variables_y variables de cada tabla.
#' @return lista de clase `par_tablas` con las matrices `X` e `Y`.
#' @export
par_tablas <- function(datos, variables_x, variables_y) {
  datos <- as.data.frame(datos, check.names = FALSE)
  comunes <- intersect(variables_x, variables_y)
  if (length(comunes)) {
    stop("Una variable no puede estar en X y en Y a la vez: ", paste(comunes, collapse = ", "),
         ".", call. = FALSE)
  }
  if (length(variables_x) < 2 || length(variables_y) < 2) {
    stop("Cada tabla (X e Y) necesita al menos 2 variables.", call. = FALSE)
  }
  X <- matriz_numerica(datos, variables_x)
  Y <- matriz_numerica(datos, variables_y)
  if (nrow(X) < 3) stop("Se necesitan al menos 3 individuos.", call. = FALSE)
  structure(list(X = X, Y = Y), class = "par_tablas")
}

#' Resumen de una K-tabla: individuos por ocasión
#' @param x objeto `ktabla` o `par_ktablas`.
#' @export
resumen_ktabla <- function(x) {
  if (inherits(x, "par_ktablas")) {
    r <- resumen_ktabla(x$X)
    r$`Variables Y` <- ncol(x$Y$X)
    names(r)[names(r) == "Variables"] <- "Variables X"
    return(r)
  }
  data.frame(Ocasión = x$niveles, Individuos = as.integer(x$n[x$niveles]),
             Variables = ncol(x$X), check.names = FALSE, row.names = NULL)
}

#' @export
print.ktabla <- function(x, ...) {
  cat("K-tabla:", x$K, "ocasiones,", ncol(x$X), "variables,", nrow(x$X), "filas\n")
  print(resumen_ktabla(x), row.names = FALSE)
  invisible(x)
}

# --- Comprobaciones comunes ---------------------------------------------------

comprobar_columnas <- function(datos, columnas) {
  faltan <- setdiff(columnas, names(datos))
  if (length(faltan)) {
    stop("No existen en los datos: ", paste(faltan, collapse = ", "), ".", call. = FALSE)
  }
}

matriz_numerica <- function(datos, variables) {
  comprobar_columnas(datos, variables)
  no_num <- variables[!vapply(datos[variables], is.numeric, logical(1))]
  if (length(no_num)) {
    stop("Estas variables no son numéricas: ", paste(no_num, collapse = ", "),
         ". Úsalas como ocasión o individuo, no como variables.", call. = FALSE)
  }
  X <- as.matrix(datos[, variables, drop = FALSE])
  storage.mode(X) <- "double"
  if (anyNA(X)) {
    filas <- rownames(datos)[!stats::complete.cases(X)]
    stop("Hay ", sum(is.na(X)), " datos faltantes en las variables elegidas (filas: ",
         paste(utils::head(filas, 6), collapse = ", "), if (length(filas) > 6) ", ..." else "",
         "). Complétalos o elimina esas filas en la hoja de datos.", call. = FALSE)
  }
  rownames(X) <- rownames(datos) %||% as.character(seq_len(nrow(X)))
  X
}
