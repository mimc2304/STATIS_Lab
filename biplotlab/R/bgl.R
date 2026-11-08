# =============================================================================
# OBJETO BGL de los métodos K-tablas: reúne en secciones lo que produce cada capa
# (Preprocessing, Factorization, Geometry, Diagnostics, Graphics, Methods, History
# y Options). No calcula nada nuevo: se arma con el resultado de nuevo_resultado(),
# así que todo método que use ese resultado lo tiene sin más trabajo.
# =============================================================================

# Gráficos que ofrece cada método en BGL_Plot()
GRAFICOS_BGL <- list(
  coinercia = c("coestructura", "variables", "test"),
  statico = c("interestructura", "pesos", "variables", "trayectorias"),
  costatis = c("interestructura", "pesos", "coestructura", "variables", "trayectorias", "test"),
  statis_dual = c("interestructura", "pesos", "circulo", "trayectorias"),
  statis_dual_neutro = c("interestructura", "pesos", "circulo", "trayectorias", "indeterminacion")
)

#' Crear un objeto BGL
#'
#' @param x resultado de un método K-tablas (por ejemplo, de [statico()]) o un
#'   data.frame con los datos.
#' @param metodo si `x` son datos, nombre de la función que los analiza:
#'   `"coinercia"`, `"statico"`, `"costatis"`, `"statis_dual"` o `"statis_dual_neutro"`.
#' @param ... argumentos para esa función.
#' @return objeto de clase `BGL`.
#' @examples
#' meau <- ejemplo_statis("meau")
#' b <- BLOQUES_EJEMPLO$meau
#' bgl <- BGL_Create(meau, "statico", variables_x = b$x, variables_y = b$y,
#'                   ocasion = "Estacion", individuo = "Sitio")
#' BGL_Summary(bgl)
#' @export
BGL_Create <- function(x, metodo = NULL, ...) {
  res <- x
  if (!inherits(x, "statislab")) {
    if (!is.data.frame(x) || is.null(metodo)) {
      stop("Indica un resultado K-tablas, o unos datos con su método.", call. = FALSE)
    }
    metodo <- match.arg(metodo, c("coinercia", "statico", "costatis", "statis_dual", "statis_dual_neutro"))
    res <- switch(metodo,
                  coinercia = coinercia(x, ...),
                  statico = statico(x, ...),
                  costatis = costatis(x, ...),
                  statis_dual = statis_dual(x, ...),
                  statis_dual_neutro = statis_dual_neutro(x, ...))
  }
  a <- res$llamada
  bgl <- list(
    Preprocessing = list(X = a$preparacion_x, Y = a$preparacion_y, descripcion = res$preparacion,
                         nombres = res$nombres, variables_x = a$variables_x %||% a$variables, variables_y = a$variables_y,
                         ocasion = a$ocasion, individuo = a$individuo),
    Factorization = list(motor = class(res$motor), valores_propios = res$inercia, ejes = a$ejes),
    Geometry = list(filas = res$filas, variables = res$variables, trayectorias = res$trayectorias,
                    trayectorias_variables = res$trayectorias_variables, volteados = res$volteados),
    Diagnostics = list(interestructura = res$interestructura, descomposicion = res$diagnosticos,
                       test = res$test, tablas = res$tablas),
    Graphics = list(disponibles = GRAFICOS_BGL[[res$metodo]] %||% character()),
    Methods = list(metodo = res$metodo, titulo = res$titulo, resultado = res),
    History = data.frame(Paso = 1L, Operación = "BGL_Create",
                         Detalle = paste("Método:", res$titulo),
                         Fecha = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), check.names = FALSE),
    Options = list(permutaciones = a$permutaciones, semilla = a$semilla, R = R.version.string,
                   ade4 = as.character(utils::packageVersion("ade4")))
  )
  structure(bgl, class = "BGL")
}

# Agrega una fila a la historia del objeto
anotar_bgl <- function(bgl, operacion, detalle) {
  h <- bgl$History
  bgl$History <- rbind(h, data.frame(Paso = nrow(h) + 1L, Operación = operacion, Detalle = detalle,
                                     Fecha = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), check.names = FALSE))
  bgl
}

#' @export
print.BGL <- function(x, ...) BGL_Show(x)

#' Mostrar un objeto BGL
#' @param bgl objeto de [BGL_Create()].
#' @export
BGL_Show <- function(bgl) {
  cat("Objeto BGL - ", bgl$Methods$titulo, "\n", sep = "")
  p <- bgl$Preprocessing$descripcion
  for (n in names(p)) cat("  Tabla ", bgl$Preprocessing$nombres[[n]], ": ", p[[n]], "\n", sep = "")
  cat("  Secciones: ", paste0("$", names(unclass(bgl)), collapse = " "), "\n", sep = "")
  invisible(bgl)
}

#' Resumen de lo que guarda cada sección
#' @param bgl objeto de [BGL_Create()].
#' @return data.frame con una fila por sección.
#' @export
BGL_Summary <- function(bgl) {
  ve <- bgl$Factorization$valores_propios
  data.frame(
    Sección = c("Preprocessing", "Factorization", "Geometry", "Diagnostics", "Graphics", "Methods", "History", "Options"),
    Contenido = c(
      paste(unlist(bgl$Preprocessing$descripcion), collapse = " | "),
      sprintf("%s: %d valores propios, %s ejes", paste(bgl$Factorization$motor, collapse = "/"),
              if (is.null(ve)) 0L else nrow(ve), bgl$Factorization$ejes %||% "?"),
      paste(names(Filter(Negate(is.null), bgl$Geometry)), collapse = ", "),
      paste(names(bgl$Diagnostics$tablas), collapse = ", "),
      paste(bgl$Graphics$disponibles, collapse = ", "),
      bgl$Methods$titulo,
      sprintf("%d operaciones", nrow(bgl$History)),
      paste(names(Filter(Negate(is.null), bgl$Options)), collapse = ", ")),
    check.names = FALSE)
}

#' Historia de las operaciones del objeto
#' @param bgl objeto de [BGL_Create()].
#' @export
BGL_History <- function(bgl) bgl$History

#' Opciones con que se calculó el objeto (semilla, permutaciones y versiones)
#' @param bgl objeto de [BGL_Create()].
#' @export
BGL_Options <- function(bgl) bgl$Options

#' Comprobar la coherencia de un objeto BGL
#' @param bgl objeto de [BGL_Create()].
#' @return data.frame con cada comprobación y si se cumple.
#' @export
BGL_Validate <- function(bgl) {
  res <- bgl$Methods$resultado
  coordenadas <- c(res$filas, res$variables)
  numeros <- unlist(lapply(coordenadas, as.numeric), use.names = FALSE)
  ve <- res$inercia
  rv <- unlist(c(lapply(res$interestructura, function(i) as.numeric(i$RV)), res$diagnosticos$RV))
  suma <- if (is.null(ve)) NA_real_ else sum(ve[[3]])
  entre <- function(v, a, b) all(v >= a - 1e-8 & v <= b + 1e-8)
  data.frame(
    Comprobación = c("Están todas las secciones", "La preparación está registrada",
                     "Valores propios no negativos", "Los porcentajes de inercia suman el total",
                     "Coordenadas finitas", "RV entre 0 y 1", "p-valor entre 0 y 1"),
    Correcto = c(
      all(c("Preprocessing", "Factorization", "Geometry", "Diagnostics", "Graphics", "Methods",
            "History", "Options") %in% names(unclass(bgl))),
      length(res$preparacion) > 0,
      !is.null(ve) && all(ve[[1 + which(names(ve)[-1] == "Valor propio")[1]]] >= -1e-8),
      isTRUE(abs(suma - 100) < 1e-6) || isTRUE(abs(suma - 1) < 1e-6),
      length(numeros) > 0 && all(is.finite(numeros)),
      entre(rv, 0, 1),
      is.null(res$test) || entre(res$test$p, 0, 1)),
    check.names = FALSE)
}

#' Voltear ejes y dejarlo anotado en la historia
#' @param bgl objeto de [BGL_Create()].
#' @param ejes números de los ejes.
#' @export
BGL_Flip <- function(bgl, ejes) {
  res <- voltear_resultado(bgl$Methods$resultado, ejes)
  bgl$Methods$resultado <- res
  bgl$Geometry[c("filas", "variables", "trayectorias", "trayectorias_variables", "volteados")] <-
    list(res$filas, res$variables, res$trayectorias, res$trayectorias_variables, res$volteados)
  anotar_bgl(bgl, "BGL_Flip", paste("Ejes volteados:", paste(ejes, collapse = ", ")))
}

#' Dibujar un gráfico del objeto
#' @param bgl objeto de [BGL_Create()].
#' @param tipo uno de `bgl$Graphics$disponibles`; por defecto, el primero.
#' @param ejes plano que se dibuja.
#' @param serie en CoSTATIS, la interestructura que se dibuja (`"X"` o `"Y"`).
#' @param ... argumentos para la función del gráfico.
#' @export
BGL_Plot <- function(bgl, tipo = bgl$Graphics$disponibles[1], ejes = c(1, 2), serie = "X", ...) {
  if (!tipo %in% bgl$Graphics$disponibles) {
    stop("Este método ofrece: ", paste(bgl$Graphics$disponibles, collapse = ", "), ".", call. = FALSE)
  }
  res <- bgl$Methods$resultado
  inter <- if (!is.null(res$interestructura)) res$interestructura[[if (bgl$Methods$metodo == "statico") "XY" else serie]]
  switch(tipo,
         coestructura = grafico_coestructura(res, ejes, ...),
         variables = grafico_variables(res, ejes, ...),
         interestructura = grafico_interestructura(inter, ...),
         pesos = grafico_pesos(inter, ...),
         trayectorias = grafico_trayectorias(res, ejes = ejes, ...),
         circulo = grafico_circulo(res, ejes, ...),
         indeterminacion = grafico_indeterminacion(res),
         test = grafico_test(res$test))
}

#' Guardar un objeto BGL
#'
#' RDS lo guarda completo, para volver a abrirlo con `readRDS()`. JSON guarda
#' las secciones sin el objeto de ade4, para leerlas desde otros programas.
#' @param bgl objeto de [BGL_Create()].
#' @param archivo ruta terminada en `.rds` o `.json`.
#' @export
BGL_Export <- function(bgl, archivo) {
  ext <- tolower(tools::file_ext(archivo))
  if (!ext %in% c("rds", "json")) stop("Usa un archivo .rds o .json.", call. = FALSE)
  bgl <- anotar_bgl(bgl, "BGL_Export", paste0("Formato ", toupper(ext)))
  if (ext == "rds") {
    saveRDS(bgl, archivo)
  } else {
    salida <- unclass(bgl)
    salida$Methods$resultado <- NULL
    a_tablas <- function(l) lapply(l, function(m) if (is.matrix(m)) coordenadas_a_tabla(m) else m)
    salida$Geometry$filas <- a_tablas(salida$Geometry$filas)
    salida$Geometry$variables <- a_tablas(salida$Geometry$variables)
    jsonlite::write_json(salida, archivo, digits = NA, auto_unbox = TRUE, null = "null",
                         na = "null", pretty = TRUE, force = TRUE)
  }
  invisible(archivo)
}
