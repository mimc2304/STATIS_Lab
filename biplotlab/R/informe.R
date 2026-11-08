# =============================================================================
# Informe Word editable en español o inglés, para todos los métodos. Cada módulo
# registra de dónde salen sus gráficos y tablas con registrar_informe(); los
# botones de app.R descargan el informe del método que está abierto.
# =============================================================================

# Tablas muy largas o anchas no caben en una página: el total queda en el Excel
MAX_FILAS_INFORME <- 60L
MAX_COLUMNAS_INFORME <- 9L

#' Registrar lo que lleva el informe de un método
#' @param session sesión del módulo.
#' @param titulo nombre del método, o función que lo devuelve.
#' @param graficos función que devuelve una lista de ggplot.
#' @param tablas función que devuelve una lista con nombre de tablas.
#' @param lectura función que devuelve frases de interpretación, o `NULL`.
#' @param preparacion función que devuelve las frases de preparación, o `NULL`.
#' @export
registrar_informe <- function(session, titulo, graficos, tablas, lectura = NULL, preparacion = NULL) {
  # userData es un entorno; una sesión de módulo no admite asignar con session$userData$...
  compartidos <- session$userData
  compartidos$informes[[session$ns(NULL)]] <- list(
    titulo = titulo, graficos = graficos, tablas = tablas, lectura = lectura, preparacion = preparacion)
  invisible(NULL)
}

# Lee una parte del método: si aún no hay resultado, el informe sigue sin ella
parte_informe <- function(f) {
  if (is.null(f)) return(NULL)
  tryCatch(f(), error = function(e) NULL)
}

# Tabla lista para Word: traducida, con los números en texto y recortada si hace falta
tabla_informe <- function(t, idioma) {
  if (is.matrix(t)) t <- as.data.frame(t, check.names = FALSE)
  if (!is.data.frame(t) || !nrow(t) || !ncol(t)) return(NULL)
  if (!identical(rownames(t), as.character(seq_len(nrow(t))))) {
    t <- cbind(stats::setNames(data.frame(rownames(t), check.names = FALSE), " "), t)
  }
  t <- traducir_tabla(t, idioma)
  filas <- nrow(t); columnas <- ncol(t)
  t <- t[seq_len(min(filas, MAX_FILAS_INFORME)), seq_len(min(columnas, MAX_COLUMNAS_INFORME)), drop = FALSE]
  dec <- marca_decimal(idioma)
  t[] <- lapply(t, function(v) {
    if (is.numeric(v)) ifelse(is.na(v), "", formatC(v, format = "fg", digits = 5, decimal.mark = dec, flag = "#"))
    else ifelse(is.na(v), "", as.character(v))
  })
  rownames(t) <- NULL
  list(tabla = t, recortada = filas > nrow(t) || columnas > ncol(t), filas = filas, columnas = columnas)
}

#' Escribir el informe Word
#' @param archivo ruta del `.docx`.
#' @param idioma `"es"` o `"en"`.
#' @param titulo nombre del método.
#' @param datos texto que describe la base analizada.
#' @param graficos lista de ggplot.
#' @param tablas lista con nombre de tablas.
#' @param lectura frases de interpretación.
#' @param preparacion frases de preparación.
#' @export
informe_word <- function(archivo, idioma = "es", titulo = "", datos = NULL, graficos = list(),
                         tablas = list(), lectura = NULL, preparacion = NULL) {
  en <- identical(idioma, "en")
  tx <- function(es, ing) if (en) ing else es
  doc <- officer::read_docx()
  parrafo <- function(texto, estilo = "Normal") doc <<- officer::body_add_par(doc, texto, style = estilo)
  seccion <- function(texto) parrafo(texto, "heading 1")

  parrafo(paste(tx("Informe de resultados:", "Results report:"), titulo), "heading 1")
  parrafo(format(Sys.Date(), "%Y-%m-%d"))
  seccion(tx("Datos y preparación", "Data and preparation"))
  if (length(datos)) parrafo(datos)
  for (f in preparacion) parrafo(f)

  if (length(graficos)) {
    seccion(tx("Gráficos", "Figures"))
    for (p in graficos) {
      if (!inherits(p, "ggplot")) next
      png <- tempfile(fileext = ".png")
      e <- suppressWarnings(encuadre_biplot(p, W = 6.3, H = 5.2, unidad = "in"))
      ok <- tryCatch({
        suppressWarnings(ggplot2::ggsave(png, traducir_grafico(p, idioma), width = e$ancho, height = e$alto,
                                         units = "in", dpi = 200, bg = "white"))
        TRUE
      }, error = function(err) FALSE)
      if (ok) doc <- officer::body_add_img(doc, png, width = e$ancho, height = e$alto)
    }
  }
  if (length(lectura)) {
    seccion(tx("Interpretación", "Interpretation"))
    for (f in lectura) parrafo(f)
  }
  seccion(tx("Tablas", "Tables"))
  recortadas <- FALSE
  for (n in names(tablas)) {
    t <- tryCatch(tabla_informe(tablas[[n]], idioma), error = function(e) NULL)
    if (is.null(t)) next
    parrafo(traducir(n, idioma), "heading 2")
    doc <- officer::body_add_table(doc, t$tabla, style = "table_template")
    if (t$recortada) {
      recortadas <- TRUE
      parrafo(sprintf(tx("Se muestran las primeras filas y columnas de %d × %d.",
                         "First rows and columns of %d × %d shown."), t$filas, t$columnas))
    }
  }
  if (recortadas) parrafo(tx("Las tablas completas están en la descarga de Excel del método.",
                             "Complete tables are available in the method's Excel download."))
  seccion(tx("Reproducibilidad", "Reproducibility"))
  parrafo(paste0(tx("Generado con BiplotLab en ", "Generated with BiplotLab on "), R.version.string, "."))
  print(doc, target = archivo)
  invisible(archivo)
}

# Crea el informe del método con el idioma pedido, sin cambiar el de la pantalla
descargar_informe <- function(session, id, idioma, archivo, datos) {
  anterior <- session$userData$idioma
  session$userData$idioma <- function() idioma
  on.exit(session$userData$idioma <- anterior)
  i <- session$userData$informes[[id]]
  en <- identical(idioma, "en")
  d <- datos()
  texto <- if (!is.null(d)) sprintf(if (en) "Data: %s (%d rows × %d columns)." else "Datos: %s (%d filas × %d columnas).",
                                    d$etiqueta, nrow(d$df), ncol(d$df))
  titulo <- if (is.function(i$titulo)) parte_informe(i$titulo) else i$titulo
  tablas <- parte_informe(i$tablas) %||% list()
  # Los métodos sin lectura propia se interpretan desde sus tablas (interpretacion_biplot.R)
  lectura <- if (is.null(i$lectura)) tryCatch(interpretar_tablas(tablas, idioma), error = function(e) NULL)
             else parte_informe(i$lectura)
  informe_word(archivo, idioma, titulo %||% id, texto,
               graficos = parte_informe(i$graficos) %||% list(),
               tablas = tablas, lectura = lectura, preparacion = parte_informe(i$preparacion))
}

# Frases de la Lectura guiada de K-tablas en inglés: conservan los nombres y los
# números que calculó cada método
LECTURA_EN <- c(
  "las co-estructuras " = "the co-structures ",
  "^Interestructura de (.+?): el primer eje recoge el (.+?) de la semejanza entre ocasiones\\." =
    "Interstructure of \\1: the first axis accounts for \\2 of the similarity between occasions.",
  "^Las ocasiones más parecidas son (.+?) y (.+?) \\(RV = (.+?)\\); las más distintas, (.+?) y (.+?) \\(RV = (.+?)\\)\\." =
    "The most similar occasions are \\1 and \\2 (RV = \\3); the most different, \\4 and \\5 (RV = \\6).",
  "^(.+?) es la ocasión mejor representada por el compromiso \\(cos² = (.+?)\\) y (.+?) la peor \\(cos² = (.+?)\\): lo que el compromiso muestra vale sobre todo para las ocasiones con cos² alto\\." =
    "\\1 is the occasion best represented by the compromise (cos² = \\2) and \\3 the worst (cos² = \\4): what the compromise shows applies mainly to occasions with high cos².",
  "^Pesos en el compromiso \\(suma 1\\): " = "Weights in the compromise (sum 1): ",
  "^El plano (\\d+)-(\\d+) del compromiso recoge el (.+?) de la co-estructura estable\\." =
    "Plane \\1-\\2 of the compromise accounts for \\3 of the stable co-structure.",
  "^Eje (\\d+): en (.+?) destacan (.+?); en (.+?), (.+?)\\.$" =
    "Axis \\1: in \\2 the most prominent are \\3; in \\4, \\5.",
  "^Una variable de (.+?) y una de (.+?) en la misma dirección están asociadas en forma estable en todas las ocasiones; en direcciones opuestas, la asociación es inversa\\." =
    "A variable of \\1 and one of \\2 in the same direction are stably associated across all occasions; in opposite directions, the association is inverse.",
  "^En «Trayectorias» se ve cómo se ubica cada individuo en cada ocasión respecto a esa estructura común: los cambios grandes de posición indican ocasiones en que el individuo se aparta del patrón estable\\." =
    "«Trajectories» shows where each individual lies on each occasion relative to that common structure: large changes of position indicate occasions on which the individual departs from the stable pattern.",
  "^El signo de los ejes es arbitrario: voltear el gráfico no cambia ninguna conclusión\\." =
    "The sign of the axes is arbitrary: flipping the plot does not change any conclusion.",
  "^El plano (\\d+)-(\\d+) recoge el (.+?) de la co-inercia entre (.+?) y (.+?) \\(eje (\\d+): (.+?)\\)\\." =
    "Plane \\1-\\2 accounts for \\3 of the co-inertia between \\4 and \\5 (axis \\6: \\7).",
  "^El plano (\\d+)-(\\d+) recoge el (.+?) de la co-inercia entre el compromiso de (.+?) y el de (.+?)\\." =
    "Plane \\1-\\2 accounts for \\3 of the co-inertia between the compromise of \\4 and that of \\5.",
  "^En el eje 1, las coordenadas de los individuos según (.+?) y según (.+?) tienen una correlación de (.+?)\\.$" =
    "On axis 1, the coordinates of the individuals according to \\1 and \\2 have a correlation of \\3.",
  "^El eje 1 de co-inercia conserva el (.+?) de la inercia que tendría el primer eje del análisis de (.+?) por separado, y el (.+?) en (.+?)\\.$" =
    "Co-inertia axis 1 retains \\1 of the inertia of the first axis of the separate analysis of \\2, and \\3 in \\4.",
  "^El coeficiente RV entre las dos tablas es (.+?) \\(0 = sin relación, 1 = misma estructura\\)\\." =
    "The RV coefficient between the two tables is \\1 (0 = no relation, 1 = same structure).",
  "^Test de permutación \\((\\d+) repeticiones\\): p = (.+?) < 0,05\\. Se rechaza H0: existe co-estructura entre (.+?) y (.+?)\\.$" =
    "Permutation test (\\1 repetitions): p = \\2 < 0.05. H0 is rejected: there is co-structure between \\3 and \\4.",
  "^Test de permutación \\((\\d+) repeticiones\\): p = (.+?) ≥ 0,05\\. No hay evidencia de co-estructura\\. Con pocos individuos el test tiene poca potencia: el plano sigue siendo descriptivo\\." =
    "Permutation test (\\1 repetitions): p = \\2 ≥ 0.05. There is no evidence of co-structure. With few individuals the test has low power: the plane remains descriptive.",
  "^Co-estructura más fuerte \\(flechas más cortas\\): " = "Strongest co-structure (shortest arrows): ",
  "^Co-estructura más débil \\(flechas más largas\\): " = "Weakest co-structure (longest arrows): ",
  "^Las trayectorias muestran cada ocasión proyectada sobre los ejes de co-inercia de los compromisos: una ocasión lejos del resto es un momento en que la estructura se aparta de la parte estable\\." =
    "The trajectories show each occasion projected onto the co-inertia axes of the compromises: an occasion far from the rest is a moment when the structure departs from the stable part.",
  # STATIS DUAL (clásico y neutrosófico)
  "^Interstructure of las ocasiones:" = "Interstructure of the occasions:",
  "^Cada terna se resumió con S = (.+?) antes del análisis\\." =
    "Each triplet was summarized as S = \\1 before the analysis.",
  "^En estos datos F = 1 - T en todas las celdas: la falsedad no aporta información propia\\. La puntuación se reduce a (.+?); solo importa la razón entre esos dos pesos\\." =
    "In these data F = 1 - T in every cell: falsity adds no information of its own. The score reduces to \\1; only the ratio between those two weights matters.",
  "^I = 0 en todas las celdas: no hay indeterminación y el análisis equivale a un STATIS DUAL de wT·T - wF·F\\." =
    "I = 0 in every cell: there is no indeterminacy and the analysis is equivalent to a dual STATIS of wT·T - wF·F.",
  "^El plano (\\d+)-(\\d+) del compromiso recoge el (.+?) de la inercia de la estructura común entre variables\\." =
    "Plane \\1-\\2 of the compromise accounts for \\3 of the inertia of the common structure among variables.",
  "^Eje (\\d+): destacan (.+?)\\.$" = "Axis \\1: the most prominent are \\2.",
  "^Variables bien representadas en el plano \\(calidad ≥ 500 ‰\\): " =
    "Variables well represented in the plane (quality ≥ 500 ‰): ",
  "^Variables con calidad menor que 500 ‰ en el plano \\(no interpretar sus ángulos\\): " =
    "Variables with quality below 500 ‰ in the plane (do not interpret their angles): ",
  "^En el círculo, dos variables bien representadas con un ángulo pequeño se relacionan en forma directa en el conjunto de las ocasiones; cerca de 180°, en forma inversa; cerca de 90°, sin relación lineal\\." =
    "In the circle, two well-represented variables with a small angle are directly related across the occasions; near 180°, inversely; near 90°, with no linear relation.",
  "^Frente al STATIS DUAL de T solo: correlación entre las matrices RV = (.+?), Spearman entre los pesos = (.+?) y Procrustes de las variables = (.+?)\\. Valores cercanos a 1 indican que I y F cambian poco la estructura\\." =
    "Compared with the dual STATIS of T alone: correlation between the RV matrices = \\1, Spearman between the weights = \\2 and Procrustes of the variables = \\3. Values close to 1 indicate that I and F change the structure little."
)

# Traduce las frases de la Lectura guiada si el idioma es inglés
traducir_lectura <- function(frases, idioma = idioma_actual()) {
  if (!identical(idioma, "en")) return(frases)
  for (p in names(LECTURA_EN)) frases <- gsub(p, LECTURA_EN[[p]], frases, perl = TRUE)
  frases
}

# Preparación de un resultado K-tablas, en el idioma del informe
preparacion_informe <- function(res) {
  en <- identical(idioma_actual(), "en")
  a <- res$llamada
  etiqueta <- function(codigo) {
    if (en) PREPARACIONES_EN[[codigo]] %||% codigo else etiqueta_preparacion(codigo)
  }
  if (is.null(a$preparacion_y)) {
    # Una sola serie de tablas (STATIS DUAL): no hay tabla Y
    return(sprintf(if (en) "Preparation of each table: %s." else "Preparación de cada tabla: %s.",
                   etiqueta(a$preparacion_x)))
  }
  sprintf(if (en) "Table %s: %s." else "Tabla %s: %s.",
          res$nombres[c("X", "Y")], c(etiqueta(a$preparacion_x), etiqueta(a$preparacion_y)))
}

PREPARACIONES_EN <- list(
  ninguna = "No transformation",
  estandarizar = "Standardized columns (normed PCA)",
  centrar = "Centered with the mean of each occasion (or of each column, for one table)",
  afc = "Abundances or frequencies (CA)",
  centrar_global = "Centered with the global mean",
  parcial = "Standardized with the mean and SD of each occasion (partial)",
  total = "Standardized with the mean of each occasion and a common SD (total)",
  estandarizar_global = "Standardized with the global mean and SD"
)
