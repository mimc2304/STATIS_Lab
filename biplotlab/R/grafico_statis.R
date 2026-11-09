# =============================================================================
# Gráficos propios de la familia STATIS (STATIS, STATIS DUAL y sus versiones
# neutrosóficas) y la descripción que se muestra debajo de cada gráfico.
# Los demás (interestructura, pesos, círculo, trayectorias, indeterminación)
# están en grafico_ktabla.R y sirven a todos los métodos K-tablas.
# =============================================================================

#' Individuos del compromiso (STATIS)
#'
#' Cada punto es un individuo en el plano del compromiso: individuos cercanos
#' tienen una posición parecida en el conjunto de las ocasiones.
#' @inheritParams grafico_coestructura
#' @param res objeto `statislab` con `filas$X` (STATIS directo).
#' @export
grafico_individuos <- function(res, ejes = c(1, 2), etiquetas = TRUE, tam_texto = 3.2,
                               destacados = NULL, resto = c("atenuar", "ocultar")) {
  resto <- match.arg(resto)
  m <- res$filas$X
  if (is.null(m)) stop("Este método no tiene individuos en el compromiso.", call. = FALSE)
  comprobar_ejes(m, ejes)
  df <- data.frame(x = m[, ejes[1]], y = m[, ejes[2]], etiqueta = rownames(m))
  encuadre <- df
  p <- base_plano()
  if (length(destacados)) {
    otros <- df[!df$etiqueta %in% destacados, , drop = FALSE]
    if (resto == "atenuar" && nrow(otros)) {
      p <- p + ggplot2::geom_point(data = otros, ggplot2::aes(.data$x, .data$y), colour = COLOR_ATENUADO, size = 1.8)
    }
    df <- df[df$etiqueta %in% destacados, , drop = FALSE]
  }
  p <- p + ggplot2::geom_point(data = df, ggplot2::aes(.data$x, .data$y), colour = COLOR_FILAS, size = 2.4)
  if (etiquetas) p <- p + etiqueta_punto(df, ggplot2::aes(.data$x, .data$y, label = .data$etiqueta), size = tam_texto)
  plano(p, encuadre$x, encuadre$y, etiqueta_eje(res, ejes[1]), etiqueta_eje(res, ejes[2]),
        titulo = "Compromiso: individuos",
        subtitulo = "Cada punto es un individuo; cerca = posición parecida en el conjunto de las ocasiones")
}

#' Inercia de los ejes del compromiso
#' @param res objeto `statislab` con `inercia`.
#' @export
grafico_inercia <- function(res) {
  ine <- res$inercia
  if (is.null(ine)) stop("Este resultado no tiene valores propios.", call. = FALSE)
  ine <- utils::head(ine, 10)
  df <- data.frame(Eje = factor(ine$Eje, levels = ine$Eje), p = ine[[3]], acum = ine[[4]])
  decimal <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = marca_decimal())
  ggplot2::ggplot(df, ggplot2::aes(.data$Eje, .data$p)) +
    ggplot2::geom_col(fill = COLOR_VECTOR, width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(decimal(.data$p), " %")), vjust = -0.4, size = 3.2,
                       colour = COLOR_TINTA) +
    ggplot2::geom_line(ggplot2::aes(y = .data$acum, group = 1), colour = COLOR_FILAS, linewidth = 0.6) +
    ggplot2::geom_point(ggplot2::aes(y = .data$acum), colour = COLOR_FILAS, size = 1.8) +
    ggplot2::scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = "% de inercia", title = "Inercia de los ejes del compromiso",
                  subtitle = "Barras: % de cada eje - línea: % acumulado") +
    tema_statislab()
}

#' Mapa de calor de la matriz RV entre ocasiones
#' @param inter lista de [interestructura_pta()].
#' @export
grafico_rv <- function(inter) {
  RV <- inter$RV
  niveles <- rownames(RV)
  df <- data.frame(A = factor(rep(niveles, ncol(RV)), levels = niveles),
                   B = factor(rep(colnames(RV), each = nrow(RV)), levels = rev(niveles)),
                   RV = as.vector(RV))
  decimal <- function(x) formatC(x, format = "f", digits = 2, decimal.mark = marca_decimal())
  df$tinta <- ifelse(df$RV > 0.6 * max(df$RV), "white", COLOR_TINTA)
  ggplot2::ggplot(df, ggplot2::aes(.data$A, .data$B, fill = .data$RV)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = decimal(.data$RV), colour = .data$tinta), size = 3.2) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_gradient(low = "white", high = COLOR_VECTOR, limits = c(0, 1), name = "RV",
                                 labels = decimal) +
    ggplot2::labs(x = NULL, y = NULL, title = "Coeficiente RV entre ocasiones",
                  subtitle = "Cerca de 1: dos ocasiones con estructura casi igual") +
    tema_statislab()
}

# ------------------------------------------- Descripción de cada gráfico ----

# Texto que aparece bajo cada gráfico. «direct» = STATIS (individuos) y «dual» =
# STATIS DUAL (variables); en la versión neutrosófica se agregan las notas de
# la puntuación. La clave es el tipo de gráfico.
DESCRIPCION_GRAFICOS <- list(
  interestructura = list(
    titulo = "Interestructura",
    que = "Cada ocasión (tabla) es una flecha desde el origen. Resume cuánto se parecen entre sí las ocasiones: la matriz RV de todas contra todas se proyecta en un plano.",
    como = c(
      "Dos flechas con ángulo pequeño: ocasiones con estructura parecida (RV alto); a 90°, sin parecido.",
      "Una flecha larga está bien representada en el plano; una corta se explica sobre todo por los ejes que no se ven.",
      "Una ocasión apartada de las demás tiene una estructura distinta: es la que más se aleja del compromiso."),
    direct = "En el STATIS se comparan las configuraciones de los individuos (matrices n × n) de cada ocasión.",
    dual = "En el STATIS DUAL se comparan las relaciones entre las variables (matrices p × p) de cada ocasión."),
  pesos = list(
    titulo = "Pesos y calidad de cada ocasión",
    que = "Barras con el peso de cada ocasión en el compromiso (primer vector propio de la matriz RV, suma 1) y su calidad de representación (cos²).",
    como = c(
      "Más peso: la ocasión se parece más al resto y pesa más al construir el compromiso.",
      "cos² cercano a 1: el compromiso describe bien a esa ocasión; cos² bajo: lo que se concluya del compromiso no vale para ella.",
      "Pesos casi iguales indican una estructura estable en todo el período."),
    direct = NULL, dual = NULL),
  rv = list(
    titulo = "Matriz RV",
    que = "Mapa de calor con el coeficiente RV (entre 0 y 1) de cada par de ocasiones.",
    como = c(
      "Celdas oscuras: pares de ocasiones muy parecidos; celdas claras: pares muy distintos.",
      "Una fila entera clara señala una ocasión atípica.",
      "Si la matriz es oscura en general, hay una estructura común fuerte y el compromiso es un buen resumen."),
    direct = "RV entre las configuraciones de los individuos.",
    dual = "RV entre las matrices de correlación (o covarianza) de las variables."),
  inercia = list(
    titulo = "Inercia de los ejes",
    que = "Cuánta información del compromiso recoge cada eje (% de la inercia) y cuánta se acumula.",
    como = c(
      "Los primeros ejes con la mayor parte de la inercia son los que vale la pena interpretar.",
      "Un codo marcado indica cuántos ejes conservar.",
      "Si la inercia está repartida entre muchos ejes, la estructura común es compleja y un plano de dos ejes resume poco."),
    direct = NULL, dual = NULL),
  compromiso_individuos = list(
    titulo = "Compromiso: individuos",
    que = "Plano de los individuos construido con todas las ocasiones a la vez, ponderadas por sus pesos: la configuración común.",
    como = c(
      "Individuos cercanos tienen una posición parecida en el conjunto de las ocasiones; lejanos, distinta.",
      "Interpreta las distancias solo en los individuos bien representados (calidad alta, ver la pestaña Tablas).",
      "Los ejes se leen con el círculo de correlaciones: las variables que apuntan hacia un individuo toman en él valores altos."),
    direct = NULL, dual = NULL),
  compromiso_variables = list(
    titulo = "Círculo de correlaciones",
    que = "Cada variable es una flecha; su posición es su correlación con los dos ejes del compromiso (promedio de las ocasiones, con los pesos del compromiso).",
    como = c(
      "Flecha larga, cerca del círculo: variable bien representada. Flecha corta: poco representada, no interpretar su ángulo.",
      "Ángulo pequeño entre dos flechas: relación directa; cerca de 180°: relación inversa; cerca de 90°: sin relación lineal.",
      "Una variable cerca de un eje es la que define a ese eje."),
    direct = "En el STATIS, es la correlación de cada variable con los individuos del compromiso.",
    dual = "En el STATIS DUAL, las flechas son las coordenadas de las variables en el compromiso de sus relaciones, llevadas a longitud total 1."),
  trayectorias = list(
    titulo = "Trayectorias",
    que = "Cada ocasión se proyecta sobre el compromiso: se ve dónde cae cada elemento (individuo o variable) en cada ocasión y se unen sus posiciones.",
    como = c(
      "Un recorrido corto: el elemento mantiene su posición en todas las ocasiones (comportamiento estable).",
      "Un recorrido largo o un salto: ese elemento cambia mucho respecto al patrón común en esas ocasiones.",
      "El promedio ponderado de las posiciones de cada elemento es su punto en el compromiso."),
    direct = "Los elementos pueden ser los individuos o las variables de cada ocasión.",
    dual = "Los elementos pueden ser las variables vistas desde cada ocasión o los individuos de cada ocasión."),
  indeterminacion = list(
    titulo = "Indeterminación",
    que = "Mapa de calor con la indeterminación media (I) de cada variable en cada ocasión, antes de resumir las ternas en puntuaciones.",
    como = c(
      "Celdas oscuras: esa variable es más incierta en esa ocasión.",
      "Si una ocasión es oscura en casi todas las variables, sus resultados merecen más cautela.",
      "La indeterminación entra en la puntuación S = wT·T + wI·(1 − I) + wF·(1 − F), pero no hace que una observación pese menos."),
    direct = NULL, dual = NULL)
)

#' Bloque «Cómo se lee este gráfico» que se muestra bajo cada gráfico
#' @param tipo clave de `DESCRIPCION_GRAFICOS`.
#' @param familia `"direct"` (STATIS) o `"dual"` (STATIS DUAL).
#' @param neutro si la versión es neutrosófica.
#' @export
descripcion_grafico_ui <- function(tipo, familia = c("direct", "dual"), neutro = FALSE) {
  familia <- match.arg(familia)
  d <- DESCRIPCION_GRAFICOS[[tipo]]
  if (is.null(d)) return(NULL)
  extra <- d[[familia]]
  shiny::tags$details(
    class = "bl-descripcion", open = NA,
    shiny::tags$summary(shiny::icon("circle-info"), paste("Cómo se lee:", d$titulo)),
    shiny::p(d$que),
    if (!is.null(extra)) shiny::p(extra),
    shiny::tags$ul(lapply(d$como, shiny::tags$li)),
    if (neutro && tipo %in% c("compromiso_individuos", "compromiso_variables", "trayectorias", "interestructura")) {
      shiny::p(class = "bl-help", "En la versión neutrosófica los datos son las puntuaciones S de cada terna (T, I, F), no los valores originales.")
    }
  )
}
