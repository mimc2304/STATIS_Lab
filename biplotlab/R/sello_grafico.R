# =============================================================================
# SELLO GRÁFICO: el aspecto de todos los gráficos del aplicativo.
#
# Colores, tema, ejes por el origen, flechas, etiquetas y títulos de los ejes
# están solo aquí. Cada método calcula sus coordenadas y dibuja sus datos con
# estas piezas, así que para cambiar el aspecto de todos basta tocar este archivo.
# Ningún cálculo depende de él. La vista dinámica (etiquetas que se arrastran,
# zoom) también es común: está en dinamico.R.
# =============================================================================

# Paleta categórica (orden fijo). El usuario puede cambiar cada color.
PALETA_GRUPOS <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300",
                   "#4a3aa7", "#e34948", "#7a5c3e", "#0e7490", "#9d174d", "#4d7c0f")
FORMAS_GRUPOS <- c(16, 17, 15, 18, 3, 4, 8, 1, 2, 0, 5, 6)
COLOR_FILAS    <- "#2a78d6"  # individuos
COLOR_VECTOR   <- "#9a3412"  # variables
COLOR_TINTA    <- "#1f1f1d"  # textos principales
COLOR_TEXTO    <- "#52514e"  # nombres de individuos y títulos de ejes
COLOR_SUAVE    <- "#a8a7a1"  # textos secundarios
COLOR_ATENUADO <- "#d6d5cf"  # lo que queda de fondo
COLOR_ORIGEN   <- "#d9d8d3"  # líneas por el origen
COLOR_GUIA     <- "#c9c8c2"  # línea que une una etiqueta con su punto
COLOR_X        <- COLOR_VECTOR  # K-tablas: tabla X (flechas)
COLOR_Y        <- COLOR_FILAS   # K-tablas: tabla Y (puntos)
COLOR_BARRA_SUAVE <- "#b9c7d8"

#' Colores por defecto para los niveles de un grupo
#' @param niveles niveles del grupo.
#' @export
colores_por_defecto <- function(niveles) {
  stats::setNames(rep_len(PALETA_GRUPOS, length(niveles)), niveles)
}

#' Tema común de los gráficos: sin cuadrícula, leyenda abajo y fondo blanco
#' @param base_size tamaño base del texto.
#' @export
tema_sello <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(colour = COLOR_TINTA, face = "bold", size = base_size),
      plot.subtitle = ggplot2::element_text(colour = "#6b6a65", size = base_size - 2),
      plot.caption = ggplot2::element_text(colour = "#6b6a65", size = base_size - 3),
      axis.title = ggplot2::element_text(colour = COLOR_TEXTO, size = base_size - 1.5),
      axis.text = ggplot2::element_text(colour = "#8a8984", size = base_size - 3),
      legend.position = "bottom",
      plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

#' Líneas horizontal y vertical por el origen
#' @export
ejes_cero <- function() {
  list(ggplot2::geom_hline(yintercept = 0, colour = COLOR_ORIGEN, linewidth = 0.4),
       ggplot2::geom_vline(xintercept = 0, colour = COLOR_ORIGEN, linewidth = 0.4))
}

#' Punta de flecha común
#' @export
flecha_sello <- function() grid::arrow(length = grid::unit(0.16, "cm"), type = "closed")

#' Etiquetas de puntos (individuos): no se pisan y se unen a su punto
#' @param data,mapping datos y estética de la capa.
#' @param size tamaño del texto.
#' @param ... cambios a los valores comunes de `ggrepel::geom_text_repel()`.
#' @export
etiqueta_punto <- function(data, mapping, size = 2.9, ...) {
  args <- utils::modifyList(
    list(max.overlaps = 30, segment.colour = COLOR_GUIA, segment.size = 0.3, seed = 1,
         show.legend = FALSE),
    list(...))
  # El color fijo solo se usa si el color no viene de los datos
  if (is.null(args$colour) && is.null(mapping$colour)) args$colour <- COLOR_TEXTO
  do.call(ggrepel::geom_text_repel, c(list(data = data, mapping = mapping, size = size), args))
}

#' Etiquetas de vectores (variables): en negrita, más allá de la punta
#' @param data datos con las columnas `x` e `y` (punta de cada vector).
#' @param mapping estética de la capa.
#' @param size tamaño del texto.
#' @param ... cambios a los valores comunes de `ggrepel::geom_text_repel()`.
#' @export
etiqueta_vector <- function(data, mapping, size = 3.2, ...) {
  args <- utils::modifyList(
    list(fontface = "bold", max.overlaps = Inf, min.segment.length = Inf, seed = 2,
         nudge_x = 0.04 * data$x, nudge_y = 0.04 * data$y, show.legend = FALSE),
    list(...))
  if (is.null(args$colour) && is.null(mapping$colour)) args$colour <- COLOR_TINTA
  do.call(ggrepel::geom_text_repel, c(list(data = data, mapping = mapping, size = size), args))
}

#' Título de un eje: «Eje 1 (28,37 %)»
#' @param eje número del eje.
#' @param porcentaje inercia del eje, en porcentaje.
#' @export
titulo_eje_sello <- function(eje, porcentaje) {
  sprintf("Eje %d (%s %%)", as.integer(eje), format(round(porcentaje, 2), decimal.mark = ",", nsmall = 2))
}
