# =============================================================================
# CAPA 5 - GRÁFICOS comunes a todos los métodos K-tablas (ggplot2).
#
# Reciben el objeto `statislab` (resultado.R), así que cualquier método nuevo
# que llene las mismas secciones los reutiliza. Todos los planos usan el
# encuadre rectangular de encuadre.R (misma escala en los dos ejes, mínimo
# 4:3) y el mismo estilo que BiplotLab.
# Convención de colores: tabla X en tono óxido (flechas), tabla Y en azul
# (puntos), ocasiones con la paleta categórica.
# =============================================================================

colores_ocasiones <- function(niveles) stats::setNames(rep_len(PALETA_GRUPOS, length(niveles)), niveles)

flecha <- function() flecha_sello()

tema_statislab <- function() tema_sello()

# Plano con ejes por el origen, encuadre rectangular y etiquetas de los ejes
plano <- function(p, x, y, etiqueta_x, etiqueta_y, con_origen = TRUE, titulo = NULL,
                  subtitulo = NULL) {
  lim <- limites_encuadre(x, y, incluir_origen = con_origen)
  p + ggplot2::coord_equal(xlim = lim$x, ylim = lim$y, expand = FALSE) +
    ggplot2::labs(x = etiqueta_x, y = etiqueta_y, title = titulo, subtitle = subtitulo) +
    tema_statislab()
}

base_plano <- function() ggplot2::ggplot() + ejes_cero()

comprobar_ejes <- function(m, ejes) {
  if (any(ejes > ncol(m))) stop("Solo se calcularon ", ncol(m), " ejes.", call. = FALSE)
}

#' Co-estructura de los individuos (Co-inercia, CoSTATIS)
#'
#' Cada individuo es una flecha que va de su posición según X (círculo) a su
#' posición según Y (punta). Flecha corta: co-estructura fuerte.
#' @param res objeto `statislab` con `filas$X` y `filas$Y`.
#' @param ejes plano.
#' @param etiquetas mostrar los nombres.
#' @param tam_texto tamaño de las etiquetas.
#' @param destacados nombres de los individuos que se pintan (`NULL` = todos). Solo
#'   cambia el dibujo; el encuadre no se mueve.
#' @param resto qué hacer con los demás: `"atenuar"` (gris, sin nombre) u `"ocultar"`.
#' @export
grafico_coestructura <- function(res, ejes = c(1, 2), etiquetas = TRUE, tam_texto = 3.2,
                                 destacados = NULL, resto = c("atenuar", "ocultar")) {
  resto <- match.arg(resto)
  comprobar_ejes(res$filas$X, ejes)
  n <- res$nombres
  X <- res$filas$X[, ejes, drop = FALSE]; Y <- res$filas$Y[, ejes, drop = FALSE]
  df <- data.frame(x = X[, 1], y = X[, 2], xend = Y[, 1], yend = Y[, 2], etiqueta = rownames(X))
  encuadre <- df
  p <- base_plano()
  if (length(destacados)) {
    otros <- df[!df$etiqueta %in% destacados, , drop = FALSE]
    if (resto == "atenuar" && nrow(otros)) {
      p <- p + ggplot2::geom_segment(data = otros, ggplot2::aes(.data$x, .data$y, xend = .data$xend, yend = .data$yend),
                                     colour = COLOR_ATENUADO, linewidth = 0.4, arrow = flecha()) +
        ggplot2::geom_point(data = otros, ggplot2::aes(.data$x, .data$y), colour = COLOR_ATENUADO, size = 1.8)
    }
    df <- df[df$etiqueta %in% destacados, , drop = FALSE]
  }
  p <- p +
    ggplot2::geom_segment(data = df, ggplot2::aes(.data$x, .data$y, xend = .data$xend, yend = .data$yend),
                          colour = "#6b6a65", linewidth = 0.5, arrow = flecha()) +
    ggplot2::geom_point(data = df, ggplot2::aes(.data$x, .data$y, colour = n[["X"]]), size = 2.4) +
    ggplot2::geom_point(data = df, ggplot2::aes(.data$xend, .data$yend, colour = n[["Y"]]),
                        size = 1.2) +
    ggplot2::scale_colour_manual(values = stats::setNames(c(COLOR_X, COLOR_Y), c(n[["X"]], n[["Y"]])),
                                 breaks = c(n[["X"]], n[["Y"]]), name = NULL)
  if (etiquetas) {
    p <- p + etiqueta_punto(df, ggplot2::aes(.data$x, .data$y, label = .data$etiqueta), size = tam_texto * 0.9)
  }
  plano(p, c(encuadre$x, encuadre$xend), c(encuadre$y, encuadre$yend), etiqueta_eje(res, ejes[1]), etiqueta_eje(res, ejes[2]),
        titulo = "Co-estructura de los individuos",
        subtitulo = sprintf("Círculo: posición según %s - punta: según %s", n[["X"]], n[["Y"]]))
}

#' Variables de las dos tablas en el mismo plano
#'
#' Las variables de X son flechas desde el origen y las de Y, puntos. Una
#' variable de Y en la dirección de una flecha de X se asocia directamente con
#' ella.
#' @inheritParams grafico_coestructura
#' @param mostrar `"ambas"`, `"X"` o `"Y"`.
#' @param titulo título del gráfico.
#' @export
grafico_variables <- function(res, ejes = c(1, 2), etiquetas = TRUE, tam_texto = 3.2,
                              mostrar = c("ambas", "X", "Y"), titulo = "Variables de las dos tablas") {
  mostrar <- match.arg(mostrar)
  comprobar_ejes(res$variables$X, ejes)
  n <- res$nombres
  dX <- data.frame(res$variables$X[, ejes, drop = FALSE], etiqueta = rownames(res$variables$X))
  dY <- data.frame(res$variables$Y[, ejes, drop = FALSE], etiqueta = rownames(res$variables$Y))
  names(dX)[1:2] <- names(dY)[1:2] <- c("x", "y")
  verX <- mostrar != "Y"; verY <- mostrar != "X"
  p <- base_plano()
  if (verY) {
    p <- p + ggplot2::geom_point(data = dY, ggplot2::aes(.data$x, .data$y, colour = n[["Y"]]), size = 2.2)
  }
  if (verX) {
    p <- p + ggplot2::geom_segment(data = dX, ggplot2::aes(0, 0, xend = .data$x, yend = .data$y,
                                                           colour = n[["X"]]),
                                   linewidth = 0.6, arrow = flecha())
  }
  if (etiquetas) {
    # Una sola capa de etiquetas para que no se pisen las de X con las de Y
    et <- rbind(if (verX) cbind(dX, color = COLOR_X, letra = "bold"),
                if (verY) cbind(dY, color = COLOR_Y, letra = "plain"))
    p <- p + etiqueta_punto(et, ggplot2::aes(.data$x, .data$y, label = .data$etiqueta),
                             colour = et$color, fontface = et$letra, size = tam_texto * 0.9,
                             max.overlaps = 40)
  }
  vis <- c(if (verX) n[["X"]], if (verY) n[["Y"]])
  p <- p + ggplot2::scale_colour_manual(values = stats::setNames(c(COLOR_X, COLOR_Y), c(n[["X"]], n[["Y"]]))[vis],
                                        breaks = vis, name = NULL)
  todos <- rbind(if (verX) dX[, 1:2], if (verY) dY[, 1:2])
  plano(p, todos$x, todos$y, etiqueta_eje(res, ejes[1]), etiqueta_eje(res, ejes[2]), titulo = titulo,
        subtitulo = sprintf("Flechas: %s - puntos: %s", n[["X"]], n[["Y"]]))
}

#' Mapa de la interestructura
#'
#' Cada ocasión es una flecha desde el origen (el "paraguas" de Lavit). Ángulo
#' pequeño entre dos flechas: ocasiones con estructura parecida (RV alto).
#' @param inter lista de [interestructura_pta()].
#' @param titulo título.
#' @param tam_texto tamaño de las etiquetas.
#' @export
grafico_interestructura <- function(inter, titulo = "Interestructura", tam_texto = 3.4) {
  co <- inter$coordenadas
  df <- data.frame(x = co[, 1], y = co[, 2], etiqueta = rownames(co))
  df$etiqueta <- factor(df$etiqueta, levels = df$etiqueta)
  p <- base_plano() +
    ggplot2::geom_segment(data = df, ggplot2::aes(0, 0, xend = .data$x, yend = .data$y,
                                                  colour = .data$etiqueta),
                          linewidth = 0.8, arrow = flecha(), show.legend = FALSE) +
    etiqueta_vector(df, ggplot2::aes(.data$x, .data$y, label = .data$etiqueta, colour = .data$etiqueta),
                    size = tam_texto) +
    ggplot2::scale_colour_manual(values = colores_ocasiones(levels(df$etiqueta)))
  e <- inter$inercia[[3]]
  plano(p, df$x, df$y, paste0("Eje 1 (", pct(e[1]), ")"), paste0("Eje 2 (", pct(e[2]), ")"),
        titulo = titulo, subtitulo = "Ángulo pequeño entre ocasiones: estructuras parecidas (RV alto)")
}

#' Pesos de las ocasiones en el compromiso y su calidad (cos²)
#' @inheritParams grafico_interestructura
#' @export
grafico_pesos <- function(inter, titulo = "Pesos del compromiso y calidad de cada ocasión") {
  oc <- inter$ocasiones
  df <- data.frame(Ocasión = factor(rep(oc$Ocasión, 2), levels = oc$Ocasión),
                   Medida = rep(c("Peso (suma 1)", "cos²"), each = nrow(oc)),
                   Valor = c(oc$`Peso (suma 1)`, oc$`cos²`))
  df$Medida <- factor(df$Medida, levels = c("Peso (suma 1)", "cos²"))
  ggplot2::ggplot(df, ggplot2::aes(.data$Ocasión, .data$Valor, fill = .data$Medida)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7), width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = formatC(.data$Valor, format = "f", digits = 2,
                                                    decimal.mark = marca_decimal())),
                       position = ggplot2::position_dodge(width = 0.7), vjust = -0.4, size = 3.2,
                       colour = COLOR_TEXTO) +
    ggplot2::scale_fill_manual(values = c(COLOR_FILAS, COLOR_BARRA_SUAVE), name = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, 1.08), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL, title = titulo) +
    tema_statislab()
}

#' Trayectorias: cada ocasión proyectada sobre el compromiso
#'
#' @inheritParams grafico_coestructura
#' @param que `"X"` o `"Y"`.
#' @param tipo `"individuos"` (filas de cada ocasión) o `"variables"` (solo
#'   STATICO: las variables vistas desde cada ocasión).
#' @param conectar unir con una línea las posiciones del mismo elemento.
#' @param ocasiones ocasiones a mostrar (`NULL` = todas).
#' @export
grafico_trayectorias <- function(res, que = c("X", "Y"), tipo = c("individuos", "variables"),
                                 ejes = c(1, 2), etiquetas = TRUE, conectar = TRUE,
                                 ocasiones = NULL, tam_texto = 3.2, destacados = NULL,
                                 resto = c("atenuar", "ocultar")) {
  que <- match.arg(que); tipo <- match.arg(tipo); resto <- match.arg(resto)
  tr <- if (tipo == "individuos") res$trayectorias[[que]] else res$trayectorias_variables[[que]]
  if (is.null(tr)) stop("Este método no tiene trayectorias de ", tipo, ".", call. = FALSE)
  cols <- paste("Eje", ejes)
  if (!all(cols %in% names(tr))) stop("Solo se calcularon ", ncol(tr) - 2, " ejes.", call. = FALSE)
  niveles <- levels(tr$Ocasión)
  if (length(ocasiones)) tr <- tr[tr$Ocasión %in% ocasiones, , drop = FALSE]
  df <- data.frame(elemento = tr[[1]], Ocasión = factor(tr$Ocasión, levels = niveles),
                   x = tr[[cols[1]]], y = tr[[cols[2]]])
  df <- df[order(df$elemento, as.integer(df$Ocasión)), ]
  encuadre <- df
  p <- base_plano()
  if (length(destacados)) {
    otros <- df[!as.character(df$elemento) %in% destacados, , drop = FALSE]
    if (resto == "atenuar" && nrow(otros)) {
      p <- p + ggplot2::geom_point(data = otros, ggplot2::aes(.data$x, .data$y), colour = COLOR_ATENUADO, size = 1.8)
    }
    df <- df[as.character(df$elemento) %in% destacados, , drop = FALSE]
  }
  if (conectar && any(duplicated(df$elemento))) {
    p <- p + ggplot2::geom_path(data = df, ggplot2::aes(.data$x, .data$y, group = .data$elemento),
                                colour = "#c9c8c2", linewidth = 0.45,
                                arrow = grid::arrow(length = grid::unit(0.12, "cm"), type = "closed"))
  }
  p <- p + ggplot2::geom_point(data = df, ggplot2::aes(.data$x, .data$y, colour = .data$Ocasión), size = 2.3) +
    ggplot2::scale_colour_manual(values = colores_ocasiones(niveles), name = NULL, drop = TRUE)
  if (etiquetas) {
    p <- p + etiqueta_punto(df, ggplot2::aes(.data$x, .data$y, label = .data$elemento, colour = .data$Ocasión),
                             size = tam_texto * 0.85)
  }
  plano(p, encuadre$x, encuadre$y, etiqueta_eje(res, ejes[1]), etiqueta_eje(res, ejes[2]),
        titulo = sprintf("Trayectorias: %s de %s", tipo, res$nombres[[que]]),
        subtitulo = "Cada ocasión proyectada sobre los ejes del compromiso")
}

#' Distribución del test de permutación
#' @param test lista de [prueba_permutacion()].
#' @export
grafico_test <- function(test) {
  df <- data.frame(sim = test$simulados)
  ggplot2::ggplot(df, ggplot2::aes(.data$sim)) +
    ggplot2::geom_histogram(bins = 30, fill = COLOR_BARRA_SUAVE, colour = "white") +
    ggplot2::geom_vline(xintercept = test$observado, colour = COLOR_X, linewidth = 1) +
    ggplot2::annotate("text", x = test$observado, y = Inf, vjust = 1.5, hjust = 1.05,
                      label = sprintf("RV observado = %s\np = %s", num(test$observado, 3), num(test$p, 3)),
                      colour = COLOR_X, size = 3.6) +
    ggplot2::labs(x = "RV simulado", y = "Frecuencia",
                  title = sprintf("Test de permutación (%d permutaciones)", test$repeticiones),
                  subtitle = test$hipotesis) +
    tema_statislab()
}

#' Círculo de correlaciones del compromiso (STATIS DUAL)
#'
#' Cada variable es una flecha con longitud total 1 en el compromiso; en el
#' plano, su largo es la raíz de su calidad. Solo se interpretan los ángulos de
#' las flechas que llegan cerca del círculo.
#' @inheritParams grafico_coestructura
#' @param titulo título del gráfico.
#' @export
grafico_circulo <- function(res, ejes = c(1, 2), etiquetas = TRUE, tam_texto = 3.2,
                            titulo = "Compromiso: círculo de correlaciones") {
  m <- res$variables$X
  comprobar_ejes(m, ejes)
  df <- data.frame(x = m[, ejes[1]], y = m[, ejes[2]], etiqueta = rownames(m))
  ang <- seq(0, 2 * pi, length.out = 241)
  circ <- data.frame(x = cos(ang), y = sin(ang))
  p <- base_plano() +
    ggplot2::geom_path(data = circ, ggplot2::aes(.data$x, .data$y), colour = COLOR_GUIA, linewidth = 0.4) +
    ggplot2::geom_segment(data = df, ggplot2::aes(0, 0, xend = .data$x, yend = .data$y),
                          colour = COLOR_VECTOR, linewidth = 0.6, arrow = flecha())
  if (etiquetas) p <- p + etiqueta_vector(df, ggplot2::aes(.data$x, .data$y, label = .data$etiqueta), size = tam_texto)
  plano(p, c(-1, 1, df$x), c(-1, 1, df$y), etiqueta_eje(res, ejes[1]), etiqueta_eje(res, ejes[2]),
        titulo = titulo, subtitulo = "Flecha cerca del círculo: variable bien representada en el plano")
}

#' Indeterminación media por variable y ocasión (STATIS DUAL neutrosófico)
#' @param res resultado de [statis_dual_neutro()].
#' @export
grafico_indeterminacion <- function(res) {
  I <- res$indeterminacion
  if (is.null(I)) stop("Este resultado no tiene indeterminación.", call. = FALSE)
  df <- data.frame(Ocasión = factor(rep(rownames(I), ncol(I)), levels = rev(rownames(I))),
                   Variable = factor(rep(colnames(I), each = nrow(I)), levels = colnames(I)),
                   I = as.vector(I))
  tope <- max(1e-9, max(df$I))
  df$tinta <- ifelse(df$I > 0.6 * tope, "white", COLOR_TINTA)
  decimal <- function(x) formatC(x, format = "f", digits = 2, decimal.mark = marca_decimal())
  ggplot2::ggplot(df, ggplot2::aes(.data$Variable, .data$Ocasión, fill = .data$I)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = decimal(.data$I), colour = .data$tinta), size = 3) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_gradient(low = "white", high = COLOR_VECTOR, limits = c(0, tope), name = "I media",
                                 breaks = pretty(c(0, tope), n = 3), labels = decimal,
                                 guide = ggplot2::guide_colourbar(barwidth = grid::unit(5, "cm"))) +
    ggplot2::labs(x = NULL, y = NULL, title = "Indeterminación media por variable y ocasión",
                  subtitle = "Una celda oscura: esa variable es más incierta en esa ocasión") +
    tema_statislab()
}
