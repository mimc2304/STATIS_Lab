# =============================================================================
# Código del compañero (app BiplotLab de DPCA, Disjoint, Cenet y Logístico),
# archivo original R/plots.R. Se integra sin cambios en los cálculos.
# =============================================================================

# =====================================================================
#  Gráficos (ggplot2 + ggrepel)
# =====================================================================
# (ggplot2 y ggrepel se importan en el NAMESPACE del paquete)

# El aspecto viene del sello gráfico (sello_grafico.R)
theme_biplot <- function(base = 12) tema_sello(base)

plot_biplot <- function(X, fact, type = "HJ", dims = c(1, 2), groups = NULL,
                        show_rows = TRUE, row_labels = FALSE, col_labels = TRUE,
                        fit_arrows = TRUE, point_size = 2.4, label_size = 3.4,
                        col_colors = NULL, title = NULL, centroids = NULL) {
  co <- biplot_coords(fact, type)
  R <- as.data.frame(co$rows[, dims, drop = FALSE]); names(R) <- c("x", "y")
  C <- as.data.frame(co$cols[, dims, drop = FALSE]); names(C) <- c("x", "y")
  R$lab <- rownames(X); C$lab <- colnames(X)
  Cen <- NULL
  if (!is.null(centroids)) {                           # centroides de clusters (CDBiplot) en la escala de las filas
    rs <- apply(abs(co$rows[, dims, drop = FALSE]) / pmax(abs(fact$S[, dims, drop = FALSE]), 1e-12), 2, stats::median)
    Cen <- as.data.frame(sweep(centroids[, dims, drop = FALSE], 2, rs, "*")); names(Cen) <- c("x", "y")
    Cen$lab <- rownames(centroids)
  }
  if (fit_arrows) {                                    # ajusta la escala visual de las flechas
    k <- 0.9 * max(abs(c(R$x, R$y))) / max(abs(c(C$x, C$y)), 1e-12)
    C$x <- C$x * k; C$y <- C$y * k
  }
  C$grp <- if (is.null(col_colors)) "Variables" else col_colors
  ev <- fact$d^2 / sum(X^2) * 100
  lab <- function(i) titulo_eje_sello(dims[i], ev[dims[i]])
  p <- ggplot() +
    ejes_cero()
  if (show_rows) {
    if (!is.null(groups)) {
      R$g <- groups
      p <- p + geom_point(data = R, aes(x, y, colour = g), size = point_size, alpha = 0.85) +
        scale_colour_manual(values = rep(PALETA_GRUPOS, length.out = nlevels(factor(groups))), name = NULL)
    } else {
      p <- p + geom_point(data = R, aes(x, y), colour = COLOR_FILAS, size = point_size, alpha = 0.8)
    }
    if (row_labels)
      p <- p + geom_text_repel(data = R, aes(x, y, label = lab), size = label_size,
                               colour = COLOR_TEXTO, max.overlaps = 25, segment.alpha = 0.3)
  }
  p <- p + geom_segment(data = C, aes(x = 0, y = 0, xend = x, yend = y),
                        arrow = flecha_sello(),
                        colour = COLOR_VECTOR, linewidth = 0.6, lineend = "round")
  if (col_labels)
    p <- p + geom_text_repel(data = C, aes(x, y, label = lab), size = label_size + 0.3,
                             colour = COLOR_VECTOR, fontface = "bold", max.overlaps = 40,
                             nudge_x = 0.05 * C$x, nudge_y = 0.05 * C$y,   # la etiqueta queda más allá de la punta
                             min.segment.length = Inf, seed = 2)
  if (!is.null(Cen))
    p <- p + geom_point(data = Cen, aes(x, y), shape = 23, size = point_size + 2.5, fill = PALETA_GRUPOS[4], colour = COLOR_TEXTO, stroke = 0.9) +
      geom_text_repel(data = Cen, aes(x, y, label = lab), size = label_size, colour = COLOR_TEXTO, fontface = "italic", max.overlaps = 40)
  p + labs(x = lab(1), y = lab(2), title = title) + coord_equal() + theme_biplot()
}

plot_scree <- function(tab) {
  ggplot(tab, aes(factor(Eje, levels = Eje), `Varianza explicada (%)`)) +
    geom_col(fill = COLOR_FILAS, width = 0.6) +
    geom_line(aes(y = `Acumulada (%)`, group = 1), colour = PALETA_GRUPOS[2], linewidth = 0.9) +
    geom_point(aes(y = `Acumulada (%)`), colour = PALETA_GRUPOS[2], size = 2.4) +
    labs(x = NULL, y = "% de varianza", title = "Varianza explicada") + theme_biplot()
}

plot_convergence <- function(hist, ylab = "Error relativo F", title = "Convergencia") {
  d <- data.frame(it = seq_along(hist) - 1, y = hist)
  ggplot(d, aes(it, y)) + geom_line(colour = COLOR_FILAS, linewidth = 0.9) +
    geom_point(colour = COLOR_FILAS, size = 2) + labs(x = "Iteración", y = ylab, title = title) +
    theme_biplot()
}

# Biplot logístico: puntos = individuos; flecha de cada variable va del
# punto P = 0.5 al punto P = prob
plot_logistic <- function(fit, dims = c(1, 2), groups = NULL, prob = 0.75, show_rows = TRUE,
                          row_labels = FALSE, point_size = 2.4, label_size = 3.4,
                          full_lines = FALSE, title = NULL, keep = NULL, clusters = NULL, cl_style = "none",
                          xlab = NULL, ylab = NULL) {
  R <- as.data.frame(fit$A[, dims, drop = FALSE]); names(R) <- c("x", "y"); R$lab <- rownames(fit$A)
  Ar <- logistic_arrows(fit, dims, prob)
  if (!is.null(keep)) { Ar <- Ar[keep, , drop = FALSE]; fit$B <- fit$B[keep, , drop = FALSE]; fit$b0 <- fit$b0[keep] }
  if (!is.null(clusters)) groups <- clusters
  p <- ggplot() +
    ejes_cero()
  if (show_rows) {
    if (!is.null(groups)) {
      R$g <- groups
      p <- p + geom_point(data = R, aes(x, y, colour = g), size = point_size, alpha = 0.8) +
        scale_colour_manual(values = rep(PALETA_GRUPOS, length.out = nlevels(factor(groups))), name = NULL)
    } else p <- p + geom_point(data = R, aes(x, y), colour = COLOR_FILAS, size = point_size, alpha = 0.7)
    if (row_labels) p <- p + geom_text_repel(data = R, aes(x, y, label = lab), size = label_size,
                                             colour = COLOR_TEXTO, max.overlaps = 20, segment.alpha = 0.3)
  }
  if (!is.null(clusters) && cl_style != "none") {
    R$g <- clusters
    if (cl_style == "hull") {
      H <- do.call(rbind, lapply(split(R, R$g), function(d) d[chull(d$x, d$y), ]))
      p <- p + geom_polygon(data = H, aes(x, y, fill = g, colour = g), alpha = 0.12, linewidth = 0.4, show.legend = FALSE) +
        scale_fill_manual(values = rep(PALETA_GRUPOS, length.out = nlevels(factor(clusters))))
    } else if (cl_style == "ellipse") {
      p <- p + stat_ellipse(data = R, aes(x, y, colour = g), level = 0.9, linewidth = 0.5, show.legend = FALSE)
    } else if (cl_style == "star") {
      Cn <- aggregate(cbind(cx = x, cy = y) ~ g, R, mean); S <- merge(R, Cn, by = "g")
      p <- p + geom_segment(data = S, aes(x = cx, y = cy, xend = x, yend = y, colour = g), alpha = 0.35, show.legend = FALSE)
    }
  }
  if (full_lines) {
    B <- fit$B[, dims, drop = FALSE]; n2 <- pmax(rowSums(B^2), 1e-12)
    ext <- Ar; ext$xa <- Ar$x0 - 6 * B[, 1] / sqrt(n2); ext$ya <- Ar$y0 - 6 * B[, 2] / sqrt(n2)
    ext$xb <- Ar$x0 + 6 * B[, 1] / sqrt(n2); ext$yb <- Ar$y0 + 6 * B[, 2] / sqrt(n2)
    p <- p + geom_segment(data = ext, aes(x = xa, y = ya, xend = xb, yend = yb),
                          colour = COLOR_VECTOR, linetype = "dotted", linewidth = 0.3)
  }
  p + geom_segment(data = Ar, aes(x = x0, y = y0, xend = x1, yend = y1),
                   arrow = flecha_sello(),
                   colour = COLOR_VECTOR, linewidth = 0.6, lineend = "round") +
    geom_point(data = Ar, aes(x0, y0), shape = 21, fill = "white", colour = COLOR_VECTOR, size = 2) +
    geom_text_repel(data = Ar, aes(x1, y1, label = Variable), size = label_size + 0.3,
                    colour = COLOR_VECTOR, fontface = "bold", max.overlaps = 40,
                    nudge_x = 0.5 * (Ar$x1 - Ar$x0), nudge_y = 0.5 * (Ar$y1 - Ar$y0),
                    min.segment.length = Inf, seed = 2) +
    labs(x = if (is.null(xlab)) paste0("Dim ", dims[1]) else xlab, y = if (is.null(ylab)) paste0("Dim ", dims[2]) else ylab, title = title,
         caption = paste0("Círculo = punto con P(x=1)=0.5; flecha = dirección hasta P(x=1)=", prob)) +
    coord_equal() + theme_biplot()
}
