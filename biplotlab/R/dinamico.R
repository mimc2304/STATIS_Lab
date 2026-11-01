# =============================================================================
# Gráficos dinámicos: el mismo ggplot de cada método se muestra con plotly (zoom,
# desplazamiento, datos al pasar el cursor y etiquetas que se arrastran). La vista
# «Para publicar» sigue siendo el gráfico estático, que es el que se descarga.
# =============================================================================

# Con más etiquetas, moverlas una a una deja de ser útil y el navegador se vuelve lento
MAX_ETIQUETAS_MOVILES <- 400
# Con más puntos se dibuja con WebGL y sin datos al pasar el cursor, para que la página responda
MAX_PUNTOS_DETALLE <- 5000L

#' Salida de un plano con las vistas dinámica y para publicar
#' @param ns función `NS` del módulo.
#' @param id id de la salida (el mismo de [render_biplot()]).
#' @export
salida_biplot <- function(ns, id = "biplot") {
  modo <- ns(paste0(id, "_modo"))
  shiny::tagList(
    shiny::radioButtons(modo, NULL, c("Dinámico" = "din", "Para publicar" = "est"), inline = TRUE),
    shiny::conditionalPanel(
      sprintf("input['%s'] === 'din'", modo),
      plotly::plotlyOutput(ns(paste0(id, "_din")), height = "640px"),
      shiny::p(class = "bl-help",
               "Rueda del ratón: zoom. Arrastrar el fondo: mover la vista. Arrastrar una etiqueta: colocarla. Doble clic: volver a la vista completa.")),
    shiny::conditionalPanel(
      sprintf("input['%s'] === 'est'", modo),
      shiny::plotOutput(ns(id), height = "auto"))
  )
}

# ggrepel no existe en plotly: sus etiquetas pasan a texto normal para no perderlas
sin_repel <- function(p) {
  for (i in seq_along(p$layers)) {
    if (inherits(p$layers[[i]]$geom, c("GeomTextRepel", "GeomLabelRepel"))) {
      # Una capa es un objeto por referencia: se crea otra para no tocar el gráfico para publicar
      l <- p$layers[[i]]
      p$layers[[i]] <- ggplot2::layer(geom = ggplot2::GeomText, stat = l$stat, data = l$data,
                                      mapping = l$mapping, position = "identity",
                                      params = l$aes_params[intersect(names(l$aes_params), ggplot2::GeomText$aesthetics())],
                                      inherit.aes = l$inherit.aes, show.legend = FALSE)
    }
  }
  p
}

# plotly tampoco dibuja las puntas de geom_segment(arrow = ...): se agregan como anotaciones
puntas_flecha <- function(p) {
  datos <- ggplot2::ggplot_build(p)$data
  unlist(lapply(seq_along(p$layers), function(i) {
    if (is.null(p$layers[[i]]$geom_params$arrow)) return(NULL)
    d <- datos[[i]]
    lapply(seq_len(nrow(d)), function(k) {
      list(x = d$xend[k], y = d$yend[k], ax = d$x[k], ay = d$y[k], xref = "x", yref = "y",
           axref = "x", ayref = "y", text = "", showarrow = TRUE, arrowhead = 2, arrowsize = 1,
           arrowwidth = 1.2, arrowcolor = d$colour[k])
    })
  }), recursive = FALSE)
}

#' Convertir un ggplot en gráfico dinámico
#' @param p ggplot del método.
#' @return objeto plotly.
#' @export
grafico_dinamico <- function(p) {
  shiny::req(inherits(p, "ggplot"))
  p <- sin_repel(p)
  flechas <- suppressWarnings(puntas_flecha(p))
  g <- suppressWarnings(plotly::plotly_build(plotly::ggplotly(p, tooltip = NULL)))
  trazos <- g$x$data
  es_texto <- vapply(trazos, function(t) identical(t$mode, "text"), TRUE)

  etiquetas <- do.call(rbind, lapply(trazos[es_texto], function(t) {
    if (!length(t$x)) return(NULL)
    data.frame(x = as.numeric(t$x), y = as.numeric(t$y), texto = as.character(t$text),
               stringsAsFactors = FALSE)
  }))
  grande <- sum(vapply(trazos[!es_texto], function(t) length(t$x), 0L)) > MAX_PUNTOS_DETALLE
  # Al pasar el cursor por un punto o una flecha: su nombre y sus coordenadas
  for (i in if (!grande) which(!es_texto)) {
    x <- suppressWarnings(as.numeric(trazos[[i]]$x)); y <- suppressWarnings(as.numeric(trazos[[i]]$y))
    if (!length(x) || !all(lengths(list(x, y)) == length(x))) next
    nombre <- rep("", length(x))
    if (!is.null(etiquetas)) {
      pos <- match(paste(round(x, 8), round(y, 8)), paste(round(etiquetas$x, 8), round(etiquetas$y, 8)))
      nombre[!is.na(pos)] <- paste0("<b>", etiquetas$texto[pos[!is.na(pos)]], "</b><br>")
    }
    trazos[[i]]$text <- ifelse(is.na(x) | is.na(y), "", paste0(nombre, "(", num(x, 3), "; ", num(y, 3), ")"))
    trazos[[i]]$hoverinfo <- "text"
    trazos[[i]]$hoveron <- NULL
  }

  anotaciones <- flechas
  moviles <- !is.null(etiquetas) && nrow(etiquetas) <= MAX_ETIQUETAS_MOVILES
  if (moviles) {
    anotaciones <- c(anotaciones, unlist(lapply(trazos[es_texto], function(t) {
      if (!length(t$x)) return(NULL)
      n <- length(t$x)
      color <- rep_len(t$textfont$color %||% "#1f1f1d", n)
      tam <- rep_len(t$textfont$size %||% 12, n)
      lapply(seq_len(n), function(k) {
        # as.numeric: plotly marca con I() los trazos de un solo punto y en JSON saldrían
        # como lista; así la anotación no se coloca y queda en la esquina del gráfico
        list(x = as.numeric(t$x[k]), y = as.numeric(t$y[k]), xref = "x", yref = "y", text = as.character(t$text[k]),
             showarrow = FALSE, font = list(color = color[k], size = tam[k]), yshift = 9)
      })
    }), recursive = FALSE))
    trazos <- trazos[!es_texto]
  }
  g$x$data <- trazos
  g$x$layout$annotations <- c(g$x$layout$annotations, anotaciones)
  g$x$layout$dragmode <- "pan"
  if (inherits(p$coordinates, "CoordFixed")) {
    g$x$layout$yaxis$scaleanchor <- "x"
    g$x$layout$yaxis$scaleratio <- p$coordinates$ratio
  }
  if (grande) {
    g$x$data <- lapply(g$x$data, function(t) {
      if (identical(t$type, "scatter")) t$type <- "scattergl"
      t$hoverinfo <- "skip"
      t
    })
  }
  plotly::config(g, displaylogo = FALSE, scrollZoom = TRUE,
                 edits = list(annotationPosition = moviles),
                 toImageButtonOptions = list(format = "png", scale = 3),
                 modeBarButtonsToRemove = c("lasso2d", "select2d"))
}
