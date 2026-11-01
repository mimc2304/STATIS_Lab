# Informe Word ES/EN y gráficos dinámicos, comunes a Biplot y K-tablas.

texto_docx <- function(archivo) {
  d <- officer::docx_summary(officer::read_docx(archivo))
  paste(d$text[!is.na(d$text)], collapse = "\n")
}

resultados_ktabla <- function() {
  m <- ejemplo_statis("meau"); bm <- BLOQUES_EJEMPLO$meau
  d <- ejemplo_statis("banco_mundial"); bb <- BLOQUES_EJEMPLO$banco_mundial
  list(
    list(res = coinercia(d, bb$x, bb$y, permutaciones = 49), lectura = lectura_coinercia),
    list(res = statico(m, bm$x, bm$y, "Estacion", "Sitio", "total", "centrar"), lectura = lectura_statico),
    list(res = costatis(m, bm$x, bm$y, "Estacion", "Sitio", "total", "centrar", permutaciones = 49),
         lectura = lectura_costatis))
}

test_that("la lectura guiada queda sin frases en español al traducirla", {
  restos <- "\\b(recoge|según|tienen|conserva|ocasión|ocasiones|individuos|Pesos|flechas|signo|plano|Una variable)\\b"
  for (r in resultados_ktabla()) {
    en <- traducir_lectura(r$lectura(r$res), "en")
    expect_false(any(grepl(restos, en, perl = TRUE)), info = paste(en[grepl(restos, en, perl = TRUE)], collapse = " | "))
    # sin idioma inglés no cambia nada
    expect_identical(traducir_lectura(r$lectura(r$res), "es"), r$lectura(r$res))
  }
})

test_that("el informe sale en cada idioma, con sus tablas y gráficos", {
  r <- resultados_ktabla()[[2]]
  b <- BGL_Create(r$res)
  for (idioma in c("es", "en")) {
    f <- tempfile(fileext = ".docx")
    informe_word(f, idioma, "STATICO", "x", list(BGL_Plot(b, "variables")), r$res$tablas,
                 traducir_lectura(r$lectura(r$res), idioma))
    expect_gt(file.size(f), 5000)
    t <- texto_docx(f)
    if (idioma == "en") {
      expect_match(t, "Results report: STATICO")
      expect_match(t, "Interpretation")
      expect_false(grepl("Informe de resultados|Interpretación", t))
    } else {
      expect_match(t, "Informe de resultados: STATICO")
      expect_match(t, "Interpretación")
    }
    imagenes <- utils::unzip(f, list = TRUE)$Name
    expect_true(any(grepl("media/.*\\.png$", imagenes) | grepl("\\.png$", imagenes)))
  }
})

test_that("el informe recorta tablas enormes y avisa", {
  f <- tempfile(fileext = ".docx")
  grande <- as.data.frame(matrix(1.5, 200, 20))
  informe_word(f, "en", "X", tablas = list(Grande = grande))
  t <- officer::docx_summary(officer::read_docx(f))
  expect_match(paste(t$text, collapse = " "), "200 × 20")
  expect_match(paste(t$text, collapse = " "), "Excel")
  celdas <- t[t$content_type == "table cell", ]
  expect_equal(max(celdas$row_id), MAX_FILAS_INFORME + 1L)
  expect_equal(max(celdas$cell_id), MAX_COLUMNAS_INFORME)
})

test_that("los números del informe usan el decimal de su idioma", {
  t <- data.frame(Nombre = "a", Valor = 1.5)
  expect_match(tabla_informe(t, "es")$tabla[[2]], ",")
  expect_match(tabla_informe(t, "en")$tabla[[2]], ".", fixed = TRUE)
})

test_that("el gráfico dinámico conserva etiquetas, flechas y escala", {
  r <- resultados_ktabla()[[2]]$res
  p <- grafico_variables(r)
  g <- grafico_dinamico(p)
  expect_s3_class(g, "plotly")
  an <- g$x$layout$annotations
  textos <- vapply(an, function(a) as.character(a$text %||% ""), "")
  etiquetas <- c(rownames(r$variables$X), rownames(r$variables$Y))
  expect_true(all(etiquetas %in% textos))
  flechas <- Filter(function(a) isTRUE(a$showarrow), an)
  expect_gte(length(flechas), nrow(r$variables$X))
  expect_true(isTRUE(g$x$config$edits$annotationPosition))
  expect_true(isTRUE(g$x$config$scrollZoom))
  # La vista dinámica no toca el gráfico original: sigue con ggrepel
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomTextRepel"), TRUE)))
})

test_that("con muchos puntos usa WebGL y deja de calcular datos por punto", {
  d <- data.frame(x = rnorm(6000), y = rnorm(6000))
  g <- grafico_dinamico(ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point())
  expect_equal(g$x$data[[1]]$type, "scattergl")
  expect_equal(g$x$data[[1]]$hoverinfo, "skip")
  chico <- grafico_dinamico(ggplot2::ggplot(d[1:50, ], ggplot2::aes(x, y)) + ggplot2::geom_point())
  expect_equal(chico$x$data[[1]]$type, "scatter")
})

test_that("los biplots de los métodos del grupo también se convierten", {
  d <- ejemplo_biplot("pisa")
  num_col <- names(d)[vapply(d, is.numeric, TRUE)]
  h <- hj_biplot(d, variables = num_col)
  g <- grafico_dinamico(grafico_biplot(h))
  expect_s3_class(g, "plotly")
  expect_gt(length(g$x$layout$annotations), 0)
})
