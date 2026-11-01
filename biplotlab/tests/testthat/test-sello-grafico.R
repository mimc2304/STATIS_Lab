# El aspecto de los gráficos está en un solo lugar: R/sello_grafico.R

test_that("ningún método define su propio tema ni su propia paleta", {
  dir_r <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(dir_r))
  archivos <- setdiff(list.files(dir_r, "\\.R$"), "sello_grafico.R")
  con_tema <- Filter(function(f) {
    any(grepl("theme_minimal(", readLines(file.path(dir_r, f), encoding = "UTF-8"), fixed = TRUE))
  }, archivos)
  # El canónico conserva su selector de temas, pero parte del sello
  expect_setequal(con_tema, character())
  con_paleta <- Filter(function(f) {
    any(grepl("(pal_groups|COLOR_FLECHA_EQUIPO) *<-", readLines(file.path(dir_r, f), encoding = "UTF-8")))
  }, archivos)
  expect_length(con_paleta, 0)
})

test_that("los gráficos de todos los métodos llevan el mismo tema", {
  d <- ejemplo_biplot("riano")
  v <- setdiff(names(d), c("Fecha", "Nivel", "Regimen", "Groupdeep", "Groupweek"))
  hj <- hj_biplot(d, variables = v)
  b <- BLOQUES_EJEMPLO$frutas
  co <- coinercia(ejemplo_statis("frutas"), b$x, b$y, permutaciones = 0)
  bm <- BLOQUES_EJEMPLO$meau
  st <- statico(ejemplo_statis("meau"), bm$x, bm$y, "Estacion", "Sitio", "total", "centrar")
  graficos <- list(
    hj = grafico_biplot(hj),
    coinercia = grafico_coestructura(co),
    variables = grafico_variables(co),
    inter = grafico_interestructura(st$interestructura$XY),
    pesos = grafico_pesos(st$interestructura$XY),
    tray = grafico_trayectorias(st, "X"),
    scree = plot_scree(data.frame(Eje = 1:2, `Varianza explicada (%)` = c(60, 40),
                                  `Acumulada (%)` = c(60, 100), check.names = FALSE)))
  for (n in names(graficos)) {
    expect_true(inherits(graficos[[n]]$theme$panel.grid, "element_blank"), info = n)
    expect_equal(graficos[[n]]$theme$legend.position, "bottom", info = n)
    expect_s3_class(ggplot2::ggplot_build(graficos[[n]]), "ggplot_built")
  }
})

test_that("título de eje único y paleta del grupo", {
  expect_equal(titulo_eje_sello(1, 28.3676), "Eje 1 (28,37 %)")
  expect_equal(titulo_eje_sello(2, 40), "Eje 2 (40,00 %)")
  expect_equal(unname(colores_por_defecto(c("a", "b"))), PALETA_GRUPOS[1:2])
  expect_equal(colores_paleta(2), PALETA_GRUPOS[1:2])
})

test_that("el sello no cambia ningún resultado numérico", {
  d <- ejemplo_biplot("riano")
  v <- setdiff(names(d), c("Fecha", "Nivel", "Regimen", "Groupdeep", "Groupweek"))
  expect_equal(round(hj_biplot(d, variables = v)$inercia[1:2, 4], 2), c(28.37, 14.89))
})
