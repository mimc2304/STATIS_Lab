# Extracted from test-gge.R:269

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "biplotlab", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
ontario <- function() {
  ruta <- system.file("extdata", "ontario.csv", package = "biplotlab")
  if (!nzchar(ruta)) ruta <- "../../inst/extdata/ontario.csv"
  leer_datos(ruta)
}
capa <- function(grafico, geom) {
  construido <- suppressMessages(ggplot2::ggplot_build(grafico))
  cual <- which(vapply(grafico$layers, function(l) inherits(l$geom, geom), logical(1)))
  construido$data[cual]
}

# test -------------------------------------------------------------------------
res <- calcular_gge(ontario(), particion = "GH")
etiquetas <- function(g) unlist(lapply(capa(g, "GeomTextRepel"), `[[`, "label"))
colores <- function(g) unlist(lapply(c(capa(g, "GeomPoint"), capa(g, "GeomTextRepel")), `[[`, "colour"))
for (h in GGE_HERRAMIENTAS) {
    args <- list(res, h, ambiente = "OA93", genotipo = "kat", genotipo1 = "fun", genotipo2 = "zav")
    g <- do.call(gge_grafico_herramienta, c(args, nombres_filas = FALSE, nombres_columnas = FALSE))
    expect_false(any(c(rownames(res$filas), colnames(res$tabla)) %in% etiquetas(g)))
    g <- do.call(gge_grafico_herramienta, c(args, list(resaltar_genotipos = c("fun", "zav", "kat"),
                                                       color_resaltar_genotipos = "#00ff00")))
    # (relación entre ambientes y ranking de ambientes no dibujan individuos)
    if (any(c("fun", "zav", "kat") %in% etiquetas(g))) expect_true("#00ff00" %in% colores(g))
    g <- do.call(gge_grafico_herramienta, c(args, list(resaltar_ambientes = "OA93", color_resaltar_ambientes = "#00ff00")))
    if ("OA93" %in% etiquetas(g)) expect_true("#00ff00" %in% colores(g))
    # Color propio para los nombres de los individuos
    g <- do.call(gge_grafico_herramienta, c(args, list(color_nombres_filas = "#123456")))
    if (any(rownames(res$filas) %in% etiquetas(g))) expect_true("#123456" %in% colores(g))
  }
g <- gge_grafico_herramienta(res, "biplot", negrita = FALSE)
expect_false(any(unlist(lapply(capa(g, "GeomTextRepel"), `[[`, "fontface")) %in% c(2, "bold")))
q <- round(gge_resultados(res)$calidad_filas[, "Plano"])
g <- gge_grafico_herramienta(res, "biplot", cal_filas = 400, mal = "atenuar")
expect_true(all(names(q) %in% etiquetas(g)))
puntos <- capa(g, "GeomPoint")[[1]]
expect_equal(sum(puntos$colour == "grey80"), sum(q < 400))
