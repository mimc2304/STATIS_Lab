# Transformaciones de tres vías: media y desviación de cada ocasión o globales.

matriz_ocasion <- function(prep, kt, k) {
  t(as.matrix(prep$ktab[[match(k, kt$niveles)]]))
}

test_that("cada transformación de tres vías da la tabla que promete", {
  set.seed(11)
  d <- data.frame(Ocasion = rep(c("A", "B", "C"), each = 6), Sitio = rep(paste0("S", 1:6), 3),
                  V1 = rnorm(18, 10, 2), V2 = rnorm(18, 50, 8), V3 = rnorm(18), W1 = rnorm(18), W2 = rnorm(18))
  pk <- par_ktablas(d, c("V1", "V2", "V3"), c("W1", "W2"), "Ocasion", "Sitio")
  kt <- pk$X
  X <- as.matrix(kt$X)
  k <- kt$niveles[2]
  fila <- kt$ocasion == k
  Xk <- X[fila, , drop = FALSE]
  sd_n <- function(m) sqrt(colMeans(sweep(m, 2, colMeans(m))^2))
  verifica <- function(codigo, esperado) {
    obtenido <- matriz_ocasion(preparar_ktabla(kt, codigo), kt, k)
    expect_equal(unname(obtenido), unname(esperado), tolerance = 1e-8, info = codigo)
  }
  verifica("ninguna", Xk)
  verifica("centrar", sweep(Xk, 2, colMeans(Xk)))
  verifica("centrar_global", sweep(Xk, 2, colMeans(X)))
  verifica("parcial", sweep(sweep(Xk, 2, colMeans(Xk)), 2, sd_n(Xk), "/"))
  # total (ade4): una sola desviación para todas las ocasiones, la de la tabla ya centrada por ocasión
  intra <- X
  for (o in kt$niveles) intra[kt$ocasion == o, ] <- sweep(X[kt$ocasion == o, , drop = FALSE], 2,
                                                          colMeans(X[kt$ocasion == o, , drop = FALSE]))
  verifica("total", sweep(sweep(Xk, 2, colMeans(Xk)), 2, sd_n(intra), "/"))
  verifica("estandarizar_global", sweep(sweep(Xk, 2, colMeans(X)), 2, sd_n(X), "/"))
})

test_that("STATICO y CoSTATIS corren con todas las transformaciones y cambian el resultado", {
  d <- ejemplo_statis("meau")
  b <- BLOQUES_EJEMPLO$meau
  valores <- c()
  for (p in setdiff(PREPARACIONES_KTABLA, "parcial")) {  # parcial exige variables no constantes
    # Y sin transformar: si Y va centrada por ocasión, centrar X no cambia su producto cruzado
    s <- statico(d, b$x, b$y, "Estacion", "Sitio", p, "ninguna")
    c1 <- costatis(d, b$x, b$y, "Estacion", "Sitio", p, "centrar", permutaciones = 49)
    expect_s3_class(s, "statislab")
    expect_true(is.finite(c1$test$p), info = p)
    valores[p] <- s$inercia$`Valor propio`[1]
  }
  expect_equal(length(unique(round(valores, 6))), length(valores))
  # Con Y centrada por ocasión, las medias de X se anulan
  a <- statico(d, b$x, b$y, "Estacion", "Sitio", "ninguna", "centrar")
  c2 <- statico(d, b$x, b$y, "Estacion", "Sitio", "centrar", "centrar")
  expect_equal(a$inercia$`Valor propio`, c2$inercia$`Valor propio`)
})

test_that("Co-inercia admite tablas sin transformar", {
  d <- ejemplo_statis("banco_mundial")
  b <- BLOQUES_EJEMPLO$banco_mundial
  r <- coinercia(d, b$x, b$y, "ninguna", "ninguna", permutaciones = 49, semilla = 7)
  expect_true(is.finite(r$test$p))
  par <- par_tablas(d, b$x, b$y)
  expect_equal(unname(as.matrix(preparar_tabla(par$X, "ninguna")$tab)), unname(as.matrix(par$X)))
  expect_true(all(is.finite(r$inercia$`Valor propio`)))
  expect_false(isTRUE(all.equal(r$inercia$`Valor propio`,
                                coinercia(d, b$x, b$y, "estandarizar", "estandarizar", permutaciones = 0)$inercia$`Valor propio`)))
  doubs <- ejemplo_statis("doubs")
  bd <- BLOQUES_EJEMPLO$doubs
  r2 <- coinercia(doubs, bd$x, bd$y, "ninguna", "centrar", permutaciones = 0)
  expect_true(all(is.finite(r2$inercia$`Valor propio`)))
})

test_that("descripciones de STATIS y un método colgado de dos pestañas", {
  a <- registro_analisis()
  ids <- vapply(a, `[[`, "", "id")
  statis <- a[[match("statis", ids)]]$descripcion
  dual <- a[[match("statis_dual", ids)]]$descripcion
  expect_match(statis, "individuos en todas las ocasiones (obligatorio)", fixed = TRUE)
  expect_match(statis, "pueden coincidir o no", fixed = TRUE)
  expect_match(dual, "variables en todas las ocasiones (obligatorio)", fixed = TRUE)
  expect_match(dual, "pueden coincidir o no", fixed = TRUE)
  # Orden por número de tablas: dos, K y K pares
  k <- del_grupo(a, "K-tablas")
  expect_equal(vapply(k, `[[`, "", "id")[1:4], c("coinercia", "coinercia_neutro", "statis", "statis_dual"))
  expect_equal(lengths(por_subgrupo(k, "K-tablas")), c(2L, 8L, 4L))
  expect_equal(vapply(por_subgrupo(k, "K-tablas"), function(s) s[[1]]$subgrupo, ""),
               c("Dos tablas", "K tablas", "K pares de tablas"))
  # HJ Composicional: solo en Biplot, debajo de HJ Dinámico y en desarrollo
  b <- del_grupo(a, "Biplot")
  ids_b <- vapply(b, `[[`, "", "id")
  expect_equal(utils::tail(ids_b, 2), c("hj_dinamico", "hj_composicional"))
  expect_false(isTRUE(b[[length(b)]]$listo))
  expect_false("hj_composicional" %in% vapply(k, `[[`, "", "id"))
  expect_true("hj_dinamico" %in% vapply(del_grupo(a, "Biplot"), `[[`, "", "id"))
  expect_true("hj_dinamico" %in% vapply(del_grupo(a, "K-tablas"), `[[`, "", "id"))
  # Cuando esté listo, su copia no repite el id del botón original
  listo <- list(id = "x", titulo = "X", grupo = "K-tablas", tambien_en = "Biplot",
                icono = "chart-line", descripcion = "d", listo = TRUE)
  html <- as.character(grupo_cinta(list(listo), "Biplot", "Biplot"))
  expect_false(grepl("id=\"abrir_x\"", html, fixed = TRUE))
  expect_match(html, "#abrir_x", fixed = TRUE)
})
