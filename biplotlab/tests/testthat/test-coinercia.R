# Co-inercia: los números deben coincidir con las prácticas del Módulo 4
# (PRACTICA CoIA, M. González-Narváez) y con ade4.

test_that("Doubs: ambiente estandarizado y peces centrados, como en la práctica", {
  d <- ejemplo_statis("doubs")
  b <- BLOQUES_EJEMPLO$doubs
  res <- coinercia(d, b$x, b$y, "estandarizar", "centrar", permutaciones = 999, semilla = 1)
  expect_equal(res$inercia$`Valor propio`[1:2], c(119.0194, 13.8714), tolerance = 1e-6)
  expect_equal(res$inercia$`% Acumulado`[1:2], c(88.36, 98.65), tolerance = 1e-4)
  expect_equal(res$diagnosticos$RV, 0.4505569, tolerance = 1e-6)
  # summary(coin1) de la práctica: covarianza, desviaciones y correlación
  ej <- res$diagnosticos$ejes
  expect_equal(ej$Covarianza, c(10.909602, 3.724429), tolerance = 1e-6)
  expect_equal(ej$`Desv. X`, c(2.326324, 1.685078), tolerance = 1e-6)
  expect_equal(ej$`Desv. Y`, c(6.422570, 2.863743), tolerance = 1e-6)
  expect_equal(ej$Correlación, c(0.7301798, 0.7718017), tolerance = 1e-6)
  expect_equal(res$diagnosticos$inercia_X$Razón, c(0.8560752, 0.9646978), tolerance = 1e-6)
  expect_equal(res$diagnosticos$inercia_Y$Razón, c(0.9649824, 0.9714331), tolerance = 1e-6)
  expect_lt(res$test$p, 0.01)
})

test_that("Banco Mundial: pobreza y protección social (RV 0,516; p 0,213 con semilla 123)", {
  d <- ejemplo_statis("banco_mundial")
  b <- BLOQUES_EJEMPLO$banco_mundial
  res <- coinercia(d, b$x, b$y, permutaciones = 999, semilla = 123)
  expect_equal(res$inercia$`% Co-inercia`[1], 82.8, tolerance = 1e-3)
  expect_equal(res$inercia$`% Acumulado`[2], 99.65, tolerance = 1e-4)
  expect_equal(res$diagnosticos$RV, 0.516, tolerance = 1e-3)
  expect_equal(res$test$p, 0.213)
  expect_equal(res$diagnosticos$ejes$Correlación, c(0.80, 0.65), tolerance = 0.01)
  # Longitud de las flechas en el plano 1-2 (MEMORIA del módulo, sección 11.7)
  lon <- longitud_flechas(res)
  expect_equal(unname(round(lon[c("DOM", "PHL", "MEX", "PRY")], 2)), c(0.29, 0.50, 1.59, 1.57))
})

test_that("coincide con ade4::coinertia y su test", {
  d <- ejemplo_statis("banco_mundial")
  b <- BLOQUES_EJEMPLO$banco_mundial
  dX <- ade4::dudi.pca(d[b$x], scannf = FALSE, nf = 3)
  dY <- ade4::dudi.pca(d[b$y], scannf = FALSE, nf = 2)
  co <- ade4::coinertia(dX, dY, scannf = FALSE, nf = 2)
  res <- coinercia(d, b$x, b$y, permutaciones = 0)
  expect_equal(unname(res$filas$X), unname(as.matrix(co$mX)), tolerance = 1e-10)
  expect_equal(unname(res$variables$Y), unname(as.matrix(co$li)), tolerance = 1e-10)
  expect_null(res$test)
})

test_that("AFC en una tabla: la otra toma sus pesos de filas", {
  d <- ejemplo_statis("doubs")
  d <- d[rowSums(d[BLOQUES_EJEMPLO$doubs$y]) > 0, ]   # el sitio 8 no tiene peces
  b <- BLOQUES_EJEMPLO$doubs
  res <- coinercia(d, b$x, b$y, "estandarizar", "afc", permutaciones = 0)
  coa <- ade4::dudi.coa(d[b$y], scannf = FALSE, nf = 2)
  pca <- ade4::dudi.pca(d[b$x], row.w = coa$lw, scannf = FALSE, nf = 2)
  co <- ade4::coinertia(pca, coa, scannf = FALSE, nf = 2)
  expect_equal(res$motor$eig, co$eig, tolerance = 1e-10)
  expect_error(coinercia(d, b$x, b$y, "afc", "afc"), "no pueden ir con AFC")
})

test_that("errores claros", {
  d <- ejemplo_statis("banco_mundial")
  b <- BLOQUES_EJEMPLO$banco_mundial
  expect_error(coinercia(d, b$x, c(b$x[1], b$y)), "X y en Y a la vez")
  expect_error(coinercia(d, b$x, b$y[1]), "al menos 2 variables")
  d$DesempleoTotal[2] <- NA
  expect_error(coinercia(d, b$x, b$y), "faltantes")
})

test_that("voltear cambia el signo y no la co-inercia ni las flechas", {
  d <- ejemplo_statis("banco_mundial")
  b <- BLOQUES_EJEMPLO$banco_mundial
  res <- coinercia(d, b$x, b$y, permutaciones = 0)
  v <- voltear_resultado(res, 1)
  expect_equal(v$filas$X[, 1], -res$filas$X[, 1])
  expect_equal(v$variables$Y[, 2], res$variables$Y[, 2])
  expect_equal(longitud_flechas(v), longitud_flechas(res))
  expect_equal(v$inercia, res$inercia)
})
