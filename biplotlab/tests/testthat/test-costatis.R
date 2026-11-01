# CoSTATIS: debe dar lo mismo que ade4::costatis() con la base meau.

costatis_meau <- function(...) {
  b <- BLOQUES_EJEMPLO$meau
  costatis(ejemplo_statis("meau"), b$x, b$y, "Estacion", "Sitio", "total", "centrar", ...)
}

test_that("RV, valores propios y proyecciones iguales a ade4::costatis", {
  res <- costatis_meau(permutaciones = 0)
  k <- ktabs_meau_ade4()        # helper-meau.R
  cs <- ade4::costatis(k$X, k$Y, scannf = FALSE)
  expect_equal(res$diagnosticos$RV, 0.8204725, tolerance = 1e-6)
  expect_equal(res$diagnosticos$RV, cs$RV, tolerance = 1e-10)
  expect_equal(res$inercia$`Valor propio`[1], 34.5152, tolerance = 1e-5)
  expect_equal(abs(unname(res$filas$X)), abs(unname(as.matrix(cs$mX))), tolerance = 1e-10)
  expect_equal(abs(unname(res$variables$Y)), abs(unname(as.matrix(cs$li))), tolerance = 1e-10)
  expect_equal(abs(unname(as.matrix(res$trayectorias$X[, 3:4]))), abs(unname(as.matrix(cs$supIX))),
               tolerance = 1e-10)
  expect_equal(abs(unname(as.matrix(res$trayectorias$Y[, 3:4]))), abs(unname(as.matrix(cs$supIY))),
               tolerance = 1e-10)
})

test_that("test de permutación igual a ade4::costatis.randtest con la misma semilla", {
  res <- costatis_meau(permutaciones = 199, semilla = 1)
  k <- ktabs_meau_ade4()
  set.seed(1)
  rt <- ade4::costatis.randtest(k$X, k$Y, nrepet = 199)
  expect_equal(res$test$p, rt$pvalue)
  expect_equal(res$test$observado, rt$obs)
})

test_that("ordena los individuos si cambian de orden entre ocasiones", {
  d <- ejemplo_statis("meau")
  d <- d[c(1:6, 12:7, 13:24), ]     # verano con los sitios al revés
  b <- BLOQUES_EJEMPLO$meau
  res <- costatis(d, b$x, b$y, "Estacion", "Sitio", "total", "centrar", permutaciones = 0)
  expect_equal(res$diagnosticos$RV, 0.8204725, tolerance = 1e-6)
  expect_error(costatis(d, b$x, b$y, "Estacion", NULL, permutaciones = 0), "columna de individuos")
})

test_that("interestructura de cada serie y tablas", {
  res <- costatis_meau(permutaciones = 0)
  expect_named(res$interestructura, c("X", "Y"))
  expect_equal(sum(res$interestructura$Y$ocasiones$`Peso (suma 1)`), 1)
  expect_true(all(vapply(res$tablas, is.data.frame, logical(1))))
})
