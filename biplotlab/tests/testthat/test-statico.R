# STATICO: debe dar lo mismo que ade4::statico() con la base meau de
# Thioulouse (2011): ambiente con withinpca total, efemerópteros centrados.


statico_meau <- function(...) {
  b <- BLOQUES_EJEMPLO$meau
  statico(ejemplo_statis("meau"), b$x, b$y, "Estacion", "Sitio", "total", "centrar", ...)
}

test_that("pesos, cos² y valores propios iguales a ade4::statico", {
  res <- statico_meau()
  k <- ktabs_meau_ade4()
  st <- ade4::statico(k$X, k$Y, scannf = FALSE)
  oc <- res$interestructura$XY$ocasiones
  expect_equal(oc$Ocasión, c("spring", "summer", "autumn", "winter"))
  expect_equal(oc$`Peso (vector propio)`, c(0.394, 0.534, 0.583, 0.469), tolerance = 1e-3)
  expect_equal(oc$`cos²`, c(0.498, 0.813, 0.956, 0.685), tolerance = 1e-3)
  expect_equal(res$inercia$`Valor propio`[1], 593.5954, tolerance = 1e-6)
  expect_equal(unname(res$interestructura$XY$RV), unname(as.matrix(st$RV)), tolerance = 1e-10)
  expect_equal(abs(unname(res$variables$X)), abs(unname(as.matrix(st$li))), tolerance = 1e-10)
  expect_equal(abs(unname(res$variables$Y)), abs(unname(as.matrix(st$co))), tolerance = 1e-10)
  expect_equal(abs(unname(as.matrix(res$trayectorias$X[, 3:4]))), abs(unname(as.matrix(st$supIX))),
               tolerance = 1e-10)
  expect_equal(sum(oc$`Peso (suma 1)`), 1)
})

test_that("más ejes que ade4::statico (que fija 2)", {
  res <- statico_meau(ejes = 3)
  expect_equal(ncol(res$variables$X), 3)
  expect_equal(ncol(res$trayectorias$Y), 2 + 3)
})

test_that("las ocasiones siguen el orden de los datos, no el alfabético", {
  d <- ejemplo_statis("meau")
  d <- d[c(19:24, 1:18), ]          # invierno primero
  b <- BLOQUES_EJEMPLO$meau
  res <- statico(d, b$x, b$y, "Estacion", "Sitio", "total", "centrar")
  expect_equal(res$interestructura$XY$ocasiones$Ocasión, c("winter", "spring", "summer", "autumn"))
})

test_that("admite individuos distintos en cada ocasión", {
  d <- ejemplo_statis("meau")
  d <- d[!(d$Estacion == "winter" & d$Sitio == "S6"), ]
  b <- BLOQUES_EJEMPLO$meau
  res <- statico(d, b$x, b$y, "Estacion", "Sitio", "total", "centrar")
  expect_equal(nrow(res$trayectorias$X), 23)
  expect_error(costatis(d, b$x, b$y, "Estacion", "Sitio", permutaciones = 0), "mismos individuos")
})

test_that("errores claros", {
  d <- ejemplo_statis("meau")
  b <- BLOQUES_EJEMPLO$meau
  expect_error(statico(d, b$x, b$y, "Estacion", "Sitio", "parcial", "parcial"), "constantes")
  d1 <- d[d$Estacion == "spring", ]
  expect_error(statico(d1, b$x, b$y, "Estacion", "Sitio"), "al menos 2 ocasiones")
  d2 <- d; d2$Sitio[2] <- "S1"
  expect_error(statico(d2, b$x, b$y, "Estacion", "Sitio"), "repetidos")
})
