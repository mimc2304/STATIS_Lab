# Comprobación de Co-inercia, STATICO y CoSTATIS con el álgebra directa (sin ade4) y con
# los resultados publicados en las guías de las prácticas del Módulo 4.

centrar_por <- function(M, g) {
  for (k in unique(g)) M[g == k, ] <- sweep(M[g == k, , drop = FALSE], 2, colMeans(M[g == k, , drop = FALSE]))
  M
}
pesos_rv <- function(tablas) {
  C <- sapply(tablas, function(a) sapply(tablas, function(b) sum(a * b)))
  R <- C / sqrt(diag(C) %o% diag(C))
  list(RV = R, alfa = abs(eigen(R, symmetric = TRUE)$vectors[, 1]))
}

test_that("Co-inercia coincide con el álgebra directa", {
  d <- ejemplo_statis("doubs"); b <- BLOQUES_EJEMPLO$doubs
  res <- coinercia(d, b$x, b$y, "estandarizar", "centrar", permutaciones = 0)
  n <- nrow(d)
  X <- scale(as.matrix(d[b$x])) * sqrt(n / (n - 1))
  Y <- scale(as.matrix(d[b$y]), scale = FALSE)
  s <- svd(t(Y) %*% X / n)
  expect_equal(unname(res$motor$eig[1:2]), s$d[1:2]^2, tolerance = 1e-8)
  expect_equal(abs(unname(as.matrix(res$motor$c1[, 1:2]))), abs(s$v[, 1:2]), tolerance = 1e-6)
  WX <- X %*% t(X) / n; WY <- Y %*% t(Y) / n
  expect_equal(res$diagnosticos$RV,
               sum(WX * WY) / sqrt(sum(WX * WX) * sum(WY * WY)), tolerance = 1e-8)
})

test_that("STATICO y CoSTATIS coinciden con el álgebra directa", {
  m <- ejemplo_statis("meau"); b <- BLOQUES_EJEMPLO$meau
  g <- m$Estacion; n <- nrow(m) / length(unique(g))
  X <- centrar_por(as.matrix(m[b$x]), g); X <- sweep(X, 2, sqrt(colMeans(X^2)), "/")
  Y <- centrar_por(as.matrix(m[b$y]), g)
  oc <- levels(ejemplo_statis("meau")$Estacion %||% factor(g))
  oc <- unique(g)

  st <- statico(m, b$x, b$y, "Estacion", "Sitio", "total", "centrar")
  Z <- lapply(oc, function(k) t(X[g == k, ]) %*% Y[g == k, ] / n)
  p <- pesos_rv(Z)
  Zmas <- Reduce(`+`, Map(`*`, p$alfa, Z))
  expect_equal(unname(st$motor$eig[1:3]), svd(Zmas)$d[1:3]^2, tolerance = 1e-6)
  expect_equal(sort(as.numeric(st$motor$RV)), sort(as.numeric(p$RV)), tolerance = 1e-8)

  co <- costatis(m, b$x, b$y, "Estacion", "Sitio", "total", "centrar", permutaciones = 0)
  compromiso <- function(T) {
    L <- lapply(oc, function(k) T[g == k, ])
    a <- pesos_rv(L)$alfa
    A <- Reduce(`+`, Map(`*`, a, L))
    sweep(A, 2, sqrt(colMeans(A^2)), "/")  # ACP sin centrar y con columnas normadas
  }
  expect_equal(unname(co$motor$coinercia$eig[1:3]),
               svd(t(compromiso(Y)) %*% compromiso(X) / n)$d[1:3]^2, tolerance = 1e-6)
})

test_that("resultados de las guías de las prácticas (fitoplancton, datos de la docente)", {
  # Estos datos no se publican con el paquete: la prueba corre en el equipo que los tiene
  f <- Sys.getenv("BIPLOTLAB_PRACTICA_KVIAS",
                  "../../../../Modulo#4/Practica/DATOS PRACTICA K_VIAS.xlsx")
  skip_if_not(file.exists(f))
  skip_if_not_installed("readxl")
  dat <- as.data.frame(readxl::read_excel(f, sheet = "STATICO", .name_repair = "minimal"))
  env <- dat[, 4:10]; spe <- dat[, 14:26]
  fp <- data.frame(Estacion = dat[[2]], Mes = dat[[3]], env, spe, check.names = FALSE)
  st <- suppressWarnings(statico(fp, names(env), names(spe), "Estacion", "Mes", "total", "centrar"))
  expect_equal(st$motor$eig[1], 56.92842, tolerance = 1e-5)
  expect_equal(st$motor$RV.eig[1], 1.9987993, tolerance = 1e-6)
  expect_equal(st$motor$tabw, c(0.2613319, 0.5295738, 0.5976700, 0.5422618), tolerance = 1e-6)
  expect_equal(unname(st$motor$cos2), c(0.3318054, 0.7192757, 0.829835, 0.8194139), tolerance = 1e-6)
  co <- costatis(fp, names(env), names(spe), "Estacion", "Mes", "total", "centrar", permutaciones = 0)
  expect_equal(co$motor$coinercia$eig[1], 24.296075, tolerance = 1e-6)
  expect_equal(co$diagnosticos$RV, 0.5375044, tolerance = 1e-6)
})

test_that("los ejemplos de K-tablas son didácticos: varios ejes con información", {
  pct <- function(e) 100 * e / sum(e)
  b <- BLOQUES_EJEMPLO$frutas
  r <- coinercia(ejemplo_statis("frutas"), b$x, b$y, b$preparacion_x, b$preparacion_y,
                 permutaciones = 999, semilla = 123)
  expect_lt(pct(r$motor$eig)[1], 65); expect_gt(pct(r$motor$eig)[2], 15)
  expect_lt(r$test$p, 0.05)

  b <- BLOQUES_EJEMPLO$meau_sitios
  s <- statico(ejemplo_statis("meau_sitios"), b$x, b$y, b$ocasion, b$individuo,
               b$preparacion_x, b$preparacion_y)
  expect_lt(pct(s$motor$eig)[1], 60); expect_gt(pct(s$motor$eig)[2], 20)

  b <- BLOQUES_EJEMPLO$meau
  co <- costatis(ejemplo_statis("meau"), b$x, b$y, b$ocasion, b$individuo,
                 b$preparacion_x, b$preparacion_y, permutaciones = 999, semilla = 123)
  expect_lt(co$test$p, 0.05); expect_gt(pct(co$motor$coinercia$eig)[2], 10)
})

test_that("fitoplancton de Riaño: reproduce el informe y sus columnas para pintar", {
  d <- ejemplo_biplot("riano")
  expect_equal(dim(d), c(70, 18))
  expect_true(all(c("Fecha", "Nivel", "Regimen", "Groupdeep", "Groupweek") %in% detectar_nominales(d)))
  v <- setdiff(names(d), c("Fecha", "Nivel", "Regimen", "Groupdeep", "Groupweek"))
  expect_length(v, 13)
  h <- hj_biplot(d, variables = v)
  expect_equal(round(h$inercia[1:2, 4], 2), c(28.37, 14.89))
  expect_equal(round(1000 * sum(h$calidad_columnas["XANTOF", 1:2]), 1), 798.6)
  expect_equal(round(1000 * sum(h$calidad_filas["G10", 1:2]), 1), 4.0)
  expect_equal(unique(d$Regimen[d$Fecha %in% c("A", "E")]), "Mezcla")
  expect_equal(unique(d$Regimen[d$Fecha %in% c("F", "J")]), "Estratificación")
})

test_that("pintar solo algunos puntos no cambia el encuadre ni el análisis", {
  d <- ejemplo_biplot("riano")
  v <- setdiff(names(d), c("Fecha", "Nivel", "Regimen", "Groupdeep", "Groupweek"))
  h <- hj_biplot(d, variables = v)
  rango <- function(p) ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range
  todos <- grafico_biplot(h, grupos = d$Fecha)
  for (r in c("atenuar", "ocultar")) {
    p <- grafico_biplot(h, grupos = d$Fecha, destacados = d$Fecha %in% c("A", "J"), resto = r)
    expect_equal(rango(p), rango(todos))
    expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  }
  pp <- grafico_biplot(h, grupos = d$Fecha, destacados = d$Fecha %in% c("A", "J"), resto = "ocultar")
  puntos <- Filter(function(l) inherits(l$geom, "GeomPoint"), pp$layers)
  expect_equal(nrow(puntos[[1]]$data), 14)
  expect_setequal(levels(puntos[[1]]$data$grupo), c("A", "J"))

  b <- BLOQUES_EJEMPLO$frutas
  r <- coinercia(ejemplo_statis("frutas"), b$x, b$y, permutaciones = 0)
  elegidos <- rownames(r$filas$X)[1:3]
  a <- grafico_coestructura(r); c1 <- grafico_coestructura(r, destacados = elegidos, resto = "ocultar")
  expect_equal(rango(a), rango(c1))

  bm <- BLOQUES_EJEMPLO$meau
  s <- statico(ejemplo_statis("meau"), bm$x, bm$y, "Estacion", "Sitio", "total", "centrar")
  ta <- grafico_trayectorias(s, "X"); tb <- grafico_trayectorias(s, "X", destacados = "S1", resto = "ocultar")
  expect_equal(rango(ta), rango(tb))
})

test_that("el menú de ejemplos cambia con el apartado y cada base tiene su texto", {
  bi <- opciones_ejemplos("Biplot"); kt <- opciones_ejemplos("K-tablas")
  expect_true("riano" %in% bi); expect_false("riano" %in% kt)
  expect_true(all(c("frutas", "meau_sitios", "meau") %in% kt)); expect_false("frutas" %in% bi)
  for (n in c(EJEMPLOS_BIPLOT, EJEMPLOS_STATIS)) expect_true(nzchar(INFO_EJEMPLOS[[n]] %||% ""), info = n)
  expect_false(any(c("doubs", "banco_mundial") %in% kt))
  expect_s3_class(ejemplo_biplot("doubs"), "data.frame")  # siguen disponibles en R
})
