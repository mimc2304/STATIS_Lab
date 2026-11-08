# STATIS DUAL y DUAL Neutrosófico: validación contra ade4 y contra el álgebra directa.

datos_jv73 <- function() {
  data(jv73, package = "ade4", envir = environment())
  list(morpho = jv73$morpho, rio = jv73$fac.riv)
}

ternas_jv73 <- function(I = NULL, F = NULL, semilla = NULL) {
  j <- datos_jv73()
  M <- as.matrix(j$morpho) / 9
  if (!is.null(semilla)) set.seed(semilla)
  I <- I %||% (M * 0)
  F <- F %||% (1 - M)
  v <- colnames(M)
  d <- data.frame(stats::setNames(as.data.frame(M), paste0(v, "_T")),
                  stats::setNames(as.data.frame(I), paste0(v, "_I")),
                  stats::setNames(as.data.frame(F), paste0(v, "_F")),
                  Rio = j$rio, check.names = FALSE)
  list(d = d, v = v, M = M)
}

test_that("STATIS DUAL reproduce ade4::statis() en los ríos del Doubs (jv73)", {
  j <- datos_jv73()
  d <- data.frame(j$morpho, Rio = j$rio)
  res <- statis_dual(d, names(j$morpho), "Rio", preparacion = "total", ejes = 3)
  w <- ade4::withinpca(j$morpho, j$rio, scaling = "total", scannf = FALSE)
  st <- ade4::statis(ade4::ktab.within(w), scannf = FALSE, nf = 3)
  # ade4 ordena los ríos alfabéticamente; la app, en el orden de los datos
  o <- match(rownames(res$interestructura$X$RV), st$tab.names)
  expect_equal(unname(res$interestructura$X$RV), unname(as.matrix(st$RV))[o, o], tolerance = 1e-12)
  expect_equal(res$interestructura$X$ocasiones$`Peso (suma 1)`, (st$RV.tabw / sum(st$RV.tabw))[o], tolerance = 1e-12)
  expect_equal(res$inercia$`Valor propio`[1:3], st$C.eig[1:3], tolerance = 1e-10)
  expect_equal(round(res$inercia$`% Inercia`[1:3], 2), c(65.29, 14.38, 8.36))
  expect_lt(res$control[["baricentro"]], 1e-10)
})

test_that("con T = dato, I = 0 y F = 1 - T el DUAL neutrosófico es el STATIS DUAL clásico", {
  t <- ternas_jv73()
  rn <- statis_dual_neutro(t$d, t$v, "Rio", preparacion = "total", ejes = 3)
  dc <- data.frame(t$M, Rio = datos_jv73()$rio)
  rc <- statis_dual(dc, t$v, "Rio", preparacion = "total", ejes = 3)
  expect_equal(rn$inercia$`Valor propio`, rc$inercia$`Valor propio`, tolerance = 1e-10)
  expect_equal(rn$interestructura$X$RV, rc$interestructura$X$RV, tolerance = 1e-12)
  expect_equal(abs(rn$variables$X), abs(rc$variables$X), tolerance = 1e-10)
  expect_length(rn$avisos, 2)                       # F = 1 - T e I = 0
  expect_match(rn$avisos[1], "F = 1 - T")
  expect_equal(rn$comparacion$procrustes, 1, tolerance = 1e-10)
})

test_that("el cálculo coincide con el álgebra directa (RV, compromiso, círculo, calidad)", {
  t0 <- ternas_jv73()
  set.seed(42)
  n <- nrow(t0$M); p <- ncol(t0$M)
  I <- matrix(stats::runif(n * p, 0, 0.5), n); F <- matrix(stats::runif(n * p, 0, 1), n)
  t <- ternas_jv73(I = I, F = F)
  pesos <- c(T = 0.5, I = 0.3, F = 0.2)
  res <- statis_dual_neutro(t$d, t$v, "Rio", pesos = pesos, preparacion = "parcial", ejes = p)
  S <- 0.5 * t$M + 0.3 * (1 - I) + 0.2 * (1 - F)
  rio <- datos_jv73()$rio
  W <- lapply(levels(factor(rio, levels = unique(rio))), function(k) {
    Z <- S[rio == k, , drop = FALSE]
    Z <- sweep(Z, 2, colMeans(Z)); Z <- sweep(Z, 2, sqrt(colMeans(Z^2)), "/")
    R <- crossprod(Z) / nrow(Z)
    R / sqrt(sum(R^2))
  })
  K <- length(W)
  RV <- outer(seq_len(K), seq_len(K), Vectorize(function(a, b) sum(W[[a]] * W[[b]])))
  expect_equal(unname(res$interestructura$X$RV), RV, tolerance = 1e-10)
  a <- abs(eigen(RV, symmetric = TRUE)$vectors[, 1])
  Wc <- Reduce(`+`, Map(`*`, W, a))
  expect_equal(unname(res$compromiso), unname(Wc), tolerance = 1e-10)
  expect_equal(res$inercia$`Valor propio`, eigen(Wc, symmetric = TRUE)$values[seq_len(nrow(res$inercia))],
               tolerance = 1e-10)
  # Círculo: longitud total 1 con todos los ejes; calidad total = 1000 ‰
  expect_equal(unname(rowSums(res$variables$X^2)), rep(1, p), tolerance = 1e-8)
  cal <- res$tablas$`Compromiso: variables`[paste0("Calidad ", seq_len(p), " (‰)")]
  expect_equal(unname(rowSums(cal)), rep(1000, p), tolerance = 1e-6)
  ctr <- res$tablas$`Compromiso: variables`[paste0("Contribución ", 1:2, " (‰)")]
  expect_equal(unname(colSums(ctr)), c(1000, 1000), tolerance = 1e-6)
  expect_lt(res$control[["baricentro"]], 1e-10)
  # Sin avisos sobre las ternas (F no es 1 - T e I no es 0); solo puede faltar la
  # comparación, porque T sola es constante en algún río con la estandarización parcial
  expect_false(any(grepl("F = 1 - T|I = 0", res$avisos)))
})

test_that("la puntuación valida las ternas y normaliza los pesos", {
  expect_equal(puntuacion_neutrosofica(0.8, 0.1, 0.2), (0.8 + 0.9 + 0.8) / 3)
  expect_equal(puntuacion_neutrosofica(0.8, 0.1, 0.2, c(T = 5, I = 3, F = 2)),
               0.5 * 0.8 + 0.3 * 0.9 + 0.2 * 0.8)
  expect_error(puntuacion_neutrosofica(1.2, 0, 0), "entre 0 y 1")
  expect_error(puntuacion_neutrosofica(0.5, 0, 0, c(T = -1, I = 1, F = 1)), "no negativos")
  expect_error(puntuacion_neutrosofica(0.5, 0, 0, c(a = 1, b = 1, c = 1)), "T, I y F")
  # Pesos iguales = T - I - F salvo una transformación afín
  set.seed(3)
  T <- matrix(stats::runif(20), 5); I <- matrix(stats::runif(20), 5); F <- matrix(stats::runif(20), 5)
  expect_equal(puntuacion_neutrosofica(T, I, F), (T - I - F + 2) / 3)
})

test_that("con F = 1 - T solo importa la razón entre wI y wT + wF", {
  t0 <- ternas_jv73()
  set.seed(7)
  I <- matrix(stats::runif(length(t0$M), 0, 0.4), nrow(t0$M))
  t <- ternas_jv73(I = I)                            # F = 1 - T
  r1 <- statis_dual_neutro(t$d, t$v, "Rio", pesos = c(T = 0.5, I = 0.3, F = 0.2), preparacion = "total")
  r2 <- statis_dual_neutro(t$d, t$v, "Rio", pesos = c(T = 0.7, I = 0.3, F = 0), preparacion = "total")
  expect_equal(r1$inercia, r2$inercia, tolerance = 1e-10)
  expect_equal(r1$interestructura$X$RV, r2$interestructura$X$RV, tolerance = 1e-12)
  expect_match(r1$avisos[1], "0,700|0.700")
})

test_that("las respuestas categóricas se recodifican y conservan la ocasión", {
  set.seed(11)
  n <- 60
  r <- function() sample(c("De acuerdo", "No sabe", "En desacuerdo", "Ni de acuerdo ni en desacuerdo"), n, TRUE)
  d <- data.frame(P1 = r(), P2 = r(), P3 = r(), Ronda = rep(c("R1", "R2", "R3"), each = 20),
                  Persona = rep(paste0("p", 1:20), 3))
  regla <- c("De acuerdo" = "T", "No sabe" = "I", "En desacuerdo" = "F", "Ni de acuerdo ni en desacuerdo" = "N")
  res <- statis_dual_neutro(d, c("P1", "P2", "P3"), "Ronda", "Persona", formato = "respuestas", regla = regla)
  expect_s3_class(res, "statislab")
  expect_equal(res$ktablas$Individuos, c(20L, 20L, 20L))
  expect_equal(dim(res$indeterminacion), c(3L, 3L))
  expect_equal(unname(res$indeterminacion[1, "P1"]), mean(d$P1[d$Ronda == "R1"] == "No sabe"))
  expect_error(statis_dual_neutro(d, c("P1", "P2", "P3"), "Ronda", formato = "respuestas"), "regla")
})

test_that("BGL, gráficos, lectura y sintaxis funcionan para el DUAL neutrosófico", {
  t0 <- ternas_jv73()
  set.seed(5)
  I <- matrix(stats::runif(length(t0$M), 0, 0.3), nrow(t0$M))
  t <- ternas_jv73(I = I, F = matrix(stats::runif(length(t0$M)), nrow(t0$M)))
  res <- statis_dual_neutro(t$d, t$v, "Rio", preparacion = "total")
  b <- BGL_Create(res)
  expect_setequal(b$Graphics$disponibles, c("interestructura", "pesos", "circulo", "trayectorias", "indeterminacion"))
  for (g in b$Graphics$disponibles) expect_s3_class(BGL_Plot(b, g), "ggplot")
  expect_s3_class(grafico_trayectorias(res, tipo = "variables"), "ggplot")
  expect_s3_class(grafico_circulo(voltear_resultado(res, 1)), "ggplot")
  l <- lectura_statis_dual(res)
  expect_type(l, "character")
  expect_true(any(grepl("Frente al STATIS DUAL de T solo", l)))
  sx <- sintaxis_statis_dual_neutro(res, list(tipo = "archivo", nombre = "datos.xlsx"))
  expect_match(sx, "statis_dual_neutro\\(")
  expect_match(sx, "pesos = c\\(T = ")
  expect_match(preparacion_informe(res), "Preparación de cada tabla")
  reg <- Filter(function(a) a$id == "statis_dual_neutro", registro_analisis())
  expect_length(reg, 1)
  expect_true(isTRUE(reg[[1]]$listo))
})

test_that("pantalla del DUAL neutrosófico: de los datos al resultado", {
  t0 <- ternas_jv73()
  set.seed(9)
  I <- matrix(stats::runif(length(t0$M), 0, 0.3), nrow(t0$M))
  t <- ternas_jv73(I = I, F = matrix(stats::runif(length(t0$M)), nrow(t0$M)))
  datos <- shiny::reactive(list(df = t$d, origen = list(tipo = "archivo", nombre = "ternas.xlsx"),
                                etiqueta = "ternas", carga = 1L, nominales = "Rio"))
  shiny::testServer(mod_statis_dual_neutro_server, args = list(datos = datos), {
    session$setInputs(ocasion = "Rio", individuo = "", formato = "ternas", vars_ternas = t$v,
                      w_T = 0.5, w_I = 0.3, w_F = 0.2, prep = "total", ejes = 2, eje_h = 1, eje_v = 2,
                      voltear_h = TRUE, voltear_v = FALSE, etiquetas = TRUE, tam_texto = 3.2,
                      tray_que = "variables.X", tray_conectar = TRUE)
    directo <- statis_dual_neutro(t$d, t$v, "Rio", pesos = c(T = 0.5, I = 0.3, F = 0.2), preparacion = "total")
    expect_equal(res()$inercia, directo$inercia)
    expect_equal(res()$volteados, 1L)
    expect_match(output$sintaxis, "statis_dual_neutro(", fixed = TRUE)
    session$setInputs(w_T = -1)
    expect_error(res(), class = "shiny.silent.error")
  })
})

test_that("el círculo y las trayectorias usan el encuadre apaisado (mínimo 4:3)", {
  t0 <- ternas_jv73()
  set.seed(2)
  t <- ternas_jv73(I = matrix(stats::runif(length(t0$M), 0, 0.3), nrow(t0$M)))
  res <- statis_dual_neutro(t$d, t$v, "Rio", preparacion = "total")
  for (p in list(grafico_circulo(res), grafico_trayectorias(res, tipo = "variables"),
                 grafico_interestructura(res$interestructura$X))) {
    e <- encuadre_biplot(p, 10, 8, "in")
    expect_gte(e$dx / e$dy, 4 / 3 - 1e-8)
  }
})
