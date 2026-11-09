# STATIS y STATIS neutrosófico: validación contra ade4 y contra el álgebra directa.
# La familia STATIS en pantalla (mod_statis_familia.R) se comprueba al final.

meau_statis <- function() {
  list(d = ejemplo_statis("meau"), b = BLOQUES_EJEMPLO$meau)
}

test_that("STATIS reproduce ade4::statis() sobre una tabla por ocasión (meau)", {
  m <- meau_statis()
  res <- statis_directo(m$d, m$b$x, "Estacion", "Sitio", preparacion = "centrar", ejes = 3)
  e <- new.env(); data(meau, package = "ade4", envir = e)
  est <- as.character(e$meau$design$season)
  tabs <- split(e$meau$env, factor(est, levels = unique(est)))
  kt <- ade4::ktab.list.dudi(lapply(tabs, function(d) ade4::dudi.pca(d, scale = FALSE, scannf = FALSE, nf = 2)))
  st <- ade4::statis(kt, scannf = FALSE, nf = 3)
  expect_equal(res$inercia$`Valor propio`[1:3], st$C.eig[1:3], tolerance = 1e-10)
  expect_equal(unname(res$interestructura$X$RV), unname(as.matrix(st$RV)), tolerance = 1e-12)
  expect_equal(res$interestructura$X$ocasiones$`Peso (suma 1)`, as.numeric(st$RV.tabw / sum(st$RV.tabw)),
               tolerance = 1e-12)
  expect_equal(abs(unname(res$filas$X)), abs(unname(as.matrix(st$C.li)[, 1:3])), tolerance = 1e-10)
  expect_lt(res$control[["baricentro"]], 1e-10)
})

test_that("el compromiso coincide con el álgebra directa (RV, pesos, valores propios)", {
  m <- meau_statis()
  res <- statis_directo(m$d, m$b$x, "Estacion", "Sitio", preparacion = "total", ejes = 3)
  X <- as.matrix(m$d[, m$b$x])
  occ <- m$d$Estacion; niveles <- unique(occ)
  Z <- X
  for (k in niveles) Z[occ == k, ] <- sweep(X[occ == k, ], 2, colMeans(X[occ == k, ]))
  Z <- sweep(Z, 2, sqrt(colMeans(Z^2)), "/")
  ind <- m$d$Sitio[occ == niveles[1]]
  W <- lapply(niveles, function(k) {
    z <- Z[occ == k, ][match(ind, m$d$Sitio[occ == k]), ]
    z %*% t(z) / nrow(z)
  })
  n <- length(W)
  RV <- outer(seq_len(n), seq_len(n), Vectorize(function(i, j) sum(W[[i]] * W[[j]]) / sqrt(sum(W[[i]]^2) * sum(W[[j]]^2))))
  expect_equal(unname(res$interestructura$X$RV), RV, tolerance = 1e-10)
  v <- abs(eigen(RV)$vectors[, 1])
  expect_equal(res$interestructura$X$ocasiones$`Peso (suma 1)`, v / sum(v), tolerance = 1e-8)
  Wc <- Reduce(`+`, Map(function(w, a) a * w / sqrt(sum(w^2)), W, v))
  expect_equal(res$inercia$`Valor propio`[1:3], eigen(Wc)$values[1:3], tolerance = 1e-8)
})

test_that("STATIS con tablas de variables distintas (bloques) funciona y cumple el baricentro", {
  d <- ejemplo_statis("frutas")
  v <- names(d)[2:16]
  res <- statis_bloques(d, list(Descriptores = v[1:8], Otros = v[9:15], Jueces = paste0("Juez", 1:8)))
  expect_s3_class(res, "statislab")
  expect_equal(nrow(res$interestructura$X$RV), 3)
  expect_lt(res$control[["baricentro"]], 1e-10)
  expect_error(statis_bloques(d, list(A = v[1:3])), "al menos 2 bloques")
  expect_error(statis_bloques(d, list(A = v[1], B = v[2:4])), "al menos 2 variables")
})

test_that("STATIS exige los mismos individuos y avisa cuáles faltan", {
  m <- meau_statis()
  d <- m$d[!(m$d$Estacion == "winter" & m$d$Sitio == "S3"), ]
  expect_error(statis_directo(d, m$b$x, "Estacion", "Sitio", "centrar"), "mismos individuos.*winter.*S3")
  expect_error(statis_directo(m$d, m$b$x, "Estacion", NULL, "centrar"), "columna de individuos")
  expect_error(statis_directo(m$d, m$b$x, "Estacion", "Sitio", "parcial"), "constantes")   # Oxyg en winter
  expect_error(statis_directo(m$d, m$b$x[1], "Estacion", "Sitio", "centrar"), "al menos 2 variables")
})

test_that("con T = dato, I = 0 y F = 1 - T el STATIS neutrosófico es el STATIS clásico", {
  m <- meau_statis()
  M <- as.matrix(m$d[, m$b$x])
  M <- sweep(sweep(M, 2, apply(M, 2, min)), 2, apply(M, 2, function(z) diff(range(z))), "/")
  v <- colnames(M)
  d <- data.frame(stats::setNames(as.data.frame(M), paste0(v, "_T")),
                  stats::setNames(as.data.frame(M * 0), paste0(v, "_I")),
                  stats::setNames(as.data.frame(1 - M), paste0(v, "_F")),
                  Estacion = m$d$Estacion, Sitio = m$d$Sitio, check.names = FALSE)
  rn <- statis_neutro(d, v, "Estacion", "Sitio", preparacion = "total", ejes = 3)
  rc <- statis_directo(data.frame(M, Estacion = m$d$Estacion, Sitio = m$d$Sitio), v, "Estacion", "Sitio",
                       preparacion = "total", ejes = 3)
  expect_equal(rn$inercia$`Valor propio`, rc$inercia$`Valor propio`, tolerance = 1e-10)
  expect_equal(rn$interestructura$X$RV, rc$interestructura$X$RV, tolerance = 1e-12)
  expect_equal(abs(rn$filas$X), abs(rc$filas$X), tolerance = 1e-10)
  expect_length(rn$avisos, 2)
  expect_match(rn$avisos[1], "F = 1 - T")
  expect_equal(rn$comparacion$procrustes, 1, tolerance = 1e-10)
  expect_equal(dim(rn$indeterminacion), c(4, length(v)))
  expect_true(all(rn$indeterminacion == 0))
})

test_that("la puntuación neutrosófica cambia el resultado cuando I tiene información", {
  m <- meau_statis()
  M <- as.matrix(m$d[, m$b$x[1:5]])
  M <- sweep(sweep(M, 2, apply(M, 2, min)), 2, apply(M, 2, function(z) diff(range(z))), "/")
  set.seed(7)
  I <- matrix(stats::runif(length(M), 0, 0.6), nrow(M)); v <- colnames(M)
  d <- data.frame(stats::setNames(as.data.frame(M), paste0(v, "_T")),
                  stats::setNames(as.data.frame(I), paste0(v, "_I")),
                  stats::setNames(as.data.frame(1 - M), paste0(v, "_F")),
                  Estacion = m$d$Estacion, Sitio = m$d$Sitio, check.names = FALSE)
  rn <- statis_neutro(d, v, "Estacion", "Sitio", pesos = c(T = 0.5, I = 0.3, F = 0.2), preparacion = "total")
  expect_lt(rn$comparacion$procrustes, 1)
  expect_equal(unname(rn$llamada$pesos), c(0.5, 0.3, 0.2))
  expect_true("Comparación con T solo" %in% names(rn$tablas))
  expect_error(statis_neutro(d, v, "Estacion", NULL), "columna de individuos")
})

test_that("gráficos, descripciones, lectura, BGL y sintaxis de la familia STATIS", {
  m <- meau_statis()
  res <- statis_directo(m$d, m$b$x, "Estacion", "Sitio", "total", ejes = 3)
  gs <- list(grafico_interestructura(res$interestructura$X), grafico_pesos(res$interestructura$X),
             grafico_rv(res$interestructura$X), grafico_individuos(res), grafico_circulo(res),
             grafico_inercia(res), grafico_trayectorias(res), grafico_trayectorias(res, tipo = "variables"),
             grafico_individuos(res, destacados = c("S1", "S2")))
  for (g in gs) expect_s3_class(g, "ggplot")
  expect_s3_class(grafico_individuos(voltear_resultado(res, 1)), "ggplot")
  l <- lectura_statis(res)
  expect_type(l, "character"); expect_gt(length(l), 6)
  bgl <- BGL_Create(res)
  expect_true(all(c("individuos", "circulo") %in% bgl$Graphics$disponibles))
  for (g in bgl$Graphics$disponibles) expect_s3_class(BGL_Plot(bgl, g), "ggplot")
  expect_true(all(BGL_Validate(bgl)$Correcto))
  expect_s3_class(BGL_Create(m$d, "statis", variables = m$b$x, ocasion = "Estacion", individuo = "Sitio",
                             preparacion = "total"), "BGL")
  # Cada gráfico tiene su descripción
  for (tipo in names(DESCRIPCION_GRAFICOS)) {
    expect_s3_class(descripcion_grafico_ui(tipo, "direct"), "shiny.tag")
    expect_s3_class(descripcion_grafico_ui(tipo, "dual", neutro = TRUE), "shiny.tag")
    d <- DESCRIPCION_GRAFICOS[[tipo]]
    expect_true(nzchar(d$que) && length(d$como) >= 3, info = tipo)
  }
  # La Sintaxis R reproduce el resultado
  txt <- sintaxis_statis(res, list(tipo = "ejemplo", nombre = "meau"))
  codigo <- sub("^library\\(biplotlab\\)$", "", strsplit(txt, "\n")[[1]])
  e <- new.env()
  eval(parse(text = codigo[seq_len(grep("^\\)$", codigo)[1])]), envir = e)
  expect_equal(e$res$inercia, res$inercia)
})

test_that("STATIS DUAL clásico y STATIS neutrosófico tienen su sintaxis, lectura y Fundamentos", {
  data(jv73, package = "ade4", envir = environment())
  d <- data.frame(jv73$morpho, Rio = jv73$fac.riv)
  rd <- statis_dual(d, names(jv73$morpho), "Rio", preparacion = "total", ejes = 3)
  txt <- sintaxis_statis(rd, list(tipo = "ejemplo", nombre = "doubs"))
  expect_match(txt, "statis_dual\\(")
  expect_gt(length(lectura_statis_dual(rd)), 5)
  expect_s3_class(grafico_rv(rd$interestructura$X), "ggplot")
  expect_s3_class(grafico_inercia(rd), "ggplot")
  for (id in c("statis", "statis_dual", "statis_neutro")) {
    t <- TEORIA_KTABLAS[[id]]
    expect_false(is.null(t), info = id)
    expect_named(t$resumen, c("para_que", "entrada", "no_usar", "lectura"))
    expect_true(length(t$referencias$modulo) > 0 && length(t$referencias$original) > 0, info = id)
    expect_s3_class(teoria_ui(id), "shiny.tag")
  }
})

test_that("los tres botones de la familia STATIS están activos en el registro", {
  a <- registro_analisis()
  ids <- vapply(a, `[[`, "", "id")
  listos <- ids[vapply(a, function(m) isTRUE(m$listo), TRUE)]
  expect_true(all(c("statis", "statis_dual", "statis_neutro", "statis_dual_neutro") %in% listos))
  expect_false(anyDuplicated(ids) > 0)
  for (id in c("statis", "statis_dual", "statis_neutro")) {
    m <- a[[match(id, ids)]]
    expect_equal(m$grupo, "K-tablas")
    expect_s3_class(m$opciones_ui("x"), "shiny.tag.list")
    expect_s3_class(m$resultados_ui("x"), "shiny.tag.list")
  }
})

test_that("el módulo STATIS corre en Shiny con la base meau y con una base propia", {
  skip_if_not_installed("shiny")
  m <- meau_statis()
  datos <- shiny::reactive(list(df = m$d, nominales = c("Estacion", "Sitio"), origen = list(tipo = "ejemplo", nombre = "meau"),
                                carga = 1L))
  a <- analisis_statis()
  shiny::testServer(a$server, args = list(datos = datos), {
    session$setInputs(ocasion = "Estacion", individuo = "Sitio", var_x = m$b$x, prep_x = "total", ejes = 2,
                      eje_h = 1, eje_v = 2, etiquetas = TRUE, tam_texto = 3.2, tray_que = "individuos.X")
    r <- res()
    expect_s3_class(r, "statislab")
    expect_equal(r$metodo, "statis")
  })
})
