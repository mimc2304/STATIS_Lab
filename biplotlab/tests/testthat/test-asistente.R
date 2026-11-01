# Revisión de la base, asistente de métodos e interpretación de Biplot.

estado <- function(t, id) t$Estado[t$id == id]

test_that("el asistente aplica las reglas de STATIS y STATIS DUAL", {
  set.seed(3)
  base <- function(ind) data.frame(Ocasion = rep(c("A", "B", "C"), each = 5), Sitio = ind,
                                   V1 = rnorm(15), V2 = rnorm(15), V3 = rnorm(15), V4 = rnorm(15))
  # Mismos individuos y mismas variables: los dos sirven, también CoSTATIS
  m <- asistente_metodos(base(rep(paste0("S", 1:5), 3)), c("Ocasion", "Sitio"), "Ocasion", "Sitio")
  for (id in c("statis", "statis_dual", "statico", "costatis")) expect_equal(estado(m, id), "Sirve", info = id)
  # Individuos distintos: STATIS no, DUAL sí
  d <- asistente_metodos(base(paste0("S", 1:15)), c("Ocasion", "Sitio"), "Ocasion", "Sitio")
  expect_equal(estado(d, "statis"), "No sirve")
  expect_equal(estado(d, "costatis"), "No sirve")
  expect_equal(estado(d, "statis_dual"), "Sirve")
  expect_equal(estado(d, "statico"), "Sirve")
  # Variables distintas entre ocasiones: STATIS sí, DUAL no
  v <- base(rep(paste0("S", 1:5), 3))
  v$V4[v$Ocasion == "A"] <- NA
  dv <- asistente_metodos(v, c("Ocasion", "Sitio"), "Ocasion", "Sitio")
  expect_equal(estado(dv, "statis"), "Sirve")
  expect_equal(estado(dv, "statis_dual"), "No sirve")
  expect_equal(estado(dv, "statico"), "No sirve")
  # Sin ocasión, ninguno de tres vías
  s <- asistente_metodos(base(rep(paste0("S", 1:5), 3))[3:6])
  expect_equal(estado(s, "statis"), "No sirve")
  expect_equal(estado(s, "hj"), "Sirve")
})

test_that("el asistente coincide con lo que las bases de ejemplo permiten", {
  meau <- ejemplo_biplot("meau")
  a <- asistente_metodos(meau, c("Estacion", "Sitio"), "Estacion", "Sitio")
  expect_equal(estado(a, "statico"), "Sirve")
  expect_equal(estado(a, "costatis"), "Sirve")
  b <- BLOQUES_EJEMPLO$meau
  expect_s3_class(costatis(meau, b$x, b$y, "Estacion", "Sitio", "total", "centrar", permutaciones = 0), "statislab")
  iris <- ejemplo_biplot("iris")
  i <- asistente_metodos(iris, "Species")
  expect_equal(estado(i, "canonico"), "Sirve")
  expect_equal(estado(i, "logistic"), "No sirve")
  expect_setequal(asistente_metodos(iris, "Species")$id, vapply(registro_analisis(), `[[`, "", "id"))
  expect_false(any(is.na(a$Estado)))
})

test_that("se proponen la ocasión y los individuos", {
  v <- proponer_vias(ejemplo_biplot("meau"))
  expect_equal(v$ocasion, "Estacion")
  expect_equal(v$individuo, "Sitio")
  expect_equal(proponer_vias(ejemplo_biplot("pisa"))$ocasion, "")
})

test_that("la revisión avisa de cada problema", {
  d <- data.frame(Grupo = rep(c("a", "b"), each = 5), A = c(1:9, NA), B = 2, C = rep(1:2, 5) * 1000,
                  D = c(rep(1, 9), 500), T1 = as.character(1:10))
  r <- revisar_base(d, "Grupo")
  e <- function(x) r$Estado[r$Revisión == x]
  expect_equal(e("Datos faltantes"), "Problema")
  expect_equal(e("Variables constantes"), "Problema")
  expect_equal(e("Escalas"), "Aviso")
  expect_equal(e("Números como texto"), "Aviso")
  expect_equal(e("Ocasiones"), "Aviso")
  bien <- revisar_base(ejemplo_biplot("meau"), c("Estacion", "Sitio"), "Estacion", "Sitio")
  expect_false("Problema" %in% bien$Estado)
  expect_equal(bien$Estado[bien$Revisión == "Individuos entre ocasiones"], "Correcto")
  # Una ocasión sin una variable no se cuenta como dato faltante
  m <- ejemplo_biplot("meau")
  m$Temp[m$Estacion == m$Estacion[1]] <- NA
  x <- revisar_base(m, c("Estacion", "Sitio"), "Estacion", "Sitio")
  expect_equal(x$Estado[x$Revisión == "Datos faltantes"], "Correcto")
  expect_equal(x$Estado[x$Revisión == "Variables entre ocasiones"], "Aviso")
})

test_that("la interpretación de Biplot sale en cada idioma con los números correctos", {
  d <- ejemplo_biplot("pisa")
  h <- hj_biplot(d, variables = names(d)[vapply(d, is.numeric, TRUE)])
  t <- tablas_biplot(h, c(1, 2))
  hojas <- list("Inercia" = t$inercia, "Coordenadas variables" = t$coordenadas_columnas,
                "CRFE individuos" = t$calidad_filas, "CRFE variables" = t$calidad_columnas,
                "CREF individuos" = t$contrib_filas, "Correlaciones" = t$correlaciones)
  es <- interpretar_tablas(hojas, "es")
  en <- interpretar_tablas(hojas, "en")
  expect_equal(length(es), length(en))
  expect_match(es[1], "65,4 %", fixed = TRUE)
  expect_match(en[1], "65.4 %", fixed = TRUE)
  expect_match(paste(en, collapse = " "), "Axis 1")
  expect_false(grepl("\\b(Eje|plano|variables con mayor|individuo)\\b", paste(en, collapse = " "), perl = TRUE))
  expect_length(interpretar_tablas(list(), "es"), 0)
})

test_that("se interpretan el clúster y el canónico", {
  a <- data.frame(Elemento = letters[1:4], Grupo = c("1", "1", "2", "2"), Silueta = c(.6, .7, .6, .7))
  f <- interpretar_tablas(list(Asignacion = a, Grupos = data.frame(G = 1:2)), "en")
  expect_match(f[1], "2 groups")
  expect_match(f[1], "well separated")
  g <- interpretar_tablas(list(Grupos = data.frame(Grupo = 1:2, Tamaño = c(3L, 4L)), Asignacion = a), "es")
  expect_match(g[1], "1 (n = 3), 2 (n = 4)", fixed = TRUE)
  m <- interpretar_tablas(list(
    Variables = data.frame(Variable = c("x", "y", "z"), Coord1 = c(3, -5, 1), Coord2 = c(1, 1, 9),
                           `Calidad ejes 1-Q (%)` = c(90, 50, 10), check.names = FALSE),
    Particion = data.frame(Variable = c("x", "y", "z"), Componente = c("Eje 1", "Eje 1", "Eje 2"))), "en")
  expect_match(m[1], "y (-5.000)", fixed = TRUE)
  expect_true(any(grepl("best representation quality: x", m, fixed = TRUE)))
  expect_true(any(grepl("Eje 1 = x, y; Eje 2 = z", m, fixed = TRUE)))
  w <- interpretar_tablas(list(Bartlett = data.frame(p = .001), ANOVA = data.frame(Variable = c("x", "y"), p = c(.01, .5))), "es")
  expect_match(w[1], "indica diferencias")
  expect_match(w[2], ": x\\.")
})
