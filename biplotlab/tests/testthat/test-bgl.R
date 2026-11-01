# Objeto BGL de los métodos K-tablas: reúne los resultados sin cambiarlos.

objetos_bgl <- function() {
  m <- ejemplo_statis("meau"); bm <- BLOQUES_EJEMPLO$meau
  d <- ejemplo_statis("banco_mundial"); bb <- BLOQUES_EJEMPLO$banco_mundial
  list(
    coinercia = BGL_Create(d, "coinercia", variables_x = bb$x, variables_y = bb$y, permutaciones = 49),
    statico = BGL_Create(m, "statico", variables_x = bm$x, variables_y = bm$y, ocasion = "Estacion",
                         individuo = "Sitio", preparacion_x = "total", preparacion_y = "centrar"),
    costatis = BGL_Create(m, "costatis", variables_x = bm$x, variables_y = bm$y, ocasion = "Estacion",
                          individuo = "Sitio", preparacion_x = "total", preparacion_y = "centrar",
                          permutaciones = 49))
}

test_that("los tres métodos dan un objeto BGL válido con sus secciones", {
  for (n in names(objetos_bgl())) {
    b <- objetos_bgl()[[n]]
    expect_s3_class(b, "BGL")
    expect_setequal(names(unclass(b)), c("Preprocessing", "Factorization", "Geometry", "Diagnostics",
                                         "Graphics", "Methods", "History", "Options"))
    v <- BGL_Validate(b)
    expect_true(all(v$Correcto), info = paste(n, paste(v$Comprobación[!v$Correcto], collapse = "; ")))
    expect_equal(nrow(BGL_Summary(b)), 8)
    expect_equal(BGL_History(b)$Operación, "BGL_Create")
    for (g in b$Graphics$disponibles) expect_s3_class(BGL_Plot(b, g), "ggplot")
  }
  expect_error(BGL_Plot(objetos_bgl()$statico, "test"), "ofrece")
})

test_that("el objeto no cambia los números de referencia", {
  m <- ejemplo_statis("meau"); b <- BLOQUES_EJEMPLO$meau
  s <- statico(m, b$x, b$y, "Estacion", "Sitio", "total", "centrar")
  o <- BGL_Create(s)
  expect_equal(o$Factorization$valores_propios$`Valor propio`[1], 593.5954, tolerance = 1e-6)
  expect_identical(o$Geometry$variables, s$variables)
  expect_identical(o$Methods$resultado, s)
})

test_that("voltear queda anotado en la historia y cambia solo el signo", {
  b <- objetos_bgl()$coinercia
  v <- BGL_Flip(b, 1)
  expect_equal(v$Geometry$filas$X[, 1], -b$Geometry$filas$X[, 1])
  expect_equal(v$Geometry$filas$X[, 2], b$Geometry$filas$X[, 2])
  expect_equal(v$Geometry$volteados, 1L)
  expect_equal(BGL_History(v)$Operación, c("BGL_Create", "BGL_Flip"))
})

test_that("se exporta a RDS y JSON", {
  b <- objetos_bgl()$statico
  rds <- tempfile(fileext = ".rds")
  BGL_Export(b, rds)
  leido <- readRDS(rds)
  expect_s3_class(leido, "BGL")
  expect_equal(utils::tail(BGL_History(leido)$Operación, 1), "BGL_Export")
  js <- tempfile(fileext = ".json")
  BGL_Export(b, js)
  j <- jsonlite::read_json(js, simplifyVector = TRUE)
  expect_equal(j$Factorization$valores_propios$`Valor propio`[1], 593.5954, tolerance = 1e-6)
  expect_null(j$Methods$resultado)
  expect_error(BGL_Export(b, tempfile(fileext = ".txt")), "rds")
})
