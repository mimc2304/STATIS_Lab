# =============================================================================
# Pruebas del HJ-Biplot Composicional (Hernández Suárez et al., 2016)
# =============================================================================

colores_datos <- function() {
  ruta <- system.file("extdata", "colour_composition_paintings.csv", package = "biplotlab")
  if (!nzchar(ruta)) ruta <- "../../inst/extdata/colour_composition_paintings.csv"
  leer_datos(ruta)
}

# (A) Prueba de transformación clr, varianzas clr y varianza total
test_that("Cálculo de transformación CLR (centered log-ratio)", {
  X_clr_prueba <- data.frame(
    black = c(-0.09965,	-0.00898,	-0.00243,	0.11594,	0.34709,	0.02834,	0.03092,	-0.15158,	0.22281,	0.17789,	0.14625,	0.15478,	0.01795,	-0.12575,	0.29889,	0.12415,	0.04665,	0.28715,	0.13509,	0.01911,	0.30337,	0.02916),
    white = c(0.56510,	0.43982,	0.44955,	0.35841,	0.08117,	0.51727,	0.44721,	0.62094,	0.15902,	0.28751,	0.33332,	0.28831,	0.49997,	0.64744,	0.14138,	0.30851,	0.35435,	0.09609,	0.33791,	0.51614,	0.15254,	0.53998),
    blue = c(0.10247,	-0.26230,	-0.93242,	-0.19643,	-0.05331,	-0.75124,	-0.38440,	0.27397,	-1.08777,	0.14061,	-0.77004,	-0.18169,	-0.51462,	-1.15537,	-0.61239,	-0.29677,	0.22159,	-0.22731,	-0.28528,	-0.01574,	-0.91741,	0.50378),
    red = c(-1.49398,	-1.04000,	-0.13305,	-1.13377,	-1.43961,	-0.27432,	-0.87239,	-1.67763,	0.00387,	-1.62614,	-0.37415,	-1.18500,	-0.58283,	0.08783,	-0.66368,	-0.98062,	-1.87028,	-1.26423,	-0.95175,	-1.32691,	-0.20746,	-1.93168),
    yellow = c(0.27053,	0.09708,	-0.10252,	0.19786,	0.30033,	0.00253,	0.13628,	0.27397,	-0.18829,	0.30377,	0.03771,	0.17248,	0.05593,	-0.08683,	0.06818,	0.00021,	0.39668,	0.17816,	0.05505,	0.25044,	-0.08229,	0.35499),
    other = c(0.65553,	0.77438,	0.72087,	0.65799,	0.76433,	0.47742,	0.64238,	0.66032,	0.89035,	0.71637,	0.62691,	0.75113,	0.52360,	0.63268,	0.76763,	0.84453,	0.85102,	0.93014,	0.70898,	0.55696,	0.75125,	0.50378)
  )
  datos <- colores_datos()
  variables <- c("black", "white", "blue", "red", "yellow", "other")
  X <- as.matrix(datos[, variables, drop = FALSE])
  rownames(X_clr_prueba) <- seq_len(22)
  X_clr <- transformar_clr(X)
  # Compara las dos matrices
  expect_equal(X_clr, X_clr_prueba, tolerance = 1e-4)
  # Valores de las varianzas clr
  v_arr <- matriz_variacion(X)
  
  clr_vars <- c(black = 0.01964,	
               white = 0.02888,	
               blue = 0.21221,	
               red = 0.37563,	
               yellow = 0.02612,	
               other = 0.01482)
  # Las varianzas clr
  expect_equal(v_arr$clr_varianzas, clr_vars, tolerance = 1e-4)
  
  # La varianza total
  var_total <- sum(clr_vars)
  expect_equal(v_arr$varianza_total, var_total, tolerance = 1e-4)
}
)

# (B) Prueba de Matriz de Variación de Aitchison
test_that("Cálculo de la Matriz de Variación de Aitchison", {
  variables <- c("black", "white", "blue", "red", "yellow", "other")
  M <- data.frame(
    black = c(0.00000,	0.27477,	-0.43178,	-1.04704,	0.02705,	0.60507),
    white = c(0.09458,	0.00000,	-0.70655,	-1.32181,	-0.24771,	0.33030),
    blue = c(0.25236,	0.21663,	0.00000,	-0.61526,	0.45883,	1.03685),
    red = c(0.38087,	0.41777,	1.14696,	0.00000,	1.07409,	1.65211),
    yellow = c(0.05056,	0.04866,	0.09847,	0.59075,	0.00000,	0.57801),
    other = c(0.01677,	0.07296,	0.23617,	0.39476,	0.04559,	0.00000)
  )
  M <- as.matrix(M)
  rownames(M) <- variables
  
  datos <- colores_datos()
  X <- as.matrix(datos[, variables, drop = FALSE])
  rownames(X_clr_prueba) <- seq_len(22)
  v_arr <- matriz_variacion(X)
  expect_equal(v_arr$array, M, tolerance = 1e-4)
}
)

test_that("HJ-Biplot Composicional", {
  variables <- c("black", "white", "blue", "red", "yellow", "other")
  datos <- colores_datos()
  res <- calcular_composicional(datos, variables = variables, tipo = "HJ", dimensiones = 3)
  
  # Estructura del resultado
  expect_s3_class(res, "biplot_composicional")
  expect_s3_class(res, "biplot_hj")
  expect_equal(nrow(res$filas), 22)
  expect_equal(nrow(res$columnas), 6)
  expect_equal(ncol(res$inercia), 5)
  
  # Inercia explicada positiva y monótona decreciente
  expect_true(res$inercia$`% Inercia`[1] > res$inercia$`% Inercia`[2])
  expect_true(res$inercia$`% Acumulado`[2] > 90) 
  
  # Links
  expect_true(!is.null(res$links))
  expect_equal(nrow(res$links), choose(6, 2))
  expect_true(all(c("Parte_1", "Parte_2", "Var ln(Xi/Xj)", "SD log-ratio", "Longitud plano") %in% names(res$links)))
  
  # Gráfico
  p <- grafico_composicional(res, ejes = c(1, 2), mostrar_links = TRUE)
  expect_s3_class(p, "ggplot")
}
)
