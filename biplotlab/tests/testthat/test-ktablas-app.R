# Datos, gráficos, Fundamentos, Sintaxis R y la pantalla de cada módulo.

test_that("bases de ejemplo y reparto de columnas", {
  for (n in EJEMPLOS_STATIS) {
    d <- ejemplo_statis(n)
    b <- BLOQUES_EJEMPLO[[n]]
    expect_true(all(c(b$x, b$y, b$ocasion, b$individuo) %in% names(d)), info = n)
  }
  expect_equal(dim(ejemplo_statis("meau")), c(24, 25))
  expect_setequal(detectar_nominales(ejemplo_statis("meau")), c("Estacion", "Sitio"))
  # las abundancias enteras (0 a 5) siguen siendo variables en K-tablas, aunque BiplotLab
  # proponga como nominales los enteros con pocos valores
  d <- ejemplo_statis("doubs")
  sugeridas <- detectar_nominales(d)
  expect_true(length(sugeridas) > 0)
  expect_setequal(variables_numericas(list(df = d, nominales = sugeridas)), names(d))
  expect_length(detectar_nominales(d, enteros = FALSE), 0)
  expect_equal(BLOQUES_EJEMPLO$banco_mundial$nominales, "Anio")
})

test_that("los gráficos se construyen y el plano es apaisado (mínimo 4:3)", {
  b <- BLOQUES_EJEMPLO$meau
  m <- ejemplo_statis("meau")
  s <- statico(m, b$x, b$y, "Estacion", "Sitio", "total", "centrar")
  c1 <- costatis(m, b$x, b$y, "Estacion", "Sitio", "total", "centrar", permutaciones = 99)
  planos <- list(grafico_interestructura(s$interestructura$XY), grafico_variables(s),
                 grafico_trayectorias(s, "X"), grafico_trayectorias(s, "Y", "variables"),
                 grafico_coestructura(c1), grafico_trayectorias(c1, "Y", ocasiones = "winter"))
  for (p in planos) {
    e <- encuadre_biplot(p, 10, 8, "in")
    expect_gte(e$dx / e$dy, 4 / 3 - 1e-8)
  }
  expect_s3_class(grafico_pesos(s$interestructura$XY), "ggplot")
  expect_s3_class(grafico_test(c1$test), "ggplot")
  expect_error(grafico_trayectorias(c1, "X", "variables"), "no tiene trayectorias")
})

test_that("Fundamentos completos en los métodos disponibles", {
  listos <- Filter(function(a) isTRUE(a$listo) && a$grupo == "K-tablas", registro_analisis())
  for (a in listos) {
    t <- TEORIA_KTABLAS[[a$id]]
    expect_false(is.null(t), info = a$id)
    expect_true(all(c("titulo", "subtitulo", "resumen", "para_que", "entrada", "no_usar", "calculos",
                      "lectura", "donde", "referencias") %in% names(t)), info = a$id)
    expect_named(t$resumen, c("para_que", "entrada", "no_usar", "lectura"))
    expect_true(length(t$referencias$modulo) > 0 && length(t$referencias$original) > 0, info = a$id)
    expect_s3_class(teoria_ui(a$id), "shiny.tag")
  }
})

test_that("la Sintaxis R reproduce el resultado", {
  b <- BLOQUES_EJEMPLO$banco_mundial
  res <- coinercia(ejemplo_statis("banco_mundial"), b$x, b$y, permutaciones = 99, semilla = 7)
  txt <- sintaxis_coinercia(res, list(tipo = "ejemplo", nombre = "banco_mundial"))
  codigo <- sub("^library\\(biplotlab\\)$", "", strsplit(txt, "\n")[[1]])
  e <- new.env()
  eval(parse(text = codigo[seq_len(grep("^\\)$", codigo)[1])]), envir = e)
  expect_equal(e$res$diagnosticos$RV, res$diagnosticos$RV)
  expect_equal(e$res$test$p, res$test$p)
  b <- BLOQUES_EJEMPLO$meau
  s <- statico(ejemplo_statis("meau"), b$x, b$y, "Estacion", "Sitio", "total", "centrar")
  txt <- sintaxis_statico(s, list(tipo = "ejemplo", nombre = "meau"))
  codigo <- sub("^library\\(biplotlab\\)$", "", strsplit(txt, "\n")[[1]])
  e <- new.env()
  eval(parse(text = codigo[seq_len(grep("^\\)$", codigo)[1])]), envir = e)
  expect_equal(e$res$inercia, s$inercia)
})

test_that("las tablas se guardan en Excel con nombres de hoja válidos", {
  b <- BLOQUES_EJEMPLO$meau
  c1 <- costatis(ejemplo_statis("meau"), b$x, b$y, "Estacion", "Sitio", permutaciones = 0,
                 nombre_x = "Ambiente", nombre_y = "Efemerópteros")
  f <- tempfile(fileext = ".xlsx")
  guardar_xlsx(c1$tablas, f, idioma = "es")
  hojas <- readxl::excel_sheets(f)
  expect_length(hojas, length(c1$tablas))
  expect_true(all(nchar(hojas) <= 31) && !any(grepl(":", hojas)))
})

datos_prueba <- function(nombre) {
  df <- ejemplo_statis(nombre)
  shiny::reactive(list(df = df, origen = list(tipo = "ejemplo", nombre = nombre),
                       etiqueta = nombre, carga = 1L,
                       nominales = BLOQUES_EJEMPLO[[nombre]]$nominales))
}

test_that("pantalla de Co-inercia: del ejemplo al resultado", {
  shiny::testServer(mod_coinercia_server, args = list(datos = datos_prueba("banco_mundial")), {
    b <- BLOQUES_EJEMPLO$banco_mundial
    session$setInputs(var_x = b$x, var_y = b$y, prep_x = "estandarizar", prep_y = "estandarizar",
                      ejes = 2, eje_h = 1, eje_v = 2, voltear_h = TRUE, voltear_v = FALSE,
                      permutaciones = 999, semilla = 123, nombre_x = "Pobreza",
                      nombre_y = "Protección social", etiquetas = TRUE, tam_texto = 3.2)
    expect_equal(res()$test$p, 0.213)
    expect_equal(res()$volteados, 1L)
    expect_match(output$sintaxis, "coinercia\\(")
  })
})

test_that("pantallas de STATICO y CoSTATIS", {
  b <- BLOQUES_EJEMPLO$meau
  entradas <- list(var_x = b$x, var_y = b$y, ocasion = "Estacion", individuo = "Sitio",
                   prep_x = "total", prep_y = "centrar", ejes = 2, eje_h = 1, eje_v = 2,
                   voltear_h = FALSE, voltear_v = FALSE, nombre_x = "Ambiente",
                   nombre_y = "Efemerópteros", etiquetas = TRUE, tam_texto = 3.2,
                   tray_que = "variables.X", tray_conectar = TRUE, permutaciones = 99, semilla = 1)
  shiny::testServer(mod_statico_server, args = list(datos = datos_prueba("meau")), {
    do.call(session$setInputs, entradas)
    expect_equal(res()$interestructura$XY$ocasiones$`cos²`, c(0.498, 0.813, 0.956, 0.685), tolerance = 1e-3)
    expect_match(output$sintaxis, "statico\\(")
  })
  shiny::testServer(mod_costatis_server, args = list(datos = datos_prueba("meau")), {
    do.call(session$setInputs, entradas)
    expect_equal(res()$diagnosticos$RV, 0.8204725, tolerance = 1e-6)
    session$setInputs(individuo = "")
    expect_error(res(), class = "shiny.silent.error")
  })
})

test_that("la interfaz completa se construye", {
  ui <- app_ui(NULL)
  html <- as.character(ui)
  expect_match(html, "BiplotLab")
  expect_match(html, "Co-inercia")
  expect_match(html, "STATICO")
  expect_match(html, "CoSTATIS")
})

test_that("formato largo con la ocasión en la primera columna: no se pierde", {
  f <- tempfile(fileext = ".csv")
  m <- ejemplo_statis("meau")
  utils::write.csv(m, f, row.names = FALSE)
  expect_warning(d <- leer_datos(f), "repetidos")
  expect_true(all(c("Estacion", "Sitio") %in% names(d)))
  b <- BLOQUES_EJEMPLO$meau
  s1 <- statico(d, b$x, b$y, "Estacion", "Sitio", "total", "centrar")
  s2 <- statico(m, b$x, b$y, "Estacion", "Sitio", "total", "centrar")
  expect_equal(s1$diagnosticos, s2$diagnosticos)
  # sus columnas de ocasión e individuo no entran por defecto como variables
  via <- columnas_via(list(df = d, nominales = detectar_nominales(d)))
  expect_true(all(c("Estacion", "Sitio") %in% via))
})

test_that("la app unida reúne los dos grupos y el reparto del grupo", {
  a <- registro_analisis()
  ids <- vapply(a, `[[`, "", "id")
  expect_false(anyDuplicated(ids) > 0)
  expect_true(all(vapply(a, function(m) m$grupo %in% c("Biplot", "K-tablas"), TRUE)))
  expect_true(all(c("hj", "gge", "coinercia", "statico", "costatis") %in% ids[vapply(a, function(m) isTRUE(m$listo), TRUE)]))
  expect_true(all(c("statis_canonico", "statis_dual_canonico", "statis_neutro", "statis_dual_neutro", "metabiplot",
                    "hj_dinamico", "coinercia_neutro", "statico_neutro", "costatis_neutro") %in% ids))
  expect_false(any(c("pta", "afm") %in% ids))
  expect_equal(ejemplo_biplot("meau"), ejemplo_statis("meau"))
  for (m in c("coinercia", "statico", "costatis")) expect_false(is.null(teoria_metodo(m)$titulo))
})
