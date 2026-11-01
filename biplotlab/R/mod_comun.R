# =============================================================================
# Piezas de pantalla comunes a los módulos K-tablas (Co-inercia, STATICO,
# CoSTATIS y los que agregue el grupo). Cada módulo arma su panel con estas
# secciones y conecta sus salidas con estos ayudantes, así todos se ven y se
# comportan igual: tablas X e Y, preparación, plano, «Voltear», test,
# descargas, Tablas, Lectura guiada, Fundamentos y Sintaxis R.
# =============================================================================

# ---------------------------------------------------------------- Opciones ----

#' Sección «Tablas»: ocasión, individuos y variables de X e Y
#' @param ns función `NS` del módulo.
#' @param ocasion si el método usa una tercera vía (ocasiones).
#' @param individuo_obligatorio si hay que elegir la columna de individuos.
#' @export
opciones_tablas <- function(ns, ocasion = TRUE, individuo_obligatorio = FALSE) {
  multi <- function(id, etiqueta) {
    shiny::selectizeInput(ns(id), etiqueta, choices = NULL, multiple = TRUE,
                          options = list(plugins = list("remove_button"), placeholder = "Elige variables"))
  }
  seccion_opciones(
    "Tablas",
    if (ocasion) shiny::tagList(
      shiny::selectInput(ns("ocasion"), "Ocasión (tercera vía)", choices = NULL),
      shiny::selectInput(ns("individuo"), if (individuo_obligatorio) "Individuos (obligatorio)" else "Individuos",
                         choices = NULL),
      shiny::p(class = "bl-help",
               "Hoja en formato largo: una fila por individuo y ocasión. Las ocasiones se toman en el orden en que aparecen en los datos.")
    ),
    shiny::textInput(ns("nombre_x"), "Nombre de la tabla X", "X"),
    multi("var_x", "Variables de X"),
    shiny::textInput(ns("nombre_y"), "Nombre de la tabla Y", "Y"),
    multi("var_y", "Variables de Y"),
    shiny::p(class = "bl-help", "Las dos tablas describen a los mismos individuos. Una variable no puede estar en las dos.")
  )
}

#' Sección «Preparación» de las dos tablas
#' @param ns función `NS`.
#' @param opciones vector con nombre de preparaciones.
#' @param x,y preparación por defecto de cada tabla.
#' @param ayuda texto de ayuda.
#' @export
opciones_preparacion <- function(ns, opciones, x, y, ayuda = NULL) {
  seccion_opciones(
    "Preparación",
    shiny::selectInput(ns("prep_x"), "Tabla X", opciones, selected = x),
    shiny::selectInput(ns("prep_y"), "Tabla Y", opciones, selected = y),
    if (!is.null(ayuda)) shiny::p(class = "bl-help", ayuda)
  )
}

#' Sección «Plano»: ejes, volteo y etiquetas
#' @param ns función `NS`.
#' @export
opciones_plano <- function(ns) {
  seccion_opciones(
    "Plano",
    shiny::numericInput(ns("ejes"), "Ejes que se conservan", 2, min = 2, max = 6, step = 1),
    shiny::div(class = "bl-fila-2",
               shiny::selectInput(ns("eje_h"), "Eje horizontal", 1:2, selected = 1),
               shiny::selectInput(ns("eje_v"), "Eje vertical", 1:2, selected = 2)),
    shiny::checkboxInput(ns("voltear_h"), "Voltear eje horizontal", FALSE),
    shiny::checkboxInput(ns("voltear_v"), "Voltear eje vertical", FALSE),
    shiny::p(class = "bl-help", "Voltear cambia solo la orientación: distancias, ángulos e inercia no cambian."),
    shiny::checkboxInput(ns("etiquetas"), "Mostrar etiquetas", TRUE),
    shiny::sliderInput(ns("tam_texto"), "Tamaño de las etiquetas", min = 2, max = 6, value = 3.2, step = 0.2)
  )
}

#' Sección «Pintar solo algunos puntos» (K-tablas)
#' @param ns función `NS`.
#' @export
opciones_destacar <- function(ns) {
  seccion_opciones(
    "Pintar solo algunos puntos",
    shiny::p(class = "bl-help", "Solo cambia el dibujo: el análisis no se recalcula y el encuadre no se mueve."),
    shiny::selectizeInput(ns("destacar"), "Qué se pinta", choices = NULL, multiple = TRUE,
                          options = list(plugins = list("remove_button"), placeholder = "Todos")),
    shiny::radioButtons(ns("destacar_resto"), "Los demás puntos",
                        c("Atenuar (gris, sin nombre)" = "atenuar", "Ocultar" = "ocultar"), inline = TRUE),
    abierta = FALSE
  )
}

#' Rellena el selector de «Pintar solo algunos puntos»
#' @param session sesión del módulo.
#' @param elementos reactivo con los nombres que se pueden pintar.
#' @export
observar_destacar <- function(session, elementos) {
  shiny::observeEvent(elementos(), {
    e <- elementos()
    shiny::updateSelectizeInput(session, "destacar", choices = e,
                                selected = intersect(shiny::isolate(session$input$destacar), e))
  })
}

#' Sección «Test de permutación»
#' @param ns función `NS`.
#' @export
opciones_test <- function(ns) {
  seccion_opciones(
    "Test de permutación",
    shiny::numericInput(ns("permutaciones"), "Permutaciones (0 = sin test)", 999, min = 0, max = 99999, step = 100),
    shiny::numericInput(ns("semilla"), "Semilla", 123, min = 1, step = 1),
    shiny::p(class = "bl-help", "H0: no existe co-estructura entre X e Y. Con la misma semilla el p-valor se repite exactamente.")
  )
}

# -------------------------------------------------------------- Resultados ----

#' Gráfico con botones de descarga (PNG, PDF, SVG)
#' @param ns función `NS`.
#' @param id id de la salida.
#' @param plano `TRUE` si es un plano con encuadre rectangular.
#' @export
salida_grafico <- function(ns, id, plano = TRUE) {
  shiny::tagList(
    if (plano) salida_biplot(ns, id) else shiny::plotOutput(ns(id), height = "420px"),
    shiny::div(class = "bl-descargas",
               shiny::downloadButton(ns(paste0(id, "_png")), "PNG", class = "btn-sm"),
               shiny::downloadButton(ns(paste0(id, "_pdf")), "PDF", class = "btn-sm"),
               shiny::downloadButton(ns(paste0(id, "_svg")), "SVG", class = "btn-sm"))
  )
}

#' Pestañas comunes del final: Tablas, Lectura guiada, Fundamentos, Sintaxis R
#' @param ns función `NS`.
#' @param metodo id del método (para Fundamentos).
#' @export
pestanas_comunes <- function(ns, metodo) {
  list(
    shiny::tabPanel("Tablas",
                    shiny::div(class = "bl-fila-tabla",
                               shiny::selectInput(ns("tabla_sel"), NULL, choices = NULL, width = "100%"),
                               shiny::downloadButton(ns("tablas_xlsx"), "Todas en Excel", class = "btn-sm"),
                               shiny::downloadButton(ns("bgl_rds"), "Objeto BGL (RDS)", class = "btn-sm"),
                               shiny::downloadButton(ns("bgl_json"), "Objeto BGL (JSON)", class = "btn-sm")),
                    DT::DTOutput(ns("tabla"))),
    shiny::tabPanel("Lectura guiada", shiny::uiOutput(ns("lectura"))),
    shiny::tabPanel("Fundamentos", teoria_ui(metodo)),
    shiny::tabPanel(shiny::span(shiny::icon("code"), "Sintaxis R"),
                    shiny::div(class = "bl-code", shiny::verbatimTextOutput(ns("sintaxis"))))
  )
}

# ------------------------------------------------------------------ Server ----

# Nombres que sugieren una vía del cubo (ocasión o individuo), aunque sean números
PATRON_VIA <- paste0("^(ocasi[oó]n|occasion|estaci[oó]n|season|periodo|period|anio|año|year|",
                     "sitio|site|pa[ií]s|country|individuo|tiempo|time|mes|month)")

# Nominales que valen en K-tablas: BiplotLab también propone los enteros con pocos
# valores, que aquí suelen ser abundancias (0 a 5 individuos, por ejemplo) y sí
# son variables
nominales_ktabla <- function(d) {
  solo_enteros <- setdiff(detectar_nominales(d$df), detectar_nominales(d$df, enteros = FALSE))
  setdiff(d$nominales, solo_enteros)
}

# Columnas numéricas que pueden ser variables (no nominales)
variables_numericas <- function(d) {
  df <- d$df
  setdiff(names(df)[vapply(df, is.numeric, logical(1))], nominales_ktabla(d))
}

# Columnas que se proponen como vías: primero las nominales y luego las numéricas
# cuyo nombre lo sugiere (un año guardado como número, por ejemplo)
columnas_via <- function(d) {
  union(nominales_ktabla(d), grep(PATRON_VIA, names(d$df), value = TRUE, ignore.case = TRUE))
}

# Ejemplo cargado con su reparto de columnas (BLOQUES_EJEMPLO), o NULL
bloques_de <- function(d) {
  if (identical(d$origen$tipo, "ejemplo")) BLOQUES_EJEMPLO[[d$origen$nombre]] else NULL
}

#' Rellenar los controles de tablas cuando cambian los datos
#'
#' Con una base de ejemplo propone el reparto de las prácticas; con un
#' archivo, todas las variables numéricas en X y ninguna en Y.
#' @param session sesión del módulo.
#' @param datos reactivo con los datos de la app.
#' @param ocasion si el módulo tiene ocasión e individuos.
#' @export
observar_tablas <- function(session, datos, ocasion = TRUE) {
  input <- session$input
  ultima_carga <- -1L
  shiny::observeEvent(datos(), {
    d <- datos()
    num <- variables_numericas(d)
    if (identical(d$carga, ultima_carga)) {
      # Misma base editada (celdas, columnas, nominales): se conserva la elección
      shiny::updateSelectizeInput(session, "var_x", choices = num,
                                  selected = intersect(shiny::isolate(input$var_x), num))
      shiny::updateSelectizeInput(session, "var_y", choices = num,
                                  selected = intersect(shiny::isolate(input$var_y), num))
      return()
    }
    ultima_carga <<- d$carga
    b <- bloques_de(d)
    sel_x <- if (!is.null(b)) intersect(b$x, num) else setdiff(num, columnas_via(d))
    sel_y <- if (!is.null(b)) intersect(b$y, num) else character()
    shiny::updateSelectizeInput(session, "var_x", choices = num, selected = sel_x)
    shiny::updateSelectizeInput(session, "var_y", choices = num, selected = sel_y)
    shiny::updateTextInput(session, "nombre_x", value = b$nombre_x %||% "X")
    shiny::updateTextInput(session, "nombre_y", value = b$nombre_y %||% "Y")
    if (!is.null(b)) {
      shiny::updateSelectInput(session, "prep_x", selected = b$preparacion_x)
      shiny::updateSelectInput(session, "prep_y", selected = b$preparacion_y)
    }
    if (ocasion) {
      via <- columnas_via(d)
      cand <- union(via, names(d$df))
      oc <- b$ocasion %||% cand[1]
      shiny::updateSelectInput(session, "ocasion", choices = cand, selected = oc)
      ind <- b$individuo %||% utils::head(setdiff(via, oc), 1)
      shiny::updateSelectInput(session, "individuo",
                               choices = c("(nombres de fila)" = "", setdiff(cand, oc)),
                               selected = if (length(ind)) ind else "")
    }
  })
}

#' Ejes del plano: actualiza los selectores al cambiar «Ejes que se conservan»
#' @param input,session del módulo.
#' @return reactivo con los dos ejes elegidos.
#' @export
ejes_del_plano <- function(input, session) {
  shiny::observeEvent(input$ejes, {
    k <- max(2, min(6, as.integer(input$ejes %||% 2)))
    shiny::updateSelectInput(session, "eje_h", choices = seq_len(k),
                             selected = min(as.integer(input$eje_h %||% 1), k))
    shiny::updateSelectInput(session, "eje_v", choices = seq_len(k),
                             selected = min(as.integer(input$eje_v %||% 2), k))
  })
  shiny::reactive({
    e <- as.integer(c(input$eje_h %||% 1, input$eje_v %||% 2))
    shiny::validate(shiny::need(e[1] != e[2], "Elige dos ejes distintos."))
    e
  })
}

#' Resultado con los ejes volteados según las casillas
#' @param res_ok reactivo con el resultado.
#' @param ejes reactivo de [ejes_del_plano()].
#' @param input del módulo.
#' @export
resultado_volteado <- function(res_ok, ejes, input) {
  shiny::reactive({
    e <- ejes()
    voltear_resultado(res_ok(), e[c(isTRUE(input$voltear_h), isTRUE(input$voltear_v))])
  })
}

#' Cálculo protegido: devuelve el resultado o el error para mostrarlo
#' @param expr expresión que calcula.
#' @export
calcular_seguro <- function(expr) tryCatch(expr, error = function(e) e)

#' Conectar un gráfico con su salida y sus descargas
#' @param output,session del módulo.
#' @param id id de la salida (el mismo de [salida_grafico()]).
#' @param grafico reactivo que devuelve el ggplot.
#' @param nombre nombre base del archivo.
#' @param plano `TRUE` si usa el encuadre rectangular.
#' @export
conectar_grafico <- function(output, session, id, grafico, nombre, plano = TRUE) {
  if (plano) {
    output[[id]] <- render_biplot(grafico, session, salida = id)
  } else {
    output[[id]] <- shiny::renderPlot(suppressWarnings(print(traducir_grafico(grafico()))), res = 96)
  }
  for (ext in c("png", "pdf", "svg")) local({
    ext <- ext
    archivo <- function() paste0(nombre, ".", ext)
    args <- if (ext == "png") list(dpi = 300) else list()
    output[[paste0(id, "_", ext)]] <- if (plano) {
      do.call(descarga_biplot, c(list(grafico, archivo, ext), args))
    } else {
      shiny::downloadHandler(archivo, function(file) {
        do.call(ggplot2::ggsave, c(list(file, traducir_grafico(grafico()), width = 9, height = 5.5), args))
      })
    }
  })
}

#' Conectar las pestañas comunes (Tablas, Lectura guiada, Sintaxis R)
#' @param output,input,session del módulo.
#' @param res reactivo con el resultado (ya volteado).
#' @param lectura función que recibe el resultado y los ejes y da las frases.
#' @param ejes reactivo con el plano.
#' @param sintaxis reactivo con el texto de la Sintaxis R.
#' @param nombre nombre base del Excel.
#' @export
conectar_comunes <- function(output, input, session, res, lectura, ejes, sintaxis, nombre) {
  shiny::observeEvent(names(res()$tablas), {
    nm <- names(res()$tablas)
    shiny::updateSelectInput(session, "tabla_sel", choices = nm,
                             selected = if (isTRUE(input$tabla_sel %in% nm)) input$tabla_sel else nm[1])
  })
  output$tabla <- DT::renderDT({
    t <- res()$tablas[[input$tabla_sel %||% 1]]
    shiny::req(t)
    tabla_resultados(t, 4)
  })
  output$tablas_xlsx <- shiny::downloadHandler(
    function() paste0(nombre, "_tablas.xlsx"),
    function(file) guardar_xlsx(res()$tablas, file))
  for (ext in c("rds", "json")) local({
    ext <- ext
    output[[paste0("bgl_", ext)]] <- shiny::downloadHandler(
      function() paste0(nombre, "_bgl.", ext),
      function(file) {
        # El nombre temporal de Shiny puede no llevar la extensión que BGL_Export() lee
        destino <- tempfile(fileext = paste0(".", ext))
        BGL_Export(BGL_Create(res()), destino)
        file.copy(destino, file, overwrite = TRUE)
      })
  })
  output$lectura <- shiny::renderUI({
    frases <- traducir_lectura(lectura(res(), ejes()))
    shiny::div(class = "bl-lectura", shiny::tags$ul(lapply(frases, shiny::tags$li)))
  })
  output$sintaxis <- shiny::renderText(sintaxis())
  registrar_informe(
    session, function() res()$titulo,
    graficos = function() {
      b <- BGL_Create(res())
      inter <- if (length(res()$interestructura) > 1) c("X", "Y") else "X"
      unlist(lapply(b$Graphics$disponibles, function(g) {
        if (g %in% c("interestructura", "pesos")) lapply(inter, function(s) BGL_Plot(b, g, ejes(), serie = s))
        else if (g == "test" && is.null(res()$test)) list()
        else list(BGL_Plot(b, g, ejes()))
      }), recursive = FALSE)
    },
    tablas = function() res()$tablas,
    lectura = function() traducir_lectura(lectura(res(), ejes())),
    preparacion = function() preparacion_informe(res()))
}

#' Mostrar el error del cálculo (o nada)
#' @param resultado reactivo con el resultado o un error.
#' @export
render_error <- function(resultado) {
  shiny::renderUI({
    r <- resultado()
    if (inherits(r, "error")) caja_error(conditionMessage(r))
  })
}

#' Resultado válido (detiene las salidas si hubo error)
#' @param resultado reactivo con el resultado o un error.
#' @export
resultado_valido <- function(resultado) {
  shiny::reactive({
    r <- resultado()
    shiny::validate(shiny::need(!inherits(r, "error"), ""))
    r
  })
}
