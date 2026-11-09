# =============================================================================
# Módulos de la familia STATIS: la pantalla de
#   STATIS                  (mismos individuos)            -> statis_directo()
#   STATIS DUAL             (mismas variables)             -> statis_dual()
#   STATIS Neutrosófico     (STATIS sobre puntuaciones)    -> statis_neutro()
# El DUAL Neutrosófico está en mod_statis_dual_neutro.R. Los tres comparten esta
# pantalla: opciones de datos, preparación, plano, «pintar solo algunos puntos»,
# trayectorias y, bajo cada gráfico, la descripción de cómo se lee. El cálculo
# está en calculo_statis.R y calculo_statis_dual_neutro.R.
# Las bases son de formato largo: una fila por individuo y ocasión, con una
# columna de ocasión. Sirven para cualquier base de datos con esa forma.
# =============================================================================

#' @export
analisis_statis <- function() {
  analisis_familia_statis("statis", "STATIS", "table-cells", "direct", FALSE,
    "Mismos individuos en todas las ocasiones (obligatorio); las variables pueden coincidir o no",
    "Escoufier (1973); Lavit et al. (1994); Abdi et al. (2012)")
}

#' @export
analisis_statis_dual <- function() {
  analisis_familia_statis("statis_dual", "STATIS DUAL", "table-columns", "dual", FALSE,
    "Mismas variables en todas las ocasiones (obligatorio); los individuos pueden coincidir o no",
    "Lavit (1988); Abdi et al. (2012)")
}

#' @export
analisis_statis_neutro <- function() {
  analisis_familia_statis("statis_neutro", "STATIS Neutrosófico", "circle-half-stroke", "direct", TRUE,
    "STATIS con datos neutrosóficos (T, I, F): mismos individuos en todas las ocasiones",
    "Lavit et al. (1994); Abdi et al. (2012); Bustos-Chiliquinga et al. (2026)")
}

analisis_familia_statis <- function(metodo, titulo, icono, familia, neutro, descripcion, referencia) {
  list(
    id = metodo, titulo = titulo, grupo = "K-tablas", subgrupo = "K tablas", icono = icono,
    descripcion = descripcion, listo = TRUE,
    opciones_ui = function(id) opciones_familia_statis(id, titulo, referencia, familia, neutro),
    resultados_ui = function(id) resultados_familia_statis(id, metodo, titulo, familia, neutro),
    server = function(id, datos, compartido = NULL) {
      servidor_familia_statis(id, datos, metodo, familia, neutro)
    }
  )
}

# ------------------------------------------------------------------ Opciones --

opciones_familia_statis <- function(id, titulo, referencia, familia, neutro) {
  ns <- shiny::NS(id)
  ayuda <- function(...) shiny::p(class = "bl-help", ...)
  directo <- familia == "direct"
  shiny::tagList(
    encabezado_opciones(titulo, referencia),
    seccion_opciones(
      "Datos de entrada",
      shiny::selectInput(ns("ocasion"), "Ocasión (tercera vía)", choices = NULL),
      shiny::selectInput(ns("individuo"), if (directo) "Individuos (obligatorio)" else "Individuos", choices = NULL),
      ayuda(if (directo) "Hoja en formato largo: una fila por individuo y ocasión. Los mismos individuos deben estar en todas las ocasiones; el aplicativo los ordena igual."
            else "Hoja en formato largo: una fila por individuo y ocasión. Las mismas variables en todas las ocasiones; los individuos pueden cambiar."),
      if (!neutro) shiny::tagList(
        shiny::selectizeInput(ns("var_x"), "Variables", choices = NULL, multiple = TRUE,
                              options = list(plugins = list("remove_button"), placeholder = "Elige variables")),
        ayuda("Solo variables numéricas. Las columnas de agrupación (nominales) no entran como variables.")),
      if (neutro) opciones_datos_neutro_statis(ns)
    ),
    if (neutro) shiny::conditionalPanel(
      "input.formato == 'respuestas'", ns = ns,
      seccion_opciones(
        "Regla de recodificación",
        ayuda("Cada respuesta se asigna a verdad, indeterminación, falsedad o neutral. La propuesta inicial sale del texto de la respuesta: revísala y justifícala. Los faltantes siempre se excluyen."),
        shiny::uiOutput(ns("reglas")))),
    if (neutro) seccion_opciones(
      "Puntuación",
      shiny::div(class = "bl-fila-2",
                 shiny::numericInput(ns("w_T"), "Peso de T", 1 / 3, min = 0, max = 1, step = 0.05),
                 shiny::numericInput(ns("w_I"), "Peso de 1 − I", 1 / 3, min = 0, max = 1, step = 0.05)),
      shiny::numericInput(ns("w_F"), "Peso de 1 − F", 1 / 3, min = 0, max = 1, step = 0.05),
      ayuda(AYUDA_PUNTUACION_NEUTRO)),
    seccion_opciones(
      "Preparación",
      shiny::selectInput(ns("prep_x"), NULL, PREPARACIONES_KTABLA, selected = "parcial"),
      ayuda(AYUDA_PREPARACION_KTABLA)),
    opciones_plano(ns),
    opciones_destacar(ns),
    seccion_opciones(
      "Trayectorias",
      shiny::selectInput(ns("tray_que"), "Qué se proyecta",
                         if (directo) c("Individuos de cada ocasión" = "individuos.X",
                                        "Variables de cada ocasión" = "variables.X")
                         else c("Variables vistas desde cada ocasión" = "variables.X",
                                "Individuos de cada ocasión" = "individuos.X")),
      shiny::selectizeInput(ns("tray_ocasiones"), "Ocasiones", choices = NULL, multiple = TRUE,
                            options = list(plugins = list("remove_button"), placeholder = "Todas")),
      shiny::checkboxInput(ns("tray_conectar"), "Unir las posiciones del mismo elemento", TRUE),
      abierta = FALSE
    )
  )
}

opciones_datos_neutro_statis <- function(ns) {
  ayuda <- function(...) shiny::p(class = "bl-help", ...)
  shiny::tagList(
    shiny::radioButtons(ns("formato"), "¿Cómo vienen T, I y F?",
                        c("Ternas ya calculadas: columnas _T, _I y _F" = "ternas",
                          "Respuestas categóricas: recodificar" = "respuestas")),
    shiny::conditionalPanel(
      "input.formato == 'ternas'", ns = ns,
      shiny::selectizeInput(ns("vars_ternas"), "Variables con ternas", choices = NULL, multiple = TRUE,
                            options = list(plugins = list("remove_button"))),
      ayuda("Para cada variable deben existir nombre_T, nombre_I y nombre_F, con valores entre 0 y 1.")),
    shiny::conditionalPanel(
      "input.formato == 'respuestas'", ns = ns,
      shiny::selectizeInput(ns("vars_resp"), "Preguntas (variables)", choices = NULL, multiple = TRUE,
                            options = list(plugins = list("remove_button"))),
      ayuda("Elige preguntas con la misma escala de respuesta."))
  )
}

# ----------------------------------------------------------------- Resultados --

resultados_familia_statis <- function(id, metodo, titulo, familia, neutro) {
  ns <- shiny::NS(id)
  d <- function(tipo) descripcion_grafico_ui(tipo, familia, neutro)
  pestanas <- c(list(
    shiny::tabPanel("Interestructura",
                    salida_grafico(ns, "g_inter"), d("interestructura"),
                    salida_grafico(ns, "g_pesos", plano = FALSE), d("pesos"),
                    salida_grafico(ns, "g_rv", plano = FALSE), d("rv")),
    shiny::tabPanel("Compromiso",
                    if (familia == "direct") shiny::tagList(salida_grafico(ns, "g_ind"), d("compromiso_individuos")),
                    salida_grafico(ns, "g_circulo"), d("compromiso_variables"),
                    salida_grafico(ns, "g_inercia", plano = FALSE), d("inercia")),
    shiny::tabPanel("Trayectorias", salida_grafico(ns, "g_tray"), d("trayectorias"))),
    if (neutro) list(shiny::tabPanel("Indeterminación", salida_grafico(ns, "g_indet", plano = FALSE),
                                     d("indeterminacion"))),
    pestanas_comunes(ns, metodo))
  shiny::tagList(
    encabezado_resultados(titulo),
    shiny::uiOutput(ns("error")),
    shiny::uiOutput(ns("avisos")),
    shiny::div(class = "bl-card", do.call(shiny::tabsetPanel, pestanas))
  )
}

# ------------------------------------------------------------------- Servidor --

servidor_familia_statis <- function(id, datos, metodo, familia, neutro) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    directo <- familia == "direct"

    if (!neutro) {
      observar_tablas(session, datos, ocasion = TRUE)
    } else {
      shiny::observeEvent(datos(), {
        d <- datos(); df <- d$df
        via <- columnas_via(d)
        cand <- union(via, names(df))
        oc <- cand[1]
        shiny::updateSelectInput(session, "ocasion", choices = cand, selected = oc)
        ind <- utils::head(setdiff(via, oc), 1)
        shiny::updateSelectInput(session, "individuo", choices = c("(nombres de fila)" = "", setdiff(cand, oc)),
                                 selected = if (length(ind)) ind else "")
        tern <- variables_ternas(df)
        shiny::updateSelectizeInput(session, "vars_ternas", choices = tern, selected = tern)
        resp <- names(df)[vapply(df, function(z) { u <- length(unique(stats::na.omit(z))); u >= 2 && u <= 40 },
                                 logical(1))]
        shiny::updateSelectizeInput(session, "vars_resp", choices = setdiff(resp, via),
                                    selected = intersect(shiny::isolate(input$vars_resp), resp))
        if (!length(tern) && length(resp) >= 2) shiny::updateRadioButtons(session, "formato", selected = "respuestas")
      })
    }
    ejes <- ejes_del_plano(input, session)

    # Regla de recodificación de respuestas (igual que el HJ neutrosófico)
    valores <- shiny::reactive({
      shiny::req(input$vars_resp)
      df <- datos()$df
      v <- unique(stats::na.omit(unlist(lapply(df[input$vars_resp], function(z) trimws(as.character(z))))))
      v <- v[nzchar(v)]
      num <- suppressWarnings(as.numeric(v))
      if (!anyNA(num)) v[order(num)] else sort(v)
    })
    if (neutro) {
      output$reglas <- shiny::renderUI({
        v <- valores()
        if (length(v) > 40) return(caja_error("Hay más de 40 respuestas distintas: elige preguntas categóricas."))
        shiny::tagList(lapply(seq_along(v), function(i) {
          anterior <- shiny::isolate(input[[paste0("regla_", i)]])
          shiny::selectInput(ns(paste0("regla_", i)), v[i], CATEGORIAS_NEUTRO,
                             selected = anterior %||% proponer_categoria(v[i]))
        }))
      })
    }
    regla <- shiny::reactive({
      if (!identical(input$formato, "respuestas")) return(NULL)
      v <- valores()
      stats::setNames(vapply(seq_along(v), function(i) input[[paste0("regla_", i)]] %||% proponer_categoria(v[i]),
                             character(1)), v)
    })

    resultado <- shiny::reactive({
      if (is.null(datos())) return(simpleError("Primero carga una base de datos en el panel Datos."))
      shiny::req(input$ocasion %in% names(datos()$df))
      ind <- individuo_input(input$individuo, datos()$df)
      if (neutro) {
        vars <- if (identical(input$formato, "respuestas")) input$vars_resp else input$vars_ternas
        if (length(vars) < 2) return(simpleError("Elige al menos 2 variables."))
        pesos <- c(T = input$w_T %||% NA, I = input$w_I %||% NA, F = input$w_F %||% NA)
        calcular_seguro(statis_neutro(
          datos()$df, vars, input$ocasion, ind, formato = input$formato, regla = regla(), pesos = pesos,
          preparacion = input$prep_x, ejes = input$ejes %||% 2))
      } else {
        if (length(input$var_x) < 2) return(simpleError("Elige al menos 2 variables."))
        calcular_seguro(if (directo) {
          statis_directo(datos()$df, input$var_x, input$ocasion, ind, input$prep_x, input$ejes %||% 2)
        } else {
          statis_dual(datos()$df, input$var_x, input$ocasion, ind, input$prep_x, input$ejes %||% 2)
        })
      }
    })
    output$error <- render_error(resultado)
    output$avisos <- shiny::renderUI({
      r <- resultado()
      if (!inherits(r, "error") && length(r$avisos)) shiny::tagList(lapply(r$avisos, caja_error))
    })
    res <- resultado_volteado(resultado_valido(resultado), ejes, input)
    observar_ocasiones(session, res)
    observar_destacar(session, shiny::reactive(elementos_trayectorias(res(), input)))

    nom <- paste0(metodo, "_")
    conectar_grafico(output, session, "g_inter", shiny::reactive(
      grafico_interestructura(res()$interestructura$X, "Interestructura de las ocasiones")),
      paste0(nom, "interestructura"))
    conectar_grafico(output, session, "g_pesos", shiny::reactive(
      grafico_pesos(res()$interestructura$X)), paste0(nom, "pesos"), plano = FALSE)
    conectar_grafico(output, session, "g_rv", shiny::reactive(
      grafico_rv(res()$interestructura$X)), paste0(nom, "rv"), plano = FALSE)
    conectar_grafico(output, session, "g_circulo", shiny::reactive(
      grafico_circulo(res(), ejes(), input$etiquetas, input$tam_texto)), paste0(nom, "circulo"))
    conectar_grafico(output, session, "g_inercia", shiny::reactive(
      grafico_inercia(res())), paste0(nom, "inercia"), plano = FALSE)
    if (directo) {
      conectar_grafico(output, session, "g_ind", shiny::reactive(
        grafico_individuos(res(), ejes(), input$etiquetas, input$tam_texto,
                           destacados = input$destacar, resto = input$destacar_resto %||% "atenuar")),
        paste0(nom, "individuos"))
    }
    conectar_grafico(output, session, "g_tray", shiny::reactive(
      grafico_trayectorias_input(res(), input, ejes())), paste0(nom, "trayectorias"))
    if (neutro) {
      conectar_grafico(output, session, "g_indet", shiny::reactive(
        grafico_indeterminacion(res())), paste0(nom, "indeterminacion"), plano = FALSE)
    }

    conectar_comunes(output, input, session, res,
                     if (directo) lectura_statis else lectura_statis_dual, ejes,
                     shiny::reactive(sintaxis_statis(res(), datos()$origen)), metodo)
    res
  })
}

# ----------------------------------------------------------------- Sintaxis R --

#' Sintaxis R de un STATIS, STATIS DUAL o STATIS neutrosófico
#' @param res resultado de [statis_directo()], [statis_dual()] o [statis_neutro()].
#' @param origen origen de los datos.
#' @export
sintaxis_statis <- function(res, origen) {
  a <- res$llamada
  m <- res$metodo
  funcion <- c(statis = "statis_directo", statis_dual = "statis_dual", statis_neutro = "statis_neutro")[[m]]
  titulo <- c(statis = "STATIS", statis_dual = "STATIS DUAL", statis_neutro = "STATIS sobre puntuaciones neutrosóficas")[[m]]
  neutro <- m == "statis_neutro"
  regla <- if (neutro && identical(a$formato, "respuestas")) c(
    "# Regla de recodificación: respuesta -> T, I, F, N (neutral: 0, 0, 0) o X (excluir la fila)",
    sprintf("regla <- c(%s)", paste(sprintf("%s = \"%s\"", encodeString(names(a$regla), quote = "`"), a$regla),
                                    collapse = ",\n           ")))
  graficos <- c(
    "grafico_interestructura(res$interestructura$X)   # mapa de las ocasiones",
    "grafico_pesos(res$interestructura$X)             # pesos y cos²",
    "grafico_rv(res$interestructura$X)                # matriz RV",
    if (m != "statis_dual") "grafico_individuos(res)                          # compromiso: individuos",
    "grafico_circulo(res)                             # círculo de correlaciones",
    "grafico_inercia(res)                             # valores propios",
    "grafico_trayectorias(res, tipo = \"variables\")    # o tipo = \"individuos\"",
    if (neutro) "grafico_indeterminacion(res)")
  paste(c(
    paste("#", titulo), sintaxis_paquete(), "", sintaxis_lectura(origen), "",
    regla,
    sprintf("res <- %s(", funcion),
    "  datos,",
    sprintf("  variables = %s,", vector_a_r(a$variables, 16)),
    sprintf("  ocasion = %s, individuo = %s,", arg_r(a$ocasion), arg_r(a$individuo)),
    if (neutro) c(sprintf("  formato = %s,", arg_r(a$formato)),
                  if (identical(a$formato, "respuestas")) "  regla = regla,",
                  sprintf("  pesos = c(T = %s, I = %s, F = %s),", format(a$pesos[["T"]], digits = 6),
                          format(a$pesos[["I"]], digits = 6), format(a$pesos[["F"]], digits = 6))),
    sprintf("  preparacion = %s, ejes = %d", arg_r(a$preparacion_x), as.integer(a$ejes)),
    ")",
    "res",
    if (neutro) "res$avisos                          # F = 1 - T, I = 0, ...",
    "res$tablas$`Pesos y cos²`",
    if (m == "statis_dual") "res$tablas$`Compromiso: variables`" else "res$tablas$`Compromiso: individuos`",
    if (neutro) "res$tablas$`Comparación con T solo`",
    graficos,
    if (length(res$volteados)) sprintf("# Voltear ejes: grafico_circulo(voltear_resultado(res, %s))",
                                       vector_a_r(res$volteados)),
    "",
    "# Lo mismo con ade4 (Abdi et al., 2012):",
    if (m == "statis_dual") c(
      "# w <- ade4::withinpca(X, ocasion, scaling = \"partial\", scannf = FALSE)",
      "# st <- ade4::statis(ade4::ktab.within(w), scannf = FALSE); st$RV; st$RV.tabw; st$C.eig")
    else c(
      "# tablas: una data.frame por ocasión, con los mismos individuos en el mismo orden",
      "# kt <- ade4::ktab.list.dudi(lapply(tablas, ade4::dudi.pca, center = FALSE, scale = FALSE, scannf = FALSE))",
      "# st <- ade4::statis(kt, scannf = FALSE); st$RV; st$RV.tabw; st$C.eig")
  ), collapse = "\n")
}
