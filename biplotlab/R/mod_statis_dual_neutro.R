# =============================================================================
# Módulo DUAL Neutrosófico: la pantalla. El cálculo está en
# calculo_statis_dual_neutro.R (statis_dual_neutro()).
# Las ternas se leen como en el HJ neutrosófico (columnas _T, _I, _F o
# respuestas recodificadas), con una columna de ocasión: formato largo.
# =============================================================================

#' @export
analisis_statis_dual_neutro <- function() {
  list(
    id = "statis_dual_neutro", titulo = "DUAL Neutrosófico", grupo = "K-tablas", subgrupo = "K tablas",
    icono = "circle-half-stroke",
    descripcion = "STATIS DUAL sobre puntuaciones neutrosóficas: mismas variables en todas las ocasiones; los individuos pueden coincidir o no",
    listo = TRUE,
    opciones_ui = mod_statis_dual_neutro_opciones_ui,
    resultados_ui = mod_statis_dual_neutro_resultados_ui,
    server = mod_statis_dual_neutro_server
  )
}

AYUDA_PUNTUACION_NEUTRO <- paste(
  "Cada terna se resume con S = wT·T + wI·(1 − I) + wF·(1 − F). Los pesos se normalizan para que sumen 1.",
  "Con pesos iguales es la misma puntuación T − I − F del HJ neutrosófico (después de centrar).",
  "Si en tus datos F = 1 − T, la falsedad no aporta nada propio y la app lo avisa.")

mod_statis_dual_neutro_opciones_ui <- function(id) {
  ns <- shiny::NS(id)
  ayuda <- function(...) shiny::p(class = "bl-help", ...)
  shiny::tagList(
    encabezado_opciones("DUAL Neutrosófico", "Lavit (1988); Abdi et al. (2012); Bustos-Chiliquinga et al. (2026)"),
    seccion_opciones(
      "Datos de entrada",
      shiny::selectInput(ns("ocasion"), "Ocasión (tercera vía)", choices = NULL),
      shiny::selectInput(ns("individuo"), "Individuos", choices = NULL),
      ayuda("Hoja en formato largo: una fila por individuo y ocasión. Las mismas variables en todas las ocasiones; los individuos pueden cambiar."),
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
    ),
    shiny::conditionalPanel(
      "input.formato == 'respuestas'", ns = ns,
      seccion_opciones(
        "Regla de recodificación",
        ayuda("Cada respuesta se asigna a verdad, indeterminación, falsedad o neutral. La propuesta inicial sale del texto de la respuesta: revísala y justifícala. Los faltantes siempre se excluyen."),
        shiny::uiOutput(ns("reglas")))
    ),
    seccion_opciones(
      "Puntuación",
      shiny::div(class = "bl-fila-2",
                 shiny::numericInput(ns("w_T"), "Peso de T", 1 / 3, min = 0, max = 1, step = 0.05),
                 shiny::numericInput(ns("w_I"), "Peso de 1 − I", 1 / 3, min = 0, max = 1, step = 0.05)),
      shiny::numericInput(ns("w_F"), "Peso de 1 − F", 1 / 3, min = 0, max = 1, step = 0.05),
      ayuda(AYUDA_PUNTUACION_NEUTRO)
    ),
    seccion_opciones(
      "Preparación",
      shiny::selectInput(ns("prep"), NULL, PREPARACIONES_KTABLA, selected = "parcial"),
      ayuda("Parcial (por defecto): cada ocasión da su matriz de correlaciones, como en el STATIS DUAL de Abdi et al. (2012). Necesita que ninguna puntuación sea constante dentro de una ocasión.")
    ),
    opciones_plano(ns),
    opciones_destacar(ns),
    seccion_opciones(
      "Trayectorias",
      shiny::selectInput(ns("tray_que"), "Qué se proyecta",
                         c("Variables vistas desde cada ocasión" = "variables.X",
                           "Individuos de cada ocasión" = "individuos.X")),
      shiny::selectizeInput(ns("tray_ocasiones"), "Ocasiones", choices = NULL, multiple = TRUE,
                            options = list(plugins = list("remove_button"), placeholder = "Todas")),
      shiny::checkboxInput(ns("tray_conectar"), "Unir las posiciones del mismo elemento", TRUE),
      abierta = FALSE
    )
  )
}

mod_statis_dual_neutro_resultados_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_resultados("STATIS DUAL neutrosófico"),
    shiny::uiOutput(ns("error")),
    shiny::uiOutput(ns("avisos")),
    shiny::div(
      class = "bl-card",
      do.call(shiny::tabsetPanel, c(list(
        shiny::tabPanel("Interestructura", salida_grafico(ns, "g_inter"),
                        salida_grafico(ns, "g_pesos", plano = FALSE)),
        shiny::tabPanel("Compromiso", salida_grafico(ns, "g_circulo")),
        shiny::tabPanel("Trayectorias", salida_grafico(ns, "g_tray")),
        shiny::tabPanel("Indeterminación", salida_grafico(ns, "g_indet", plano = FALSE))),
        pestanas_comunes(ns, "statis_dual_neutro")))
    )
  )
}

mod_statis_dual_neutro_server <- function(id, datos, compartido = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
    ejes <- ejes_del_plano(input, session)

    # Regla de recodificación (igual que el HJ neutrosófico)
    valores <- shiny::reactive({
      shiny::req(input$vars_resp)
      df <- datos()$df
      v <- unique(stats::na.omit(unlist(lapply(df[input$vars_resp], function(z) trimws(as.character(z))))))
      v <- v[nzchar(v)]
      num <- suppressWarnings(as.numeric(v))
      if (!anyNA(num)) v[order(num)] else sort(v)
    })
    output$reglas <- shiny::renderUI({
      v <- valores()
      if (length(v) > 40) return(caja_error("Hay más de 40 respuestas distintas: elige preguntas categóricas."))
      shiny::tagList(lapply(seq_along(v), function(i) {
        anterior <- shiny::isolate(input[[paste0("regla_", i)]])
        shiny::selectInput(ns(paste0("regla_", i)), v[i], CATEGORIAS_NEUTRO,
                           selected = anterior %||% proponer_categoria(v[i]))
      }))
    })
    regla <- shiny::reactive({
      if (!identical(input$formato, "respuestas")) return(NULL)
      v <- valores()
      stats::setNames(vapply(seq_along(v), function(i) input[[paste0("regla_", i)]] %||% proponer_categoria(v[i]),
                             character(1)), v)
    })

    resultado <- shiny::reactive({
      if (is.null(datos())) return(simpleError("Primero carga una base de datos en el panel Datos."))
      shiny::req(input$ocasion %in% names(datos()$df))
      vars <- if (identical(input$formato, "respuestas")) input$vars_resp else input$vars_ternas
      if (length(vars) < 2) return(simpleError("Elige al menos 2 variables."))
      pesos <- c(T = input$w_T %||% NA, I = input$w_I %||% NA, F = input$w_F %||% NA)
      calcular_seguro(statis_dual_neutro(
        datos()$df, vars, input$ocasion, individuo_input(input$individuo, datos()$df),
        formato = input$formato, regla = regla(), pesos = pesos, preparacion = input$prep,
        ejes = input$ejes %||% 2))
    })
    output$error <- render_error(resultado)
    output$avisos <- shiny::renderUI({
      r <- resultado()
      if (!inherits(r, "error") && length(r$avisos)) shiny::tagList(lapply(r$avisos, caja_error))
    })
    res <- resultado_volteado(resultado_valido(resultado), ejes, input)
    observar_ocasiones(session, res)
    observar_destacar(session, shiny::reactive(elementos_trayectorias(res(), input)))

    conectar_grafico(output, session, "g_inter", shiny::reactive(
      grafico_interestructura(res()$interestructura$X, "Interestructura de las ocasiones")),
      "dual_neutro_interestructura")
    conectar_grafico(output, session, "g_pesos", shiny::reactive(
      grafico_pesos(res()$interestructura$X)), "dual_neutro_pesos", plano = FALSE)
    conectar_grafico(output, session, "g_circulo", shiny::reactive(
      grafico_circulo(res(), ejes(), input$etiquetas, input$tam_texto)), "dual_neutro_compromiso")
    conectar_grafico(output, session, "g_tray", shiny::reactive(
      grafico_trayectorias_input(res(), input, ejes())), "dual_neutro_trayectorias")
    conectar_grafico(output, session, "g_indet", shiny::reactive(
      grafico_indeterminacion(res())), "dual_neutro_indeterminacion", plano = FALSE)

    conectar_comunes(output, input, session, res, lectura_statis_dual, ejes,
                     shiny::reactive(sintaxis_statis_dual_neutro(res(), datos()$origen)), "dual_neutro")
    res
  })
}

#' Sintaxis R de un STATIS DUAL neutrosófico
#' @param res resultado de [statis_dual_neutro()].
#' @param origen origen de los datos.
#' @export
sintaxis_statis_dual_neutro <- function(res, origen) {
  a <- res$llamada
  regla <- if (identical(a$formato, "respuestas")) c(
    "# Regla de recodificación: respuesta -> T, I, F, N (neutral: 0, 0, 0) o X (excluir la fila)",
    sprintf("regla <- c(%s)", paste(sprintf("%s = \"%s\"", encodeString(names(a$regla), quote = "`"), a$regla),
                                    collapse = ",\n           ")))
  paste(c(
    "# STATIS DUAL sobre puntuaciones neutrosóficas", sintaxis_paquete(), "", sintaxis_lectura(origen), "",
    regla,
    "res <- statis_dual_neutro(",
    "  datos,",
    sprintf("  variables = %s,", vector_a_r(a$variables, 16)),
    sprintf("  ocasion = %s, individuo = %s, formato = %s,", arg_r(a$ocasion), arg_r(a$individuo), arg_r(a$formato)),
    if (identical(a$formato, "respuestas")) "  regla = regla,",
    sprintf("  pesos = c(T = %s, I = %s, F = %s),", format(a$pesos[["T"]], digits = 6),
            format(a$pesos[["I"]], digits = 6), format(a$pesos[["F"]], digits = 6)),
    sprintf("  preparacion = %s, ejes = %d", arg_r(a$preparacion_x), as.integer(a$ejes)),
    ")",
    "res",
    "res$avisos                          # F = 1 - T, I = 0, ...",
    "res$tablas$`Pesos y cos²`",
    "res$tablas$`Compromiso: variables`  # coordenadas, círculo, calidad y contribución",
    "res$tablas$`Comparación con T solo`",
    "grafico_interestructura(res$interestructura$X)",
    "grafico_circulo(res)",
    "grafico_trayectorias(res, tipo = \"variables\")",
    "grafico_indeterminacion(res)",
    if (length(res$volteados)) sprintf("# Voltear ejes: grafico_circulo(voltear_resultado(res, %s))",
                                       vector_a_r(res$volteados)),
    "",
    "# El STATIS DUAL de las puntuaciones es el de ade4: con la tabla de puntuaciones S",
    "# en formato largo, ade4::statis(ade4::ktab.within(ade4::withinpca(S, ocasion,",
    "#   scaling = \"partial\")), scannf = FALSE) da los mismos RV, pesos y valores propios."
  ), collapse = "\n")
}
