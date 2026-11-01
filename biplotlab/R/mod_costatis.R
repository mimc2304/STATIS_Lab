# =============================================================================
# Módulo CoSTATIS: la pantalla. El cálculo está en calculo_costatis.R.
# =============================================================================

#' @export
analisis_costatis <- function() {
  list(
    id = "costatis", titulo = "CoSTATIS", grupo = "K-tablas", subgrupo = "K pares de tablas", icono = "cubes",
    descripcion = "Co-inercia de los compromisos de dos K-tablas con los mismos individuos",
    listo = TRUE,
    opciones_ui = mod_costatis_opciones_ui,
    resultados_ui = mod_costatis_resultados_ui,
    server = mod_costatis_server
  )
}

mod_costatis_opciones_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_opciones("CoSTATIS", "Thioulouse (2011)"),
    opciones_tablas(ns, ocasion = TRUE, individuo_obligatorio = TRUE),
    opciones_preparacion(ns, PREPARACIONES_KTABLA, "total", "centrar", AYUDA_PREPARACION_KTABLA),
    opciones_plano(ns),
    opciones_test(ns),
    opciones_destacar(ns),
    opciones_trayectorias(ns, con_variables = FALSE)
  )
}

mod_costatis_resultados_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_resultados("CoSTATIS"),
    shiny::uiOutput(ns("error")),
    shiny::div(
      class = "bl-card",
      do.call(shiny::tabsetPanel, c(list(
        shiny::tabPanel("Interestructura",
                        shiny::radioButtons(ns("serie"), NULL, c("Tabla X" = "X", "Tabla Y" = "Y"),
                                            inline = TRUE),
                        salida_grafico(ns, "g_inter"), salida_grafico(ns, "g_pesos", plano = FALSE)),
        shiny::tabPanel("Co-estructura", salida_grafico(ns, "g_individuos")),
        shiny::tabPanel("Variables", salida_grafico(ns, "g_variables")),
        shiny::tabPanel("Trayectorias", salida_grafico(ns, "g_tray")),
        shiny::tabPanel("Test", salida_grafico(ns, "g_test", plano = FALSE))),
        pestanas_comunes(ns, "costatis")))
    )
  )
}

mod_costatis_server <- function(id, datos, compartido = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    observar_tablas(session, datos, ocasion = TRUE)
    ejes <- ejes_del_plano(input, session)

    resultado <- shiny::reactive({
      if (is.null(datos())) return(simpleError("Primero carga una base de datos en el panel Datos."))
      if (length(input$var_y) < 2) return(simpleError("Elige al menos 2 variables para la tabla Y."))
      shiny::req(input$ocasion %in% names(datos()$df))
      if (is.null(individuo_input(input$individuo, datos()$df))) {
        return(simpleError("CoSTATIS necesita la columna de individuos para alinear las ocasiones."))
      }
      calcular_seguro(costatis(
        datos()$df, input$var_x, input$var_y, input$ocasion, individuo_input(input$individuo, datos()$df),
        input$prep_x, input$prep_y, ejes = input$ejes %||% 2,
        permutaciones = input$permutaciones %||% 0, semilla = input$semilla,
        nombre_x = input$nombre_x, nombre_y = input$nombre_y))
    })
    output$error <- render_error(resultado)
    res <- resultado_volteado(resultado_valido(resultado), ejes, input)
    observar_ocasiones(session, res)
    observar_destacar(session, shiny::reactive(elementos_trayectorias(res(), input)))

    serie <- shiny::reactive(input$serie %||% "X")
    conectar_grafico(output, session, "g_inter", shiny::reactive(
      grafico_interestructura(res()$interestructura[[serie()]],
                              paste("Interestructura de", res()$nombres[[serie()]]))),
      "costatis_interestructura")
    conectar_grafico(output, session, "g_pesos", shiny::reactive(
      grafico_pesos(res()$interestructura[[serie()]],
                    paste("Pesos del compromiso de", res()$nombres[[serie()]]))),
      "costatis_pesos", plano = FALSE)
    conectar_grafico(output, session, "g_individuos", shiny::reactive(
      grafico_coestructura(res(), ejes(), input$etiquetas, input$tam_texto,
                           destacados = input$destacar, resto = input$destacar_resto %||% "atenuar")),
      "costatis_individuos")
    conectar_grafico(output, session, "g_variables", shiny::reactive(
      grafico_variables(res(), ejes(), input$etiquetas, input$tam_texto)), "costatis_variables")
    conectar_grafico(output, session, "g_tray", shiny::reactive(
      grafico_trayectorias_input(res(), input, ejes())), "costatis_trayectorias")
    conectar_grafico(output, session, "g_test", shiny::reactive({
      shiny::validate(shiny::need(!is.null(res()$test), "El test está desactivado (0 permutaciones)."))
      grafico_test(res()$test)
    }), "costatis_test", plano = FALSE)

    conectar_comunes(output, input, session, res, lectura_costatis, ejes,
                     shiny::reactive(sintaxis_costatis(res(), datos()$origen)), "costatis")
    res
  })
}

#' Sintaxis R de un CoSTATIS
#' @param res resultado de [costatis()].
#' @param origen origen de los datos.
#' @export
sintaxis_costatis <- function(res, origen) {
  a <- res$llamada
  paste(c(
    "# CoSTATIS", sintaxis_paquete(), "", sintaxis_lectura(origen), "",
    "res <- costatis(",
    "  datos,",
    sprintf("  variables_x = %s,", vector_a_r(a$variables_x, 16)),
    sprintf("  variables_y = %s,", vector_a_r(a$variables_y, 16)),
    sprintf("  ocasion = %s, individuo = %s,", arg_r(a$ocasion), arg_r(a$individuo)),
    sprintf("  preparacion_x = %s, preparacion_y = %s, ejes = %d,", arg_r(a$preparacion_x),
            arg_r(a$preparacion_y), as.integer(a$ejes)),
    sprintf("  permutaciones = %d, semilla = %s,", as.integer(a$permutaciones), arg_r(a$semilla)),
    sprintf("  nombre_x = %s, nombre_y = %s", arg_r(a$nombre_x), arg_r(a$nombre_y)),
    ")",
    "res",
    "bgl <- BGL_Create(res)              # objeto BGL; ver BGL_Summary(bgl)",
    "grafico_interestructura(res$interestructura$X)",
    "grafico_coestructura(res)",
    "grafico_variables(res)",
    "grafico_trayectorias(res, \"Y\")",
    if (length(res$volteados)) sprintf("# Voltear ejes: grafico_variables(voltear_resultado(res, %s))",
                                       vector_a_r(res$volteados)),
    "",
    "# Lo mismo con ade4 (Thioulouse, 2011): con kX y kY, los ktab.within() de",
    "# cada tabla (mismos individuos y en el mismo orden en todas las ocasiones),",
    "# cs <- ade4::costatis(kX, kY, scannf = FALSE); cs$RV",
    sprintf("# set.seed(%s); ade4::costatis.randtest(kX, kY, nrepet = %d)", arg_r(a$semilla),
            as.integer(a$permutaciones))
  ), collapse = "\n")
}
