# =============================================================================
# Módulo Co-inercia: la pantalla. El cálculo está en calculo_coinercia.R.
# =============================================================================

#' @export
analisis_coinercia <- function() {
  list(
    id = "coinercia", titulo = "Co-inercia", grupo = "K-tablas", subgrupo = "Dos tablas", icono = "arrows-left-right",
    descripcion = "Co-estructura entre dos tablas medidas sobre los mismos individuos",
    listo = TRUE,
    opciones_ui = mod_coinercia_opciones_ui,
    resultados_ui = mod_coinercia_resultados_ui,
    server = mod_coinercia_server
  )
}

mod_coinercia_opciones_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_opciones("Co-inercia", "Dolédec y Chessel (1994)"),
    opciones_tablas(ns, ocasion = FALSE),
    opciones_preparacion(ns, PREPARACIONES_TABLA, "estandarizar", "estandarizar",
                         "Estandariza las tablas con unidades distintas; centra (o usa AFC) si las variables son abundancias en la misma unidad. Sin transformar usa los datos tal como están. Si una tabla va con AFC, la otra usa sus pesos de filas."),
    opciones_plano(ns),
    opciones_destacar(ns),
    opciones_test(ns)
  )
}

mod_coinercia_resultados_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_resultados("Co-inercia"),
    shiny::uiOutput(ns("error")),
    shiny::div(
      class = "bl-card",
      do.call(shiny::tabsetPanel, c(list(
        shiny::tabPanel("Co-estructura", salida_grafico(ns, "g_individuos")),
        shiny::tabPanel("Variables", salida_grafico(ns, "g_variables")),
        shiny::tabPanel("Test", salida_grafico(ns, "g_test", plano = FALSE))),
        pestanas_comunes(ns, "coinercia")))
    )
  )
}

mod_coinercia_server <- function(id, datos, compartido = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    observar_tablas(session, datos, ocasion = FALSE)
    ejes <- ejes_del_plano(input, session)

    resultado <- shiny::reactive({
      if (is.null(datos())) return(simpleError("Primero carga una base de datos en el panel Datos."))
      if (length(input$var_y) < 2) return(simpleError("Elige al menos 2 variables para la tabla Y."))
      calcular_seguro(coinercia(
        datos()$df, input$var_x, input$var_y, input$prep_x, input$prep_y,
        ejes = input$ejes %||% 2, permutaciones = input$permutaciones %||% 0,
        semilla = input$semilla, nombre_x = input$nombre_x, nombre_y = input$nombre_y))
    })
    output$error <- render_error(resultado)
    res <- resultado_volteado(resultado_valido(resultado), ejes, input)
    observar_destacar(session, shiny::reactive(rownames(res()$filas$X)))

    conectar_grafico(output, session, "g_individuos", shiny::reactive(
      grafico_coestructura(res(), ejes(), input$etiquetas, input$tam_texto,
                           destacados = input$destacar, resto = input$destacar_resto %||% "atenuar")),
      "coinercia_individuos")
    conectar_grafico(output, session, "g_variables", shiny::reactive(
      grafico_variables(res(), ejes(), input$etiquetas, input$tam_texto)), "coinercia_variables")
    conectar_grafico(output, session, "g_test", shiny::reactive({
      shiny::validate(shiny::need(!is.null(res()$test), "El test está desactivado (0 permutaciones)."))
      grafico_test(res()$test)
    }), "coinercia_test", plano = FALSE)

    conectar_comunes(output, input, session, res, lectura_coinercia, ejes,
                     shiny::reactive(sintaxis_coinercia(res(), datos()$origen)), "coinercia")
    res
  })
}

#' Sintaxis R de una Co-inercia
#' @param res resultado de [coinercia()].
#' @param origen origen de los datos.
#' @export
sintaxis_coinercia <- function(res, origen) {
  a <- res$llamada
  paste(c(
    "# Co-inercia", sintaxis_paquete(), "", sintaxis_lectura(origen), "",
    "res <- coinercia(",
    "  datos,",
    sprintf("  variables_x = %s,", vector_a_r(a$variables_x, 16)),
    sprintf("  variables_y = %s,", vector_a_r(a$variables_y, 16)),
    sprintf("  preparacion_x = %s, preparacion_y = %s,", arg_r(a$preparacion_x), arg_r(a$preparacion_y)),
    sprintf("  ejes = %d, permutaciones = %d, semilla = %s,", as.integer(a$ejes),
            as.integer(a$permutaciones), arg_r(a$semilla)),
    sprintf("  nombre_x = %s, nombre_y = %s", arg_r(a$nombre_x), arg_r(a$nombre_y)),
    ")",
    "res                                 # resumen",
    "bgl <- BGL_Create(res)              # objeto BGL; ver BGL_Summary(bgl)",
    "res$tablas$`Co-inercia por eje`",
    "grafico_coestructura(res)           # flechas X -> Y",
    "grafico_variables(res)",
    if (length(res$volteados)) sprintf("# Voltear ejes: grafico_coestructura(voltear_resultado(res, %s))",
                                       vector_a_r(res$volteados)),
    "",
    "# Lo mismo con ade4, como en la práctica:",
    "# library(ade4)",
    sprintf("# dX <- %s", llamada_dudi(a$preparacion_x, "res$llamada$variables_x", a$preparacion_y)),
    sprintf("# dY <- %s", llamada_dudi(a$preparacion_y, "res$llamada$variables_y", a$preparacion_x)),
    sprintf("# co <- coinertia(dX, dY, scannf = FALSE, nf = %d); summary(co)", as.integer(a$ejes)),
    sprintf("# set.seed(%s); randtest(co, nrepet = %d)", arg_r(a$semilla), as.integer(a$permutaciones))
  ), collapse = "\n")
}

llamada_dudi <- function(prep, vars, otra) {
  tabla <- sprintf("datos[%s]", vars)
  w <- if (prep != "afc" && otra == "afc") ", row.w = <pesos de la AFC>" else ""
  switch(prep,
         estandarizar = sprintf("dudi.pca(%s, scale = TRUE%s, scannf = FALSE)", tabla, w),
         centrar = sprintf("dudi.pca(%s, scale = FALSE%s, scannf = FALSE)", tabla, w),
         afc = sprintf("dudi.coa(%s, scannf = FALSE)", tabla))
}
