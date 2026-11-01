# =============================================================================
# Módulo STATICO: la pantalla. El cálculo está en calculo_statico.R.
# =============================================================================

#' @export
analisis_statico <- function() {
  list(
    id = "statico", titulo = "STATICO", grupo = "K-tablas", subgrupo = "K pares de tablas", icono = "layer-group",
    descripcion = "STATIS de la co-estructura: parte estable de la relación X-Y a lo largo de las ocasiones",
    listo = TRUE,
    opciones_ui = mod_statico_opciones_ui,
    resultados_ui = mod_statico_resultados_ui,
    server = mod_statico_server
  )
}

AYUDA_PREPARACION_KTABLA <- paste(
  "En tres vías no es lo mismo usar la media o la desviación de cada ocasión que las de todas las ocasiones juntas.",
  "Parcial y total son las dos de Bouroche: parcial deja cada variable con varianza 1 dentro de cada ocasión; total centra cada ocasión y usa una sola desviación para todas (la de la tabla ya centrada), así conserva las diferencias de dispersión entre ocasiones.",
  "Las opciones globales conservan además las diferencias de nivel entre ocasiones.",
  "Sin transformar usa los datos tal como están.")

#' Sección «Trayectorias» (STATICO y CoSTATIS)
#' @param ns función `NS`.
#' @param con_variables si se ofrecen las trayectorias de las variables.
#' @export
opciones_trayectorias <- function(ns, con_variables = TRUE) {
  que <- c("Individuos de X" = "individuos.X", "Individuos de Y" = "individuos.Y")
  if (con_variables) que <- c(que, "Variables de X" = "variables.X", "Variables de Y" = "variables.Y")
  seccion_opciones(
    "Trayectorias",
    shiny::selectInput(ns("tray_que"), "Qué se proyecta", que),
    shiny::selectizeInput(ns("tray_ocasiones"), "Ocasiones", choices = NULL, multiple = TRUE,
                          options = list(plugins = list("remove_button"), placeholder = "Todas")),
    shiny::checkboxInput(ns("tray_conectar"), "Unir las posiciones del mismo elemento", TRUE),
    abierta = FALSE
  )
}

mod_statico_opciones_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_opciones("STATICO", "Simier et al. (1999); Thioulouse et al. (2004)"),
    opciones_tablas(ns, ocasion = TRUE),
    opciones_preparacion(ns, PREPARACIONES_KTABLA, "total", "centrar", AYUDA_PREPARACION_KTABLA),
    opciones_plano(ns),
    opciones_destacar(ns),
    opciones_trayectorias(ns, con_variables = TRUE)
  )
}

mod_statico_resultados_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_resultados("STATICO"),
    shiny::uiOutput(ns("error")),
    shiny::div(
      class = "bl-card",
      do.call(shiny::tabsetPanel, c(list(
        shiny::tabPanel("Interestructura", salida_grafico(ns, "g_inter"),
                        salida_grafico(ns, "g_pesos", plano = FALSE)),
        shiny::tabPanel("Compromiso", salida_grafico(ns, "g_compromiso")),
        shiny::tabPanel("Trayectorias", salida_grafico(ns, "g_tray"))),
        pestanas_comunes(ns, "statico")))
    )
  )
}

# Ocasiones del resultado en el selector de trayectorias
observar_ocasiones <- function(session, res) {
  shiny::observeEvent(levels(res()$trayectorias$X$Ocasión), {
    shiny::updateSelectizeInput(session, "tray_ocasiones", choices = levels(res()$trayectorias$X$Ocasión))
  })
}

# Gráfico de trayectorias según el selector «Qué se proyecta»
grafico_trayectorias_input <- function(res, input, ejes) {
  partes <- strsplit(input$tray_que %||% "individuos.X", ".", fixed = TRUE)[[1]]
  grafico_trayectorias(res, que = partes[2], tipo = partes[1], ejes = ejes,
                       etiquetas = input$etiquetas, conectar = isTRUE(input$tray_conectar),
                       ocasiones = input$tray_ocasiones, tam_texto = input$tam_texto,
                       destacados = input$destacar, resto = input$destacar_resto %||% "atenuar")
}

# Elementos de las trayectorias que se pueden pintar, según «Qué se proyecta»
elementos_trayectorias <- function(res, input) {
  partes <- strsplit(input$tray_que %||% "individuos.X", ".", fixed = TRUE)[[1]]
  tr <- if (partes[1] == "individuos") res$trayectorias[[partes[2]]] else res$trayectorias_variables[[partes[2]]]
  unique(as.character(tr[[1]]))
}

# "" en el selector de individuos = nombres de fila
# Una columna que ya no existe (quedó de la base anterior) también cuenta como vacía
individuo_input <- function(x, df = NULL) {
  if (is.null(x) || !nzchar(x) || (!is.null(df) && !x %in% names(df))) NULL else x
}

mod_statico_server <- function(id, datos, compartido = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    observar_tablas(session, datos, ocasion = TRUE)
    ejes <- ejes_del_plano(input, session)

    resultado <- shiny::reactive({
      if (is.null(datos())) return(simpleError("Primero carga una base de datos en el panel Datos."))
      if (length(input$var_y) < 2) return(simpleError("Elige al menos 2 variables para la tabla Y."))
      shiny::req(input$ocasion %in% names(datos()$df))
      calcular_seguro(statico(
        datos()$df, input$var_x, input$var_y, input$ocasion, individuo_input(input$individuo, datos()$df),
        input$prep_x, input$prep_y, ejes = input$ejes %||% 2,
        nombre_x = input$nombre_x, nombre_y = input$nombre_y))
    })
    output$error <- render_error(resultado)
    res <- resultado_volteado(resultado_valido(resultado), ejes, input)
    observar_ocasiones(session, res)
    observar_destacar(session, shiny::reactive(elementos_trayectorias(res(), input)))

    conectar_grafico(output, session, "g_inter", shiny::reactive(
      grafico_interestructura(res()$interestructura$XY,
                              sprintf("Interestructura de las co-estructuras %s-%s",
                                      res()$nombres[["X"]], res()$nombres[["Y"]]))), "statico_interestructura")
    conectar_grafico(output, session, "g_pesos", shiny::reactive(
      grafico_pesos(res()$interestructura$XY)), "statico_pesos", plano = FALSE)
    conectar_grafico(output, session, "g_compromiso", shiny::reactive(
      grafico_variables(res(), ejes(), input$etiquetas, input$tam_texto,
                        titulo = "Compromiso: la co-estructura estable")), "statico_compromiso")
    conectar_grafico(output, session, "g_tray", shiny::reactive(
      grafico_trayectorias_input(res(), input, ejes())), "statico_trayectorias")

    conectar_comunes(output, input, session, res, lectura_statico, ejes,
                     shiny::reactive(sintaxis_statico(res(), datos()$origen)), "statico")
    res
  })
}

#' Sintaxis R de un STATICO
#' @param res resultado de [statico()].
#' @param origen origen de los datos.
#' @export
sintaxis_statico <- function(res, origen) {
  a <- res$llamada
  paste(c(
    "# STATICO", sintaxis_paquete(), "", sintaxis_lectura(origen), "",
    "res <- statico(",
    "  datos,",
    sprintf("  variables_x = %s,", vector_a_r(a$variables_x, 16)),
    sprintf("  variables_y = %s,", vector_a_r(a$variables_y, 16)),
    sprintf("  ocasion = %s, individuo = %s,", arg_r(a$ocasion), arg_r(a$individuo)),
    sprintf("  preparacion_x = %s, preparacion_y = %s, ejes = %d,", arg_r(a$preparacion_x),
            arg_r(a$preparacion_y), as.integer(a$ejes)),
    sprintf("  nombre_x = %s, nombre_y = %s", arg_r(a$nombre_x), arg_r(a$nombre_y)),
    ")",
    "res",
    "bgl <- BGL_Create(res)              # objeto BGL; ver BGL_Summary(bgl)",
    "res$tablas$`Pesos y cos²`",
    "grafico_interestructura(res$interestructura$XY)",
    "grafico_variables(res)              # compromiso",
    "grafico_trayectorias(res, \"X\")",
    if (length(res$volteados)) sprintf("# Voltear ejes: grafico_variables(voltear_resultado(res, %s))",
                                       vector_a_r(res$volteados)),
    "",
    "# Lo mismo con ade4 (Thioulouse, 2011): con kX y kY, los ktab.within() de",
    "# cada tabla (variables en filas, una tabla por ocasión),",
    "# st <- ade4::statico(kX, kY, scannf = FALSE); st$RV; st$tabw; st$cos2"
  ), collapse = "\n")
}
