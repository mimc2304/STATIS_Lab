# =============================================================================
# PLANTILLA DE MÓDULO K-TABLAS PARA BiplotLab
#
# 1. Copia este archivo a  R/mod_<metodo>.R   (ej.: R/mod_statis_dual.R)
# 2. Reemplaza en TODO el archivo la palabra  METODO  por el nombre corto de tu
#    método, sin espacios ni tildes (ej.: statis, statis_dual, pta, afm).
# 3. Completa las partes marcadas con  TODO.
# 4. En R/registro_analisis.R cambia tu línea  proximamente("...")  por
#    analisis_METODO()
# 5. Si usas paquetes nuevos, agrégalos en DESCRIPTION (Imports) y llama sus
#    funciones con  paquete::funcion(). Excepción: las funciones de ade4 que
#    leen su propia llamada (dudi.pca, withinpca, coinertia, statis, pta...)
#    se importan en R/biplotlab-package.R (@importFrom ade4 ...) y se llaman
#    SIN «ade4::», porque con el prefijo fallan.
#
# Tal como está, la plantilla FUNCIONA: hace un STATIS-DUAL (mismas variables;
# los individuos pueden coincidir o no), como en la práctica de los ríos del
# Doubs (jv73) y del Banco Mundial. Reemplaza ese cálculo por el tuyo.
#
# Lo que ya tienes hecho y conviene reutilizar:
#   ktabla_larga()        arma la K-tabla desde la hoja en formato largo y valida
#   preparar_ktabla()     withinpca/wca + ktab.within de ade4
#   interestructura_pta() RV, pesos, cos² y mapa de las ocasiones
#   nuevo_resultado()     el objeto común; si lo llenas, te sirven gratis los
#                         gráficos (grafico_interestructura, grafico_pesos,
#                         grafico_variables, grafico_trayectorias), «Voltear»,
#                         la pestaña Tablas y la descarga a Excel.
# =============================================================================

# -----------------------------------------------------------------------------
# A) CÁLCULO: función normal de R, sin nada de Shiny.
# -----------------------------------------------------------------------------

#' TODO: título de tu método
#'
#' @param datos data.frame en formato largo (una fila por individuo y ocasión).
#' @param variables variables de la tabla.
#' @param ocasion columna de la ocasión.
#' @param preparacion código de PREPARACIONES_KTABLA.
#' @param ejes ejes del compromiso.
#' @export
calcular_METODO <- function(datos, variables, ocasion, preparacion = "total", ejes = 2) {
  kt <- ktabla_larga(datos, variables, ocasion)
  p <- preparar_ktabla(kt, preparacion, ejes)

  # ---- TODO: aquí va TU código de R (el que te funcionó en tu práctica) ----
  st <- statis(p$ktab, scannf = FALSE, nf = ejes)
  # statis() llama a sus pesos RV.tabw; interestructura_pta() los espera en tabw
  inter <- interestructura_pta(list(RV = st$RV, tabw = st$RV.tabw, cos2 = st$cos2,
                                    RV.coo = st$RV.coo, RV.eig = st$RV.eig), kt$niveles)
  variables_compromiso <- como_coordenadas(st$C.li, ejes)
  # --------------------------------------------------------------------------

  res <- nuevo_resultado(
    "METODO", "TODO Título", nombres = c(X = "Variables", Y = "Variables"),
    preparacion = list(X = etiqueta_preparacion(preparacion)),
    inercia = tabla_inercia(st$C.eig),
    interestructura = list(X = inter),
    variables = list(X = variables_compromiso, Y = variables_compromiso[0, , drop = FALSE]),
    motor = st,
    llamada = list(variables = variables, ocasion = ocasion, preparacion = preparacion, ejes = ejes))
  res$tablas <- list("Interestructura (RV)" = tabla_rv(inter$RV), "Pesos y cos²" = inter$ocasiones,
                     "Valores propios del compromiso" = res$inercia,
                     "Compromiso: variables" = coordenadas_a_tabla(variables_compromiso, "Variable"))
  res
}

# Lectura guiada: frases a partir del resultado
lectura_METODO <- function(res, ejes = c(1, 2)) {
  c(lectura_interestructura(res$interestructura$X, "las ocasiones"),
    sprintf("El plano %d-%d del compromiso recoge el %s de la inercia.",
            ejes[1], ejes[2], pct(sum(res$inercia[[3]][ejes]))))
}

# -----------------------------------------------------------------------------
# B) REGISTRO: cómo aparece tu módulo en la cinta del aplicativo.
# -----------------------------------------------------------------------------
#' @export
analisis_METODO <- function() {
  list(
    id = "METODO",                          # TODO: igual que el nombre corto
    titulo = "TODO Título corto",           # lo que se lee en el botón
    grupo = "K-tablas",
    icono = "table-columns",                # TODO: ícono de https://fontawesome.com/search?o=r&m=free
    descripcion = "TODO Una línea sobre qué hace el método",
    listo = TRUE,
    opciones_ui = mod_METODO_opciones_ui,
    resultados_ui = mod_METODO_resultados_ui,
    server = mod_METODO_server
  )
}

# -----------------------------------------------------------------------------
# C) PANEL DE OPCIONES (izquierda). Cada control lleva ns("nombre").
# -----------------------------------------------------------------------------
mod_METODO_opciones_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_opciones("TODO Título", "TODO Autor del método (año)"),
    seccion_opciones(
      "Tabla",
      shiny::selectInput(ns("ocasion"), "Ocasión (tercera vía)", choices = NULL),
      shiny::selectizeInput(ns("variables"), "Variables", choices = NULL, multiple = TRUE,
                            options = list(plugins = list("remove_button")))
    ),
    seccion_opciones(
      "Preparación",
      shiny::selectInput(ns("prep"), NULL, PREPARACIONES_KTABLA, selected = "total")
    ),
    opciones_plano(ns)    # ejes, voltear y etiquetas (común a todos los módulos)
  )
}

# -----------------------------------------------------------------------------
# D) PANEL DE RESULTADOS (derecha): tus pestañas + las comunes.
# -----------------------------------------------------------------------------
mod_METODO_resultados_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_resultados("TODO Título"),
    shiny::uiOutput(ns("error")),
    shiny::div(
      class = "bl-card",
      do.call(shiny::tabsetPanel, c(list(
        shiny::tabPanel("Interestructura", salida_grafico(ns, "g_inter"),
                        salida_grafico(ns, "g_pesos", plano = FALSE)),
        shiny::tabPanel("Compromiso", salida_grafico(ns, "g_compromiso"))),
        # Tablas, Lectura guiada, Fundamentos y Sintaxis R. Para Fundamentos,
        # agrega tu entrada en R/teoria_contenido.R
        pestanas_comunes(ns, "METODO")))
    )
  )
}

# -----------------------------------------------------------------------------
# E) LÓGICA. `datos()` devuelve list(df, origen, etiqueta, nominales, carga).
# -----------------------------------------------------------------------------
mod_METODO_server <- function(id, datos, compartido = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(datos(), {
      d <- datos()
      shiny::updateSelectInput(session, "ocasion", choices = c(d$nominales, setdiff(names(d$df), d$nominales)))
      num <- variables_numericas(d)
      shiny::updateSelectizeInput(session, "variables", choices = num, selected = num)
    })
    ejes <- ejes_del_plano(input, session)

    resultado <- shiny::reactive({
      if (is.null(datos())) return(simpleError("Primero carga una base de datos en el panel Datos."))
      shiny::req(input$ocasion)
      calcular_seguro(calcular_METODO(datos()$df, input$variables, input$ocasion, input$prep,
                                      ejes = input$ejes %||% 2))
    })
    output$error <- render_error(resultado)
    res <- resultado_volteado(resultado_valido(resultado), ejes, input)

    conectar_grafico(output, session, "g_inter", shiny::reactive(
      grafico_interestructura(res()$interestructura$X)), "METODO_interestructura")
    conectar_grafico(output, session, "g_pesos", shiny::reactive(
      grafico_pesos(res()$interestructura$X)), "METODO_pesos", plano = FALSE)
    conectar_grafico(output, session, "g_compromiso", shiny::reactive(
      grafico_variables(res(), ejes(), input$etiquetas, input$tam_texto, mostrar = "X",
                        titulo = "Compromiso")), "METODO_compromiso")

    # Sintaxis R: el código que reproduce el resultado (como en jamovi)
    sintaxis <- shiny::reactive(paste(c(
      "# TODO Título", sintaxis_paquete(), "", sintaxis_lectura(datos()$origen), "",
      sprintf("res <- calcular_METODO(datos, variables = %s,", vector_a_r(input$variables, 34)),
      sprintf("                       ocasion = %s, preparacion = %s, ejes = %d)",
              arg_r(input$ocasion), arg_r(input$prep), as.integer(input$ejes %||% 2)),
      "res"), collapse = "\n"))
    conectar_comunes(output, input, session, res, lectura_METODO, ejes, sintaxis, "METODO")
    res
  })
}
