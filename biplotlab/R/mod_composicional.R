# =============================================================================
# MÓDULO: HJ-Biplot Composicional (Hernández Suárez et al., 2016)
#
# Interfaz de usuario y servidor para el análisis de datos composicionales
# mediante transformación clr, matriz de variación de Aitchison y biplot HJ
# con rayos y enlaces (links).
# =============================================================================

# -----------------------------------------------------------------------------
# B) REGISTRO
# -----------------------------------------------------------------------------
#' Entrada del HJ-Biplot Composicional en el registro de análisis
#' @export
analisis_composicional <- function() {
  list(
    id = "hj_composicional",
    titulo = "HJ Composicional",
    grupo = "Biplot",
    icono = "chart-pie",
    descripcion = "HJ-Biplot para datos composicionales con rayos y enlaces (links)",
    listo = TRUE,
    opciones_ui = mod_composicional_opciones_ui,
    resultados_ui = mod_composicional_resultados_ui,
    server = mod_composicional_server
  )
}

# -----------------------------------------------------------------------------
# C) PANEL DE OPCIONES (izquierda)
# -----------------------------------------------------------------------------
mod_composicional_opciones_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_opciones("HJ Composicional", "Hernández Suárez et al. (2016) - Aitchison y Greenacre (2002)"),
    
    seccion_opciones(
      "Variables composicionales",
      shiny::selectizeInput(
        ns("variables"), "Partes de la composición (variables activas)",
        choices = NULL, multiple = TRUE,
        options = list(plugins = list("remove_button"), placeholder = "Elige al menos 2 partes")
      ),
      shiny::div(class = "bl-links",
                 shiny::actionLink(ns("todas"), "Todas las numéricas"),
                 shiny::actionLink(ns("ninguna"), "Quitar todas")),
      shiny::p(class = "bl-help",
               "Variables que representan partes de un todo (proporciones, concentraciones o porcentajes > 0)."),
      shiny::selectInput(ns("grupo"), "Variable de grupo (colorea; no construye ejes)",
                         choices = c("(ninguna)" = "")),
      shiny::selectizeInput(ns("excluir"), "Excluir individuos",
                            choices = NULL, multiple = TRUE,
                            options = list(plugins = list("remove_button"), placeholder = "Ninguno"))
    ),
    
    seccion_opciones(
      "Preparación composicional",
      shiny::checkboxInput(ns("imputar_ceros"), "Reemplazo multiplicativo de ceros (delta = 0.65)", TRUE),
      shiny::p(class = "bl-help",
               "Aplica la transformación clr (log-ratio centrada) y doble centrado según Aitchison & Greenacre (2002).")
    ),
    
    seccion_opciones(
      "Modelo y orientación",
      shiny::radioButtons(ns("tipo"), "Tipo de biplot",
                          c("HJ (simultáneo)" = "HJ", "GH (covarianza)" = "GH", "JK (forma)" = "JK"),
                          selected = "HJ", inline = TRUE),
      shiny::numericInput(ns("dims"), "Número de ejes a calcular", value = 3, min = 2, max = 10, step = 1),
      shiny::selectInput(ns("referencia"), "Convenio de signos",
                         c("Signo de la SVD (como MultBiplotR)" = "")),
      shiny::p(class = "bl-help", "Para cambiar la orientación usa los botones de voltear en Gráfico.")
    ),
    
    seccion_opciones(
      "Elementos composicionales en el gráfico",
      shiny::checkboxInput(ns("mostrar_links"), "Mostrar enlaces (links) entre variables", TRUE),
      shiny::radioButtons(
        ns("orden_links"), "Criterio de enlaces",
        c("Más cortos (relaciones más fuertes)" = "cortos",
          "Más largos (mayor independencia)" = "largos"),
        selected = "cortos"
      ),
      shiny::sliderInput(ns("max_links"), "Número de enlaces a mostrar", min = 1, max = 50, value = 15, step = 1),
      shiny::checkboxInput(ns("mostrar_rayos"), "Mostrar rayos (vectores desde el origen)", TRUE),
      shiny::p(class = "bl-help",
               "Un enlace corto entre dos partes indica una relación proporcional fuerte (baja varianza del log-ratio).")
    ),
    
    seccion_opciones(
      "Gráfico",
      shiny::fluidRow(
        shiny::column(6, shiny::selectInput(ns("eje_x"), "Eje horizontal", 1:3, selected = 1)),
        shiny::column(6, shiny::selectInput(ns("eje_y"), "Eje vertical", 1:3, selected = 2))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::checkboxInput(ns("voltear_h"), "Voltear horizontal", FALSE)),
        shiny::column(6, shiny::checkboxInput(ns("voltear_v"), "Voltear vertical", FALSE))
      ),
      shiny::radioButtons(ns("mostrar"), "Mostrar",
                          c("Individuos y partes" = "ambos", "Solo individuos" = "filas",
                            "Solo partes" = "columnas"), inline = TRUE),
      shiny::checkboxInput(ns("envolventes"), "Envolvente de cada grupo", FALSE),
      shiny::fluidRow(
        shiny::column(6, shiny::checkboxInput(ns("etiquetas_f"), "Nombres de individuos", TRUE)),
        shiny::column(6, shiny::checkboxInput(ns("etiquetas_c"), "Nombres de variables", TRUE))
      ),
      shiny::sliderInput(ns("escala"), "Escala visual de los vectores", min = 0.2, max = 3, value = 1, step = 0.1),
      shiny::fluidRow(
        shiny::column(6, shiny::sliderInput(ns("tam_texto"), "Texto", min = 2, max = 5, value = 3.2, step = 0.2)),
        shiny::column(6, shiny::sliderInput(ns("tam_puntos"), "Puntos", min = 0.5, max = 5, value = 2.2, step = 0.2))
      )
    )
  )
}

# -----------------------------------------------------------------------------
# D) PANEL DE RESULTADOS (derecha)
# -----------------------------------------------------------------------------
mod_composicional_resultados_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_resultados("HJ-Biplot Composicional"),
    shiny::uiOutput(ns("error")),
    shiny::div(
      class = "bl-card",
      shiny::tabsetPanel(
        shiny::tabPanel(
          "Gráfico",
          salida_biplot(ns, "grafico"),
          shiny::div(
            class = "bl-dl-bar",
            shiny::downloadButton(ns("bajar_png"), "PNG", class = "btn-sm"),
            shiny::downloadButton(ns("bajar_pdf"), "PDF", class = "btn-sm"),
            shiny::downloadButton(ns("bajar_svg"), "SVG", class = "btn-sm")
          )
        ),
        shiny::tabPanel(
          "Enlaces (Links)",
          shiny::p(class = "bl-help mt-2",
                   "La longitud del enlace entre dos rayos aproxima la desviación estándar de su log-ratio var(ln(Xi/Xj)). Enlaces cortos señalan una fuerte relación proporcional."),
          DT::DTOutput(ns("tabla_links"))
        ),
        shiny::tabPanel(
          "Matriz de Variación",
          shiny::p(class = "bl-help mt-2",
                   "Matriz de variación de Aitchison: el triángulo superior contiene las varianzas var(ln(Xi/Xj)), el triángulo inferior contiene las medias mean(ln(Xi/Xj)), y la última columna las clr-varianzas."),
          DT::DTOutput(ns("tabla_varray")),
          shiny::uiOutput(ns("resumen_var_total"))
        ),
        shiny::tabPanel(
          "Inercia y Coordenadas",
          shiny::div(
            class = "bl-two mb-3",
            shiny::div(
              shiny::h5("Valores propios e inercia"),
              DT::DTOutput(ns("tabla_inercia"))
            ),
            shiny::div(
              shiny::h5("Inercia por eje"),
              shiny::plotOutput(ns("sedimentacion"), height = "280px")
            )
          ),
          shiny::h5("Coordenadas de las partes composicionales (columnas)"),
          DT::DTOutput(ns("tabla_columnas")),
          shiny::h5("Coordenadas de los individuos (filas)"),
          DT::DTOutput(ns("tabla_filas"))
        ),
        shiny::tabPanel(
          "Variables",
          shiny::radioButtons(
            ns("tabla_columnas_tipo"), NULL, inline = TRUE,
            c("CRFE (calidad)" = "calidad_columnas",
              "CREF (contribución al eje)" = "contrib_columnas"
              #"Coordenadas" = "coordenadas_columnas",
              #"Correlaciones" = "correlaciones"
            )
          ),
          shiny::uiOutput(ns("nota_columnas")),
          DT::DTOutput(ns("tabla_c"))
        ),
        shiny::tabPanel(
          "Guía de lectura",
          shiny::div(class = "bl-lectura", shiny::uiOutput(ns("lectura_guiada")))
        ),
        shiny::tabPanel(
          shiny::span(shiny::icon("book-open"), "Teoría"),
          teoria_ui("hj_composicional")
        ),
        shiny::tabPanel(
          shiny::span(shiny::icon("code"), "Sintaxis R"),
          shiny::div(class = "bl-code", shiny::verbatimTextOutput(ns("sintaxis")))
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
# E) LÓGICA (servidor)
# -----------------------------------------------------------------------------
mod_composicional_server <- function(id, datos, compartido = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Actualizar variables al cargar datos
    shiny::observeEvent(datos(), {
      df <- datos()$df
      if (is.null(df)) return()
      num <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], datos()$nominales)
      shiny::updateSelectizeInput(session, "variables", choices = num, selected = num)
      shiny::updateSelectizeInput(session, "excluir", choices = rownames(df), selected = character())
      
      noms <- datos()$nominales
      grp_choices <- c("(ninguna)" = "", stats::setNames(noms, noms))
      shiny::updateSelectInput(session, "grupo", choices = grp_choices, selected = "")
    })
    
    shiny::observeEvent(input$todas, {
      df <- datos()$df
      if (is.null(df)) return()
      num <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], datos()$nominales)
      shiny::updateSelectizeInput(session, "variables", selected = num)
    })
    
    shiny::observeEvent(input$ninguna, {
      shiny::updateSelectizeInput(session, "variables", selected = character())
    })
    
    shiny::observe({
      k <- input$dims %||% 3
      ejes_disponibles <- seq_len(max(2, k))
      cur_x <- isolate(input$eje_x) %||% 1
      cur_y <- isolate(input$eje_y) %||% 2
      shiny::updateSelectInput(session, "eje_x", choices = ejes_disponibles,
                               selected = if (cur_x %in% ejes_disponibles) cur_x else 1)
      shiny::updateSelectInput(session, "eje_y", choices = ejes_disponibles,
                               selected = if (cur_y %in% ejes_disponibles) cur_y else 2)
    })
    
    # Datos filtrados
    datos_filtrados <- shiny::reactive({
      if (is.null(datos()$df)) return(NULL)
      df <- datos()$df
      if (length(input$excluir) > 0) {
        df <- df[!rownames(df) %in% input$excluir, , drop = FALSE]
      }
      df
    })
    
    # Cálculo reactivo
    calculo <- shiny::reactive({
      df <- datos_filtrados()
      if (is.null(df)) return(simpleError("Carga primero una base de datos."))
      vars <- input$variables
      if (length(vars) < 2) return(simpleError("Selecciona al menos 2 partes composicionales."))
      
      tryCatch({
        calcular_composicional(
          datos = df,
          variables = vars,
          tipo = input$tipo %||% "HJ",
          dimensiones = input$dims %||% 3,
          imputar_ceros = isTRUE(input$imputar_ceros),
          referencia = if (nzchar(input$referencia %||% "")) input$referencia else NULL
        )
      }, error = function(e) e)
    })
    
    calc_ok <- shiny::reactive({
      res <- calculo()
      shiny::validate(shiny::need(!inherits(res, "error"), ""))
      res
    })
    
    output$error <- shiny::renderUI({
      res <- calculo()
      if (inherits(res, "error")) caja_error(conditionMessage(res))
    })
    
    # Gráfico reactivo
    grafico_r <- shiny::reactive({
      res <- calc_ok()
      df <- datos_filtrados()
      grp <- if (nzchar(input$grupo %||% "") && input$grupo %in% names(df)) df[[input$grupo]] else NULL
      ejes <- c(as.integer(input$eje_x %||% 1), as.integer(input$eje_y %||% 2))
      
      grafico_composicional(
        res = res,
        ejes = ejes,
        grupos = grp,
        mostrar_links = isTRUE(input$mostrar_links),
        enlaces_a_mostrar = "todos",
        max_links = input$max_links %||% 15,
        orden_links = input$orden_links %||% "cortos",
        mostrar_rayos = isTRUE(input$mostrar_rayos),
        mostrar = input$mostrar %||% "ambos",
        escala_vectores = input$escala %||% 1,
        invertir = c(isTRUE(input$voltear_h), isTRUE(input$voltear_v)),
        etiquetas_filas = isTRUE(input$etiquetas_f),
        etiquetas_columnas = isTRUE(input$etiquetas_c),
        tam_texto = input$tam_texto %||% 3.2,
        tam_puntos = input$tam_puntos %||% 2.2,
        envolventes = isTRUE(input$envolventes)
      )
    })
    
    output$grafico <- render_biplot(grafico_r, session, salida = "grafico")
    
    output$grafico_din <- plotly::renderPlotly({
      p <- grafico_r()
      plotly::ggplotly(sin_repel(p))
    })
    
    # Descargas del gráfico
    output$bajar_png <- descarga_biplot(grafico_r, function() "biplot_composicional.png", "png", dpi = 300)
    output$bajar_pdf <- descarga_biplot(grafico_r, function() "biplot_composicional.pdf", "pdf")
    output$bajar_svg <- descarga_biplot(grafico_r, function() "biplot_composicional.svg", "svg")
    
    # Tablas de resultados
    output$tabla_links <- DT::renderDT({
      res <- calc_ok()
      tabla_resultados(res$links)
    })
    
    # Tabla de Varianzas de Aitchison
    output$tabla_varray <- DT::renderDT({
      res <- calc_ok()
      arr <- as.data.frame(round(res$var_array$array, 4))
      arr <- cbind(Var=colnames(arr), arr)
      arr$`clr-Varianza` <- round(res$var_array$clr_varianzas[rownames(arr)], 4)
      tabla_resultados(arr)
    })
    
    output$resumen_var_total <- shiny::renderUI({
      res <- calc_ok()
      shiny::p(
        class = "mt-2 font-weight-bold",
        sprintf("Varianza total de Aitchison: %.4f", res$var_array$varianza_total)
      )
    })
    
    output$tabla_inercia <- DT::renderDT({
      res <- calc_ok()
      tabla_resultados(round(res$inercia, 3))
    })
    
    output$sedimentacion <- shiny::renderPlot({
      res <- calc_ok()
      inr <- res$inercia
      ggplot2::ggplot(inr, ggplot2::aes(factor(.data$Eje), .data$`% Inercia`)) +
        ggplot2::geom_col(fill = COLOR_FILAS, width = 0.6) +
        ggplot2::geom_text(
          ggplot2::aes(label = format(round(.data$`% Inercia`, 1), decimal.mark = marca_decimal())),
          vjust = -0.4, size = 3.2, colour = COLOR_TEXTO
        ) +
        ggplot2::labs(x = traducir("Eje"), y = traducir("% de inercia")) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.14))) +
        tema_sello()
    }, res = 96)
    
    # Tablas completas del biplot (CRFE, CREF, coordenadas, correlaciones)
    tablas_completas <- shiny::reactive({
      res <- calc_ok()
      ejes_sel <- c(as.integer(input$eje_x %||% 1), as.integer(input$eje_y %||% 2))
      tablas_biplot(res, ejes = ejes_sel, unidad = "milesima")
    })
    
    output$nota_columnas <- shiny::renderUI({
      cual <- input$tabla_columnas_tipo %||% "calidad_columnas"
      txt <- switch(
        cual,
        calidad_columnas = "CRFE: contribución relativa del factor al elemento (coseno², por mil). Indica qué parte del elemento recoge cada eje; la columna del plano es la suma de los dos ejes del gráfico.",
        contrib_columnas = "CREF: contribución relativa del elemento al factor (por mil). En cada eje suman 1000: indica qué variables estructuran o determinan cada eje."
        # coordenadas_columnas = "Coordenadas factoriales de las partes composicionales (columnas).",
        # correlaciones = "Matriz de correlaciones de las partes composicionales originales."
      )
      shiny::p(class = "bl-help", txt)
    })
    
    output$tabla_c <- DT::renderDT({
      tb <- tablas_completas()
      cual <- input$tabla_columnas_tipo %||% "calidad_columnas"
      dec <- if (cual %in% c("calidad_columnas", "contrib_columnas")) 0 else 4
      tabla_resultados(tb[[cual]], dec)
    })
    
    output$tabla_columnas <- DT::renderDT({
      res <- calc_ok()
      tabla_resultados(round(as.data.frame(res$columnas), 4))
    })
    
    output$tabla_filas <- DT::renderDT({
      res <- calc_ok()
      tabla_resultados(round(as.data.frame(res$filas), 4))
    })
    
    # Lectura guiada
    output$lectura_guiada <- shiny::renderUI({
      res <- calc_ok()
      inr <- res$inercia
      eje1 <- round(inr$`% Inercia`[1], 2)
      eje2 <- round(inr$`% Inercia`[2], 2)
      acum <- round(inr$`% Acumulado`[2], 2)
      
      # Enlace más corto y más largo
      links_ord <- res$links[order(res$links$`Var ln(Xi/Xj)`), ]
      top_corto <- links_ord[1, ]
      top_largo <- links_ord[nrow(links_ord), ]
      
      shiny::tagList(
        shiny::h4("Lectura del Biplot Composicional"),
        shiny::p(sprintf(
          "El plano 1-2 recoge el %s %% de la inercia composicional (eje 1: %s %%; eje 2: %s %%).",
          acum, eje1, eje2
        )),
        shiny::tags$ul(
          shiny::tags$li(shiny::HTML(sprintf(
            "<b>Relación más fuerte (enlace más corto):</b> entre <i>%s</i> y <i>%s</i> con varianza de log-ratio de <b>%s</b> (SD = %s). Una longitud pequeña indica que ambas partes varían de forma prácticamente proporcional.",
            top_corto$Parte_1, top_corto$Parte_2, top_corto$`Var ln(Xi/Xj)`, top_corto$`SD log-ratio`
          ))),
          shiny::tags$li(shiny::HTML(sprintf(
            "<b>Mayor independencia relativa (enlace más largo):</b> entre <i>%s</i> y <i>%s</i> con varianza de log-ratio de <b>%s</b>.",
            top_largo$Parte_1, top_largo$Parte_2, top_largo$`Var ln(Xi/Xj)`
          ))),
          shiny::tags$li(shiny::HTML(
            "<b>Rayos (variables desde el origen):</b> su longitud al cuadrado representa la varianza de la variable clr con respecto a la media geométrica. Rayos largos tienen alta variabilidad relativa; rayos cortos están próximos a la media geométrica."
          )),
          shiny::tags$li(shiny::HTML(
            "<b>Individuos (puntos):</b> las muestras cercanas al centroide tienen valores semejantes a la media geométrica de todas las partes; muestras alejadas en la dirección de un rayo presentan concentraciones relativas elevadas en esa componente."
          ))
        )
      )
    })
    
    # Sintaxis R
    output$sintaxis <- shiny::renderText({
      origen <- datos()$origen
      vars <- input$variables
      lineas <- c(
        "library(biplotlab)",
        sintaxis_lectura(origen),
        sprintf("variables <- %s", vector_a_r(vars)),
        sprintf(
          "res <- calcular_composicional(datos, variables = variables, tipo = \"%s\", dimensiones = %d, imputar_ceros = %s)",
          input$tipo %||% "HJ", input$dims %||% 3, if (isTRUE(input$imputar_ceros)) "TRUE" else "FALSE"
        ),
        sprintf("grafico_composicional(res, ejes = c(1, 2), mostrar_links = TRUE, max_links = %d, orden_links = \"%s\")",
                as.integer(input$max_links %||% 15), input$orden_links %||% "cortos")
      )
      paste(lineas, collapse = "\n")
    })
    
    # Registro en informe Word
    registrar_informe(
      session = session,
      titulo = "HJ-Biplot Composicional",
      graficos = function() list(grafico_r()),
      tablas = function() {
        res <- calc_ok()
        list(
          Inercia = res$inercia,
          `Enlaces (Links)` = res$links,
          `Matriz de Variación` = as.data.frame(res$var_array$array),
          `Coordenadas de partes` = as.data.frame(res$columnas),
          `Coordenadas de individuos` = as.data.frame(res$filas)
        )
      },
      lectura = function() {
        res <- calc_ok()
        inr <- res$inercia
        c(
          sprintf("El plano 1-2 retiene el %.2f %% de la inercia composicional.", inr$`% Acumulado`[2]),
          sprintf("Varianza total de Aitchison: %.4f.", res$var_array$varianza_total)
        )
      }
    )
    
  })
}
