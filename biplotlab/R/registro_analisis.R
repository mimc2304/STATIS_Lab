# =============================================================================
# REGISTRO DE ANÁLISIS
#
# Aquí se "enchufa" cada método al aplicativo. Para agregar el método de un
# compañero:
#   1. Copiar inst/plantilla/mod_plantilla.R a R/mod_<metodo>.R y completarlo.
#   2. Cambiar su entrada de `proximamente(...)` por la función del módulo,
#      por ejemplo `analisis_gge()`.
# Nada más: el menú, el panel de opciones y el de resultados se generan solos.
# =============================================================================

#' Lista de análisis del aplicativo
#'
#' Cada elemento es una lista con: `id`, `titulo`, `grupo`, `icono`
#' (Font Awesome), `descripcion`, `listo` y, si está listo,
#' `opciones_ui(id)`, `resultados_ui(id)` y `server(id, datos, compartido)`.
#' @export
registro_analisis <- function() {
  list(
    analisis_hj_biplot(),     # Evelyn: núcleo HJ, Bootstrap y Clúster
    analisis_bootstrap(),
    analisis_cluster(),
    analisis_canonico(),      # Roxi y Kleber
    analisis_logistico(),     # app del compañero: Logístico, DPCA, Disjoint y Cenet
    analisis_dpca(),
    analisis_disjoint(),
    analisis_cenet(),
    analisis_neutrosofico(),  # HJ-Biplot neutrosófico
    analisis_gge(),           # GGE-Biplot

    # K-tablas. La plantilla está en inst/plantilla/mod_plantilla_ktabla.R
    # El orden sigue el número de tablas: dos, K y pares de K. Evelyn: Co-inercia, STATICO
    # y CoSTATIS; los neutrosóficos los trabaja todo el grupo
    analisis_coinercia(),
    proximamente("coinercia_neutro", "Co-inercia Neutrosófica", "circle-half-stroke",
                 "Co-inercia con datos neutrosóficos", "K-tablas", subgrupo = "Dos tablas"),
    analisis_statis(),                                                     # Isa y Evelyn
    analisis_statis_dual(),
    analisis_statis_neutro(),
    analisis_statis_dual_neutro(),
    proximamente("statis_canonico", "STATIS Canónico", "table-cells",
                 "STATIS canónico", "K-tablas", subgrupo = "K tablas"),    # Marlon
    proximamente("statis_dual_canonico", "DUAL Canónico", "table-columns",
                 "STATIS DUAL canónico", "K-tablas", subgrupo = "K tablas"),
    proximamente("metabiplot", "Metabiplot", "object-group",
                 "Metabiplot", "K-tablas", subgrupo = "K tablas"),         # Jhanelly y Mario
    # Se ve como un Biplot y como un método de tres vías: va en las dos pestañas
    proximamente("hj_dinamico", "HJ Dinámico", "chart-line",
                 "HJ-Biplot dinámico", "K-tablas", tambien_en = "Biplot", subgrupo = "K tablas"),
    proximamente("hj_composicional", "HJ Composicional", "chart-pie",
                 "HJ-Biplot composicional"),
    analisis_statico(),
    proximamente("statico_neutro", "STATICO Neutrosófico", "circle-half-stroke",
                 "STATICO con datos neutrosóficos", "K-tablas", subgrupo = "Pares de K tablas"),
    analisis_costatis(),
    proximamente("costatis_neutro", "CoSTATIS Neutrosófico", "circle-half-stroke",
                 "CoSTATIS con datos neutrosóficos", "K-tablas", subgrupo = "Pares de K tablas")
  )
}

proximamente <- function(id, titulo, icono, descripcion, grupo = "Biplot", tambien_en = NULL,
                         subgrupo = NULL) {
  list(id = id, titulo = titulo, grupo = grupo, subgrupo = subgrupo, tambien_en = tambien_en,
       icono = icono, descripcion = descripcion, listo = FALSE)
}
