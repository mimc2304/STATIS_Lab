#' biplotlab: aplicativo de métodos Biplot al estilo jamovi
#'
#' Abre el aplicativo con [run_app()]. Cada método vive en su propio módulo
#' (archivo `R/mod_<metodo>.R`) y se registra en [registro_analisis()].
#'
#' Métodos K-tablas, por capas (arquitectura BGL): 0 `ktabla.R` y `resultado.R`,
#' 1 `preparar_ktabla.R`, 2 motor ade4 dentro de `calculo_*.R`, 3
#' `geometria_ktabla.R`, 4 `diagnosticos_ktabla.R`, 5 `grafico_ktabla.R` y 6
#' `calculo_*.R` + `mod_*.R`. Las funciones de ade4 se importan y se llaman sin
#' `ade4::`: algunas (como `dudi.pca()`) leen su llamada y fallan con el prefijo.
#' [BGL_Create()] reúne el resultado de cada método en el objeto BGL (`bgl.R`).
#'
#' @keywords internal
#' @importFrom ggplot2 .data
#' @importFrom ade4 dudi.pca dudi.coa withinpca wca coinertia pta ktab.within
#'   ktab.match2ktabs randtest statis
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @importFrom grDevices chull
#' @importFrom graphics abline arrows legend mtext plot points text
#' @importFrom stats setNames qlogis runif
#' @importFrom utils write.csv
"_PACKAGE"
