# Guía para integrar tu método en BiplotLab

Cada método es un **módulo**: un archivo `R/mod_<metodo>.R` con cuatro piezas.
El HJ-Biplot (`R/mod_hj_biplot.R`) ya está hecho y sirve de ejemplo completo.

## Qué enviar a quien integra el aplicativo

1. Tu **script .R** completo y comentado.
2. La **base de datos** que usaste (.csv o .xlsx).
3. Las **salidas** que obtuviste (valores propios, % de inercia, coordenadas,
   copiadas de la consola) y el gráfico final. Sirven para comprobar que el
   aplicativo da lo mismo.
4. Los **paquetes** que usaste y su versión (`packageVersion("metan")`).
5. Dos o tres líneas sobre **cómo se interpreta** tu método.

Lo más útil es que tu cálculo ya esté como **función**: datos y opciones como
entradas, una lista de resultados como salida. Por ejemplo:

```r
calcular_gge <- function(datos, genotipo, ambiente, respuesta, svp = "genotype") {
  # ... tu código ...
  list(inercia = ..., filas = ..., columnas = ..., tabla = ...)
}
```

## Si lo integras tú

1. Copia `inst/plantilla/mod_plantilla.R` a `R/mod_<metodo>.R`.
2. Reemplaza `METODO` por el nombre corto (ej.: `gge`) en todo el archivo.
3. Completa los `TODO`:
   - **A) Cálculo:** tu código de R dentro de `calcular_<metodo>()`, sin nada de
     Shiny.
   - **B) Registro:** título, ícono, tu nombre y una descripción.
   - **C) Opciones:** los controles del panel izquierdo, cada uno con `ns("...")`.
   - **D) Resultados:** las pestañas con gráficos, tablas y la sintaxis.
   - **E) Lógica:** conecta las opciones con el cálculo y las salidas.
4. En `R/registro_analisis.R` cambia tu línea `proximamente("...")` por
   `analisis_<metodo>()`.
5. Si usas paquetes nuevos, agrégalos en `DESCRIPTION` (sección `Imports`) y
   llámalos como `paquete::funcion()`.
6. Ejecuta `roxygen2::roxygenise()` y luego `iniciar.R` para probar.

## Reglas para que todo encaje

- **Nada de ventanas tcltk.** `biplotbootGUI::biplotboot()` y
  `GGEBiplotGUI::GGEBiplot()` abren ventanas propias y no funcionan en una app
  web. Usa sus funciones de cálculo internas o el cálculo directo (por ejemplo,
  `metan::gge()` para el GGE).
- **MULTBIPLOT no es R.** Si tu práctica fue solo en MULTBIPLOT, necesitamos el
  equivalente en R; sus salidas sirven para validar.
- **Sin rutas fijas** (`C:/Users/...`) ni `setwd()` dentro del módulo: los datos
  llegan por `datos()$df`.
- **Mensajes en español y claros:** usa `stop("...", call. = FALSE)`; el
  aplicativo los muestra en un recuadro.
- **Gráficos con ggplot2** y `coord_equal()` si dibujas un biplot, para que los
  ángulos se conserven. Si tu resultado tiene `filas` y `columnas`, puedes
  reutilizar `grafico_biplot()`.
- **Pestaña Sintaxis R:** el código que reproduce tu resultado con
  `calcular_<metodo>()`. Usa `sintaxis_lectura(datos()$origen)` y
  `vector_a_r()` para armarlo.
- **Una prueba de validación** en `tests/testthat/test-<metodo>.R` que compare
  con los números de tu práctica.

## Datos que recibe tu módulo

`datos()` devuelve una lista:

| Elemento | Contenido |
|---|---|
| `df` | data.frame; los nombres de fila son las etiquetas de los individuos |
| `origen` | de dónde salió: `tipo` ("ejemplo" o "archivo"), `nombre`, `hoja` |
| `etiqueta` | texto corto para mostrar (nombre del archivo) |
| `nominales` | columnas de agrupación: úsalas para grupos o factores, nunca como variables cuantitativas |

El servidor recibe además `compartido`: el HJ-Biplot publica ahí su resultado
(`compartido$hj()`) y su configuración (`compartido$hj_config()`). Así lo usan
Bootstrap y Clúster sin tocar el núcleo; tu módulo puede hacer lo mismo.

## Si tu método es de K-tablas

Los métodos K-tablas viven en la misma app: se ven en la pestaña **K-tablas** de la cinta.

1. Copia `inst/plantilla/mod_plantilla_ktabla.R` a `R/mod_<metodo>.R` y completa sus partes `TODO`.
2. En `R/registro_analisis.R` cambia tu línea `proximamente("...", ..., "K-tablas")` por `analisis_<metodo>()`, con `grupo = "K-tablas"` en tu módulo.
3. Agrega tus Fundamentos en `R/teoria_ktablas.R` y tus bases de ejemplo en `R/datos_ktabla.R`.
4. Reutiliza las capas comunes: `ktabla.R` y `resultado.R` (datos y resultado), `preparar_ktabla.R`, `geometria_ktabla.R`, `diagnosticos_ktabla.R`, `grafico_ktabla.R` y `mod_comun.R` (pantalla).

Los datos van en formato largo: una fila por individuo y ocasión, con una columna que indique la ocasión.

### Objeto BGL

Si tu método devuelve `nuevo_resultado()`, `BGL_Create(res)` ya arma su objeto BGL con las secciones `$Preprocessing`, `$Factorization`, `$Geometry`, `$Diagnostics`, `$Graphics`, `$Methods`, `$History` y `$Options`. También tienes `BGL_Show()`, `BGL_Summary()`, `BGL_History()`, `BGL_Options()`, `BGL_Validate()`, `BGL_Flip()`, `BGL_Plot()` y `BGL_Export()`. Para que `BGL_Plot()` dibuje tu método, agrega sus gráficos a `GRAFICOS_BGL` en `R/bgl.R`.

## Informe Word y gráfico dinámico

Los botones de informe (español e inglés) y la vista dinámica son comunes a todos los métodos.

- **Gráfico dinámico:** en la pantalla usa `salida_biplot(ns, "biplot")` y en el servidor `render_biplot(grafico, session)`.
- **Informe:** en el servidor llama a `registrar_informe(session, "Tu método", function() list(grafico()), hojas_excel)`. Si usas `conectar_comunes()` de K-tablas, ya queda registrado.

## Revisión de la base y asistente

El panel Datos revisa la base e indica qué métodos sirven (`R/asistente.R`). Al agregar un método, añade su regla en la lista `reglas` de `asistente_metodos()`. La interpretación del informe sale de tus tablas (`R/interpretacion_biplot.R`); si tu método tiene una tabla propia que merezca una frase, agrégala allí en español e inglés.
