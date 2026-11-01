# =============================================================================
# Bases de ejemplo de K-tablas (las de las prácticas del Módulo 4).
# =============================================================================

#' Bases de ejemplo (las de las prácticas del Módulo 4)
#'
#' - `"doubs"`: 30 sitios del río Doubs, 11 variables ambientales y 27 especies
#'   de peces (Verneaux, 1973; `ade4::doubs`). Co-inercia.
#' - `"banco_mundial"`: 7 países (2018), 5 indicadores de pobreza y 7 de
#'   protección social y trabajo (Banco Mundial). Co-inercia.
#' - `"meau"`: arroyo Meaudret, 6 sitios en 4 estaciones, 10 variables
#'   físico-químicas y 13 taxones de efemerópteros (`ade4::meau`, ejemplo de
#'   Thioulouse, 2011). STATICO y CoSTATIS.
#' - `"meau_sitios"`: los mismos datos; cambia qué columna se propone como
#'   ocasión (los sitios) y como individuos (las estaciones).
#' - `"frutas"`: 28 lotes de melocotones y nectarinas, 15 descriptores y el orden
#'   de preferencia de 16 jueces (`ade4::fruits`; Kervella, 1991). Co-inercia.
#' @param nombre nombre de la base.
#' @return data.frame en formato largo (una fila por individuo y ocasión).
#' @export
ejemplo_statis <- function(nombre = BASES_KTABLA) {
  nombre <- match.arg(nombre)
  if (nombre == "meau_sitios") nombre <- "meau"
  e <- new.env()
  switch(
    nombre,
    doubs = {
      utils::data("doubs", package = "ade4", envir = e)
      datos <- cbind(e$doubs$env, e$doubs$fish)
      rownames(datos) <- paste0("S", seq_len(nrow(datos)))
      datos
    },
    banco_mundial = {
      ruta <- system.file("extdata", "banco_mundial_coia.csv", package = "biplotlab")
      if (!nzchar(ruta)) ruta <- file.path("inst", "extdata", "banco_mundial_coia.csv")
      datos <- utils::read.csv(ruta, check.names = FALSE, stringsAsFactors = FALSE,
                               fileEncoding = "UTF-8")
      primera_columna_a_nombres(datos)
    },
    frutas = {
      utils::data("fruits", package = "ade4", envir = e)
      f <- e$fruits
      jueces <- f$jug
      names(jueces) <- paste0("Juez", seq_len(ncol(jueces)))
      datos <- data.frame(Tipo = ifelse(as.character(f$type) == "necta", "Nectarina", "Melocotón"),
                          f$var, jueces, check.names = FALSE)
      rownames(datos) <- rownames(f$var)
      datos
    },
    meau = {
      utils::data("meau", package = "ade4", envir = e)
      m <- e$meau
      datos <- data.frame(Estacion = as.character(m$design$season),
                          Sitio = as.character(m$design$site), m$env, m$spe,
                          check.names = FALSE)
      rownames(datos) <- paste(datos$Sitio, datos$Estacion, sep = "_")
      datos
    }
  )
}

#' Cómo se reparten las columnas de cada base de ejemplo
#'
#' Al elegir un ejemplo, los módulos proponen estas tablas X e Y, la ocasión,
#' los individuos y la preparación de las prácticas.
#' @export
BLOQUES_EJEMPLO <- list(
  doubs = list(
    x = c("dfs", "alt", "slo", "flo", "pH", "har", "pho", "nit", "amm", "oxy", "bdo"),
    y = c("Cogo", "Satr", "Phph", "Neba", "Thth", "Teso", "Chna", "Chto", "Lele", "Lece",
          "Baba", "Spbi", "Gogo", "Eslu", "Pefl", "Rham", "Legi", "Scer", "Cyca", "Titi",
          "Abbr", "Icme", "Acce", "Ruru", "Blbj", "Alal", "Anan"),
    nombre_x = "Ambiente", nombre_y = "Peces",
    preparacion_x = "estandarizar", preparacion_y = "centrar", nominales = character()),
  banco_mundial = list(
    x = c("Brecha_Pobreza", "Incidencia_pobreza_2.15USDdia", "Poblacion_barriosTugurios",
          "Incidencia_pobreza_Nacional", "Participación_ingreso_10"),
    y = c("DesempleoMujeres", "DesempleoVarones", "DesempleoTotal", "Asalariados_mujeres",
          "Mujeres_Contribuyen", "Relación_EmpleoPoblación", "Vulnerable_Mujeres"),
    nombre_x = "Pobreza", nombre_y = "Protección social",
    preparacion_x = "estandarizar", preparacion_y = "estandarizar", nominales = "Anio"),
  meau = list(
    x = c("Temp", "Flow", "pH", "Cond", "Oxyg", "Bdo5", "Oxyd", "Ammo", "Nitr", "Phos"),
    y = c("Eda", "Bsp", "Brh", "Bni", "Bpu", "Cen", "Ecd", "Rhi", "Hla", "Hab", "Par", "Cae", "Eig"),
    ocasion = "Estacion", individuo = "Sitio", nombre_x = "Ambiente", nombre_y = "Efemerópteros",
    preparacion_x = "total", preparacion_y = "centrar", nominales = c("Estacion", "Sitio")),
  meau_sitios = list(
    x = c("Temp", "Flow", "pH", "Cond", "Oxyg", "Bdo5", "Oxyd", "Ammo", "Nitr", "Phos"),
    y = c("Eda", "Bsp", "Brh", "Bni", "Bpu", "Cen", "Ecd", "Rhi", "Hla", "Hab", "Par", "Cae", "Eig"),
    ocasion = "Sitio", individuo = "Estacion", nombre_x = "Ambiente", nombre_y = "Efemerópteros",
    preparacion_x = "total", preparacion_y = "centrar", nominales = c("Estacion", "Sitio")),
  frutas = list(
    x = c("taches", "stries", "abmucr", "irform", "allong", "suroug", "homlot", "homfru",
          "pubesc", "verrou", "foncee", "comucr", "impres", "coldom", "calibr"),
    y = paste0("Juez", 1:16),
    nombre_x = "Descriptores", nombre_y = "Preferencias",
    preparacion_x = "estandarizar", preparacion_y = "centrar", nominales = "Tipo")
)

# Todas las bases que lee ejemplo_statis(). Doubs y Banco Mundial siguen disponibles
# para las pruebas y la Sintaxis R, pero no se ofrecen en el menú: en Doubs el eje 1
# absorbe el 88 % y en Banco Mundial hay solo 7 países y el test no es significativo
BASES_KTABLA <- c("doubs", "banco_mundial", "meau", "meau_sitios", "frutas")

# Bases de ejemplo que ofrece el panel Datos (etiqueta = nombre en ejemplo_statis())
EJEMPLOS_STATIS <- c(
  "Frutas: descriptores y preferencias, 28 lotes (Co-inercia)" = "frutas",
  "Arroyo Meaudret por sitios: 6 sitios × 4 estaciones (STATICO)" = "meau_sitios",
  "Arroyo Meaudret por estaciones: 4 estaciones × 6 sitios (CoSTATIS)" = "meau"
)
