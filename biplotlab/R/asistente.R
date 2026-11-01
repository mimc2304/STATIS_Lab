# =============================================================================
# Revisión de la base y asistente de métodos (panel Datos). Solo mira la estructura
# de los datos: no calcula nada ni cambia la base. Las reglas de STATIS y STATIS DUAL
# son las de la Dra.: STATIS exige los mismos individuos y las variables pueden
# coincidir o no; STATIS DUAL exige las mismas variables y los individuos pueden
# coincidir o no.
# =============================================================================

# Nombres que sugieren la ocasión (no el individuo)
PATRON_OCASION <- "^(ocasi[oó]n|occasion|estaci[oó]n|season|periodo|period|anio|año|year|tiempo|time|mes|month|fecha|date)"
PATRON_INDIVIDUO <- "^(sitio|site|pa[ií]s|country|individuo|individual|sujeto|subject|id$|caso|case)"

#' Proponer las columnas de ocasión e individuos
#' @param df data.frame.
#' @return lista con `ocasion` e `individuo` (`""` si no se propone ninguna).
#' @export
proponer_vias <- function(df) {
  repite <- function(n) anyDuplicated(df[[n]]) > 0 && length(unique(df[[n]])) >= 2
  una <- function(patron) {
    n <- grep(patron, names(df), value = TRUE, ignore.case = TRUE)
    n <- n[vapply(n, repite, TRUE)]
    if (length(n)) n[1] else ""
  }
  list(ocasion = una(PATRON_OCASION), individuo = una(PATRON_INDIVIDUO))
}

# Todo lo que las reglas necesitan saber de la base
perfil_base <- function(df, nominales = character(), ocasion = "", individuo = "") {
  ocasion <- if (isTRUE(nzchar(ocasion)) && ocasion %in% names(df)) ocasion else ""
  individuo <- if (isTRUE(nzchar(individuo)) && individuo %in% names(df) && individuo != ocasion) individuo else ""
  vias <- setdiff(c(ocasion, individuo), "")
  datos <- list(df = df, nominales = nominales)
  numericas <- setdiff(variables_numericas(datos), vias)
  X <- df[numericas]
  fac <- if (nzchar(ocasion)) as.character(df[[ocasion]]) else NULL
  p <- list(df = df, nominales = nominales, ocasion = ocasion, individuo = individuo, numericas = numericas,
            n = nrow(df), faltantes = if (length(numericas)) sum(is.na(X)) else 0L,
            niveles = if (!is.null(fac)) unique(fac[!is.na(fac)]) else character(),
            hay_ocasion = !is.null(fac))
  if (p$hay_ocasion) {
    p$por_ocasion <- table(factor(fac, levels = p$niveles))
    # Una variable falta en una ocasión si toda esa ocasión está vacía
    p$vacias <- Filter(length, lapply(stats::setNames(p$niveles, p$niveles), function(k) {
      numericas[vapply(numericas, function(v) all(is.na(df[[v]][which(fac == k)])), TRUE)]
    }))
    p$mismas_variables <- !length(p$vacias)
    if (nzchar(individuo)) {
      ind <- as.character(df[[individuo]])
      conjuntos <- split(ind, factor(fac, levels = p$niveles))
      p$repetidos <- names(Filter(anyDuplicated, conjuntos))
      p$mismos_individuos <- !length(p$repetidos) &&
        all(vapply(conjuntos, function(s) setequal(s, conjuntos[[1]]), TRUE))
    }
    # Sin las celdas vacías de una variable que no se midió en esa ocasión
    p$faltantes_reales <- p$faltantes - sum(vapply(names(p$vacias), function(k) {
      length(p$vacias[[k]]) * sum(fac == k, na.rm = TRUE)
    }, 0))
  } else {
    p$faltantes_reales <- p$faltantes
  }
  p
}

#' Revisar la base antes de calcular
#' @param df data.frame.
#' @param nominales columnas nominales.
#' @param ocasion,individuo columnas elegidas (`""` si no hay).
#' @return data.frame con `Revisión`, `Estado` (`"Correcto"`, `"Aviso"` o
#'   `"Problema"`) y `Detalle`.
#' @export
revisar_base <- function(df, nominales = character(), ocasion = "", individuo = "") {
  p <- perfil_base(df, nominales, ocasion, individuo)
  filas <- list()
  anotar <- function(revision, estado, detalle) {
    filas[[length(filas) + 1L]] <<- data.frame(Revisión = revision, Estado = estado, Detalle = detalle,
                                               check.names = FALSE)
  }
  corta <- function(v, max = 6) {
    paste0(paste(utils::head(v, max), collapse = ", "), if (length(v) > max) sprintf(" y %d más", length(v) - max) else "")
  }
  nv <- length(p$numericas)
  X <- as.matrix(df[p$numericas])

  if (p$n < 3 || nv < 2) {
    anotar("Tamaño", "Problema", sprintf("%d filas y %d variables numéricas: se necesitan al menos 3 filas y 2 variables.", p$n, nv))
  } else if (p$n < nv) {
    anotar("Tamaño", "Aviso", sprintf("%d filas y %d variables numéricas: hay menos filas que variables; algunos métodos, como el Canónico, pueden no ser estables.", p$n, nv))
  } else {
    anotar("Tamaño", "Correcto", sprintf("%d filas y %d variables numéricas.", p$n, nv))
  }

  if (p$faltantes_reales > 0) {
    cols <- p$numericas[colSums(is.na(X)) > 0]
    anotar("Datos faltantes", "Problema",
           sprintf("%d celdas vacías (columnas: %s). La mayoría de los métodos no las acepta: complétalas o quita esas filas. El Canónico puede imputarlas.",
                   p$faltantes_reales, corta(cols)))
  } else {
    anotar("Datos faltantes", "Correcto", "No hay celdas vacías que impidan calcular.")
  }

  if (nv) {
    s <- apply(X, 2, stats::sd, na.rm = TRUE)
    constantes <- p$numericas[!is.finite(s) | s == 0]
    if (length(constantes)) {
      anotar("Variables constantes", "Problema", sprintf("%s no varían: no se pueden estandarizar. Quítalas.", corta(constantes)))
    } else {
      anotar("Variables constantes", "Correcto", "Todas las variables varían.")
    }
    s <- s[is.finite(s) & s > 0]
    if (length(s) >= 2 && max(s) / min(s) > 100) {
      anotar("Escalas", "Aviso", sprintf("Las desviaciones van de %s a %s: conviene estandarizar para que no dominen las variables de mayor escala.",
                                         format(min(s), digits = 3), format(max(s), digits = 3)))
    }
    z <- suppressWarnings(abs(scale(X)))
    extremos <- which(apply(z, 1, function(f) any(f > 5, na.rm = TRUE)))
    if (length(extremos)) {
      anotar("Valores extremos", "Aviso", sprintf("%d filas tienen algún valor a más de 5 desviaciones de la media (por ejemplo: %s). Revisa si son errores de digitación.",
                                                  length(extremos), corta(rownames(df)[extremos], 4)))
    }
  }

  texto <- setdiff(names(df)[vapply(df, is.character, TRUE)], c(p$ocasion, p$individuo))
  numero_como_texto <- texto[vapply(texto, function(n) {
    v <- df[[n]][!is.na(df[[n]]) & nzchar(df[[n]])]
    length(v) > 0 && mean(!is.na(suppressWarnings(as.numeric(gsub(",", ".", v, fixed = TRUE))))) > 0.8
  }, TRUE)]
  if (length(numero_como_texto)) {
    anotar("Números como texto", "Aviso", sprintf("%s parecen números guardados como texto: no se usarán como variables hasta corregirlas.", corta(numero_como_texto)))
  }
  if (anyDuplicated(df)) {
    anotar("Filas repetidas", "Aviso", sprintf("%d filas son idénticas a otra: confirma que no estén duplicadas.", sum(duplicated(df))))
  }

  if (p$hay_ocasion) {
    k <- length(p$niveles)
    if (k < 2) {
      anotar("Ocasiones", "Problema", sprintf("«%s» tiene una sola ocasión: K-tablas necesita al menos 2.", p$ocasion))
    } else if (any(p$por_ocasion < 3)) {
      anotar("Ocasiones", "Problema", sprintf("Cada ocasión necesita al menos 3 filas; no las tienen: %s.", corta(names(p$por_ocasion)[p$por_ocasion < 3])))
    } else {
      anotar("Ocasiones", "Correcto", sprintf("%d ocasiones en «%s», con entre %d y %d filas.", k, p$ocasion, min(p$por_ocasion), max(p$por_ocasion)))
    }
    anotar("Variables entre ocasiones", if (p$mismas_variables) "Correcto" else "Aviso",
           if (p$mismas_variables) "Las mismas variables en todas las ocasiones."
           else sprintf("No todas las variables se midieron en todas las ocasiones (por ejemplo, en %s): STATIS DUAL, STATICO y CoSTATIS no se pueden usar.", corta(names(p$vacias), 3)))
    if (nzchar(p$individuo)) {
      if (length(p$repetidos)) {
        anotar("Individuos entre ocasiones", "Problema", sprintf("«%s» repite individuos dentro de una misma ocasión (%s): cada individuo debe aparecer una vez por ocasión.", p$individuo, corta(p$repetidos, 3)))
      } else {
        anotar("Individuos entre ocasiones", if (isTRUE(p$mismos_individuos)) "Correcto" else "Aviso",
               if (isTRUE(p$mismos_individuos)) "Los mismos individuos en todas las ocasiones."
               else "Los individuos cambian entre ocasiones: STATIS y CoSTATIS no se pueden usar; STATIS DUAL y STATICO sí.")
      }
    } else {
      anotar("Individuos entre ocasiones", "Aviso", "No indicaste la columna de individuos: no se puede comprobar si se repiten entre ocasiones.")
    }
  } else {
    anotar("Ocasiones", "Aviso", "No indicaste una columna de ocasión: se revisa como una sola tabla y no se evalúan los métodos de tres vías.")
  }
  do.call(rbind, filas)
}

#' Indicar qué métodos sirven según la estructura de los datos
#' @inheritParams revisar_base
#' @return data.frame con `id`, `Método`, `Grupo`, `Estado` (`"Sirve"`, `"Con
#'   condiciones"`, `"No sirve"`), `Motivo` y `Disponible`.
#' @export
asistente_metodos <- function(df, nominales = character(), ocasion = "", individuo = "") {
  p <- perfil_base(df, nominales, ocasion, individuo)
  nv <- length(p$numericas)
  X <- df[p$numericas]
  completa <- p$faltantes == 0
  sin_faltantes <- "Antes hay que resolver los datos faltantes."

  base_plana <- function(sirve) {
    if (p$n < 3 || nv < 2) return(list("No sirve", "Necesita al menos 3 filas y 2 variables numéricas."))
    if (!completa) return(list("Con condiciones", sin_faltantes))
    list("Sirve", sirve)
  }
  # Regla común de los métodos de tres vías
  tres_vias <- function(mismas_var, mismos_ind, sirve, minimo = 2L) {
    if (!p$hay_ocasion) return(list("No sirve", "Necesita una columna de ocasión."))
    if (length(p$niveles) < 2) return(list("No sirve", "Necesita al menos 2 ocasiones."))
    if (any(p$por_ocasion < 3)) return(list("No sirve", "Cada ocasión necesita al menos 3 filas."))
    if (nv < minimo) return(list("No sirve", sprintf("Necesita al menos %d variables numéricas.", minimo)))
    if (mismas_var && !p$mismas_variables) return(list("No sirve", "Exige las mismas variables en todas las ocasiones, y aquí no se midieron todas."))
    if (mismos_ind) {
      if (!nzchar(p$individuo)) return(list("Con condiciones", "Exige los mismos individuos: indica su columna para comprobarlo."))
      if (!isTRUE(p$mismos_individuos)) return(list("No sirve", "Exige los mismos individuos en todas las ocasiones, y aquí cambian."))
    }
    if (!mismas_var && p$faltantes_reales > 0 || mismas_var && !completa) return(list("Con condiciones", sin_faltantes))
    list("Sirve", sirve)
  }

  numericos <- as.matrix(X)
  binarias <- p$numericas[vapply(p$numericas, function(v) all(df[[v]] %in% c(0, 1, NA)), TRUE)]
  con_grupos <- p$nominales[vapply(p$nominales, function(n) {
    t <- table(df[[n]]); length(t) >= 2 && length(t) < p$n && all(t >= 2)
  }, TRUE)]
  ternas <- sub("_T$", "", grep("_T$", names(df), value = TRUE))
  ternas <- ternas[vapply(ternas, function(b) all(paste0(b, c("_I", "_F")) %in% names(df)), TRUE)]
  respuestas <- setdiff(names(df), c(p$numericas, p$ocasion, p$individuo))

  reglas <- list(
    hj = base_plana("Tabla de individuos por variables numéricas."),
    bootstrap = base_plana("Da intervalos de confianza sobre el HJ-Biplot; primero hay que configurarlo."),
    cluster = base_plana("Agrupa individuos sobre el HJ-Biplot; primero hay que configurarlo."),
    canonico = if (nv < 2) list("No sirve", "Necesita al menos 2 variables numéricas.")
      else if (!length(con_grupos)) list("No sirve", "Necesita una columna de grupos con al menos 2 filas por grupo.")
      else list("Sirve", sprintf("Compara los grupos de: %s.", paste(utils::head(con_grupos, 3), collapse = ", "))),
    logistic = if (length(binarias) >= 2) list("Sirve", sprintf("Hay %d variables binarias (0/1).", length(binarias)))
      else list("No sirve", "Necesita al menos 2 variables binarias (0/1)."),
    dpca = base_plana("Busca componentes en las que cada variable pertenece a un solo eje."),
    disjoint = base_plana("Agrupa individuos y forma componentes disjuntas de variables."),
    cenet = base_plana("Biplot con cargas dispersas, útil con muchas variables."),
    neutro = if (length(ternas) >= 2) list("Sirve", "Hay variables con columnas _T, _I y _F.")
      else if (length(respuestas) >= 2) list("Con condiciones", "Sirve si estas columnas son respuestas de encuesta que se puedan recodificar a T, I y F.")
      else list("No sirve", "Necesita columnas _T, _I y _F, o al menos 2 preguntas de encuesta."),
    gge = { r <- base_plana("x"); if (r[[1]] == "Sirve") list("Con condiciones", "Sirve si las filas son genotipos y las columnas, ambientes en la misma unidad.") else r },
    coinercia = {
      if (nv < 4) list("No sirve", "Necesita 2 tablas de al menos 2 variables cada una.")
      else if (!completa) list("Con condiciones", sin_faltantes)
      else if (p$hay_ocasion) list("Con condiciones", "Trata todas las filas como una sola ocasión; para comparar ocasiones usa STATICO o CoSTATIS.")
      else list("Sirve", "Reparte las variables en 2 tablas sobre los mismos individuos.")
    },
    statis = tres_vias(FALSE, TRUE, "Mismos individuos; las variables pueden coincidir o no."),
    statis_dual = tres_vias(TRUE, FALSE, "Mismas variables; los individuos pueden coincidir o no."),
    statico = tres_vias(TRUE, FALSE, "Mismas variables en dos tablas; los individuos pueden cambiar entre ocasiones.", 4L),
    costatis = tres_vias(TRUE, TRUE, "Mismos individuos y mismas variables en dos tablas.", 4L),
    metabiplot = tres_vias(FALSE, FALSE, "Varias tablas por ocasión."),
    hj_dinamico = tres_vias(TRUE, FALSE, "Evolución en las ocasiones."),
    hj_composicional = if (nv >= 2 && completa && all(numericos >= 0)) list("Con condiciones", "Sirve si las variables son partes de un total (composiciones).")
      else list("No sirve", "Necesita valores no negativos y sin faltantes.")
  )
  reglas$statis_canonico <- reglas$statis
  reglas$statis_dual_canonico <- reglas$statis_dual
  reglas$statis_neutro <- reglas$statis
  reglas$statis_dual_neutro <- reglas$statis_dual
  reglas$coinercia_neutro <- reglas$coinercia
  reglas$statico_neutro <- reglas$statico
  reglas$costatis_neutro <- reglas$costatis

  do.call(rbind, lapply(registro_analisis(), function(a) {
    r <- reglas[[a$id]] %||% list("Con condiciones", "Revisa sus requisitos en Fundamentos.")
    data.frame(id = a$id, Método = a$titulo, Grupo = a$grupo, Estado = r[[1]], Motivo = r[[2]],
               Disponible = isTRUE(a$listo), check.names = FALSE)
  }))
}

# ------------------------------------------------------------------- Pantalla ----

tabla_revision <- function(t) {
  clase <- c(Correcto = "bl-ok", Aviso = "bl-aviso", Problema = "bl-problema")
  shiny::tags$table(class = "table table-sm bl-revision", shiny::tags$tbody(lapply(seq_len(nrow(t)), function(i) {
    shiny::tags$tr(shiny::tags$td(shiny::span(class = clase[[t$Estado[i]]], t$Estado[i])),
                   shiny::tags$td(shiny::strong(t$Revisión[i]), shiny::br(), t$Detalle[i]))
  })))
}

tabla_asistente <- function(t) {
  clase <- c(Sirve = "bl-ok", `Con condiciones` = "bl-aviso", `No sirve` = "bl-problema")
  orden <- order(match(t$Estado, names(clase)), !t$Disponible)
  shiny::tags$table(class = "table table-sm bl-revision", shiny::tags$tbody(lapply(orden, function(i) {
    abrir <- if (t$Disponible[i] && t$Estado[i] != "No sirve") {
      shiny::tags$a(href = "#", onclick = sprintf(
        "$('.bl-ribbon-tab[data-grupo=\"%s\"]').click(); $('#abrir_%s').click(); return false;", t$Grupo[i], t$id[i]), "Abrir")
    } else if (!t$Disponible[i]) shiny::span(class = "bl-pend", "En desarrollo")
    shiny::tags$tr(shiny::tags$td(shiny::span(class = clase[[t$Estado[i]]], t$Estado[i])),
                   shiny::tags$td(shiny::strong(t$Método[i]), shiny::br(), t$Motivo[i]),
                   shiny::tags$td(abrir))
  })))
}
