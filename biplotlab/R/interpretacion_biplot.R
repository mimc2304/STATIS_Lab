# =============================================================================
# Interpretación del informe de los métodos Biplot, en español o inglés. Se lee de
# las mismas tablas que van al Excel, así que sirve para todos los métodos: cada
# frase aparece solo si el método tiene la tabla que necesita.
# =============================================================================

# Primera hoja que exista, entre varios nombres posibles
hoja_de <- function(hojas, nombres) {
  n <- intersect(nombres, names(hojas))
  if (length(n)) hojas[[n[1]]] else NULL
}

# Vector numérico con nombres: la primera columna da los nombres
columna_con_nombres <- function(t, columna) {
  if (!is.data.frame(t) || !columna %in% names(t) || !is.numeric(t[[columna]])) return(NULL)
  stats::setNames(t[[columna]], as.character(t[[1]]))
}

lista_nombres <- function(v, en, max = 3) {
  v <- utils::head(v, max)
  n <- length(v)
  if (n < 2) return(paste(v, collapse = ""))
  paste0(paste(v[-n], collapse = ", "), if (en) " and " else " y ", v[n])
}

#' Interpretación automática de las tablas de un método Biplot
#' @param hojas lista con nombre de tablas (las del Excel del método).
#' @param idioma `"es"` o `"en"`.
#' @return frases de interpretación.
#' @export
interpretar_tablas <- function(hojas, idioma = idioma_actual()) {
  en <- identical(idioma, "en")
  tx <- function(es, ing) if (en) ing else es
  dec <- marca_decimal(idioma)
  n1 <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = dec)
  n3 <- function(x) formatC(x, format = "f", digits = 3, decimal.mark = dec)
  frases <- character()

  # Inercia del plano
  inercia <- hoja_de(hojas, c("Inercia", "Autovalores", "Valores propios"))
  if (is.data.frame(inercia) && nrow(inercia) >= 2) {
    col <- grep("%", names(inercia), value = TRUE)
    col <- col[!grepl("cumul|acum", col, ignore.case = TRUE)]
    col <- col[vapply(inercia[col], is.numeric, TRUE)]
    if (length(col)) {
      p <- inercia[[col[1]]][1:2]
      plano <- sum(p)
      frases <- c(frases, paste(
        sprintf(tx("El plano 1-2 retiene el %s %% de la inercia (eje 1: %s %%; eje 2: %s %%).",
                   "Plane 1-2 retains %s %% of the inertia (axis 1: %s %%; axis 2: %s %%)."),
                n1(plano), n1(p[1]), n1(p[2])),
        if (plano >= 70) tx("Es un plano adecuado para interpretar.", "It is an adequate plane to interpret.")
        else if (plano >= 50) tx("Es aceptable, pero conviene revisar también el eje 3.",
                                 "It is acceptable, but axis 3 should also be reviewed.")
        else tx("Es bajo: conviene mirar otros planos antes de concluir.",
                "It is low: other planes should be examined before concluding.")))
    }
  }

  # Variables que más pesan en cada eje
  variables <- hoja_de(hojas, c("Coordenadas variables", "Variables"))
  for (k in 1:2) {
    v <- columna_con_nombres(variables, paste("Eje", k)) %||% columna_con_nombres(variables, paste0("Coord", k))
    if (is.null(v) || !length(v)) next
    v <- v[order(-abs(v))]
    frases <- c(frases, sprintf(
      tx("Eje %d: las variables con mayor coordenada son %s.", "Axis %d: the variables with the largest coordinates are %s."),
      k, lista_nombres(sprintf("%s (%s)", names(v), n3(v)), en)))
  }

  # Calidad de representación en el plano
  calidad <- function(t) {
    if (!is.data.frame(t)) return(NULL)
    col <- grep("^(CRFE p|P)lano|^Calidad", names(t), value = TRUE)
    if (length(col)) columna_con_nombres(t, col[1])
  }
  frase_calidad <- function(q, es, ing) {
    if (is.null(q) || length(q) < 2) return(NULL)
    q <- sort(q, decreasing = TRUE)
    sprintf(tx("%s con mejor calidad de representación: %s. Con la peor: %s; estos no deben interpretarse sin revisar otros planos.",
               "%s with the best representation quality: %s. With the worst: %s; these should not be interpreted without checking other planes."),
            tx(es, ing), lista_nombres(names(q), en), lista_nombres(rev(names(q)), en))
  }
  frases <- c(frases,
    frase_calidad(calidad(hoja_de(hojas, c("CRFE variables", "Variables"))), "Variables", "Variables"),
    frase_calidad(calidad(hoja_de(hojas, c("CRFE individuos", "Observaciones", "Individuos"))), "Individuos", "Observations"))

  # Individuos que dominan un eje
  contrib <- hoja_de(hojas, "CREF individuos")
  for (k in 1:2) {
    v <- columna_con_nombres(contrib, paste("Eje", k))
    if (is.null(v) || length(v) < 2) next
    v <- sort(v, decreasing = TRUE)
    frases <- c(frases, sprintf(
      tx("Eje %d: %s es el individuo que más contribuye; si su contribución es muy superior a la del resto, conviene revisar si es un atípico.",
         "Axis %d: %s is the observation that contributes most; if its contribution far exceeds the rest, check whether it is an outlier."),
      k, names(v)[1]))
  }

  # Correlación más alta
  corr <- hoja_de(hojas, "Correlaciones")
  if (is.data.frame(corr) && nrow(corr) >= 2 && ncol(corr) >= 3) {
    nombres <- intersect(as.character(corr[[1]]), names(corr))
    if (length(nombres) >= 2) {
      R <- as.matrix(corr[match(nombres, corr[[1]]), nombres, drop = FALSE])
      R[upper.tri(R, diag = TRUE)] <- NA
      if (any(!is.na(R))) {
        i <- which(abs(R) == max(abs(R), na.rm = TRUE), arr.ind = TRUE)[1, ]
        frases <- c(frases, sprintf(tx("La correlación más fuerte es entre %s y %s (r = %s).",
                                       "The strongest correlation is between %s and %s (r = %s)."),
                                    nombres[i[1]], nombres[i[2]], n3(R[i[1], i[2]])))
      }
    }
  }

  # Variables de cada componente disjunta (DPCA y Disjoint)
  particion <- hoja_de(hojas, "Particion")
  if (is.data.frame(particion) && all(c("Variable", "Componente") %in% names(particion))) {
    por_eje <- split(as.character(particion$Variable), as.character(particion$Componente))
    frases <- c(frases, sprintf(tx("Componentes disjuntas: %s.", "Disjoint components: %s."),
                                paste(sprintf("%s = %s", names(por_eje), vapply(por_eje, paste, "", collapse = ", ")),
                                      collapse = "; ")))
  }
  # Tamaño de los grupos (Disjoint y Clúster)
  tamanos <- hoja_de(hojas, "Clusters")
  if (is.data.frame(tamanos) && all(c("Cluster", "n") %in% names(tamanos))) {
    frases <- c(frases, sprintf(tx("Grupos de individuos y su tamaño: %s.", "Groups of observations and their size: %s."),
                                paste(sprintf("%s (n = %d)", tamanos$Cluster, as.integer(tamanos$n)), collapse = ", ")))
  }

  # Grupos de un clúster
  grupos <- hoja_de(hojas, "Grupos")
  if (is.data.frame(grupos) && all(c("Grupo", "Tamaño") %in% names(grupos))) {
    frases <- c(frases, sprintf(tx("Tamaño de los grupos: %s.", "Group sizes: %s."),
                                paste(sprintf("%s (n = %d)", grupos$Grupo, as.integer(grupos$Tamaño)), collapse = ", ")))
  }
  asignacion <- hoja_de(hojas, "Asignacion")
  if (is.data.frame(asignacion) && is.numeric(asignacion$Silueta)) {
    s <- mean(asignacion$Silueta, na.rm = TRUE)
    frases <- c(frases, paste(
      sprintf(tx("Se formaron %d grupos, con silueta media de %s.", "%d groups were formed, with a mean silhouette of %s."),
              if (is.data.frame(grupos)) nrow(grupos) else length(unique(asignacion$Grupo)), n3(s)),
      if (s >= 0.5) tx("Los grupos están bien separados.", "The groups are well separated.")
      else if (s >= 0.25) tx("La separación es moderada.", "The separation is moderate.")
      else tx("La separación es débil: los grupos deben interpretarse con cautela.",
              "The separation is weak: the groups should be interpreted with caution.")))
  }

  # MANOVA y ANOVA del canónico
  bartlett <- hoja_de(hojas, "Bartlett")
  if (is.data.frame(bartlett) && is.numeric(bartlett$p) && nrow(bartlett)) {
    frases <- c(frases, if (any(bartlett$p < 0.05)) {
      tx("La prueba de Wilks indica diferencias multivariantes entre grupos (p < 0,05).",
         "Wilks' test indicates multivariate differences between groups (p < 0.05).")
    } else {
      tx("La prueba de Wilks no indica diferencias multivariantes entre grupos (p ≥ 0,05).",
         "Wilks' test does not indicate multivariate differences between groups (p ≥ 0.05).")
    })
  }
  anova <- hoja_de(hojas, "ANOVA")
  if (is.data.frame(anova) && is.numeric(anova$p) && "Variable" %in% names(anova)) {
    sig <- unique(as.character(anova$Variable[anova$p < 0.05]))
    frases <- c(frases, if (length(sig)) {
      sprintf(tx("Variables con diferencias univariantes (ANOVA, p < 0,05): %s.",
                 "Variables with univariate differences (ANOVA, p < 0.05): %s."), paste(sig, collapse = ", "))
    } else {
      tx("Ninguna variable muestra diferencias univariantes (ANOVA, p < 0,05).",
         "No variable shows univariate differences (ANOVA, p < 0.05).")
    })
  }

  # Indeterminación del neutrosófico
  v <- columna_con_nombres(hoja_de(hojas, "Variables"), "Media de I")
  if (!is.null(v) && length(v)) {
    v <- sort(v, decreasing = TRUE)
    frases <- c(frases, sprintf(tx("La variable con más indeterminación media es %s (I = %s).",
                                   "The variable with the highest mean indeterminacy is %s (I = %s)."),
                                names(v)[1], n3(v[1])))
  }

  if (length(frases)) {
    frases <- c(frases, tx(
      "Esta interpretación se genera automáticamente a partir de las tablas: es descriptiva y debe revisarse con el criterio del investigador.",
      "This interpretation is generated automatically from the tables: it is descriptive and should be reviewed with the researcher's judgment."))
  }
  frases
}
