# =============================================================================
# The Compositional HJ-Biplot-A New Approach to Identifying the Links
# among Bioactive Compounds of Tomatoes
# Marcos Hernández Suárez, Daniel Molina Pérez, Elena M. Rodríguez-Rodríguez
# Carlos Díaz romero, Francisco Espinosa Borreguero, Purificación Galindo-Villardón
#
#
# Matriz de entrada:
# n filas, p columnas
# 1. Datos >0 (estrictamente positivos)
# Hay opcion de reemplazar los ceros
# Transformación:
# 2. Aplicación de transformación CLR (centered log-ratio)
# clr_i_j = ln(x_i_j / gm(x_i)) = ln(x_i_j) - (i/p)*(sum(x_i))
# 3. Procedimiento del HJ Biplot sobre matriz transformada X_clr
# Centrado por columna
# Descomposición SVD: X_dc = U sigma V'
# Marcadores HJ:
# Filas: J = U sigma
# Columnas: H = V sigma
#
# Geometría:
# Rayos de las variables: Longitud al cuadrado estima la var(ln(X_i/gm(X)))
# Links (Enlaces): segmentos que unen los rayos de las variables
# Longitud al cuadrado estima la varianza: var(ln(X_i/X_j))
#
# Cálculo de Matriz de Variación (Aitchison)
# Triángulo Superior: var(ln(X_i/X_j))
# Triángulo Inferior: mean(ln(X_i/X_j))
# clr-varianzas y varianza total
# =============================================================================

#' Comprobar si los datos son composicionales o válidos para análisis composicional
#'
#' @param X data.frame o matriz numérica.
#' @param tol tolerancia para suma constante.
#' @return lógico indicando si los valores no son estrictamente positivos
#' o una lista con dos campos: es_composicional y suma_constante.
#' @export
es_datos_composicionales <- function(X, tol = 1e-2) {
  X <- as.matrix(X)
  if (any(X < 0, na.rm=TRUE) || ncol(X) < 2) return(FALSE)
  # Si suman constante o son partes de una composición
  s <- rowSums(X, na.rm=TRUE)
  es_constante <- all(is.finite(s)) && stats::sd(s) <= tol * max(1, mean(s))
  list(es_composicional = all(X > 0, na.rm=TRUE), suma_constante = es_constante)
}

#' Reemplazo multiplicativo de ceros para datos composicionales
#'
#' @param X matriz o data.frame con valores no negativos.
#' @param delta fracción del valor mínimo positivo para sustituir ceros (por defecto 0.65).
#' @return matriz con ceros reemplazados manteniendo la suma relativa de las partes.
#' @export
reemplazar_ceros_composicional <- function(X, delta = 0.65) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  tot <- rowSums(X)
  for (j in seq_len(ncol(X))) {
    z <- X[, j] == 0 & !is.na(X[, j])
    if (any(z)) {
      pos <- X[X[, j] > 0 & !is.na(X[, j]), j]
      min_val <- if (length(pos)) min(pos) else 1e-5
      X[z, j] <- delta * min_val
    }
  }
  # Re escalar por fila para que sume el total o 1
  if (any(tot > 0)) {
    s_new <- rowSums(X)
    s_new[s_new == 0] <- 1
    X <- (X / s_new) * ifelse(tot > 0, tot, 1)
  }
  
  X
}
#' Reemplazo aditivo de ceros (Método Aitchison 1986)
#'
#' @param X matriz o data.frame con valores no negativos.
#' @param delta fracción del valor mínimo positivo.
#' @return matriz con ceros reemplazados manteniendo la suma relativa de las partes.
#' @export
reemplazar_ceros_aditivo <- function(X, delta=0.65) {
  min_delta <- 1e-5
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow()
  D <- ncol(X)
  
  # Umbral basado en el mínimo
  pos_vals <- X[X > 0 & !is.na(X)]
  min_val <- if(length(pos_vals)) min(pos_vals) else min_delta
  detal_val <- delta * min_val
  
  # Reemplazo de ceros
  # Z es el número de ceros (por fila)
  # D es el número de columnas (partes de la composición)
  for (i in seq_len(n)){
    x_i <- X[i, ]
    z_idx <- (x_i == 0 & !is.na(x_i))
    Z <- sum(z_idx)
    
    if(Z > 0 && Z > D) {
      # Para posiciones con ceros: [delta * (Z+1)*(D-2)] / D^2
      # Para posiciones con valores no cero: x_j - [delta * (Z+1)*Z] / D^2
      term_cero <- detla_val * (Z + 1) * (D - Z) / (D^2)
      term_no_cero <- delta_val * (Z + 1) * Z / (D^2)
      
      x_i[z_idx] <- term_cero
      x_i[!z_idx] <- x_i[!z_idx] - term_no_cero
    } else if (Z == D) {
      # Si todo es cero se asigna un valor
      x_i[!is.na(x_i)] <- delta_val / D
    }
    X[i,] <- x_i
  }
  
  X
}


#' Transformación Centered Log-Ratio (clr)
#'
#' @param X matriz o data.frame con datos positivos.
#' @return matriz transformada clr, donde cada fila tiene suma y media cero.
#' @export
transformar_clr <- function(X) {
  X <- as.matrix(X)
  if (any(X <= 0, na.rm = TRUE)) {
    stop("La transformación clr requiere valores estrictamente mayores que cero.", call. = FALSE)
  }
  logX <- log(X)
  sweep(logX, 1, rowMeans(logX), "-")
}

#' Matriz de variación (Variation Array de Aitchison)
#'
#' Calcula la matriz de variación clásica de Aitchison: el triángulo superior
#' contiene las varianzas de los log-ratios var(ln(Xi/Xj)), el triángulo inferior
#' las medias de los log-ratios mean(ln(Xi/Xj)), y el vector de clr-varianzas.
#'
#' @param X data.frame o matriz de composiciones positivas.
#' @return lista con `variacion` (matriz cuadrada D x D), `clr_varianzas` y `varianza_total`.
#' @export
matriz_variacion <- function(X) {
  X <- as.matrix(X)
  if (any(X <= 0, na.rm = TRUE)) {
    X <- reemplazar_ceros_composicional(X)
  }
  p <- ncol(X)
  nombres <- colnames(X) %||% paste0("V", seq_len(p))
  logX <- log(X)
  
  M_var <- matrix(0, p, p, dimnames = list(nombres, nombres))
  M_mean <- matrix(0, p, p, dimnames = list(nombres, nombres))
  
  for (i in seq_len(p)) {
    for (j in seq_len(p)) {
      lr <- logX[, i] - logX[, j]
      if (i < j) {
        M_var[i, j] <- stats::var(lr)
      } else if (i > j) {
        M_mean[i, j] <- mean(lr)
      }
    }
  }
  
  # Matriz combinada estilo Aitchison (Tabla 3 del paper)
  array_comb <- M_var + M_mean
  
  # clr-varianzas: var(ln(Xi / gm(X)))
  clrX <- transformar_clr(X)
  clr_var <- apply(clrX, 2, stats::var)
  
  # Varianza total de Aitchison: suma de todas las varianzas log-ratio / (2*D) = sum(clr_var)
  tot_var <- sum(clr_var)
  
  list(
    array = array_comb,
    varianzas_logratio = M_var,
    medias_logratio = M_mean,
    clr_varianzas = clr_var,
    varianza_total = tot_var
  )
}

#' Cálculo del HJ-Biplot Composicional
#'
#' @param datos data.frame o matriz con las partes composicionales.
#' @param variables nombres de las variables activas (numéricas, partes de la composición).
#' @param tipo partición de los valores singulares: `"HJ"` (simultáneo, Galindo-Villardón 1986),
#'   `"GH"` (covarianza / columnas, Gabriel 1971; Aitchison & Greenacre 2002),
#'   o `"JK"` (forma / filas).
#' @param dimensiones número de ejes a calcular (por defecto 3).
#' @param imputar_ceros lógico: si `TRUE`, sustituye ceros por una fracción positiva.
#' @param delta fracción para sustitución de ceros si se activa.
#' @param referencia convenio de signos: nombre de una variable activa que debe
#'   quedar en el lado positivo de todos los ejes.
#' @param invertir_ejes vector de números de ejes a invertir.
#' @return objeto de clase `biplot_composicional` con inercia, coordenadas,
#'   rayos, links, calidad de representación, contribuciones y matriz de variación.
#' @export
calcular_composicional <- function(datos, variables = NULL, tipo = c("HJ", "GH", "JK"),
                                   dimensiones = 3, imputar_ceros = TRUE, delta = 0.65,
                                   referencia = NULL, invertir_ejes = integer()) {
  tipo <- match.arg(tipo)
  datos <- as.data.frame(datos, check.names = FALSE)
  
  if (is.null(variables)) {
    variables <- names(datos)[vapply(datos, is.numeric, logical(1))]
  }
  faltan <- setdiff(variables, names(datos))
  if (length(faltan)) {
    stop("No existen en los datos: ", paste(faltan, collapse = ", "), call. = FALSE)
  }
  if (length(variables) < 2) {
    stop("Selecciona al menos 2 variables composicionales.", call. = FALSE)
  }
  no_num <- variables[!vapply(datos[variables], is.numeric, logical(1))]
  if (length(no_num)) {
    stop("Estas variables no son numéricas: ", paste(no_num, collapse = ", "), call. = FALSE)
  }
  
  X0 <- as.matrix(datos[, variables, drop = FALSE])
  storage.mode(X0) <- "double"
  if (anyNA(X0)) {
    filas_na <- rownames(datos)[!stats::complete.cases(X0)]
    stop("Hay ", sum(is.na(X0)), " datos faltantes en las variables activas (filas: ",
         paste(utils::head(filas_na, 8), collapse = ", "),
         if (length(filas_na) > 8) ", ..." else "", ").", call. = FALSE)
  }
  if (nrow(X0) < 3) stop("Se necesitan al menos 3 individuos.", call. = FALSE)
  rownames(X0) <- make.unique(rownames(datos) %||% as.character(seq_len(nrow(X0))))
  
  if (any(X0 < 0)) {
    stop("Los datos composicionales no pueden tener valores negativos.", call. = FALSE)
  }
  if (any(X0 == 0)) {
    if (isTRUE(imputar_ceros)) {
      X0 <- reemplazar_ceros_composicional(X0, delta = delta)
    } else {
      stop("Hay valores cero en los datos. Activa la opción de imputar ceros o selecciona variables estrictamente positivas.",
           call. = FALSE)
    }
  }
  
  # 1. Matriz de variación de Aitchison
  var_arr <- matriz_variacion(X0)
  
  # 2. Transformación clr
  clrX <- transformar_clr(X0)
  
  # 3. Doble centrado: clrX - rowMeans(clrX) - colMeans(clrX) + mean(clrX)
  # Como rowMeans(clrX) es 0 y mean(clrX) es 0, equivale a centrar columnas de clrX.
  col_means <- colMeans(clrX)
  X_dc <- sweep(clrX, 2, col_means, "-")
  dimnames(X_dc) <- dimnames(X0)
  
  # 4. Descomposición en valores singulares (SVD)
  s <- svd(X_dc)
  tol <- max(dim(X_dc)) * max(s$d) * .Machine$double.eps
  r <- sum(s$d > tol)
  if (r < 2) stop("La matriz tiene rango menor que 2; no se puede construir el biplot.", call. = FALSE)
  
  d <- s$d[seq_len(r)]
  U <- s$u[, seq_len(r), drop = FALSE]
  V <- s$v[, seq_len(r), drop = FALSE]
  ejes <- paste("Eje", seq_len(r))
  
  # Orientación / Signos
  signos <- rep(1, r)
  if (!is.null(referencia) && nzchar(referencia)) {
    j <- match(referencia, colnames(X_dc))
    if (is.na(j)) stop("La variable de referencia debe ser una variable activa.", call. = FALSE)
    signos <- ifelse(V[j, ] < 0, -1, 1)
  }
  invertir_ejes <- as.integer(invertir_ejes)
  invertir_ejes <- invertir_ejes[invertir_ejes >= 1 & invertir_ejes <= r]
  signos[invertir_ejes] <- -signos[invertir_ejes]
  U <- sweep(U, 2, signos, "*")
  V <- sweep(V, 2, signos, "*")
  
  # 5. Marcadores según tipo de biplot
  D <- diag(d, nrow = r)
  filas <- switch(tipo, HJ = U %*% D, GH = U, JK = U %*% D)
  columnas <- switch(tipo, HJ = V %*% D, GH = V %*% D, JK = V)
  dimnames(filas) <- list(rownames(X_dc), ejes)
  dimnames(columnas) <- list(colnames(X_dc), ejes)
  
  # Calidad de representación (coseno al cuadrado)
  cos2 <- function(M) {
    tot <- rowSums(M^2)
    out <- M^2 / ifelse(tot > 0, tot, 1)
    out[tot == 0, ] <- 0
    out
  }
  calidad_filas <- cos2(filas)
  calidad_columnas <- cos2(columnas)
  
  # Contribuciones
  contrib_filas <- U^2
  contrib_columnas <- V^2
  dimnames(contrib_filas) <- dimnames(filas)
  dimnames(contrib_columnas) <- dimnames(columnas)
  
  # Inercia
  autovalores <- d^2
  inercia <- data.frame(
    Eje = seq_len(r),
    `Valor propio` = autovalores,
    `Valor singular` = d,
    `% Inercia` = 100 * autovalores / sum(autovalores),
    `% Acumulado` = cumsum(100 * autovalores / sum(autovalores)),
    check.names = FALSE
  )
  
  # 6. Tabla de Enlaces (Links) entre partes composicionales
  # Para cada par (i, j), longitud del enlace en el espacio completo y en el plano 1-2
  p_vars <- colnames(X_dc)
  p_len <- length(p_vars)
  pares <- utils::combn(p_vars, 2, simplify = FALSE)
  n_pares <- length(pares)
  
  n_obs <- nrow(X0)
  # Factor de escala para que la distancia coincida con la desviación estándar muestral:
  # h_cov = V D / sqrt(n - 1)
  factor_sd <- 1 / sqrt(n_obs - 1)
  
  links_df <- data.frame(
    Parte_1 = character(n_pares),
    Parte_2 = character(n_pares),
    `Var ln(Xi/Xj)` = numeric(n_pares),
    `SD log-ratio` = numeric(n_pares),
    `Longitud plano` = numeric(n_pares),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  for (idx in seq_along(pares)) {
    v1 <- pares[[idx]][1]
    v2 <- pares[[idx]][2]
    var_logratio <- var_arr$varianzas_logratio[v1, v2]
    sd_logratio <- sqrt(var_logratio)
    
    # Coordenadas de los vértices en el plano 1-2
    c1 <- columnas[v1, 1:2] * factor_sd
    c2 <- columnas[v2, 1:2] * factor_sd
    long_plano <- sqrt(sum((c1 - c2)^2))
    
    links_df[idx, "Parte_1"] <- v1
    links_df[idx, "Parte_2"] <- v2
    links_df[idx, "Var ln(Xi/Xj)"] <- round(var_logratio, 4)
    links_df[idx, "SD log-ratio"] <- round(sd_logratio, 4)
    links_df[idx, "Longitud plano"] <- round(long_plano, 4)
  }
  
  k <- max(2, min(as.integer(dimensiones), r))
  recorta <- function(M) M[, seq_len(k), drop = FALSE]
  
  structure(
    list(
      tipo = tipo,
      metodo = "HJ Composicional",
      dimensiones = k,
      rango = r,
      referencia = referencia,
      signos = signos,
      inercia = inercia,
      filas = recorta(filas),
      columnas = recorta(columnas),
      calidad_filas = recorta(calidad_filas),
      calidad_columnas = recorta(calidad_columnas),
      contrib_filas = recorta(contrib_filas),
      contrib_columnas = recorta(contrib_columnas),
      var_array = var_arr,
      links = links_df,
      factor_sd = factor_sd,
      original = X_dc,
      X_clr = clrX,
      X_dc = X_dc,
      X_orig = X0
    ),
    class = c("biplot_composicional", "biplot_hj")
  )
}


#' Gráfico de un biplot composicional
#'
#' Sigue la estructura y diseño de [grafico_biplot()], incorporando el trazado
#' de los rayos desde el centroide (origen) y los enlaces (links) entre las partes
#' de la composición con el sello gráfico del paquete.
#'
#' @param res objeto devuelto por [calcular_composicional()].
#' @param ejes los dos ejes a representar (por defecto c(1, 2)).
#' @param grupos vector (uno por individuo) para colorear los puntos, opcional.
#' @param etiquetas_filas,etiquetas_columnas mostrar los nombres de filas y columnas.
#' @param calidad_min_filas,calidad_min_columnas calidad mínima en el plano
#'   (CRFE acumulada, de 0 a 1000) para mostrar un individuo o una variable.
#' @param mal_representados `"atenuar"` (gris, sin etiqueta) u `"ocultar"`.
#' @param calidad_min (compatibilidad) umbral común en porcentaje.
#' @param escala_vectores multiplicador visual de los vectores (rayos). Con `"auto"`
#'   o 1 se muestra la escala directa de la partición elegida.
#' @param invertir vector lógico de longitud 2 para reflejar cada eje solo en el gráfico.
#' @param mostrar `"ambos"`, `"filas"` o `"columnas"`.
#' @param colores_grupos colores de los grupos (con nombre = nivel).
#' @param color_filas color de los individuos cuando no hay grupos.
#' @param color_vectores color de los vectores (rayos).
#' @param colores_variables colores por variable (con nombre), p. ej. por bloques.
#' @param envolventes dibujar la envolvente convexa de cada grupo.
#' @param tam_texto,tam_puntos tamaños de texto y puntos.
#' @param titulo título opcional.
#' @param destacados vector lógico (uno por individuo): pinta solo esos puntos.
#' @param resto qué hacer con los demás: `"atenuar"` (gris, sin nombre) u `"ocultar"`.
#' @param mostrar_links lógico: si `TRUE`, dibuja los segmentos de enlace entre vértices.
#' @param enlaces_a_mostrar lista o vector de pares para dibujar sólo enlaces seleccionados,
#'   o `"todos"` para todos los pares, o `NULL`/`"ninguno"` para ocultar.
#' @param max_links número máximo de enlaces a dibujar si son muchos (para evitar sobrecarga visual).
#' @param orden_links orden de selección de los enlaces cuando se limitan con `max_links`:
#'   `"cortos"` (por defecto, enlaces más cortos / menor varianza, relaciones más fuertes)
#'   o `"largos"` (enlaces más largos / mayor varianza, mayor independencia relativa).
#' @param mostrar_rayos lógico: si `TRUE`, dibuja los vectores desde el origen.
#' @param color_links color de las líneas de enlace (por defecto `COLOR_SUAVE`).
#' @return objeto `ggplot` con el biplot composicional y atributo `"aspecto"`.
#' @export
grafico_composicional <- function(res, 
                                  ejes=c(1, 2), 
                                  grupos=NULL,
                                  etiquetas_filas = TRUE, etiquetas_columnas = TRUE,
                                  calidad_min_filas = 0, calidad_min_columnas = 0,
                                  mal_representados = c("atenuar", "ocultar"),
                                  calidad_min = NULL, escala_vectores = "auto",
                                  invertir = c(FALSE, FALSE),
                                  mostrar = c("ambos", "filas", "columnas"),
                                  colores_grupos = NULL, color_filas = COLOR_FILAS,
                                  color_vectores = COLOR_VECTOR, colores_variables = NULL,
                                  envolventes = FALSE, tam_texto = 3.2, tam_puntos = 2.2,
                                  titulo = NULL, destacados = NULL,
                                  resto = c("atenuar", "ocultar"),
                                  mostrar_links = TRUE, enlaces_a_mostrar = "todos",
                                  max_links = 15, orden_links = c("cortos", "largos"),
                                  mostrar_rayos = TRUE,
                                  color_links = COLOR_SUAVE) {
  mostrar <- match.arg(mostrar)
  mal_representados <- match.arg(mal_representados)
  resto <- match.arg(resto)
  orden_links <- match.arg(orden_links)
  if (!is.null(calidad_min)) {
    calidad_min_filas <- calidad_min_columnas <- 10 * calidad_min
  }
  ejes <- as.integer(ejes)
  if (any(ejes > ncol(res$filas))) {
    stop("Solo se calcularon ", ncol(res$filas), " ejes.", call. = FALSE)
  }
  signo <- ifelse(invertir, -1, 1)
  
  F <- sweep(res$filas[, ejes, drop = FALSE], 2, signo, "*")
  G <- sweep(res$columnas[, ejes, drop = FALSE], 2, signo, "*")
  
  # --- Escala visual -----------------------------------------------------------
  scf <- 1
  if (res$tipo != "HJ") {
    sca <- sum(res$filas^2) / nrow(res$filas)
    scb <- sum(res$columnas^2) / nrow(res$columnas)
    scf <- sqrt(sqrt(scb / sca))
    F <- F * scf
    G <- G / scf
  }
  f_usuario <- if (identical(escala_vectores, "auto")) 1 else as.numeric(escala_vectores)
  G <- G * f_usuario
  
  # --- Calidad en el plano (CRFE acumulada, 0-1000) -----------------------------
  q_f <- 1000 * (res$calidad_filas[, ejes[1]] + res$calidad_filas[, ejes[2]])
  q_c <- 1000 * (res$calidad_columnas[, ejes[1]] + res$calidad_columnas[, ejes[2]])
  
  df_f <- data.frame(x = F[, 1], y = F[, 2], etiqueta = rownames(F),
                     ok = q_f >= calidad_min_filas, stringsAsFactors = FALSE)
  df_c <- data.frame(x = G[, 1], y = G[, 2], etiqueta = rownames(G),
                     ok = q_c >= calidad_min_columnas, stringsAsFactors = FALSE)
  df_c$color <- color_vectores
  if (length(colores_variables)) {
    m <- match(df_c$etiqueta, names(colores_variables))
    df_c$color[!is.na(m)] <- colores_variables[m[!is.na(m)]]
  }
  
  # --- Grupos --------------------------------------------------------------------
  hay_grupos <- !is.null(grupos) && length(grupos) == nrow(df_f)
  if (hay_grupos) {
    g <- as.character(grupos)
    g[is.na(g) | g == ""] <- "(sin dato)"
    niveles <- if (is.factor(grupos)) intersect(levels(grupos), unique(g)) else
      sort(unique(g))
    niveles <- c(niveles, setdiff(unique(g), niveles))
    df_f$grupo <- factor(g, levels = niveles)
    pal <- colores_por_defecto(niveles)
    if (length(colores_grupos)) {
      if (is.null(names(colores_grupos))) names(colores_grupos) <- niveles[seq_along(colores_grupos)]
      comunes <- intersect(names(colores_grupos), niveles)
      pal[comunes] <- colores_grupos[comunes]
    }
    formas <- stats::setNames(rep_len(FORMAS_GRUPOS, length(niveles)), niveles)
  }
  
  inr <- res$inercia
  lab_eje <- function(k) titulo_eje_sello(k, inr[k, 4])
  
  p <- ggplot2::ggplot() +
    ejes_cero()
  
  visibles_f <- if (mal_representados == "ocultar") df_f[df_f$ok, , drop = FALSE] else df_f
  visibles_c <- if (mal_representados == "ocultar") df_c[df_c$ok, , drop = FALSE] else df_c
  
  # --- Enlaces (Links) entre partes composicionales -------------------------------
  if (mostrar_links && mostrar != "filas" && nrow(df_c) >= 2) {
    p_vars <- df_c$etiqueta[df_c$ok]
    if (length(p_vars) >= 2) {
      pares <- utils::combn(p_vars, 2, simplify = FALSE)
      if (is.character(enlaces_a_mostrar) && length(enlaces_a_mostrar) == 1 && enlaces_a_mostrar == "todos") {
        if (length(pares) > max_links && !is.null(res$links)) {
          pares_ordenados <- res$links[res$links$Parte_1 %in% p_vars & res$links$Parte_2 %in% p_vars, , drop = FALSE]
          decreasing <- identical(orden_links, "largos")
          pares_ordenados <- pares_ordenados[order(pares_ordenados$`Var ln(Xi/Xj)`, decreasing = decreasing), , drop = FALSE]
          n_sel <- min(max_links, nrow(pares_ordenados))
          pares_sel <- lapply(seq_len(n_sel), function(k) {
            c(pares_ordenados$Parte_1[k], pares_ordenados$Parte_2[k])
          })
        } else {
          pares_sel <- pares
        }
      } else if (is.list(enlaces_a_mostrar)) {
        pares_sel <- enlaces_a_mostrar
      } else {
        pares_sel <- list()
      }
      
      if (length(pares_sel) > 0) {
        coords_map <- split(df_c[, c("x", "y")], df_c$etiqueta)
        seg_list <- list()
        for (par in pares_sel) {
          v1 <- par[1]; v2 <- par[2]
          if (v1 %in% names(coords_map) && v2 %in% names(coords_map)) {
            seg_list[[length(seg_list) + 1]] <- data.frame(
              x = coords_map[[v1]]$x, y = coords_map[[v1]]$y,
              xend = coords_map[[v2]]$x, yend = coords_map[[v2]]$y
            )
          }
        }
        if (length(seg_list) > 0) {
          df_seg <- do.call(rbind, seg_list)
          p <- p + ggplot2::geom_segment(
            data = df_seg,
            ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
            colour = color_links, linetype = "dashed", linewidth = 0.5, alpha = 0.8
          )
        }
      }
    }
  }
  
  # --- Filas (individuos) ----------------------------------------------------------
  if (mostrar != "columnas") {
    malas <- df_f[!df_f$ok, , drop = FALSE]
    buenas <- df_f[df_f$ok, , drop = FALSE]
    if (length(destacados) == nrow(df_f)) {
      sel <- destacados[df_f$ok] %in% TRUE
      if (resto == "atenuar" && !all(sel)) {
        p <- p + ggplot2::geom_point(data = buenas[!sel, , drop = FALSE], ggplot2::aes(.data$x, .data$y),
                                     colour = COLOR_ATENUADO, size = tam_puntos * 0.8)
      }
      buenas <- buenas[sel, , drop = FALSE]
      malas <- malas[destacados[!df_f$ok] %in% TRUE, , drop = FALSE]
      if (hay_grupos) buenas$grupo <- droplevels(buenas$grupo)
    }
    if (hay_grupos && envolventes) {
      cascos <- do.call(rbind, lapply(split(buenas, buenas$grupo), function(d) {
        if (nrow(d) < 3) return(NULL)
        d[grDevices::chull(d$x, d$y), ]
      }))
      if (!is.null(cascos) && nrow(cascos)) {
        p <- p + ggplot2::geom_polygon(
          data = cascos, ggplot2::aes(.data$x, .data$y, fill = .data$grupo,
                                      colour = .data$grupo, group = .data$grupo),
          alpha = 0.08, linewidth = 0.4, show.legend = FALSE) +
          ggplot2::scale_fill_manual(values = pal, drop = FALSE, guide = "none")
      }
    }
    if (nrow(malas) && mal_representados == "atenuar") {
      p <- p + ggplot2::geom_point(data = malas, ggplot2::aes(.data$x, .data$y),
                                   colour = COLOR_ATENUADO, size = tam_puntos * 0.8)
    }
    if (hay_grupos) {
      p <- p +
        ggplot2::geom_point(data = buenas,
                            ggplot2::aes(.data$x, .data$y, colour = .data$grupo,
                                         shape = .data$grupo), size = tam_puntos + 0.2) +
        ggplot2::scale_colour_manual(values = pal, name = NULL, drop = FALSE) +
        ggplot2::scale_shape_manual(values = formas, name = NULL, drop = FALSE)
    } else {
      p <- p + ggplot2::geom_point(data = buenas, ggplot2::aes(.data$x, .data$y),
                                   colour = color_filas, size = tam_puntos)
    }
    if (etiquetas_filas && nrow(buenas)) {
      p <- p + etiqueta_punto(buenas, ggplot2::aes(.data$x, .data$y, label = .data$etiqueta),
                              size = tam_texto * 0.9)
    }
  }
  
  # --- Columnas (variables / rayos) ------------------------------------------------
  if (mostrar != "filas") {
    flecha <- flecha_sello()
    malas_c <- df_c[!df_c$ok, , drop = FALSE]
    buenas_c <- df_c[df_c$ok, , drop = FALSE]
    
    if (mostrar_rayos) {
      if (nrow(malas_c) && mal_representados == "atenuar") {
        p <- p + ggplot2::geom_segment(
          data = malas_c, ggplot2::aes(x = 0, y = 0, xend = .data$x, yend = .data$y),
          colour = COLOR_ATENUADO, linewidth = 0.5, arrow = flecha, linetype = "22")
      }
      if (nrow(buenas_c)) {
        p <- p + ggplot2::geom_segment(
          data = buenas_c, ggplot2::aes(x = 0, y = 0, xend = .data$x, yend = .data$y),
          colour = buenas_c$color, linewidth = 0.6, arrow = flecha)
      }
    } else {
      if (nrow(malas_c) && mal_representados == "atenuar") {
        p <- p + ggplot2::geom_point(data = malas_c, ggplot2::aes(.data$x, .data$y),
                                     colour = COLOR_ATENUADO, shape = 18, size = tam_puntos + 0.8)
      }
      if (nrow(buenas_c)) {
        p <- p + ggplot2::geom_point(data = buenas_c, ggplot2::aes(.data$x, .data$y),
                                     colour = buenas_c$color, shape = 18, size = tam_puntos + 1)
      }
    }
    
    etiq_c <- visibles_c
    if (etiquetas_columnas && nrow(etiq_c)) {
      col_txt <- ifelse(etiq_c$ok,
                        ifelse(etiq_c$color == color_vectores, COLOR_TINTA, etiq_c$color),
                        COLOR_SUAVE)
      p <- p + etiqueta_vector(etiq_c, ggplot2::aes(.data$x, .data$y, label = .data$etiqueta),
                               size = tam_texto, colour = col_txt)
    }
  }
  
  # --- Encuadre rectangular (Ecuaciones_encuadre_biplot, ecuaciones 1 y 2) ---------
  todos <- rbind(if (mostrar != "columnas") visibles_f[, c("x", "y")],
                 if (mostrar != "filas") visibles_c[, c("x", "y")])
  if (is.null(todos) || !nrow(todos)) todos <- data.frame(x = c(-1, 1), y = c(-1, 1))
  con_origen <- mostrar != "filas"
  lim <- limites_encuadre(todos$x, todos$y, incluir_origen = con_origen)
  
  subt <- sprintf("%s-Biplot Composicional (clr)%s", res$tipo,
                  if (abs(f_usuario - 1) > 1e-8) sprintf(" - vectores ×%s",
                                                         format(signif(f_usuario, 3), decimal.mark = ",")) else "")
  
  p <- p +
    ggplot2::coord_equal(xlim = lim$x, ylim = lim$y, expand = FALSE) +
    ggplot2::labs(x = lab_eje(ejes[1]), y = lab_eje(ejes[2]), title = titulo,
                  subtitle = subt) +
    tema_sello()
  
  attr(p, "aspecto") <- diff(lim$y) / diff(lim$x)
  p
}
