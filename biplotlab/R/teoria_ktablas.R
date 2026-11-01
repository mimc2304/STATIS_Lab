# =============================================================================
# Contenido de la pestaña «Fundamentos» (una entrada por método). Para agregar
# el de un método nuevo, copia una entrada completa y cambia sus textos; la
# prueba tests/testthat/test-teoria.R comprueba que no falte ninguna sección.
# =============================================================================

REF <- list(
  abdi2012 = "Abdi, H., Williams, L. J., Valentin, D. y Bennani-Dosse, M. (2012). STATIS and DISTATIS: optimum multitable principal component analysis and three way metric multidimensional scaling. <i>WIREs Computational Statistics, 4</i>(2), 124-167. https://doi.org/10.1002/wics.198",
  doledec1994 = "Dolédec, S. y Chessel, D. (1994). Co-inertia analysis: an alternative method for studying species-environment relationships. <i>Freshwater Biology, 31</i>(3), 277-294. https://doi.org/10.1111/j.1365-2427.1994.tb01741.x",
  dray2003 = "Dray, S., Chessel, D. y Thioulouse, J. (2003). Co-inertia analysis and the linking of ecological data tables. <i>Ecology, 84</i>(11), 3078-3089. https://doi.org/10.1890/03-0178",
  dray2007 = "Dray, S. y Dufour, A. B. (2007). The ade4 package: implementing the duality diagram for ecologists. <i>Journal of Statistical Software, 22</i>(4), 1-20. https://doi.org/10.18637/jss.v022.i04",
  gonzalez2021 = "González-Narváez, M., Fernández-Gómez, M. J., Mendes, S., Molina, J. L., Ruiz-Barzola, O. y Galindo-Villardón, P. (2021). Study of temporal variations in species-environment association through an innovative multivariate method: MixSTATICO. <i>Sustainability, 13</i>(11), 5924. https://doi.org/10.3390/su13115924",
  robert1976 = "Robert, P. y Escoufier, Y. (1976). A unifying tool for linear multivariate statistical methods: the RV-coefficient. <i>Applied Statistics, 25</i>(3), 257-265. https://doi.org/10.2307/2347233",
  simier1999 = "Simier, M., Blanc, L., Pellegrin, F. y Nandris, D. (1999). Approche simultanée de K couples de tableaux: application à l'étude des relations pathologie végétale-environnement. <i>Revue de Statistique Appliquée, 47</i>, 31-46.",
  thioulouse2004 = "Thioulouse, J., Simier, M. y Chessel, D. (2004). Simultaneous analysis of a sequence of paired ecological tables. <i>Ecology, 85</i>(1), 272-283. https://doi.org/10.1890/02-0605",
  thioulouse2011 = "Thioulouse, J. (2011). Simultaneous analysis of a sequence of paired ecological tables: a comparison of several methods. <i>The Annals of Applied Statistics, 5</i>(4), 2300-2325. https://doi.org/10.1214/10-AOAS372"
)

# Fórmulas comunes del PTA (STATICO y CoSTATIS)
FORMULAS_PTA <- c(
  "<b>Interestructura.</b> Cada tabla \\(A_k\\) se compara con las demás con el producto escalar de Hilbert-Schmidt y su versión normada, el coeficiente RV: \\[ C_{kl}=\\operatorname{tr}(A_k^{\\top}A_l),\\qquad RV_{kl}=\\frac{C_{kl}}{\\sqrt{C_{kk}C_{ll}}}. \\] La diagonalización de la matriz RV da el mapa de las ocasiones.",
  "<b>Pesos y compromiso.</b> Los pesos \\(\\alpha_k\\) son los elementos del primer vector propio de la matriz RV; por el teorema de Perron-Frobenius todos tienen el mismo signo. El compromiso combina las tablas sin normalizarlas: \\[ A_{+}=\\sum_k \\alpha_k A_k \\] y es la tabla más próxima, en conjunto, a todas las ocasiones. La calidad con que representa a cada ocasión es su \\(\\cos^2\\)."
)

TEORIA_KTABLAS <- list(

  # ------------------------------------------------------------ Co-inercia ----
  coinercia = list(
    titulo = "Co-inercia",
    subtitulo = "Co-estructura entre dos tablas medidas sobre los mismos individuos (Dolédec y Chessel, 1994)",
    resumen = list(
      para_que = "Saber si dos grupos de variables medidos sobre los mismos individuos (por ejemplo, ambiente y especies) varían juntos, y en qué sentido.",
      entrada = "Dos tablas cuantitativas con los mismos individuos en las filas: X (p variables) e Y (q variables).",
      no_usar = "Individuos distintos en las dos tablas, datos faltantes o una secuencia de ocasiones (para eso, STATICO o CoSTATIS).",
      lectura = "Cada individuo es una flecha de su posición según X a su posición según Y: flecha corta, co-estructura fuerte. Variables de X (flechas) y de Y (puntos) en la misma dirección se asocian directamente."),
    para_que = c(
      "La Co-inercia relaciona dos tablas medidas sobre los mismos individuos. Busca los ejes en los que las coordenadas de los individuos según X y según Y tienen la <b>máxima covarianza</b>: la co-estructura. A diferencia del análisis canónico, no necesita más individuos que variables ni invertir matrices de covarianza, por lo que admite tablas con muchas variables (como las de especies).",
      "Un test de permutación dice si la co-estructura es mayor que la esperada por azar. Es la base de STATICO y CoSTATIS, que la extienden a una secuencia de pares de tablas."),
    entrada = c(
      "Dos grupos de variables cuantitativas medidos sobre los mismos individuos, sin datos faltantes.",
      "Cada tabla con su preparación: ACP estandarizado (unidades distintas), ACP centrado (misma unidad) o AFC (abundancias o frecuencias). Si una tabla usa AFC, la otra toma sus pesos de filas.",
      "Al menos 3 individuos y 2 variables por tabla."),
    no_usar = c(
      "Tablas con individuos distintos o en otro orden: cada fila debe ser el mismo individuo en X y en Y.",
      "Una secuencia de ocasiones (tiempos, estaciones): una Co-inercia por ocasión no da una estructura común; para eso están STATICO y CoSTATIS.",
      "Concluir causalidad: la co-inercia mide asociación."),
    calculos = c(
      "<b>1. Preparación.</b> Cada tabla es un triplete (tabla, pesos de columnas, pesos de filas): \\((X, Q_X, D)\\) y \\((Y, Q_Y, D)\\), con los mismos pesos \\(D\\) para los individuos.",
      "<b>2. Tabla cruzada.</b> \\[ Z = Y^{\\top} D X \\qquad (q \\times p), \\] que reúne las covarianzas entre las variables de Y y las de X.",
      "<b>3. Ejes de co-inercia.</b> La descomposición de \\(Z\\) da los vectores \\(u_k\\) (en X) y \\(v_k\\) (en Y) que maximizan \\[ \\operatorname{cov}^2(XQ_Xu_k,\\,YQ_Yv_k)=\\operatorname{corr}^2\\cdot\\operatorname{var}(XQ_Xu_k)\\cdot\\operatorname{var}(YQ_Yv_k). \\] El valor propio \\(\\lambda_k\\) es esa co-inercia. Por eso el método equilibra la correlación entre las tablas con la inercia propia de cada una.",
      "<b>4. Diagnósticos.</b> La correlación entre las coordenadas según X y según Y en cada eje, y la razón entre la inercia que recogen los ejes de co-inercia y la máxima posible en cada tabla (la de su propio análisis).",
      "<b>5. Coeficiente RV</b> (Robert y Escoufier, 1976), con \\(W_X=XQ_XX^{\\top}\\): \\[ RV=\\frac{\\operatorname{tr}(W_XDW_YD)}{\\sqrt{\\operatorname{tr}\\big((W_XD)^2\\big)\\operatorname{tr}\\big((W_YD)^2\\big)}}\\in[0,1]. \\]",
      "<b>6. Test de permutación.</b> Se permutan las filas de una tabla \\(B\\) veces y se recalcula el RV. H0: no existe co-estructura. El p-valor es la proporción de RV simulados mayores o iguales que el observado."),
    lectura = c(
      "<b>Co-estructura</b>: el círculo es la posición del individuo según X y la punta, según Y (coordenadas normadas). Una flecha corta indica que X e Y describen igual a ese individuo (co-estructura fuerte); una larga, que lo describen de forma distinta.",
      "<b>Variables</b>: las de X como flechas y las de Y como puntos en el mismo plano. Una variable de Y en la dirección de una flecha de X se asocia directamente con ella; en dirección opuesta, inversamente.",
      "<b>Test</b>: si el RV observado queda en la cola derecha de los simulados (p < 0,05), hay co-estructura. Con pocos individuos el test tiene poca potencia.",
      "El signo de los ejes es arbitrario: voltear el gráfico no cambia ninguna conclusión."),
    donde = c(
      "<code>R/preparar_ktabla.R</code>: <code>preparar_par_tablas()</code> (ACP o AFC de cada tabla con <code>ade4</code>).",
      "<code>R/calculo_coinercia.R</code>: <code>coinercia()</code> (<code>ade4::coinertia()</code> y <code>randtest()</code>) y la lectura guiada.",
      "<code>R/diagnosticos_ktabla.R</code>: descomposición de la co-inercia, RV y longitud de las flechas; <code>R/grafico_ktabla.R</code>: los gráficos.",
      "Validado con <code>ade4::coinertia()</code>, con las prácticas del módulo (ríos del Doubs: eje 1 = 88,36 %, RV = 0,4506; Banco Mundial: RV = 0,516, p = 0,213) y con el cálculo algebraico directo de la tabla cruzada y del RV, en <code>tests/testthat/test-coinercia.R</code> y <code>test-validacion-modulo4.R</code>."),
    referencias = list(
      modulo = c(REF$doledec1994),
      original = c(REF$dray2003, REF$robert1976, REF$dray2007))),

  # --------------------------------------------------------------- STATICO ----
  statico = list(
    titulo = "STATICO",
    subtitulo = "STATIS sobre la co-estructura de una secuencia de pares de tablas (Simier et al., 1999)",
    resumen = list(
      para_que = "Encontrar la parte estable de la relación entre dos grupos de variables (X e Y) a lo largo de varias ocasiones, y ver en qué ocasiones esa relación se cumple mejor.",
      entrada = "Pares de K tablas: en cada ocasión, X e Y sobre los mismos individuos; siempre las mismas variables. Los individuos pueden cambiar de una ocasión a otra.",
      no_usar = "Una sola ocasión (usar Co-inercia) o variables distintas en cada ocasión.",
      lectura = "Interestructura: ocasiones con flechas cercanas tienen co-estructuras parecidas. Compromiso: variables de X y de Y en la misma dirección se asocian de forma estable. Trayectorias: cómo se ubica cada individuo en cada ocasión."),
    para_que = c(
      "STATICO combina la Co-inercia con el Análisis Triádico Parcial (PTA o X-STATIS). Primero calcula, en cada ocasión, la tabla cruzada que resume la co-estructura entre X e Y; después aplica un PTA a esa secuencia de tablas cruzadas.",
      "El resultado es la <b>parte estable</b> de la relación X-Y (el compromiso), el grado en que cada ocasión comparte esa relación (pesos y cos²) y la posición de los individuos y variables de cada ocasión respecto a ella. Es el método de la secuencia ambiente-especies de Thioulouse et al. (2004) y la base de MixSTATICO (González-Narváez et al., 2021)."),
    entrada = c(
      "Hoja en formato largo: una fila por individuo y ocasión, con una columna que indique la ocasión.",
      "Las mismas variables de X y de Y en todas las ocasiones; en cada ocasión X e Y describen a los mismos individuos (la misma fila).",
      "Los individuos pueden ser distintos en cada ocasión; si se repiten y se indica su columna, las trayectorias los unen.",
      "Al menos 2 ocasiones y 3 individuos por ocasión, sin datos faltantes."),
    no_usar = c(
      "Una sola ocasión: es una Co-inercia.",
      "Variables que cambian de una ocasión a otra: las tablas cruzadas deben tener el mismo tamaño.",
      "Datos mixtos (cualitativos y cuantitativos): para ellos se propuso MixSTATICO, que no está implementado aquí."),
    calculos = c(
      "<b>1. Preparación.</b> En tres vías importa con qué se transforma: sin transformar; centrar con la media de cada ocasión o con la global; o estandarizar. Las dos estandarizaciones de Bouroche (<code>withinpca()</code>) centran cada ocasión y dividen por la desviación de la ocasión (parcial) o por una sola desviación común, la de la tabla ya centrada por ocasión (total); la opción global usa la media y la desviación de todas las ocasiones juntas.",
      "<b>2. Co-estructura de cada ocasión.</b> \\[ Z_k = X_k^{\\top} D_k\\, Y_k \\qquad (p \\times q), \\] la tabla cruzada de la Co-inercia de la ocasión \\(k\\).",
      paste("<b>3. PTA de la secuencia</b> \\(Z_1,\\dots,Z_K\\).", FORMULAS_PTA[1]),
      FORMULAS_PTA[2],
      "<b>4. Compromiso.</b> La descomposición de \\(Z_{+}=\\sum_k\\alpha_kZ_k\\) da las coordenadas de las variables de X (filas) y de Y (columnas), ambas escaladas por los valores singulares, como en un HJ-Biplot de la tabla cruzada.",
      "<b>5. Intraestructura.</b> Los individuos de cada ocasión (filas de \\(X_k\\) y de \\(Y_k\\)) y las variables vistas desde cada ocasión se proyectan sobre los ejes del compromiso: son las trayectorias."),
    lectura = c(
      "<b>Interestructura</b>: cada ocasión es una flecha. Ángulo pequeño entre dos ocasiones: co-estructuras parecidas (RV alto). Una ocasión apartada tiene una relación X-Y distinta de las demás.",
      "<b>Pesos y cos²</b>: las ocasiones con más peso construyen el compromiso; las de cos² alto están bien representadas por él. El compromiso no describe bien a una ocasión con cos² bajo.",
      "<b>Compromiso</b>: variables de X (flechas) y de Y (puntos) en la misma dirección están asociadas en forma estable; en direcciones opuestas, inversamente.",
      "<b>Trayectorias</b>: muestran dónde queda cada individuo (o cada variable) en cada ocasión respecto a la estructura común; los saltos grandes señalan ocasiones en que se aparta de ella.",
      "El signo de los ejes es arbitrario: voltear el gráfico no cambia ninguna conclusión."),
    donde = c(
      "<code>R/ktabla.R</code>: <code>par_ktablas()</code> arma las dos series desde la hoja en formato largo y valida los datos.",
      "<code>R/preparar_ktabla.R</code>: <code>preparar_ktabla()</code> (<code>withinpca()</code> o <code>wca()</code> y <code>ktab.within()</code> de ade4).",
      "<code>R/calculo_statico.R</code>: <code>statico()</code> (<code>ktab.match2ktabs()</code> y <code>pta()</code> de ade4, los mismos pasos de <code>ade4::statico()</code> con el número de ejes elegido) y la lectura guiada.",
      "Validado con <code>ade4::statico()</code>, la base <code>meau</code> de Thioulouse (2011), la práctica del fitoplancton del módulo (primer valor propio = 56,928) y el cálculo algebraico directo del RV y del compromiso, en <code>tests/testthat/test-statico.R</code> y <code>test-validacion-modulo4.R</code>."),
    referencias = list(
      modulo = c(REF$abdi2012, REF$gonzalez2021, REF$thioulouse2011),
      original = c(REF$simier1999, REF$thioulouse2004))),

  # -------------------------------------------------------------- CoSTATIS ----
  costatis = list(
    titulo = "CoSTATIS",
    subtitulo = "Co-inercia de los compromisos de dos análisis K-tablas (Thioulouse, 2011)",
    resumen = list(
      para_que = "Relacionar la estructura estable de una serie de tablas X con la estructura estable de una serie de tablas Y medidas sobre los mismos individuos en varias ocasiones.",
      entrada = "Dos series de K tablas con los mismos individuos en todas las ocasiones y las mismas variables en cada serie.",
      no_usar = "Individuos que cambian de una ocasión a otra (usar STATICO) o una sola ocasión (usar Co-inercia).",
      lectura = "Se lee como una Co-inercia de los dos compromisos: flechas cortas, individuos con co-estructura fuerte; variables de X y de Y en la misma dirección, asociadas. Cada interestructura dice cuán estable es cada serie."),
    para_que = c(
      "CoSTATIS invierte el orden de STATICO: primero obtiene la parte estable de cada serie con un PTA (un compromiso de X y otro de Y) y después relaciona esos dos compromisos con una Co-inercia.",
      "Responde a cómo se relacionan las estructuras estables de X y de Y, mientras que STATICO responde a cuán estable es la relación entre X e Y. Thioulouse (2011) compara los dos enfoques con los mismos datos."),
    entrada = c(
      "Hoja en formato largo: una fila por individuo y ocasión, con una columna de ocasión y una de individuos.",
      "Los mismos individuos en todas las ocasiones (el aplicativo los ordena igual en cada una) y las mismas variables en cada serie.",
      "Al menos 2 ocasiones y 3 individuos, sin datos faltantes."),
    no_usar = c(
      "Individuos que no se repiten en todas las ocasiones: cada PTA necesita el cubo completo. En ese caso, STATICO.",
      "Series en las que el compromiso representa mal a varias ocasiones (cos² bajos): la Co-inercia de los compromisos describiría solo una parte de los datos."),
    calculos = c(
      "<b>1. Preparación</b>, con las mismas opciones de tres vías que en STATICO.",
      paste("<b>2. PTA de cada serie</b>, \\(X_1,\\dots,X_K\\) y \\(Y_1,\\dots,Y_K\\).", FORMULAS_PTA[1]),
      FORMULAS_PTA[2],
      "<b>3. Co-inercia de los compromisos.</b> Cada compromiso, \\(X_{+}\\) e \\(Y_{+}\\), entra a un ACP sin centrar y con las columnas normadas a varianza 1, como en <code>ade4::costatis()</code>. Con ellos: tabla cruzada \\(Z=Y_{+}^{\\top}DX_{+}\\), ejes de máxima covarianza y coeficiente RV entre los dos compromisos.",
      "<b>4. Ocasiones suplementarias.</b> Cada \\(X_k\\) e \\(Y_k\\) se proyecta sobre los ejes de co-inercia (coordenadas normadas): son las trayectorias.",
      "<b>5. Test de permutación</b> sobre la Co-inercia de los compromisos (H0: no hay co-estructura)."),
    lectura = c(
      "<b>Interestructura</b> de cada serie: como en STATICO, dice qué ocasiones se parecen y cuáles representa bien su compromiso.",
      "<b>Co-estructura</b>: cada individuo va de su posición según el compromiso de X a su posición según el de Y; flecha corta, co-estructura fuerte.",
      "<b>Variables</b>: las de X como flechas y las de Y como puntos; en la misma dirección, asociadas.",
      "<b>Trayectorias</b>: la posición de cada individuo en cada ocasión sobre los ejes de co-inercia.",
      "El signo de los ejes es arbitrario: voltear el gráfico no cambia ninguna conclusión."),
    donde = c(
      "<code>R/ktabla.R</code>: <code>par_ktablas(mismos_individuos = TRUE)</code> comprueba que todas las ocasiones tengan los mismos individuos y los ordena.",
      "<code>R/calculo_costatis.R</code>: <code>costatis()</code> (<code>pta()</code>, <code>dudi.pca()</code>, <code>coinertia()</code> y <code>randtest()</code> de ade4, los mismos pasos de <code>ade4::costatis()</code> con el número de ejes elegido).",
      "Validado con <code>ade4::costatis()</code>, la base <code>meau</code> (RV = 0,8205), la práctica del fitoplancton del módulo (RV = 0,5375) y el cálculo algebraico directo de los compromisos, en <code>tests/testthat/test-costatis.R</code> y <code>test-validacion-modulo4.R</code>."),
    referencias = list(
      modulo = c(REF$abdi2012, REF$thioulouse2011),
      original = c(REF$doledec1994, REF$thioulouse2004)))
)
