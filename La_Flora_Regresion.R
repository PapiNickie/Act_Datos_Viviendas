# =====================================================================
# Precio de la vivienda en La Flora (Cali): regresión lineal simple y múltiple
# Código R de La_Flora_Regresion.Rmd (los mismos chunks, sin el texto)
# =====================================================================
#
# Antes de correr:
#  1. Deja Datos_Vivienda.xlsx en la misma carpeta que este script.
#  2. En RStudio: Session > Set Working Directory > To Source File Location.
#  3. Corre todo con Ctrl+Shift+Enter (Source with Echo) o por partes con Ctrl+Enter.
#     El botón "Source" a secas corre el código pero no muestra los resultados.
#  Los gráficos base salen en la pestaña Plots y los de plotly en Viewer.

# =====================================================================
# 1. PREPARACIÓN DE LOS DATOS ----
# =====================================================================

# 1.1 Librerías ----

# Si falta alguna librería, instálala una sola vez con:
# install.packages(c("readxl", "dplyr", "ggplot2", "plotly", "lmtest", "car"))

library(readxl)   # leer el archivo de Excel
library(dplyr)    # filtrar, transformar y resumir datos
library(ggplot2)  # gráficos
library(plotly)   # gráficos interactivos
library(lmtest)   # pruebas de supuestos: Breusch-Pagan, Durbin-Watson y RESET
library(car)      # VIF (multicolinealidad) y errores estándar robustos

# 1.2 Carga de la base ----

datos <- read_excel("Datos_Vivienda.xlsx",
    col_types = c("text", "text", "text",
        "numeric", "numeric", "numeric",
        "numeric", "numeric", "text", "text",
        "numeric", "numeric"),
    na = c("", "NA"))   # en el Excel, los faltantes vienen escritos como "NA"

# Se fijan los nombres de las 12 columnas (en el mismo orden del archivo) para que
# el código no dependa de mayúsculas o tildes. Si tu archivo ya trae estos nombres,
# esta línea no cambia nada.
names(datos) <- c("Zona", "piso", "Estrato", "precio_millon", "Area_contruida",
                  "parqueaderos", "Banos", "Habitaciones", "Tipo", "Barrio",
                  "cordenada_longitud", "Cordenada_latitud")

# Identificador único por fila, como en el ejemplo de clase
datos$id <- 1:nrow(datos)

dim(datos)

# 1.3 Unificación del nombre del barrio ----

# Versión estandarizada del nombre: minúsculas y sin espacios sobrantes
datos$barrio_std <- tolower(trimws(datos$Barrio))

# Todas las formas en que aparece "flora" en la base
table(datos$Barrio[grepl("flora", datos$barrio_std)])

es_flora <- grepl("flora", datos$barrio_std) & !grepl("industrial|floralia", datos$barrio_std)
table(datos$Zona[es_flora])

datos_sub <- datos %>%
  filter(grepl("flora", barrio_std),
         !grepl("industrial|floralia", barrio_std),
         Zona == "Zona Norte") %>%
  mutate(Barrio  = "La Flora",
         Estrato = as.numeric(Estrato),          # viene como texto desde el Excel
         Tipo    = factor(Tipo, levels = c("Apartamento", "Casa")))

nrow(datos_sub)
table(datos_sub$Tipo)

# 1.4 Depuración ----

datos_sub %>%
  filter(Habitaciones == 0 | Banos == 0) %>%
  select(id, Tipo, precio_millon, Area_contruida, Habitaciones, Banos)

datos_sub <- datos_sub %>% filter(Habitaciones > 0, Banos > 0)
nrow(datos_sub)

# Valores faltantes por variable
colSums(is.na(datos_sub[, c("precio_millon", "Area_contruida", "Habitaciones",
                            "Banos", "parqueaderos", "Estrato", "Tipo", "piso")]))

# 1.5 Mirada exploratoria ----

# Resumen por tipo de vivienda y total del barrio
resumen <- datos_sub %>%
  group_by(Tipo) %>%
  summarise(ofertas = n(),
            precio_promedio = round(mean(precio_millon), 1),
            precio_mediano  = median(precio_millon),
            area_promedio   = round(mean(Area_contruida), 1),
            habitaciones    = round(mean(Habitaciones), 1),
            banos           = round(mean(Banos), 1),
            precio_m2       = round(median(precio_millon / Area_contruida), 2))

total <- datos_sub %>%
  summarise(ofertas = n(),
            precio_promedio = round(mean(precio_millon), 1),
            precio_mediano  = median(precio_millon),
            area_promedio   = round(mean(Area_contruida), 1),
            habitaciones    = round(mean(Habitaciones), 1),
            banos           = round(mean(Banos), 1),
            precio_m2       = round(median(precio_millon / Area_contruida), 2)) %>%
  mutate(Tipo = "Total La Flora")

rbind(as.data.frame(resumen), as.data.frame(total))

# Distribución del estrato
table(datos_sub$Estrato)

# Correlaciones entre el precio y las variables cuantitativas
cor_tabla <- round(cor(datos_sub[, c("precio_millon", "Area_contruida", "Habitaciones",
                                     "Banos", "parqueaderos", "Estrato")],
                       use = "complete.obs"), 2)
cor_tabla

ggplotly(
  ggplot(datos_sub, aes(x = precio_millon)) +
    geom_histogram(binwidth = 50, fill = "#2a78d6", color = "white", alpha = 0.9) +
    labs(title = "Figura 1. Distribución del precio de la vivienda en La Flora",
         x = "Precio (millones de pesos)", y = "Número de ofertas") +
    theme_minimal()
)

ggplotly(
  ggplot(datos_sub, aes(x = Tipo, y = precio_millon, fill = Tipo)) +
    geom_boxplot(alpha = 0.8, outlier.colour = "black") +
    scale_fill_manual(values = c("Apartamento" = "#2a78d6", "Casa" = "#eb6834")) +
    labs(title = "Figura 2. Precio según el tipo de vivienda",
         x = "", y = "Precio (millones de pesos)") +
    theme_minimal()
)

ggplotly(
  ggplot(datos_sub, aes(x = Area_contruida, y = precio_millon, color = Tipo)) +
    geom_point(alpha = 0.7, size = 2) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
    scale_color_manual(values = c("Apartamento" = "#2a78d6", "Casa" = "#eb6834")) +
    labs(title = paste("Figura 3. Precio vs. área construida  |  r =",
                       round(cor(datos_sub$precio_millon, datos_sub$Area_contruida), 2)),
         x = "Área construida (m²)", y = "Precio (millones de pesos)") +
    theme_minimal()
)

# 1.6 Funciones de apoyo para validar supuestos ----

# 0) Formato de números y p-valores (para las tablas y para el texto)
num  <- function(x, d = 2) formatC(as.numeric(x), format = "f", digits = d)
pct  <- function(b, d = 1) formatC((exp(as.numeric(b)) - 1) * 100, format = "f", digits = d)
sgn  <- function(x, d = 3) ifelse(x < 0, paste("-", num(abs(x), d)), paste("+", num(x, d)))
pnum <- function(p) {
  if (is.character(p)) return(p)
  ifelse(p < 0.001, "< 0.001", formatC(as.numeric(p), format = "f", digits = 3))
}
pval <- function(p) { s <- pnum(p); ifelse(grepl("<", s), s, paste("=", s)) }

# 1) Las cuatro pruebas de supuestos en una sola tabla
validar_supuestos <- function(modelo) {
  pruebas <- list(shapiro.test(residuals(modelo)),
                  bptest(modelo),
                  dwtest(modelo),
                  resettest(modelo, power = 2:3))
  p   <- sapply(pruebas, function(x) x$p.value)
  est <- sapply(pruebas, function(x) unname(x$statistic))
  data.frame(Supuesto    = c("Normalidad de los residuos", "Varianza constante",
                             "Independencia de los errores", "Forma funcional lineal"),
             Prueba      = c("Shapiro-Wilk", "Breusch-Pagan", "Durbin-Watson", "RESET de Ramsey"),
             Estadistico = round(est, 3),
             p_valor     = pnum(p),
             Resultado   = ifelse(p > 0.05, "Se cumple", "No se cumple"))
}

# 2) MAE en la escala original (millones), aunque el modelo esté en logaritmos.
#    Así se pueden comparar modelos con y sin log en la misma unidad.
mae <- function(modelo) {
  y_log <- grepl("^log\\(", deparse(formula(modelo)[[2]]))
  real  <- model.response(model.frame(modelo))
  pred  <- fitted(modelo)
  if (y_log) { real <- exp(real); pred <- exp(pred) }
  mean(abs(real - pred))
}

# 3) Tabla comparativa de varios modelos
comparar_modelos <- function(modelos) {
  filas <- lapply(names(modelos), function(nombre) {
    m <- modelos[[nombre]]
    s <- validar_supuestos(m)
    data.frame(Modelo         = nombre,
               R2_ajustado    = round(summary(m)$adj.r.squared, 3),
               MAE            = round(mae(m), 1),
               p_Shapiro      = s$p_valor[1],
               p_BreuschPagan = s$p_valor[2],
               p_DurbinWatson = s$p_valor[3],
               p_RESET        = s$p_valor[4],
               Cook_max       = round(max(cooks.distance(m)), 3))
  })
  tabla <- do.call(rbind, filas)
  rownames(tabla) <- NULL
  tabla
}

# 4) Las cuatro especificaciones (con y sin logaritmo) de un modelo simple
comparar_log <- function(variable, datos) {
  formulas <- c("Lin-Lin" = paste0("precio_millon ~ ", variable),
                "Log-Lin" = paste0("log(precio_millon) ~ ", variable),
                "Lin-Log" = paste0("precio_millon ~ log(", variable, ")"),
                "Log-Log" = paste0("log(precio_millon) ~ log(", variable, ")"))
  modelos <- lapply(formulas, function(f) lm(as.formula(f), data = datos))
  comparar_modelos(modelos)
}

# =====================================================================
# 2. MODELACIÓN SIMPLE ----
# =====================================================================

# 2.1 Modelo simple 1: precio ~ área construida ----

# -- Estimación

mod1 <- lm(precio_millon ~ Area_contruida, data = datos_sub)
summary(mod1)
b1 <- coef(mod1)

# -- Validación de supuestos

par(mfrow = c(2, 2))
plot(mod1)
par(mfrow = c(1, 1))

sup1 <- validar_supuestos(mod1)
sup1

# La observación más influyente del modelo
cook1 <- cooks.distance(mod1)
datos_sub[which.max(cook1), c("id", "Tipo", "precio_millon", "Area_contruida",
                              "Habitaciones", "Banos", "Estrato")]
max(cook1)

# -- ¿Conviene una transformación logarítmica?

cmp1 <- comparar_log("Area_contruida", datos_sub)
cmp1

mod1_log <- lm(log(precio_millon) ~ log(Area_contruida), data = datos_sub)
summary(mod1_log)

par(mfrow = c(2, 2))
plot(mod1_log)
par(mfrow = c(1, 1))

sup1_log <- validar_supuestos(mod1_log)
sup1_log
b1l <- coef(mod1_log)

# 2.2 Modelo simple 2: precio ~ baños ----

# -- Estimación

mod2 <- lm(precio_millon ~ Banos, data = datos_sub)
summary(mod2)
b2 <- coef(mod2)

# -- Validación de supuestos

par(mfrow = c(2, 2))
plot(mod2)
par(mfrow = c(1, 1))

sup2 <- validar_supuestos(mod2)
sup2

# -- ¿Conviene una transformación logarítmica?

cmp2 <- comparar_log("Banos", datos_sub)
cmp2

mod2_log <- lm(log(precio_millon) ~ Banos, data = datos_sub)
summary(mod2_log)

par(mfrow = c(2, 2))
plot(mod2_log)
par(mfrow = c(1, 1))

sup2_log <- validar_supuestos(mod2_log)
sup2_log
b2l <- coef(mod2_log)

# 2.3 Modelo simple 3: precio ~ habitaciones ----

# -- Estimación

mod3 <- lm(precio_millon ~ Habitaciones, data = datos_sub)
summary(mod3)
b3 <- coef(mod3)

# -- Validación de supuestos

par(mfrow = c(2, 2))
plot(mod3)
par(mfrow = c(1, 1))

sup3 <- validar_supuestos(mod3)
sup3

# -- ¿Conviene una transformación logarítmica?

cmp3 <- comparar_log("Habitaciones", datos_sub)
cmp3

mod3_log <- lm(log(precio_millon) ~ Habitaciones, data = datos_sub)
summary(mod3_log)

par(mfrow = c(2, 2))
plot(mod3_log)
par(mfrow = c(1, 1))

sup3_log <- validar_supuestos(mod3_log)
sup3_log
b3l <- coef(mod3_log)

# 2.4 Resumen de la modelación simple ----

data.frame(
  Modelo         = c("1. Área construida", "2. Baños", "3. Habitaciones"),
  Especificacion = c("Log-Log", "Log-Lin", "Log-Lin"),
  R2_ajustado    = round(c(summary(mod1_log)$adj.r.squared, summary(mod2_log)$adj.r.squared,
                           summary(mod3_log)$adj.r.squared), 3),
  MAE_millones   = round(c(mae(mod1_log), mae(mod2_log), mae(mod3_log)), 1)
)

# =====================================================================
# 3. MODELACIÓN MÚLTIPLE ----
# =====================================================================

# 3.1 Base común para los modelos múltiples ----

datos_mod <- datos_sub %>%
  select(id, precio_millon, Area_contruida, Habitaciones, Banos, parqueaderos, Estrato, Tipo) %>%
  na.omit()

nrow(datos_mod)
table(datos_mod$Tipo)

# 3.2 Modelo múltiple 1: tamaño y distribución ----

# -- Estimación

mm1 <- lm(precio_millon ~ Area_contruida + Habitaciones + Banos + parqueaderos, data = datos_mod)
summary(mm1)
c1 <- coef(mm1)
p1 <- summary(mm1)$coefficients[, 4]

# -- Validación de supuestos

par(mfrow = c(2, 2))
plot(mm1)
par(mfrow = c(1, 1))

sup_mm1 <- validar_supuestos(mm1)
sup_mm1

# Multicolinealidad: un VIF mayor a 5 sería preocupante
vif(mm1)

# Observación más influyente
cook_mm1 <- cooks.distance(mm1)
datos_mod[which.max(cook_mm1), ]
max(cook_mm1)

# -- Validación de la transformación logarítmica

mm1_log <- lm(log(precio_millon) ~ log(Area_contruida) + Habitaciones + Banos + parqueaderos,
              data = datos_mod)
summary(mm1_log)

par(mfrow = c(2, 2))
plot(mm1_log)
par(mfrow = c(1, 1))

validar_supuestos(mm1_log)
comparar_modelos(list("MM1 lineal" = mm1, "MM1 log" = mm1_log))
c1l <- coef(mm1_log)
p1l <- summary(mm1_log)$coefficients[, 4]

# 3.3 Modelo múltiple 2: con variables dummy ----

# Codificación de la dummy: Apartamento es la base (fila de ceros)
contrasts(datos_mod$Tipo)

# -- Estimación

mm2 <- lm(precio_millon ~ Area_contruida + Banos + parqueaderos + Tipo + Estrato, data = datos_mod)
summary(mm2)
c2 <- coef(mm2)
p2 <- summary(mm2)$coefficients[, 4]

# -- Validación de supuestos

par(mfrow = c(2, 2))
plot(mm2)
par(mfrow = c(1, 1))

sup_mm2 <- validar_supuestos(mm2)
sup_mm2
vif(mm2)

# -- Validación de la transformación logarítmica

mm2_log <- lm(log(precio_millon) ~ log(Area_contruida) + Banos + parqueaderos + Tipo + Estrato,
              data = datos_mod)
summary(mm2_log)

par(mfrow = c(2, 2))
plot(mm2_log)
par(mfrow = c(1, 1))

validar_supuestos(mm2_log)
comparar_modelos(list("MM2 lineal" = mm2, "MM2 log" = mm2_log))
c2l <- coef(mm2_log)
p2l <- summary(mm2_log)$coefficients[, 4]

# 3.4 Selección de variables paso a paso (stepwise) ----

modelo_completo <- lm(log(precio_millon) ~ log(Area_contruida) + Habitaciones + Banos +
                        parqueaderos + Tipo + Estrato, data = datos_mod)

modelo_step <- step(modelo_completo, direction = "both", trace = 1)

# Resumen de los pasos realizados
modelo_step$anova

# Comprobación: la selección hacia adelante, partiendo del modelo vacío
modelo_nulo <- lm(log(precio_millon) ~ 1, data = datos_mod)
modelo_fwd  <- step(modelo_nulo, scope = formula(modelo_completo), direction = "forward", trace = 0)
formula(modelo_fwd)

summary(modelo_step)
cs <- coef(modelo_step)
ps <- summary(modelo_step)$coefficients[, 4]

# -- Validación de supuestos del modelo seleccionado

par(mfrow = c(2, 2))
plot(modelo_step)
par(mfrow = c(1, 1))

sup_step <- validar_supuestos(modelo_step)
sup_step
vif(modelo_step)

# Errores estándar robustos (HC3): corrigen la inferencia ante heterocedasticidad
rob <- coeftest(modelo_step, vcov. = hccm(modelo_step, type = "hc3"))
rob

# 3.5 Predicción hipotética ----

nuevas <- data.frame(
  Tipo           = factor(c("Apartamento", "Casa"), levels = levels(datos_mod$Tipo)),
  Area_contruida = c(100, 200),
  Habitaciones   = c(3, 4),
  Banos          = c(2, 3),
  parqueaderos   = c(1, 2),
  Estrato        = c(5, 5))

pred_conf <- exp(predict(modelo_step, newdata = nuevas, interval = "confidence", level = 0.95))
pred_pred <- exp(predict(modelo_step, newdata = nuevas, interval = "prediction", level = 0.95))

data.frame(nuevas,
           Precio_estimado = round(pred_pred[, "fit"], 1),
           IC95_media_inf  = round(pred_conf[, "lwr"], 1),
           IC95_media_sup  = round(pred_conf[, "upr"], 1),
           IP95_inf        = round(pred_pred[, "lwr"], 1),
           IP95_sup        = round(pred_pred[, "upr"], 1))

# 3.6 Métricas de evaluación: MAE ----

# Referencia: el mejor modelo simple, reestimado en la misma base
ref_simple <- lm(log(precio_millon) ~ log(Area_contruida), data = datos_mod)

modelos_finales <- list("Simple Log-Log (área)" = ref_simple,
                        "MM1 lineal"            = mm1,
                        "MM1 log"               = mm1_log,
                        "MM2 lineal (dummy)"    = mm2,
                        "MM2 log (dummy)"       = mm2_log,
                        "Stepwise (log)"        = modelo_step)

# MAE fuera de muestra con validación cruzada de 10 pliegues
set.seed(123)
k <- 10
pliegues <- sample(rep(1:k, length.out = nrow(datos_mod)))

mae_cv <- function(modelo) {
  errores <- c()
  for (i in 1:k) {
    entrenamiento <- datos_mod[pliegues != i, ]
    prueba        <- datos_mod[pliegues == i, ]
    m    <- lm(formula(modelo), data = entrenamiento)
    pred <- predict(m, newdata = prueba)
    if (grepl("^log\\(", deparse(formula(modelo)[[2]]))) pred <- exp(pred)
    errores <- c(errores, abs(prueba$precio_millon - pred))
  }
  mean(errores)
}

tabla_mae <- data.frame(
  Modelo            = names(modelos_finales),
  R2_ajustado       = sapply(modelos_finales, function(m) round(summary(m)$adj.r.squared, 3)),
  MAE_dentro        = sapply(modelos_finales, function(m) round(mae(m), 1)),
  MAE_val_cruzada   = sapply(modelos_finales, function(m) round(mae_cv(m), 1)))
tabla_mae$MAE_relativo_pct <- round(100 * sapply(modelos_finales, mae) / mean(datos_mod$precio_millon), 1)
rownames(tabla_mae) <- NULL
tabla_mae

ggplotly(
  ggplot(data.frame(real = datos_mod$precio_millon, estimado = exp(fitted(modelo_step)),
                    Tipo = datos_mod$Tipo),
         aes(x = estimado, y = real, color = Tipo)) +
    geom_point(alpha = 0.7, size = 2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
    scale_color_manual(values = c("Apartamento" = "#2a78d6", "Casa" = "#eb6834")) +
    labs(title = "Figura 4. Precio real vs. estimado (modelo stepwise)",
         x = "Precio estimado (millones)", y = "Precio real (millones)") +
    theme_minimal()
)
