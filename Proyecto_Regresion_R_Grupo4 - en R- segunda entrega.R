# Instalación e importación de librerias

paquetes <- c("tidyverse", "ggplot2", "corrplot", "moments",
              "gridExtra", "scales", "knitr", "reshape2", "GGally")

nuevos <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(nuevos) > 0) install.packages(nuevos, quiet = TRUE)

# Cargar librerías
library(tidyverse)
library(ggplot2)
library(corrplot)
library(moments)    # skewness y kurtosis
library(gridExtra)
library(scales)
library(knitr)
library(reshape2)
library(GGally)

# Opciones globales
options(scipen = 999, digits = 3)
cat("✅ Librerías cargadas correctamente\n")

# Exploración inicial y estructura de datos

# Carga directa
df <- read.csv("Train real state.csv", stringsAsFactors = TRUE)

# Eliminar columna índice si existe
df$X <- NULL

cat("Dimensiones del dataset:", nrow(df), "filas x", ncol(df), "columnas\n")
head(df)

# Tipos de datos y estructura general
cat("=== ESTRUCTURA DEL DATASET ===\n")
str(df)

# Identificar columnas categóricas y numéricas
cat_cols <- names(df)[sapply(df, is.factor)]
num_cols <- names(df)[sapply(df, is.numeric)]

# Remover el índice ('Unnamed..0' o 'X') y la variable objetivo para no tratarla como un predictor # nolint
num_cols_feat <- num_cols[!num_cols %in% c("SalePrice")]

cat("\nVariables categóricas (", length(cat_cols), "):\n")
print(cat_cols)

cat("\nVariables numéricas sin target (", length(num_cols_feat), "):\n")
print(num_cols_feat)

# Calidad y limpieza de datos

# === Valores faltantes y duplicados ===
missing_df <- data.frame(
  Variable    = names(df),
  Faltantes   = colSums(is.na(df)),
  Porcentaje  = round(colSums(is.na(df)) / nrow(df) * 100, 2)
)

missing_df <- missing_df[missing_df$Faltantes > 0, ]
missing_df <- missing_df[order(-missing_df$Porcentaje), ]

if (nrow(missing_df) == 0) {
  cat("No hay valores faltantes en el dataset\n")
} else {
  print(missing_df)
}

# === DUPLICADOS ===
n_dup <- sum(duplicated(df))
cat("Filas duplicadas exactas:", n_dup, "\n")

# Ver los grupos de duplicados, ordenados para comparar pares
dup_view <- df[duplicated(df) | duplicated(df, fromLast = TRUE), ]
dup_view <- dup_view[order(dup_view$SalePrice, dup_view$Size.sqf., dup_view$Floor), ]
head(dup_view[, c("SalePrice","YearBuilt","Size.sqf.","Floor","SubwayStation")], 20)

# Eliminar duplicados exactos
df <- df[!duplicated(df), ]

cat("Dimensiones tras eliminar duplicados:", nrow(df), "filas x", ncol(df), "columnas\n")

# Estadísticas descriptivas, distribución y outliers - variables numéricas

# Estadisticas descriptivas para variables numéricas
vars_num <- df[, c(num_cols_feat, "SalePrice")]

tabla_desc <- data.frame(
  COLUMNA = names(vars_num),
  CONTEO  = sapply(vars_num, function(x) sum(!is.na(x))),
  MEDIA   = round(sapply(vars_num, mean,   na.rm = TRUE), 2),
  MEDIANA = round(sapply(vars_num, median, na.rm = TRUE), 2),
  STD     = round(sapply(vars_num, sd,     na.rm = TRUE), 2),
  MIN     = sapply(vars_num, min, na.rm = TRUE),
  Q25     = sapply(vars_num, quantile, probs = 0.25, na.rm = TRUE),
  Q50     = sapply(vars_num, quantile, probs = 0.50, na.rm = TRUE),
  Q75     = sapply(vars_num, quantile, probs = 0.75, na.rm = TRUE),
  MAX     = sapply(vars_num, max, na.rm = TRUE),
  stringsAsFactors = FALSE
)
rownames(tabla_desc) <- NULL

# Mostrar en el notebook
library(knitr)
kable(tabla_desc, col.names = c("COLUMNA","CONTEO","MEDIA","MEDIANA","STD",
                                "MIN","25%","50%","75%","MAX"))
# Distribución de variables numéricas 
library(ggplot2)
library(reshape2)

# Convertir a formato largo para graficar todas juntas
df_long <- melt(df[, num_cols_feat])

ggplot(df_long, aes(x = value)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white", alpha = 0.85) +
  facet_wrap(~ variable, scales = "free", ncol = 5) +
  theme_minimal(base_size = 8) +
  labs(title = "Distribución de variables numéricas", x = "", y = "Frecuencia") +
  theme(strip.text = element_text(size = 7),
        axis.text = element_text(size = 6))
ggsave("distribucion_numericas.png", width = 14, height = 12, dpi = 150)

# Boxplots para detectar outliers
library(ggplot2)
library(reshape2)

df_long <- melt(df[, num_cols_feat])

ggplot(df_long, aes(y = value)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7,
               outlier.color = "coral", outlier.size = 0.8) +
  facet_wrap(~ variable, scales = "free", ncol = 5) +
  theme_minimal(base_size = 8) +
  labs(title = "Boxplots — variables numéricas", x = "", y = "") +
  theme(strip.text = element_text(size = 7),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 6))
ggsave("boxplots_numericas.png", width = 14, height = 12, dpi = 150)

# Estadísticas descriptivas y frecuencia - variables categóricas

# Estadísticas descriptivas para variables categóricas
tabla_cat <- data.frame(
  COLUMNA    = cat_cols,
  CONTEO     = sapply(cat_cols, function(c) sum(!is.na(df[[c]]))),
  CATEGORIAS = sapply(cat_cols, function(c) nlevels(df[[c]])),
  TOP        = sapply(cat_cols, function(c) names(which.max(table(df[[c]])))),
  FRECUENCIA = sapply(cat_cols, function(c) max(table(df[[c]]))),
  PORCENTAJE = sapply(cat_cols, function(c) round(max(table(df[[c]])) / sum(!is.na(df[[c]])) * 100, 2))
)
rownames(tabla_cat) <- NULL

library(knitr)
kable(tabla_cat, align = "lrrlrr",
      col.names = c("COLUMNA","CONTEO","CATEGORÍAS","TOP","FRECUENCIA","% TOP"))

write.csv2(tabla_cat, "estadisticas_categoricas.csv", row.names = FALSE)

# Variables categóricas: frecuencia y boxplot de SalePrice
library(ggplot2)
library(gridExtra)

plots <- list()

for (col in cat_cols) {
  # Gráfico de frecuencia
  p1 <- ggplot(df, aes(x = .data[[col]])) +
    geom_bar(fill = "steelblue", alpha = 0.85) +
    theme_minimal(base_size = 8) +
    labs(title = paste("Frecuencia:", col), x = "", y = "Conteo") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 6))

  # Boxplot SalePrice por categoría
  p2 <- ggplot(df, aes(x = .data[[col]], y = SalePrice)) +
    geom_boxplot(fill = "steelblue", alpha = 0.7,
                 outlier.color = "coral", outlier.size = 0.8) +
    theme_minimal(base_size = 8) +
    labs(title = paste("SalePrice por", col), x = "", y = "SalePrice") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 6))

  plots[[length(plots) + 1]] <- p1
  plots[[length(plots) + 1]] <- p2
}

# Organizar en grilla (2 columnas: frecuencia + boxplot por fila)
grid.arrange(grobs = plots, ncol = 2)

g <- arrangeGrob(grobs = plots, ncol = 2)
ggsave("categoricas_frecuencia.png", g, width = 12, height = 18, dpi = 150)

# Análisis de la variable objetivo (SalePrice)

# === VARIABLE OBJETIVO: SALEPRICE ===
library(ggplot2)
library(gridExtra)
library(scales)

# Histograma
p1 <- ggplot(df, aes(x = SalePrice)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white", alpha = 0.85) +
  scale_x_continuous(labels = comma) +
  theme_minimal(base_size = 10) +
  labs(title = "Distribución de SalePrice", x = "SalePrice (USD)", y = "Frecuencia")

# Boxplot
p2 <- ggplot(df, aes(y = SalePrice)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7,
               outlier.color = "coral", outlier.size = 1) +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 10) +
  labs(title = "Boxplot de SalePrice", y = "SalePrice (USD)") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

grid.arrange(p1, p2, ncol = 2)

# Estadísticas de forma
library(moments)
cat("Media:    ", round(mean(df$SalePrice), 2), "\n")
cat("Mediana:  ", round(median(df$SalePrice), 2), "\n")
cat("Skewness: ", round(skewness(df$SalePrice), 3), "\n")
cat("Kurtosis: ", round(kurtosis(df$SalePrice), 3), "\n")

g <- arrangeGrob(p1, p2, ncol = 2)
ggsave("variable_objetivo.png", g, width = 14, height = 5, dpi = 150)

# Correlación entre variables numéricas y SalePrice

# === MATRIZ DE CORRELACIÓN (VARIABLES NUMÉRICAS) ===
library(corrplot)

# Calcular correlaciones (predictores + target)
corr_matrix <- cor(df[, c(num_cols_feat, "SalePrice")])

# Visualización
corrplot(corr_matrix, method = "color", type = "lower",
         tl.cex = 0.6, tl.col = "black",
         number.cex = 0.45, addCoef.col = "black",
         col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200),
         title = "Matriz de Correlación (variables numéricas)",
         mar = c(0,0,1,0))

png("matriz_correlacion.png", width = 1600, height = 1400, res = 130)
corrplot(corr_matrix, method = "color", type = "lower",
         tl.cex = 0.6, tl.col = "black",
         number.cex = 0.45, addCoef.col = "black",
         col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200),
         mar = c(0,0,1,0))
dev.off()

# Top correlaciones con SalePrice (ordenadas por valor absoluto)
corr_target <- corr_matrix["SalePrice", ]
corr_target <- corr_target[names(corr_target) != "SalePrice"]
corr_target <- sort(corr_target, decreasing = FALSE)  # ascendente para barh

corr_df <- data.frame(
  Variable    = names(corr_target),
  Correlacion = as.numeric(corr_target)
)
corr_df$Color <- ifelse(corr_df$Correlacion >= 0, "positiva", "negativa")

ggplot(corr_df, aes(x = Correlacion,
                    y = reorder(Variable, Correlacion),
                    fill = Color)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("positiva" = "steelblue",
                               "negativa" = "coral")) +
  labs(title = "Correlación de cada variable con SalePrice",
       x = "Correlación de Pearson", y = "") +
  theme_minimal() +
  theme(legend.position = "none")

# Scatter plots - variables más correlacionadas con SalePrice

# Top 8 variables con mayor correlación absoluta
top8 <- names(sort(abs(corr_target), decreasing = TRUE))[1:8]

plots_scatter <- lapply(top8, function(col) {
  r_val <- round(cor(df[[col]], df$SalePrice, use = "complete.obs"), 3)
  ggplot(df, aes_string(x = col, y = "SalePrice")) +
    geom_point(alpha = 0.3, color = "steelblue", size = 0.8) +
    geom_smooth(method = "lm", se = FALSE, color = "coral", linewidth = 1) +
    scale_y_continuous(labels = comma) +
    labs(title = paste0(col, "\nr = ", r_val),
         x = col, y = "SalePrice") +
    theme_minimal(base_size = 9) +
    theme(plot.title = element_text(size = 8, face = "bold"))
})

do.call(grid.arrange, c(plots_scatter, ncol = 4,
  top = "Variables más correlacionadas con SalePrice"))

# Resumen final de la exploración

cat(strrep("=", 60), "\n")
cat("         RESUMEN DE LA EXPLORACIÓN DE DATOS\n")
cat(strrep("=", 60), "\n")
cat(sprintf("  Observaciones:            %d\n",   nrow(df)))
cat(sprintf("  Variables totales:        %d\n",   ncol(df)))
cat(sprintf("  Variables numéricas:      %d\n",   length(num_cols_feat)))
cat(sprintf("  Variables categóricas:    %d\n",   length(cat_cols)))
cat(sprintf("  Columnas con nulos:       %d\n",   sum(colSums(is.na(df)) > 0)))
cat(sprintf("  SalePrice — Media:        $%s\n",  format(round(mean(df$SalePrice), 0), big.mark=",")))
cat(sprintf("  SalePrice — Mediana:      $%s\n",  format(median(df$SalePrice), big.mark=",")))
cat(sprintf("  SalePrice — Std:          $%s\n",  format(round(sd(df$SalePrice), 0), big.mark=",")))
cat(sprintf("  SalePrice — Min:          $%s\n",  format(min(df$SalePrice), big.mark=",")))
cat(sprintf("  SalePrice — Max:          $%s\n",  format(max(df$SalePrice), big.mark=",")))
cat(sprintf("  Asimetría SalePrice:      %.4f\n", skewness(df$SalePrice)))
cat("\n  Top 5 correlaciones con SalePrice:\n")
top5_corr <- sort(abs(corr_target), decreasing = TRUE)[1:5]
for (nm in names(top5_corr)) {
  cat(sprintf("    %-35s r = %.4f\n", nm, corr_matrix["SalePrice", nm]))
}
cat(strrep("=", 60), "\n")

##################################################################################################
#------------------------------------------------------------------------------------------------#
# Entrega 2 - modelo de regresión propuesto, justificación de su elección y análisis de resultados
#------------------------------------------------------------------------------------------------#
##################################################################################################

# ETAPA 1
# Preparación de los datos para el modelado

#=========================================
# Preparación de los datos para el modelado
#=========================================

# Eliminar la columna índice
train <- train %>%
  select(-X)

# Eliminar registros duplicados
train <- train %>%
  distinct()

# Verificar dimensiones
dim(train)

# Verificar valores faltantes
colSums(is.na(train))

# Convertir variables categóricas a factor

variables_factor <- c(
  "HallwayType",
  "HeatingType",
  "AptManageType",
  "TimeToBusStop",
  "TimeToSubway",
  "SubwayStation"
)

train[variables_factor] <-
  lapply(train[variables_factor], as.factor)

# Comprobar estructura

str(train)

# Paquetes a usar

library(tidyverse)
library(car)
library(MASS)
library(lmtest)
library(corrplot)
library(GGally)
library(performance)
library(broom)
library(caret)

# ETAPA 2
# Construcción del modelo de regresión completo
#=========================================
# Modelo lineal múltiple completo
#=========================================

modelo_completo <- lm(SalePrice ~ ., data = train)
summary(modelo_completo)

# Parámetros estimados
length(coef(modelo_completo))

# Tabla de coeficientes

library(broom)

tabla_coeficientes <-
  tidy(modelo_completo)

tabla_coeficientes

# Medidas generales del modelo

resumen <- summary(modelo_completo)

resumen$r.squared

resumen$adj.r.squared

resumen$sigma

resumen$fstatistic

# Comparación entre número de observaciones y parámetros

nrow(train)

length(coef(modelo_completo))

# ETAPA 3
# Diagnóstico de multicolinealidad

#=========================================
# Diagnóstico de multicolinealidad
#=========================================

library(car)

vif_modelo <- vif(modelo_completo)

vif_modelo

# Con variables categóricas
vif_tabla <- data.frame(vif_modelo)

vif_tabla$GVIF_Ajustado <-
    vif_tabla$GVIF^(1/(2*vif_tabla$Df))

vif_tabla

# Ordenar los VIF
sort(vif(modelo_completo),
     decreasing = TRUE)

vif_tabla |>
    arrange(desc(GVIF_Ajustado))

# Visualización

library(ggplot2)

ggplot(vif_tabla,
       aes(x = reorder(rownames(vif_tabla),
                       GVIF_Ajustado),
           y = GVIF_Ajustado)) +

    geom_col() +

    coord_flip() +

    geom_hline(yintercept = 5,
               linetype = 2) +

    geom_hline(yintercept = 10,
               linetype = 2)

# ETAPA 3.5
# Depuración preliminar de variables

modelo_base <- lm(SalePrice ~ YearBuilt + YrSold + MonthSold + Size.sqf. + Floor + N_Parkinglot.Ground. + N_Parkinglot.Basement.
  + N_APT + N_manager + N_elevators + N_FacilitiesNearBy.PublicOffice. + N_FacilitiesNearBy.Hospital. + N_FacilitiesNearBy.Dpartmentstore.
  + N_FacilitiesNearBy.Mall. + N_FacilitiesNearBy.ETC. + N_FacilitiesNearBy.Park. + N_SchoolNearBy.Elementary. + N_SchoolNearBy.Middle. 
  + N_SchoolNearBy.High. + N_SchoolNearBy.University. + N_FacilitiesInApt + HallwayType + HeatingType + AptManageType + TimeToBusStop 
  + TimeToSubway + SubwayStation, data = train)

# ETAPA 4
# Selección del modelo de regresión


#=========================================
# Modelo nulo
#=========================================

modelo_nulo <- lm(
  SalePrice ~ 1,
  data = train
)

# Backward selection

library(MASS)

modelo_backward <- stepAIC(
  modelo_base,
  direction = "backward",
  trace = TRUE
)

# Foward selection

modelo_forward <- stepAIC(
  modelo_nulo,
  scope = list(
    lower = modelo_nulo,
    upper = modelo_base
  ),
  direction = "forward",
  trace = TRUE
)

# Stepwise

modelo_stepwise <- stepAIC(
  modelo_base,
  direction = "both",
  trace = TRUE
)

# Comparación de modelos

comparacion <- data.frame(

Modelo = c(
  "Completo",
  "Backward",
  "Forward",
  "Stepwise"
),

AIC = c(
  AIC(modelo_base),
  AIC(modelo_backward),
  AIC(modelo_forward),
  AIC(modelo_stepwise)
),

BIC = c(
  BIC(modelo_base),
  BIC(modelo_backward),
  BIC(modelo_forward),
  BIC(modelo_stepwise)
),

R2 = c(
  summary(modelo_base)$r.squared,
  summary(modelo_backward)$r.squared,
  summary(modelo_forward)$r.squared,
  summary(modelo_stepwise)$r.squared
),

R2_Ajustado = c(
  summary(modelo_base)$adj.r.squared,
  summary(modelo_backward)$adj.r.squared,
  summary(modelo_forward)$adj.r.squared,
  summary(modelo_stepwise)$adj.r.squared
)

)

comparacion

# ¿Cómo elegir el mejor?

# Criterio	| Lo deseable
# AIC	| Menor
# BIC	| Menor
# R² ajustado	| Mayor
# Número de variables	| Menor
# Interpretabilidad	| Mayor

summary(modelo_stepwise)

# ETAPA 4.5
# Validación del modelo seleccionado

# Suponiendo que el mejor modelo es el stepwise

summary(modelo_stepwise)

# Observar la significacia global del modelo

# Estadístico F
summary(modelo_stepwise)$fstatistic

# Valor p global
pf(
  summary(modelo_stepwise)$fstatistic[1],
  summary(modelo_stepwise)$fstatistic[2],
  summary(modelo_stepwise)$fstatistic[3],
  lower.tail = FALSE
)

# Interpretación
# Si Si el valor-p es menor que 0.05: Se rechaza la hipótesis nula de que todos los coeficientes de 
# regresión son iguales a cero. En consecuencia, existe evidencia estadísticamente significativa de 
# que al menos una de las variables independientes contribuye a explicar el precio de venta de las viviendas.

# Significancia individual de los coeficientes
library(broom)

coeficientes <- tidy(modelo_stepwise)

coeficientes

coeficientes |>
  filter(p.value > 0.05)

# Importancia relativa de las variables
coeficientes |>
  arrange(desc(abs(statistic)))

# Intervalos de confianza

confint(modelo_stepwise)

# Revisar el número de variables

length(coef(modelo_stepwise))

# Comparar el modelo completo con el seleccionado

anova(modelo_base,
      modelo_stepwise)

# Tabla resumen (recomendación)

# |Indicador | Modelo completo	| Modelo final |
# |Variables	|	                |              |
# |AIC		    |                 |              | 
# |BIC		    |                 |              |
# |R²		      |                 |              |
# |R² ajustado	|               |              |
# |Error residual	|             |              |

# ETAPA 5
# Verificación de los supuestos del modelo de regresión

# Análisis del gráfico general

#=========================================
# Diagnóstico gráfico del modelo
#=========================================

par(mfrow = c(2,2))

plot(modelo_stepwise)

par(mfrow = c(1,1))

# Supuesto de linealidad

plot(
    modelo_stepwise$fitted.values,
    resid(modelo_stepwise),

    xlab="Valores ajustados",
    ylab="Residuos"
)

abline(h=0,
       col="red",
       lwd=2)

library(car)

crPlots(modelo_stepwise)

# Normalidad de los residuos

residuos <- residuals(modelo_stepwise)

hist(residuos, breaks=30, probability=TRUE,
      main="Histograma de residuos",
      xlab="Residuos")

lines(density(residuos), lwd=2, col="blue")

qqnorm(residuos)

qqline(residuos, col="red", lwd=2)

# Prueba de normalidad de Shapiro-Wilk

# Ho: Los residuos siguen una distribución normal.
# Ha: Los residuos no siguen una distribución normal.

# Rechazar si p-value < 0.05

shapiro.test(residuos)

# Homocedasticidad

plot(modelo_stepwise)

library(lmtest)

# Prueba de Breusch-Pagan

# Ho: Los residuos tienen varianza constante (homocedasticidad).
# Ha: Los residuos no tienen varianza constante (heterocedasticidad).

# Rechazar si p-value < 0.05

bptest(modelo_stepwise)

# Independencia de los residuos

# Prueba de Durbin-Watson

# Ho: No hay autocorrelación de primer orden en los residuos.
# Ha: Hay autocorrelación de primer orden en los residuos.

dwtest(modelo_stepwise)

# Observaciones influyentes

# Distancia de Cook

cook <- cooks.distance(modelo_stepwise)

plot(cook, type="h",
     main="Distancia de Cook",
     ylab="Distancia de Cook",
     xlab="Índice de observación")

abline(h=4/length(cook),col="red",lwd=2)

# Cook > 4/n es un umbral comúnmente utilizado para
# identificar observaciones influyentes.

# Valores leverage

plot(hatvalues(modelo_stepwise), type="h",
     main="Valores leverage",
     ylab="Leverage",
     xlab="Índice de observación")

# Residuos estudentizados

rstudent(modelo_stepwise)

# Multicolinealidad

vif(modelo_stepwise)

# Esta tabla puede quedar muy bien en el informe pienso yo.

# |Supuesto	|Método	|Resultado| ¿Se cumple?|
# |Linealidad	|Residuos vs Ajustados + CR Plots|		|   |
# |Normalidad	|QQ Plot + Shapiro|		|   |
# |Homocedasticidad	|Breusch-Pagan|		|   |
# |Independencia	|Durbin-Watson|		|   |
# |Multicolinealidad	|VIF	|	    |    |
# |Observaciones influyentes	|Cook + Leverage	|	  |   |

# ETAPA 6
# Validación del desempeño predictivo

#=========================================
# División entrenamiento - prueba
#=========================================

library(caret)

set.seed(123)

indices <- createDataPartition(train$SalePrice, p = 0.80, list = FALSE)

datos_train <- train[indices, ]

datos_test <- train[-indices, ]

# Ajustar el modelo usando únicamente entrenamiento

modelo_final <- lm(

SalePrice ~
# fórmula seleccionada del modelo seleccionada por el equipo
,
data = datos_train
)

# Realizar las predicciones

predicciones <- predict(modelo_final, newdata = datos_test)

# Calcular errores

errores <- datos_test$SalePrice - predicciones

# Calcular métricas de desempeño

MSE <- mean(errores^2)
MSE

RMSE <- sqrt(MSE)
RMSE

MAE <- mean(abs(errores))
MAE

SST <- sum(

(datos_test$SalePrice -
 mean(datos_test$SalePrice))^2

)

SSE <- sum(errores^2)
R2_pred <- 1 - SSE/SST
R2_pred

# Resumen de métricas

metricas <- data.frame(MSE = MSE, RMSE = RMSE, MAE = MAE, 
R2_Prediccion = R2_pred)
metricas

# Gráfico observado vs predicho

plot(datos_test$SalePrice, predicciones, pch = 19, xlab = 
"Precio observado", ylab = "Precio predicho")
abline(0, 1, col = "red", lwd = 2)

# Distribución de errores

hist(errores, breaks = 30, main = "Distribución de errores", xlab = "Error")

# Error relativo promedio

MAPE <- mean(abs(errores) /datos_test$SalePrice) * 100
MAPE

# Comparación entrenamiento vs prueba

data.frame( Conjunto = c("Entrenamiento", "Prueba"),
R2 = c(summary(modelo_final)$r.squared,R2_pred))

# Interpretación
# Si ambos R2 son similares, el modelo generaliza adecuadamente.
# Si el R2 de entrenamiento es mucho mayor que el de prueba,
# existe evidencia de sobreajuste.