# Graficas para el borrador de la Entrega 3
# Todas las figuras se generan con R base a partir de los datos y resultados
# reproducibles del modelo de clasificacion.

options(stringsAsFactors = FALSE, scipen = 999, digits = 5)

train_path <- "C:/Users/DELL/Documents/GitHub/entrega_proyecto_MIAD_modelos_estadisticos/data/raw clasificacion/Train bank.csv"
results_dir <- "C:/Users/DELL/Documents/Codex/2026-07-25/referenced-chatgpt-conversation-this-is-untrusted/outputs/resultados_bank"
figures_dir <- "C:/Users/DELL/Documents/Codex/2026-07-25/referenced-chatgpt-conversation-this-is-untrusted/outputs/entrega3_figures"

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

train <- read.csv(train_path, check.names = TRUE)
comparison <- read.csv(file.path(results_dir, "comparacion_modelos.csv"))
oof <- read.csv(file.path(results_dir, "predicciones_oof.csv"))
metrics <- read.csv(file.path(results_dir, "metricas_clasificacion.csv"))
coefficients <- read.csv(file.path(results_dir, "coeficientes_logistica_principal.csv"))
profiles <- read.csv(file.path(results_dir, "perfiles_probabilidad_modelo_seleccionado.csv"))

blue <- "#176B87"
teal <- "#2A9D8F"
coral <- "#E76F51"
purple <- "#7B2CBF"
navy <- "#17324D"
gray <- "#6B7280"
light_gray <- "#E5E7EB"

# -----------------------------------------------------------------------------
# Figura 1. Distribucion de la respuesta
# -----------------------------------------------------------------------------

class_counts <- table(factor(train$Subscription, levels = c(0, 1)))
class_pct <- prop.table(class_counts)

png(
  file.path(figures_dir, "figura_1_distribucion_clases.png"),
  width = 1500,
  height = 900,
  res = 160
)
par(mar = c(5, 5, 4.5, 2), family = "sans")
bars <- barplot(
  class_counts,
  names.arg = c("No suscripción (0)", "Suscripción (1)"),
  col = c(blue, coral),
  border = NA,
  ylim = c(0, max(class_counts) * 1.18),
  ylab = "Número de clientes",
  main = "Distribución de la variable objetivo",
  cex.names = 1.1,
  cex.axis = 1.05,
  cex.lab = 1.15,
  cex.main = 1.35
)
grid(nx = NA, ny = NULL, col = "#EDF0F4", lty = 1)
text(
  bars,
  as.numeric(class_counts),
  labels = sprintf(
    "%s\n(%.2f%%)",
    format(as.numeric(class_counts), big.mark = ".", decimal.mark = ","),
    100 * as.numeric(class_pct)
  ),
  pos = 3,
  cex = 1.15,
  font = 2,
  col = navy
)
box(bty = "l", col = gray)
dev.off()

# -----------------------------------------------------------------------------
# Figura 2. AUC promedio y variabilidad entre folds
# -----------------------------------------------------------------------------

labels_models <- c(
  Logistica_principal = "Logística principal",
  Logistica_transformada = "Logística transformada",
  Logistica_splines = "Logística con splines",
  LDA_transformado = "LDA transformado"
)
comparison$label <- labels_models[comparison$model]
comparison <- comparison[order(comparison$mean_fold_auc), ]
y_pos <- seq_len(nrow(comparison))

png(
  file.path(figures_dir, "figura_2_auc_modelos_v2.png"),
  width = 1600,
  height = 950,
  res = 160
)
par(mar = c(5, 12, 4.5, 2), family = "sans")
plot(
  comparison$mean_fold_auc,
  y_pos,
  xlim = c(0.885, 0.92),
  ylim = c(0.5, nrow(comparison) + 0.5),
  yaxt = "n",
  xlab = "AUC promedio en validación cruzada",
  ylab = "",
  main = "Comparación del desempeño fuera de muestra",
  pch = 19,
  cex = 1.7,
  col = ifelse(comparison$model == "Logistica_splines", coral, blue),
  cex.axis = 1.05,
  cex.lab = 1.15,
  cex.main = 1.35
)
axis(2, at = y_pos, labels = comparison$label, las = 1, tick = FALSE, cex.axis = 1.08)
abline(v = seq(0.885, 0.92, 0.005), col = "#EDF0F4", lty = 1)
segments(
  comparison$mean_fold_auc - comparison$sd_fold_auc,
  y_pos,
  comparison$mean_fold_auc + comparison$sd_fold_auc,
  y_pos,
  lwd = 3,
  col = ifelse(comparison$model == "Logistica_splines", coral, blue)
)
points(
  comparison$mean_fold_auc,
  y_pos,
  pch = 19,
  cex = 1.7,
  col = ifelse(comparison$model == "Logistica_splines", coral, blue)
)
text(
  comparison$mean_fold_auc + 0.0007,
  y_pos,
  labels = sprintf("%.4f", comparison$mean_fold_auc),
  pos = 4,
  cex = 1.02,
  col = navy
)
box(bty = "l", col = gray)
dev.off()

# -----------------------------------------------------------------------------
# Figura 3. Curvas ROC OOF
# -----------------------------------------------------------------------------

roc_table <- function(y, score) {
  ord <- order(score, decreasing = TRUE)
  y_ord <- y[ord]
  score_ord <- score[ord]
  tp <- cumsum(y_ord == 1)
  fp <- cumsum(y_ord == 0)
  keep <- !duplicated(score_ord, fromLast = TRUE)
  data.frame(
    fpr = c(0, fp[keep] / sum(y == 0), 1),
    tpr = c(0, tp[keep] / sum(y == 1), 1)
  )
}

model_cols <- c(
  Logistica_principal = blue,
  Logistica_transformada = teal,
  Logistica_splines = coral,
  LDA_transformado = purple
)
model_names <- c(
  "Logística principal",
  "Logística transformada",
  "Logística con splines",
  "LDA transformado"
)
model_vars <- names(model_cols)
auc_lookup <- setNames(comparison$pooled_oof_auc, comparison$model)

png(
  file.path(figures_dir, "figura_3_curvas_roc.png"),
  width = 1450,
  height = 1050,
  res = 160
)
par(mar = c(5, 5, 4.5, 2), family = "sans")
plot(
  0,
  0,
  type = "n",
  xlim = c(0, 1),
  ylim = c(0, 1),
  xlab = "1 - Especificidad",
  ylab = "Sensibilidad",
  main = "Curvas ROC con predicciones fuera de muestra",
  asp = 1,
  cex.axis = 1.05,
  cex.lab = 1.15,
  cex.main = 1.35
)
grid(col = "#EDF0F4")
abline(0, 1, lty = 2, col = "#9CA3AF")
for (model_var in model_vars) {
  roc <- roc_table(oof$Subscription, oof[[model_var]])
  lines(roc$fpr, roc$tpr, col = model_cols[[model_var]], lwd = 3)
}
legend(
  "bottomright",
  legend = sprintf(
    "%s (AUC = %.4f)",
    model_names,
    auc_lookup[model_vars]
  ),
  col = model_cols,
  lwd = 3,
  bty = "n",
  cex = 0.92
)
box(bty = "l", col = gray)
dev.off()

# -----------------------------------------------------------------------------
# Figura 4. Perfiles marginales del modelo seleccionado
# -----------------------------------------------------------------------------

profile_labels <- c(
  Age = "Edad",
  Balance..euros. = "Saldo (euros)",
  Last.Contact.Day = "Día del último contacto",
  Last.Contact.Duration = "Duración del contacto (segundos)",
  Campaign = "Número de contactos"
)
numeric_profiles <- profiles[profiles$variable_type == "numeric", ]

png(
  file.path(figures_dir, "figura_4_perfiles_probabilidad.png"),
  width = 1700,
  height = 1050,
  res = 160
)
par(mfrow = c(2, 3), mar = c(4.5, 4.6, 3.1, 1.2), family = "sans")
for (variable in names(profile_labels)) {
  block <- numeric_profiles[numeric_profiles$variable == variable, ]
  block <- block[order(block$value_numeric), ]
  plot(
    block$value_numeric,
    block$average_probability,
    type = "b",
    pch = 19,
    lwd = 2.5,
    col = coral,
    xlab = profile_labels[[variable]],
    ylab = "Probabilidad promedio",
    main = profile_labels[[variable]],
    ylim = range(block$average_probability) + c(-0.01, 0.01),
    cex.axis = 0.95,
    cex.lab = 1,
    cex.main = 1.12
  )
  grid(col = "#EDF0F4")
  box(bty = "l", col = gray)
}
plot.new()
text(0.5, 0.62, "Modelo seleccionado", cex = 1.15, font = 2, col = navy)
text(0.5, 0.50, "Regresión logística\ncon splines naturales", cex = 1.2, col = coral)
text(0.5, 0.31, "Cada perfil fija una variable\ny conserva las demás observadas.", cex = 0.92, col = gray)
dev.off()

# -----------------------------------------------------------------------------
# Figura 5. Odds ratios seleccionados de la logistica principal
# -----------------------------------------------------------------------------

selected_terms <- c(
  "Contactunknown",
  "Personal.Loanyes",
  "Campaign",
  "Marital.Statussingle",
  "Jobstudent",
  "Housing.Loanno",
  "Last.Contact.Monthmar",
  "Poutcomesuccess",
  "Last.Contact.Duration"
)
selected_labels <- c(
  "Contacto desconocido vs. celular",
  "Préstamo personal: sí vs. no",
  "Contactos de campaña (+1)",
  "Soltero vs. casado",
  "Estudiante vs. blue-collar",
  "Sin crédito hipotecario vs. con crédito",
  "Mes marzo vs. mayo",
  "Campaña anterior exitosa vs. desconocida",
  "Duración del contacto (+60 segundos)"
)

forest <- coefficients[match(selected_terms, coefficients$term), ]
forest$label <- selected_labels
duration_row <- forest$term == "Last.Contact.Duration"
forest$estimate[duration_row] <- forest$estimate[duration_row] * 60
forest$std_error[duration_row] <- forest$std_error[duration_row] * 60
forest$odds_ratio <- exp(forest$estimate)
forest$OR_CI_95_low <- exp(forest$estimate - 1.96 * forest$std_error)
forest$OR_CI_95_high <- exp(forest$estimate + 1.96 * forest$std_error)
forest <- forest[order(forest$odds_ratio), ]
y_forest <- seq_len(nrow(forest))

png(
  file.path(figures_dir, "figura_5_odds_ratios_v2.png"),
  width = 1750,
  height = 1100,
  res = 160
)
par(mar = c(5, 16, 4.5, 2), family = "sans")
plot(
  forest$odds_ratio,
  y_forest,
  log = "x",
  xlim = c(0.12, 16),
  ylim = c(0.5, nrow(forest) + 0.5),
  yaxt = "n",
  xaxt = "n",
  xlab = "Odds ratio (escala logarítmica)",
  ylab = "",
  main = "Asociaciones ajustadas en la logística principal",
  pch = 19,
  cex = 1.25,
  col = coral,
  cex.axis = 1,
  cex.lab = 1.12,
  cex.main = 1.3
)
axis(2, at = y_forest, labels = forest$label, las = 1, tick = FALSE, cex.axis = 0.95)
abline(v = 1, lty = 2, lwd = 2, col = "#6B7280")
segments(
  forest$OR_CI_95_low,
  y_forest,
  forest$OR_CI_95_high,
  y_forest,
  lwd = 2.7,
  col = blue
)
points(forest$odds_ratio, y_forest, pch = 19, cex = 1.25, col = coral)
axis(1, at = c(0.15, 0.25, 0.5, 1, 2, 4, 8, 16), labels = c("0,15", "0,25", "0,5", "1", "2", "4", "8", "16"))
grid(nx = NA, ny = NULL, col = "#EDF0F4")
box(bty = "l", col = gray)
dev.off()

# -----------------------------------------------------------------------------
# Figura 6. Matrices de confusion para dos umbrales
# -----------------------------------------------------------------------------

draw_confusion <- function(metric_row, title) {
  matrix_values <- matrix(
    c(metric_row$TN, metric_row$FP, metric_row$FN, metric_row$TP),
    nrow = 2,
    byrow = TRUE
  )
  image(
    1:2,
    1:2,
    matrix(c(0.25, 0.75, 0.55, 0.95), nrow = 2),
    col = colorRampPalette(c("#EAF4F7", blue))(100),
    axes = FALSE,
    xlab = "Clase predicha",
    ylab = "Clase real",
    main = title
  )
  axis(1, at = 1:2, labels = c("0", "1"))
  axis(2, at = 1:2, labels = c("0", "1"), las = 1)
  for (i in 1:2) {
    for (j in 1:2) {
      text(
        i,
        j,
        format(matrix_values[j, i], big.mark = ".", decimal.mark = ","),
        cex = 1.5,
        font = 2,
        col = navy
      )
    }
  }
  box(col = gray)
}

png(
  file.path(figures_dir, "figura_6_matrices_confusion_v2.png"),
  width = 1500,
  height = 760,
  res = 160
)
par(mfrow = c(1, 2), mar = c(5, 5, 4.5, 2), family = "sans")
draw_confusion(metrics[metrics$rule == "Umbral_Youden", ], "Umbral de Youden (0,111)")
draw_confusion(metrics[metrics$rule == "Umbral_0.5", ], "Umbral convencional (0,5)")
dev.off()

cat("Figuras creadas en:", figures_dir, "\n")
