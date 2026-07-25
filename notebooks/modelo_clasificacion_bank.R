# =============================================================================
# UniBank - Comparacion de regresion logistica y analisis discriminante lineal
# Criterio principal: area bajo la curva ROC (AUC)
#
# Ejecucion:
#   Rscript modelo_clasificacion_bank.R [train.csv] [test.csv] [directorio_salida]
#
# El script usa solamente paquetes incluidos con R: stats, splines y MASS.
# =============================================================================

options(stringsAsFactors = FALSE, scipen = 999, digits = 6, width = 140)

args <- commandArgs(trailingOnly = TRUE)
train_path <- if (length(args) >= 1) args[[1]] else {
  "C:/Users/DELL/Documents/GitHub/entrega_proyecto_MIAD_modelos_estadisticos/data/raw clasificacion/Train bank.csv"
}
test_path <- if (length(args) >= 2) args[[2]] else {
  "C:/Users/DELL/Documents/GitHub/entrega_proyecto_MIAD_modelos_estadisticos/data/raw clasificacion/Test bank.csv"
}
output_dir <- if (length(args) >= 3) args[[3]] else {
  "C:/Users/DELL/Documents/GitHub/entrega_proyecto_MIAD_modelos_estadisticos/outputs/resultados_bank"
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("MASS", quietly = TRUE)) {
  stop("El paquete MASS es necesario para LDA y no esta instalado.")
}

# -----------------------------------------------------------------------------
# Funciones auxiliares
# -----------------------------------------------------------------------------

auc_rank <- function(y, score) {
  ok <- is.finite(score) & !is.na(y)
  y <- as.integer(y[ok])
  score <- score[ok]
  n_pos <- sum(y == 1L)
  n_neg <- sum(y == 0L)
  if (n_pos == 0L || n_neg == 0L) return(NA_real_)
  ranks <- rank(score, ties.method = "average")
  (sum(ranks[y == 1L]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

log_loss <- function(y, score) {
  score <- pmin(pmax(score, 1e-15), 1 - 1e-15)
  -mean(y * log(score) + (1 - y) * log(1 - score))
}

brier_score <- function(y, score) {
  mean((score - y)^2)
}

make_stratified_folds <- function(y, k = 5L, seed = 202613L) {
  set.seed(seed)
  folds <- integer(length(y))
  for (class_value in sort(unique(y))) {
    idx <- which(y == class_value)
    folds[idx] <- sample(rep(seq_len(k), length.out = length(idx)))
  }
  folds
}

roc_table <- function(y, score) {
  ord <- order(score, decreasing = TRUE)
  y_ord <- y[ord]
  score_ord <- score[ord]
  tp <- cumsum(y_ord == 1L)
  fp <- cumsum(y_ord == 0L)
  keep <- !duplicated(score_ord, fromLast = TRUE)
  data.frame(
    threshold = c(Inf, score_ord[keep], -Inf),
    sensitivity = c(0, tp[keep] / sum(y == 1L), 1),
    false_positive_rate = c(0, fp[keep] / sum(y == 0L), 1)
  )
}

classification_metrics <- function(y, score, threshold) {
  predicted <- as.integer(score >= threshold)
  tp <- sum(predicted == 1L & y == 1L)
  tn <- sum(predicted == 0L & y == 0L)
  fp <- sum(predicted == 1L & y == 0L)
  fn <- sum(predicted == 0L & y == 1L)
  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)
  precision <- if ((tp + fp) == 0L) NA_real_ else tp / (tp + fp)
  data.frame(
    threshold = threshold,
    TP = tp,
    TN = tn,
    FP = fp,
    FN = fn,
    accuracy = (tp + tn) / length(y),
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    F1 = if (is.na(precision) || (precision + sensitivity) == 0) {
      NA_real_
    } else {
      2 * precision * sensitivity / (precision + sensitivity)
    },
    balanced_accuracy = mean(c(sensitivity, specificity))
  )
}

safe_predict_glm <- function(model, newdata) {
  p <- suppressWarnings(as.numeric(predict(model, newdata = newdata, type = "response")))
  pmin(pmax(p, 0), 1)
}

signed_log1p <- function(x) {
  sign(x) * log1p(abs(x))
}

# Alinea factores y crea transformaciones deterministicas. Las transformaciones
# no usan la respuesta, por lo que pueden aplicarse de igual forma a train/test.
prepare_data <- function(data, factor_levels = NULL) {
  factor_cols <- c("Job", "Marital.Status", "Education", "Credit", "Housing.Loan",
    "Personal.Loan", "Contact", "Last.Contact.Month", "Poutcome")

  references <- c(
    Job = "blue-collar",
    Marital.Status = "married",
    Education = "secondary",
    Credit = "no",
    Housing.Loan = "yes",
    Personal.Loan = "no",
    Contact = "cellular",
    Last.Contact.Month = "may",
    Poutcome = "unknown"
  )

  if (is.null(factor_levels)) {
    factor_levels <- lapply(factor_cols, function(nm) {
      observed <- sort(unique(as.character(data[[nm]])))
      c(references[[nm]], setdiff(observed, references[[nm]]))
    })
    names(factor_levels) <- factor_cols
  }

  for (nm in factor_cols) {
    unseen <- setdiff(unique(as.character(data[[nm]])), factor_levels[[nm]])
    if (length(unseen) > 0L) {
      stop("Niveles no vistos en ", nm, ": ", paste(unseen, collapse = ", "))
    }
    data[[nm]] <- factor(as.character(data[[nm]]), levels = factor_levels[[nm]])
  }

  data$Balance.Signed.Log <- signed_log1p(data$Balance..euros.)
  data$Duration.Log <- log1p(data$Last.Contact.Duration)
  data$Campaign.Log <- log1p(data$Campaign)
  data$Previously.Contacted <- factor(
    ifelse(data$Pdays < 0, "no", "yes"),
    levels = c("no", "yes")
  )
  data$Pdays.Log <- log1p(pmax(data$Pdays, 0))
  data$Previous.Log <- log1p(data$Previous)

  list(data = data, factor_levels = factor_levels)
}

# -----------------------------------------------------------------------------
# Carga y control de calidad
# -----------------------------------------------------------------------------

train_raw <- read.csv(train_path, check.names = TRUE)
test_raw <- read.csv(test_path, check.names = TRUE)

target <- "Subscription"
id_col <- names(test_raw)[1]

if (!target %in% names(train_raw)) stop("No se encontro la respuesta Subscription.")
if (target %in% names(test_raw)) stop("El archivo test no debe contener Subscription.")
if (!identical(setdiff(names(train_raw), target), names(test_raw))) {
  stop("Las columnas predictoras de train y test no coinciden o no estan en el mismo orden.")
}
if (anyNA(train_raw) || anyNA(test_raw)) {
  stop("Se encontraron valores NA. Revise el tratamiento antes de modelar.")
}
if (!all(sort(unique(train_raw[[target]])) == c(0, 1))) {
  stop("Subscription debe estar codificada con 0 y 1.")
}
if (anyDuplicated(train_raw[[id_col]]) || anyDuplicated(test_raw[[id_col]])) {
  stop("El identificador no es unico.")
}
if (length(intersect(train_raw[[id_col]], test_raw[[id_col]])) > 0L) {
  stop("Hay identificadores repetidos entre train y test.")
}

test_ids <- test_raw[[id_col]]

prepared_train <- prepare_data(train_raw)
train <- prepared_train$data
test <- prepare_data(test_raw, factor_levels = prepared_train$factor_levels)$data
train[[target]] <- as.integer(train[[target]])

# El identificador X se conserva para la entrega, pero nunca entra al modelo.
predictor_names <- setdiff(names(test_raw), id_col)

formula_logit_base <- as.formula(
  paste(target, "~", paste(predictor_names, collapse = " + "))
)

formula_logit_transformada <- Subscription ~
  Age + Job + Marital.Status + Education + Credit +
  Balance.Signed.Log + Housing.Loan + Personal.Loan + Contact +
  Last.Contact.Day + Last.Contact.Month + Duration.Log + Campaign.Log +
  Previously.Contacted + Pdays.Log + Previous.Log + Poutcome

formula_logit_splines <- Subscription ~
  splines::ns(Age, df = 4) +
  Job + Marital.Status + Education + Credit +
  Balance.Signed.Log + Housing.Loan + Personal.Loan + Contact +
  splines::ns(Last.Contact.Day, df = 4) +
  Last.Contact.Month +
  splines::ns(Duration.Log, df = 5) +
  Campaign.Log + Previously.Contacted +
  Pdays.Log +
  Previous.Log + Poutcome

formula_lda <- ~
  Age + Job + Marital.Status + Education + Credit +
  Balance.Signed.Log + Housing.Loan + Personal.Loan + Contact +
  Last.Contact.Day + Last.Contact.Month + Duration.Log + Campaign.Log +
  Previously.Contacted + Pdays.Log + Previous.Log + Poutcome

# -----------------------------------------------------------------------------
# Validacion cruzada estratificada
# -----------------------------------------------------------------------------

k_folds <- 5L
fold_id <- make_stratified_folds(train[[target]], k = k_folds, seed = 202613L)

cv_glm <- function(formula, model_name) {
  oof <- rep(NA_real_, nrow(train))
  fold_rows <- vector("list", k_folds)

  for (fold in seq_len(k_folds)) {
    idx_valid <- which(fold_id == fold)
    idx_train <- which(fold_id != fold)
    fit <- suppressWarnings(glm(
      formula,
      data = train[idx_train, , drop = FALSE],
      family = binomial(link = "logit"),
      control = glm.control(maxit = 100)
    ))
    oof[idx_valid] <- safe_predict_glm(fit, train[idx_valid, , drop = FALSE])
    fold_rows[[fold]] <- data.frame(
      model = model_name,
      fold = fold,
      n_validation = length(idx_valid),
      positives = sum(train[[target]][idx_valid] == 1L),
      auc = auc_rank(train[[target]][idx_valid], oof[idx_valid])
    )
  }

  list(predictions = oof, fold_metrics = do.call(rbind, fold_rows))
}

make_lda_matrix <- function(data) {
  model.matrix(formula_lda, data = data)
}

fit_lda_matrix <- function(x, y) {
  within_class_sd <- sapply(seq_len(ncol(x)), function(j) {
    min(
      sd(x[y == 0L, j]),
      sd(x[y == 1L, j])
    )
  })
  keep_variance <- is.finite(within_class_sd) & within_class_sd > 1e-8
  x_reduced <- x[, keep_variance, drop = FALSE]

  qr_fit <- qr(x_reduced, tol = 1e-8)
  independent <- sort(qr_fit$pivot[seq_len(qr_fit$rank)])
  x_reduced <- x_reduced[, independent, drop = FALSE]

  center <- colMeans(x_reduced)
  scale <- apply(x_reduced, 2, sd)
  x_scaled <- sweep(sweep(x_reduced, 2, center, "-"), 2, scale, "/")

  model <- MASS::lda(
    x = x_scaled,
    grouping = factor(y, levels = c(0, 1)),
    prior = as.numeric(prop.table(table(factor(y, levels = c(0, 1)))))
  )

  list(
    model = model,
    columns = colnames(x_reduced),
    center = center,
    scale = scale
  )
}

predict_lda_matrix <- function(object, x) {
  x_reduced <- x[, object$columns, drop = FALSE]
  x_scaled <- sweep(sweep(x_reduced, 2, object$center, "-"), 2, object$scale, "/")
  as.numeric(predict(object$model, newdata = x_scaled)$posterior[, "1"])
}

cv_lda <- function() {
  x_all <- make_lda_matrix(train)
  oof <- rep(NA_real_, nrow(train))
  fold_rows <- vector("list", k_folds)

  for (fold in seq_len(k_folds)) {
    idx_valid <- which(fold_id == fold)
    idx_train <- which(fold_id != fold)
    fit <- fit_lda_matrix(x_all[idx_train, , drop = FALSE], train[[target]][idx_train])
    oof[idx_valid] <- predict_lda_matrix(fit, x_all[idx_valid, , drop = FALSE])
    fold_rows[[fold]] <- data.frame(
      model = "LDA_transformado",
      fold = fold,
      n_validation = length(idx_valid),
      positives = sum(train[[target]][idx_valid] == 1L),
      auc = auc_rank(train[[target]][idx_valid], oof[idx_valid])
    )
  }

  list(predictions = oof, fold_metrics = do.call(rbind, fold_rows))
}

message("Validando regresion logistica principal...")
cv_logit_base <- cv_glm(formula_logit_base, "Logistica_principal")
message("Validando regresion logistica transformada...")
cv_logit_trans <- cv_glm(formula_logit_transformada, "Logistica_transformada")
message("Validando regresion logistica con splines...")
cv_logit_splines <- cv_glm(formula_logit_splines, "Logistica_splines")
message("Validando analisis discriminante lineal...")
cv_lda_result <- cv_lda()

predictions <- list(
  Logistica_principal = cv_logit_base$predictions,
  Logistica_transformada = cv_logit_trans$predictions,
  Logistica_splines = cv_logit_splines$predictions,
  LDA_transformado = cv_lda_result$predictions
)

fold_metrics <- do.call(rbind, list(
  cv_logit_base$fold_metrics,
  cv_logit_trans$fold_metrics,
  cv_logit_splines$fold_metrics,
  cv_lda_result$fold_metrics
))

y <- train[[target]]
model_comparison <- do.call(rbind, lapply(names(predictions), function(nm) {
  fold_values <- fold_metrics$auc[fold_metrics$model == nm]
  data.frame(
    model = nm,
    mean_fold_auc = mean(fold_values),
    sd_fold_auc = sd(fold_values),
    min_fold_auc = min(fold_values),
    max_fold_auc = max(fold_values),
    pooled_oof_auc = auc_rank(y, predictions[[nm]]),
    log_loss = log_loss(y, predictions[[nm]]),
    brier = brier_score(y, predictions[[nm]])
  )
}))
model_comparison <- model_comparison[order(-model_comparison$pooled_oof_auc), ]
row.names(model_comparison) <- NULL

best_model_name <- model_comparison$model[[1]]
best_oof <- predictions[[best_model_name]]
best_roc <- roc_table(y, best_oof)
youden <- best_roc$sensitivity - best_roc$false_positive_rate
valid_thresholds <- is.finite(best_roc$threshold)
best_threshold <- best_roc$threshold[valid_thresholds][which.max(youden[valid_thresholds])]

metrics_best <- rbind(
  cbind(rule = "Umbral_Youden", classification_metrics(y, best_oof, best_threshold)),
  cbind(rule = "Umbral_0.5", classification_metrics(y, best_oof, 0.5))
)

# -----------------------------------------------------------------------------
# Ajuste final, parametros y prediccion del test
# -----------------------------------------------------------------------------

final_glm_base <- suppressWarnings(glm(
  formula_logit_base,
  data = train,
  family = binomial(link = "logit"),
  control = glm.control(maxit = 100)
))
final_glm_trans <- suppressWarnings(glm(
  formula_logit_transformada,
  data = train,
  family = binomial(link = "logit"),
  control = glm.control(maxit = 100)
))
final_glm_splines <- suppressWarnings(glm(
  formula_logit_splines,
  data = train,
  family = binomial(link = "logit"),
  control = glm.control(maxit = 100)
))

x_train_lda <- make_lda_matrix(train)
x_test_lda <- make_lda_matrix(test)
final_lda <- fit_lda_matrix(x_train_lda, y)

final_models <- list(
  Logistica_principal = final_glm_base,
  Logistica_transformada = final_glm_trans,
  Logistica_splines = final_glm_splines,
  LDA_transformado = final_lda
)

if (best_model_name == "LDA_transformado") {
  test_probability <- predict_lda_matrix(final_lda, x_test_lda)
} else {
  test_probability <- safe_predict_glm(final_models[[best_model_name]], test)
}

if (length(test_probability) != nrow(test_raw) || any(!is.finite(test_probability))) {
  stop("Las probabilidades finales no son validas.")
}

# Coeficientes de la regresion logistica principal: son los mas directos para
# interpretar en terminos de odds ratios.
coef_matrix <- summary(final_glm_base)$coefficients
logit_coefficients <- data.frame(
  term = rownames(coef_matrix),
  estimate = coef_matrix[, "Estimate"],
  std_error = coef_matrix[, "Std. Error"],
  z_value = coef_matrix[, "z value"],
  p_value = coef_matrix[, "Pr(>|z|)"],
  odds_ratio = exp(coef_matrix[, "Estimate"]),
  OR_CI_95_low = exp(coef_matrix[, "Estimate"] - 1.96 * coef_matrix[, "Std. Error"]),
  OR_CI_95_high = exp(coef_matrix[, "Estimate"] + 1.96 * coef_matrix[, "Std. Error"]),
  row.names = NULL
)

lda_scaling <- data.frame(
  term = rownames(final_lda$model$scaling),
  LD1 = final_lda$model$scaling[, 1],
  row.names = NULL
)

lda_group_means <- data.frame(
  class = rownames(final_lda$model$means),
  final_lda$model$means,
  row.names = NULL,
  check.names = FALSE
)

oof_predictions <- data.frame(
  id = train_raw[[id_col]],
  Subscription = y,
  fold = fold_id,
  Logistica_principal = predictions$Logistica_principal,
  Logistica_transformada = predictions$Logistica_transformada,
  Logistica_splines = predictions$Logistica_splines,
  LDA_transformado = predictions$LDA_transformado
)

# Se preserva el encabezado vacio del identificador, igual que en train/test.
# Kaggle recibe probabilidades continuas en Subscription, no clases 0/1.
submission <- data.frame(Subscription = test_probability)
row.names(submission) <- test_ids

write.csv(
  submission,
  file.path(output_dir, "submission_bank.csv"),
  row.names = TRUE,
  quote = TRUE
)
write.csv(model_comparison, file.path(output_dir, "comparacion_modelos.csv"), row.names = FALSE)
write.csv(fold_metrics, file.path(output_dir, "auc_por_fold.csv"), row.names = FALSE)
write.csv(metrics_best, file.path(output_dir, "metricas_clasificacion.csv"), row.names = FALSE)
write.csv(logit_coefficients, file.path(output_dir, "coeficientes_logistica_principal.csv"), row.names = FALSE)
write.csv(lda_scaling, file.path(output_dir, "coeficientes_lda.csv"), row.names = FALSE)
write.csv(lda_group_means, file.path(output_dir, "medias_grupo_lda.csv"), row.names = FALSE)
write.csv(oof_predictions, file.path(output_dir, "predicciones_oof.csv"), row.names = FALSE)

saveRDS(
  list(
    selected_model = best_model_name,
    model = final_models[[best_model_name]],
    id_column = id_col,
    target = target,
    factor_levels = prepared_train$factor_levels,
    formula_logit_base = formula_logit_base,
    formula_logit_transformada = formula_logit_transformada,
    formula_logit_splines = formula_logit_splines,
    formula_lda = formula_lda,
    validation = model_comparison,
    youden_threshold = best_threshold
  ),
  file.path(output_dir, "modelo_final.rds")
)

# Perfiles marginales del modelo seleccionado. Para cada escenario se modifica
# una sola variable y se promedia la probabilidad sobre todos los clientes.
# Esto permite interpretar conjuntamente los terminos spline.
score_selected <- function(prepared_newdata) {
  if (best_model_name == "LDA_transformado") {
    predict_lda_matrix(final_lda, make_lda_matrix(prepared_newdata))
  } else {
    safe_predict_glm(final_models[[best_model_name]], prepared_newdata)
  }
}

profile_rows <- list()
profile_index <- 0L

numeric_profile_values <- list(
  Age = unique(as.numeric(quantile(train_raw$Age, c(0.10, 0.25, 0.50, 0.75, 0.90)))),
  Balance..euros. = unique(as.numeric(quantile(
    train_raw$Balance..euros.,
    c(0.10, 0.25, 0.50, 0.75, 0.90)
  ))),
  Last.Contact.Day = c(5, 10, 15, 20, 25, 30),
  Last.Contact.Duration = unique(as.numeric(quantile(
    train_raw$Last.Contact.Duration,
    c(0.10, 0.25, 0.50, 0.75, 0.90)
  ))),
  Campaign = unique(as.numeric(quantile(
    train_raw$Campaign,
    c(0.10, 0.25, 0.50, 0.75, 0.90)
  )))
)

for (nm in names(numeric_profile_values)) {
  for (value in numeric_profile_values[[nm]]) {
    scenario <- train_raw
    scenario[[nm]] <- value
    prepared_scenario <- prepare_data(
      scenario,
      factor_levels = prepared_train$factor_levels
    )$data
    profile_index <- profile_index + 1L
    profile_rows[[profile_index]] <- data.frame(
      variable = nm,
      variable_type = "numeric",
      scenario = as.character(value),
      value_numeric = value,
      n = nrow(train_raw),
      average_probability = mean(score_selected(prepared_scenario)),
      interpretation = "Estandarizacion marginal: una variable se fija y las demas conservan sus valores."
    )
  }
}

# Para categorias se usa la prediccion media del subgrupo observado. Esto evita
# crear combinaciones imposibles, especialmente entre Poutcome, Pdays y Previous.
fitted_probability <- score_selected(train)
for (nm in names(prepared_train$factor_levels)) {
  for (value in prepared_train$factor_levels[[nm]]) {
    idx <- as.character(train_raw[[nm]]) == value
    profile_index <- profile_index + 1L
    profile_rows[[profile_index]] <- data.frame(
      variable = nm,
      variable_type = "categorical",
      scenario = value,
      value_numeric = NA_real_,
      n = sum(idx),
      average_probability = mean(fitted_probability[idx]),
      interpretation = "Promedio de probabilidades ajustadas dentro del subgrupo observado."
    )
  }
}

marginal_profiles <- do.call(rbind, profile_rows)
write.csv(
  marginal_profiles,
  file.path(output_dir, "perfiles_probabilidad_modelo_seleccionado.csv"),
  row.names = FALSE
)

# Perfiles numericos en un unico grafico.
numeric_profiles <- marginal_profiles[marginal_profiles$variable_type == "numeric", ]
png(
  file.path(output_dir, "perfiles_numericos_modelo_seleccionado.png"),
  width = 1500,
  height = 1000,
  res = 150
)
par(mfrow = c(2, 3), mar = c(4.2, 4.5, 2.7, 1.2))
for (nm in names(numeric_profile_values)) {
  block <- numeric_profiles[numeric_profiles$variable == nm, ]
  block <- block[order(block$value_numeric), ]
  plot(
    block$value_numeric,
    block$average_probability,
    type = "b",
    pch = 19,
    col = "#E76F51",
    lwd = 2,
    xlab = nm,
    ylab = "Probabilidad promedio",
    main = nm,
    ylim = range(block$average_probability) + c(-0.01, 0.01)
  )
  grid(col = "gray90")
}
dev.off()

# Curvas ROC comparadas.
roc_colors <- c(
  Logistica_principal = "#1B6CA8",
  Logistica_transformada = "#2A9D8F",
  Logistica_splines = "#E76F51",
  LDA_transformado = "#7B2CBF"
)
png(
  file.path(output_dir, "curvas_roc_validacion.png"),
  width = 1400,
  height = 1000,
  res = 150
)
plot(
  0, 0,
  type = "n",
  xlim = c(0, 1),
  ylim = c(0, 1),
  xlab = "1 - Especificidad (tasa de falsos positivos)",
  ylab = "Sensibilidad (tasa de verdaderos positivos)",
  main = "Curvas ROC - validacion cruzada estratificada",
  asp = 1
)
abline(0, 1, lty = 2, col = "gray65")
legend_labels <- character(length(predictions))
for (i in seq_along(predictions)) {
  nm <- names(predictions)[i]
  rt <- roc_table(y, predictions[[nm]])
  lines(rt$false_positive_rate, rt$sensitivity, col = roc_colors[[nm]], lwd = 2.5)
  legend_labels[i] <- sprintf("%s (AUC = %.4f)", nm, auc_rank(y, predictions[[nm]]))
}
legend(
  "bottomright",
  legend = legend_labels,
  col = roc_colors[names(predictions)],
  lwd = 2.5,
  bty = "n",
  cex = 0.85
)
dev.off()

summary_lines <- c(
  "# Resumen de la ejecucion",
  "",
  paste0("- Fecha: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("- Train: ", nrow(train_raw), " filas y ", ncol(train_raw), " columnas."),
  paste0("- Test: ", nrow(test_raw), " filas y ", ncol(test_raw), " columnas."),
  sprintf("- Tasa de Subscription = 1: %.2f%%.", 100 * mean(y)),
  paste0("- Validacion: ", k_folds, " folds estratificados; semilla 202613."),
  "- El identificador no se uso como predictor.",
  paste0("- Modelo seleccionado por AUC OOF: ", best_model_name, "."),
  sprintf("- AUC OOF seleccionado: %.6f.", auc_rank(y, best_oof)),
  sprintf("- Umbral de Youden para analisis descriptivo: %.6f.", best_threshold),
  "- El submission contiene probabilidades continuas en Subscription.",
  "",
  "## Comparacion",
  "",
  paste(capture.output(print(model_comparison, row.names = FALSE)), collapse = "\n")
)
writeLines(summary_lines, file.path(output_dir, "resumen_ejecucion.md"), useBytes = TRUE)

cat("\nComparacion de modelos:\n")
print(model_comparison, row.names = FALSE)
cat("\nModelo seleccionado:", best_model_name, "\n")
cat("AUC OOF:", sprintf("%.6f", auc_rank(y, best_oof)), "\n")
cat("Submission:", file.path(output_dir, "submission_bank.csv"), "\n")
