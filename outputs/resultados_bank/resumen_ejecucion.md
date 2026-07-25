# Resumen de la ejecucion

- Fecha: 2026-07-25 11:05:28
- Train: 31648 filas y 18 columnas.
- Test: 13563 filas y 17 columnas.
- Tasa de Subscription = 1: 11.68%.
- Validacion: 5 folds estratificados; semilla 202613.
- El identificador no se uso como predictor.
- Modelo seleccionado por AUC OOF: Logistica_splines.
- AUC OOF seleccionado: 0.909008.
- Umbral de Youden para analisis descriptivo: 0.111025.
- El submission contiene probabilidades continuas en Subscription.

## Comparacion

                  model mean_fold_auc sd_fold_auc min_fold_auc max_fold_auc pooled_oof_auc log_loss     brier
      Logistica_splines      0.909094  0.00706642     0.901280     0.917212       0.909008 0.228543 0.0694636
 Logistica_transformada      0.905056  0.00718416     0.897706     0.912339       0.904914 0.230532 0.0699007
    Logistica_principal      0.903573  0.00830158     0.894086     0.912147       0.903491 0.241251 0.0714872
       LDA_transformado      0.900814  0.00706391     0.893577     0.909165       0.900741 0.262734 0.0781492
