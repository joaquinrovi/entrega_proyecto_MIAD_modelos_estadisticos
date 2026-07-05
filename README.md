# Proyecto Regresión — MAE Competencia 2026-13

Predicción del precio de venta de apartamentos (dataset **CasaRoble**) para
la competencia de Kaggle del curso *Modelos de Análisis Estadístico*
(MIAD, MIID-4104). Grupo 4.

## Integrantes
- Ana Sofía Bermúdez Moreno
- Escarlet Valerio
- Guido Alejandro Jácome Hernández
- Joaquín Rodríguez Villegas

## Descripción del proyecto
Se construye un modelo de regresión lineal múltiple para predecir
`SalePrice` a partir de 30 variables (numéricas y categóricas) que
describen características del apartamento, el edificio y el entorno.
El flujo cubre: análisis exploratorio, selección de variables (colinealidad
vía VIF y pruebas F), definición de la estructura del modelo (curvatura e
interacciones), verificación de supuestos, y generación de predicciones
para la competencia en Kaggle.

## Estructura del repositorio
\\\
data/raw/          -> datos originales de la competencia (train/test)
notebooks/         -> notebook oficial (informe) y notebook de experimentos
outputs/figures/   -> gráficos generados en el EDA y el modelado
outputs/tables/    -> tablas descriptivas exportadas desde R
outputs/submissions/ -> archivos subidos a Kaggle + tabla de seguimiento
docs/              -> entregas en PDF (EDA, informe final)
\\\

## Cómo ejecutar
1. Abrir la carpeta del proyecto en VS Code.
2. Abrir `notebooks/Proyecto_Regresion_R_Grupo4.ipynb`.
3. Seleccionar el kernel de **R** (ir) en la esquina superior derecha.
4. Ejecutar las celdas en orden (Run All), o celda por celda con Shift+Enter.
   La primera celda instala automáticamente los paquetes de R faltantes:
   `tidyverse, ggplot2, corrplot, moments, gridExtra, scales, knitr,
   reshape2, GGally, car, MASS, lmtest`.
5. Los datos deben estar en `data/raw/` (`Train_real_state.csv` y
   `Test_real_state.csv`); si el notebook los busca en la raíz, ajustar
   la ruta en la celda de carga (`read.csv(...)`).
6. El archivo de predicciones se genera en la carpeta de trabajo como
   `submission_grupoX.csv`, listo para subir a Kaggle en el formato
   `Id,Predicted`.

## Estado actual del modelo
- **v1**: modelo lineal (OLS) con selección de variables vía colinealidad
  exacta (`alias`), VIF iterativo (umbral 10) y prueba F parcial;
  estructura con `Size.sqf.²` e interacción `Size.sqf.:HallwayType`.
  - CV-RMSE: ~\,806
  - Kaggle RMSE: ~\,608 (MSE ≈ 1.06e9)
  - Posición leaderboard: 17/20

Ver `outputs/submissions/log_experimentos` para el detalle de todas las
versiones probadas.

## Datos
Provistos por la plataforma de la competencia (CasaRoble / MAE Competencia
Regresión 202613). No se recolectaron datos adicionales.
