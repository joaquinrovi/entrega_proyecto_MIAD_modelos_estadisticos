# Insumos para el informe: clasificación de suscripción bancaria

## 1. Revisión de los datos y del notebook

El conjunto de entrenamiento contiene 31.648 clientes, 16 variables
predictoras, un identificador y la respuesta binaria `Subscription`. El
conjunto de prueba contiene 13.563 clientes y las mismas columnas, salvo la
respuesta. No se encontraron valores faltantes, filas duplicadas,
identificadores repetidos ni niveles categóricos presentes únicamente en uno
de los dos conjuntos.

La clase positiva representa 3.697 observaciones (11,68%). Por este
desbalance, la exactitud no debe emplearse como criterio principal. La
comparación se realizó mediante AUC, que evalúa la capacidad de ordenar a los
clientes positivos por encima de los negativos sin depender de un umbral.

La primera columna de los CSV, leída por R como `X`, corresponde al
identificador original. Se conservó para enlazar las predicciones con el test,
pero no se utilizó como predictor, pues no describe al cliente y podría
introducir una señal artificial.

En el notebook exploratorio hay dos referencias residuales a una respuesta
anterior: las celdas de distribución y resumen final usan `df$y` y la
categoría `"yes"`. La respuesta correcta es `df$Subscription`, codificada
como `0/1`. Las demás secciones que emplean `Subscription` sí siguen la
estructura actual.

## 2. Estrategia de estimación y validación

Se compararon cuatro especificaciones permitidas por la restricción del curso:

1. Regresión logística principal con los 16 predictores originales.
2. Regresión logística transformada, con transformaciones logarítmicas para
   saldo, duración, número de contactos y antecedentes.
3. Regresión logística con splines naturales para edad, día y duración del
   último contacto, manteniendo efectos principales para las variables
   categóricas.
4. Análisis discriminante lineal (LDA) sobre variables transformadas y
   codificación indicadora de las variables categóricas.

La validación consistió en cinco folds estratificados, con semilla 202613. Así,
cada observación recibió una predicción de un modelo que no fue estimado con
ella. La selección se basó exclusivamente en el AUC de estas predicciones
fuera de muestra (OOF).

## 3. Comparación de desempeño

| Modelo | AUC promedio por fold | Desviación estándar | AUC OOF | Log-loss | Brier |
|---|---:|---:|---:|---:|---:|
| Logística con splines | 0,9091 | 0,0071 | **0,9090** | **0,2285** | **0,0695** |
| Logística transformada | 0,9051 | 0,0072 | 0,9049 | 0,2305 | 0,0699 |
| Logística principal | 0,9036 | 0,0083 | 0,9035 | 0,2413 | 0,0715 |
| LDA transformado | 0,9008 | 0,0071 | 0,9007 | 0,2627 | 0,0781 |

Se propone la regresión logística con splines porque obtuvo el mayor AUC OOF
y también los menores log-loss y Brier. Su AUC superó al LDA en 0,0083. La
elección también es coherente con la estructura de los datos: hay predictores
muy asimétricos, variables indicadoras y relaciones no lineales, condiciones
menos favorables para los supuestos de normalidad multivariada y covarianzas
comunes de LDA. La regresión logística no exige normalidad de los predictores,
y los splines permiten modelar curvatura sin cambiar de familia de
clasificación.

El AUC estimado es una medida de validación interna y no es el puntaje de
Kaggle. El resultado de la competencia solo se conocerá al subir el archivo de
predicciones.

## 4. Interpretación de resultados

Los coeficientes de una regresión logística representan cambios en el
logaritmo de los odds, manteniendo constantes las demás variables. Al
exponenciarlos se obtienen odds ratios. Algunos resultados de la especificación
principal, cuyos parámetros poseen interpretación directa, son:

- Una duración adicional de 60 segundos multiplica los odds estimados por
  aproximadamente 1,285, pues el odds ratio por segundo es 1,00418.
- Un resultado exitoso en la campaña anterior multiplica los odds por 10,32
  frente a un resultado desconocido (IC 95%: 8,45–12,60).
- Un tipo de contacto desconocido multiplica los odds por 0,196 frente al
  contacto celular, es decir, se asocia con una reducción cercana al 80,4%
  (IC 95%: 0,165–0,233).
- No tener crédito hipotecario multiplica los odds por 1,98 frente a tenerlo
  (IC 95%: 1,79–2,20).
- Cada contacto adicional dentro de la campaña multiplica los odds por 0,904,
  una reducción aproximada del 9,6% (IC 95%: 0,882–0,926).
- Tener préstamo personal multiplica los odds por 0,657 frente a no tenerlo
  (IC 95%: 0,570–0,756).
- Ser estudiante multiplica los odds por 1,92 frente a la categoría laboral
  de referencia, `blue-collar` (IC 95%: 1,49–2,48).

Estas asociaciones no deben interpretarse causalmente.

En el modelo seleccionado, los coeficientes de las bases spline no se deben
interpretar de forma aislada. Su efecto se analiza conjuntamente mediante
perfiles de probabilidad:

- Al fijar la duración en 58 segundos, la probabilidad promedio estimada es
  1,11%; con 179 segundos es 7,35%, y con 543 segundos asciende a 27,11%.
- Al fijar el número de contactos de campaña en uno, la probabilidad promedio
  es 12,86%; con cinco contactos es 9,73%.
- La relación con edad es no lineal: el perfil disminuye entre 29 y 39 años y
  vuelve a aumentar hacia los 56 años.
- El perfil del saldo es creciente, aunque su efecto es mucho más moderado que
  el de la duración.

La variable `Last.Contact.Duration` merece una salvedad. Es adecuada si el
objetivo es clasificar después de realizado el contacto, tal como permiten los
datos de la competencia. Si el uso operativo fuera decidir a quién llamar
antes de iniciar la conversación, esta variable no estaría disponible y
debería excluirse; ese sería un problema predictivo distinto.

## 5. Matriz de clasificación como análisis complementario

El umbral que maximiza el índice de Youden en las predicciones OOF fue 0,1110.
Con dicho umbral se obtuvo sensibilidad de 86,26%, especificidad de 81,52%,
exactitud de 82,08% y balanced accuracy de 83,89%. Con el umbral convencional
de 0,5, la sensibilidad cae a 36,81%, aunque la especificidad sube a 97,41%.

Este contraste muestra por qué no debe usarse 0,5 de forma automática en una
población desbalanceada. Para Kaggle no se usa ningún umbral: se entregan las
probabilidades continuas, porque el AUC depende del ordenamiento de los
puntajes y no de clases discretas.

## 6. Formato del submission

El archivo contiene 13.563 predicciones, sin valores faltantes y en el mismo
orden e identificadores del test. Su primera columna conserva el encabezado
vacío del identificador, igual que los archivos de entrada, y la segunda
columna se denomina `Subscription`. Las probabilidades se encuentran entre
0,000002 y 0,995314.

Si Kaggle muestra un error de encabezado, se debe consultar el nombre exacto
de la columna identificadora que indique la plataforma. Sin un
`sample_submission.csv`, el formato generado es la inferencia más fiel a los
CSV suministrados.
