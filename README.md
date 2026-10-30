# Regresión lineal: precios de vivienda en Cali por barrio

Dos entregables independientes en R Markdown, listos para compilar y publicar en RPubs. Cada uno filtra la base `Datos_Vivienda.xlsx` por un barrio distinto, sin filtrar por tipo de inmueble.

| Archivo | Barrio | Grafías unificadas | Ofertas analizadas |
|---|---|---|---|
| `El_Caney_Regresion.Rmd` | El Caney (Zona Sur) | "El Caney", "el caney", "caney", "caney especial" | 299 |
| `La_Flora_Regresion.Rmd` | La Flora (Zona Norte) | "La Flora", "la flora", "laflora", "flora", "norte la flora", "urbanizacion la flora" | 448 |

En La Flora se excluyen "Flora Industrial" y "Floralia", que son otros sectores. También se descarta un registro de La Flora marcado como "Zona Sur".

## Qué incluye cada documento

1. **Modelación simple**: tres modelos (área, baños, habitaciones). Cada uno tiene su interpretación, la validación de supuestos (gráficos de diagnóstico y las pruebas de Shapiro-Wilk, Breusch-Pagan, Durbin-Watson y RESET) y una comparación de cuatro especificaciones logarítmicas (Lin-Lin, Log-Lin, Lin-Log y Log-Log) con sus supuestos.
2. **Modelación múltiple**:
   - Dos modelos con su interpretación. El modelo 2 incluye la dummy `Tipo` (Casa vs. Apartamento).
   - Validación de supuestos, VIF y distancia de Cook, con su versión logarítmica.
   - Selección de variables paso a paso con `step()`.
   - Predicción hipotética con `predict()`, con intervalos de confianza y de predicción.
   - MAE dentro de muestra y con validación cruzada de 10 pliegues.

Las cifras que aparecen en el texto se calculan con código R en línea (`` `r ...` ``), así que siempre coinciden con los resultados de los modelos.

## Cómo publicarlo en RPubs

1. Descarga el repositorio completo: ya incluye `Datos_Vivienda.xlsx` junto a los `.Rmd`. Si trabajas con otra copia, deja el Excel en la misma carpeta que el `.Rmd`.
2. Abre el `.Rmd` en RStudio e instala los paquetes, una sola vez:

   ```r
   install.packages(c("readxl", "dplyr", "ggplot2", "plotly", "lmtest", "car", "rmarkdown"))
   ```

3. Cambia `author: "Nombre del estudiante"` en el encabezado.
4. Haz clic en **Knit**, que genera el HTML.
5. En la ventana del HTML, haz clic en **Publish** (el ícono azul) → **RPubs** → **Publish**. Inicia sesión o crea tu cuenta, completa el título y la URL, y copia el enlace.

## Versión en script de R

`El_Caney_Regresion.R` y `La_Flora_Regresion.R` contienen el mismo código de los chunks, sin el texto, para correrlo en una hoja de código de RStudio:

1. Deja `Datos_Vivienda.xlsx` en la misma carpeta que el script.
2. Ve a **Session → Set Working Directory → To Source File Location**.
3. Corre todo con **Ctrl+Shift+Enter** (Source with Echo) o por partes con **Ctrl+Enter**.

La carpeta `vista_previa/` tiene el HTML de ambos documentos, compilado con este mismo `Datos_Vivienda.xlsx`, para ver el resultado antes de compilar.
