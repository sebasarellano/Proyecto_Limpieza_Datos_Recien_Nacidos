# LIMPIEZA Y DEPURACIÓN DE DATOS
library(readxl)
#Importar los datos desde el computador
choose.dir()
setwd("C:\Users\sebas\Desktop\2do Semestre\Limpieza, Depuración y Curación de Datos\COIL")
dir()

#Base_Unificada

DATA_MEX_R <- read_excel("DATA_MEX_R.xlsx")  # No sheet number defaults to first

# ============================================
# 1️⃣ ELIMINACIÓN DE DUPLICADOS
# ============================================
# Qué: Calculamos la proporción de registros duplicados antes de eliminarlos.
# Por qué: Porque necesitamos saber cuánto del dataset está afectado por duplicados antes de limpiarlo.
# Para qué: Para dimensionar el problema y reportar la calidad inicial de los datos.
# Cómo: Usamos duplicated() y dividimos el número de duplicados entre el total de filas.
proporcion_duplicados <- sum(duplicated(DATA_MEX_R)) / nrow(DATA_MEX_R)
proporcion_duplicados
#SE DECIDIO NO ELIMINAR LOS DUPLICADOS

# ============================================
# 2️⃣ MANEJO DE VALORES FALTANTES
# ============================================

# Qué: Calculamos la proporción de valores faltantes por columna.
# Por qué: Porque conocer la magnitud de valores faltantes ayuda a decidir cómo tratarlos.
# Para qué: Para priorizar variables críticas o decidir estrategias de imputación/eliminación.
# Cómo: Sumamos los NA por columna y los dividimos por el número total de filas.
proporcion_nulos <- colSums(is.na(DATA_MEX_R)) / nrow(DATA_MEX_R)
proporcion_nulos  # mostramos proporciones por variable

#NO HUBO VALORES NULOS


# ============================================
# 3️⃣ DETECCIÓN Y TRATAMIENTO DE OUTLIERS
# ============================================

# Qué: Calculamos la proporción de outliers en una variable numérica antes de tratarlos.
# Por qué: Porque conocer cuántos valores están fuera de rango permite evaluar el impacto.
# Para qué: Para decidir si eliminarlos, imputarlos o analizarlos aparte.
# Cómo: Usamos el método del IQR (intercuartílico) para identificar outliers y contamos cuántos son.
#PROPORCIÓN OUTLIERS PARA edad_mad
Q1 <- quantile(DATA_MEX_R$edad_mad, 0.25, na.rm = TRUE)
Q3 <- quantile(DATA_MEX_R$edad_mad, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
outliers <- which(DATA_MEX_R$edad_mad < (Q1 - 1.5 * IQR) | DATA_MEX_R$edad_mad > (Q3 + 1.5 * IQR))
proporcion_outliers <- length(outliers) / sum(!is.na(DATA_MEX_R$edad_mad))
proporcion_outliers  # mostramos la proporción de outliers

# Eliminarlos
DATA_MEX_R_sin_outliers <- DATA_MEX_R[-outliers,]

#PROPORCIÓN OUTLIERS PARA hij_vivo
Q1 <- quantile(DATA_MEX_R$hijos_vivo, 0.25, na.rm = TRUE)
Q3 <- quantile(DATA_MEX_R$hijos_vivo, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
outliers <- which(DATA_MEX_R$hijos_vivo < (Q1 - 1.5 * IQR) | DATA_MEX_R$hijos_vivo > (Q3 + 1.5 * IQR))
proporcion_outliers <- length(outliers) / sum(!is.na(DATA_MEX_R$hijos_vivo))
proporcion_outliers  # mostramos la proporción de outliers


# Eliminarlos
DATA_MEX_R_sin_outliers2 <- DATA_MEX_R_sin_outliers[-outliers,]

# ============================================
# 4️⃣ IMPUTACIÓN DE DATOS:  LA MEDIA
# ============================================
#NO SE REALIZO LA IMPUTACIÓN POR LA MEDIA YA QUE EL POCO PORCENTAJE DE OUTLIERS FUE ELIMINADO

# ============================================
# 5️⃣ CONVERSIÓN DE TIPOS DE DATOS
# ============================================

# Revisamos estructura original 
str(DATA_MEX_R)
#NO SE REALIZA ESTE PROCESO YA QUE FUE HECHO MANUALMENTE

# ============================================
# 6️⃣ NORMALIZACIÓN DE VARIABLES NUMÉRICAS
# ============================================
#ESTE PROCESO NO ES NECESARIO BASADO EN EL TIPO DE DATOS QUE TENEMOS YA QUE SIGUEN UNA MISMA ESCALA

# ============================================
# 7️⃣ VALIDACIÓN DE RANGOS ESPERADOS
# ============================================
#EN LAS VARIABLES NÚMERICAS LOS RANGOS YA FUERON DETERMINADOS Y SOLO SE CONSIDERARON LAS DATOS DENTRO DEL RANGO

# ============================================
# 8️⃣ RENOMBRADO DE VARIABLES
# ============================================
# Qué: Renombramos una variable por una más clara.
# Por qué: Porque nombres confusos o mal formateados dificultan el trabajo.
# Para qué: Para mejorar legibilidad y evitar errores de referencia.
# Cómo: Usamos rename() de dplyr.
library(dplyr)
DATA_MEX_R_sin_outliers2 <- DATA_MEX_R_sin_outliers2 %>% rename(hij_vivo = hijos_vivo)

# ============================================
# ✅ REVISIÓN FINAL DE LA BASE
# ============================================

# Qué: Revisamos la estructura final de la base.
# Por qué: Porque debemos confirmar que los cambios se aplicaron correctamente.
# Para qué: Para validar que la base está lista para análisis/modelado.
# Cómo: Usamos str().
str(DATA_MEX_R_sin_outliers2)

#EXPORTAMOS LA BASE CON LA ESTRUCTURA FINAL
install.packages("writexl")
library(writexl)
write_xlsx(DATA_MEX_R_sin_outliers2, "BASE_MEX_LIMPIA.xlsx")
