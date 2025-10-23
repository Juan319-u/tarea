# ⃣ Instalar y cargar las librerías necesarias
install.packages("readxl")
install.packages("car")
install.packages("olsrr")

library(readxl)
library(car)
library(olsrr)

# ⃣ Leer el archivo Excel (.xlsx)
ruta <- "C:\\Users\\Felipe\\Desktop\\data.xlsx"
datos <- read_excel(ruta)

# ⃣ Visualizar la estructura de los datos
str(datos)
summary(datos)

# ⃣ Crear el modelo de regresión lineal múltiple
modelo <- lm(Compressive_Strength ~ Cement + Slag + Fly_ash + Water + 
               SP + Coarse_Aggr + Fine_Aggr + `SLUMP(cm)` + `FLOW(cm)`, data = datos)

# ⃣ Resumen del modelo
summary(modelo)

# ⃣ Significancia global del modelo (Prueba F)
anova(modelo)

# ⃣ Calcular el R² ajustado
cat("R² ajustado:", summary(modelo)$adj.r.squared, "\n")

# ⃣ Multicolinealidad (VIF)
vif(modelo)

# ⃣ Diagnóstico de los residuos
par(mfrow = c(2,2))
plot(modelo)

# ⃣ Pruebas específicas de supuestos
# Normalidad de los residuos
shapiro.test(resid(modelo))

# Homocedasticidad
ols_test_breusch_pagan(modelo)

# Influencia de observaciones atípicas
ols_plot_cooksd_chart(modelo)

# ⃣ Conclusión automática
cat("\n--- CONCLUSIÓN ---\n")
if (summary(modelo)$adj.r.squared > 0.7) {
  cat("El modelo tiene una alta capacidad explicativa y es confiable.\n")
} else if (summary(modelo)$adj.r.squared > 0.5) {
  cat("El modelo tiene capacidad explicativa moderada, puede mejorarse.\n")
} else {
  cat("El modelo explica poco la variabilidad, se recomienda ajustar variables.\n")
}


install.packages("reshape2")
install.packages("RColorBrewer")
install.packages("ggplot2")
library(reshape2)
library(RColorBrewer)
library(ggplot2)
# ⃣ Calcular la matriz de correlación
correlaciones <- cor(datos)

# ⃣ Convertir a formato largo para ggplot
melted_cor <- melt(correlaciones)

# ⃣ Crear el mapa de calor
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Correlación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  labs(title = "Mapa de calor de correlaciones - Variables del concreto",
       x = "Variables", y = "Variables")
