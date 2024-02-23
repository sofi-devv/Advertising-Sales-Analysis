options(scipen = 999) # Penalizamos los valores científicos

# Modelo de Presupuesto de Radio
df = Cleaned_Advertising_Budget_and_Sales
Y = df$Sales
X = df$Radio_Budget

plot(X, Y) # Graficamos los datos

modelo_radio = lm(Y ~ X, data = df) # Ajustamos el modelo de regresión lineal
abline(modelo_radio, col='red') # Dibujamos la línea de regresión en el gráfico

resumen_radio <- summary(modelo_radio) # Hacemos un resumen del modelo

# Modelo de Presupuesto de TV
y = df$Sales
x2 = df$TV_Budget
plot(x2, y) # Graficamos los datos

modelo_tv = lm(y ~ x2, data = df) # Ajustamos el modelo de regresión lineal
abline(modelo_tv, col='red') # Dibujamos la línea de regresión en el gráfico

resumen_tv <- summary(modelo_tv) # Hacemos un resumen del modelo

# Linealización Logarítmica
Y <- round(log(y),2) # Transformamos la variable de respuesta
X2 <- round(log(x2),2) # Transformamos la variable explicativa

modelo_tv_ajustado = lm(Y ~ X2, data = df) # Aplicamos el modelo a las variables transformadas
modelo_tv_ajustado$coefficients

beta0_est <- exp( 12.2696539 ) # Estimamos el valor de beta0 en la transformación logarítmica
beta1_est <- 0.3549444 # Estimamos el valor de beta1 en la transformación logarítmica

points(x2, beta0_est * (x2^{beta1_est}), col='blue', pch=20) # Revisamos visualmente el modelo ajustado

summary(modelo_tv_ajustado) # Hacemos un resumen del modelo ajustado
