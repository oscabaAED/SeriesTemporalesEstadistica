#Análisis de una serie temporal. Tráfico comercial en el aeropuerto de Valencia.

#En este caso práctico analizamos la serie temporal correspondiente al tráfico comercial en el aeropuerto de Valencia.  Cargamos los dos paquetes de R que vamos a utilizar en el análisis.


library(readr)
library(forecast)
remove(list=ls())
## **Ejemplo**: Tráfico comercial, aeropuerto de Valencia. Fuente: Ministerio de transportes, movilidad y agenda urbana (https://www.fomento.gob.es/BE/?nivel=2&orden=03000000).  

Dat_Pasajeros <- read.csv(file="Pasajeros.csv",header=TRUE,sep=";")
attach(Dat_Pasajeros)

T <- length(Total)
Pasajeros_ts <- ts(Total[121:T],start=c(2010,1),end=c(2019,12),frequency=12)

# Creamos una serie temporal con las observaciones a partir de enero de 2010. 
# Como se trata de datos mensuales, definimos frequency=12

# Descripción gráfica de la serie temporal

#Es habitual comenzar el análisis de una serie con la representación gráfica de los valores observados de la variable de interés en función del tiempo: 

plot(Pasajeros_ts,ylab="Num Pasajeros")


# A partir del gráfico temporal podemos apreciar una cierta evolución en el largo plazo (*tendencia*): durante los primeros cuatro años se observa una tendencia decreciente. A partir de 2014, la serie temporal toma valores cada vez mayores, es decir, la serie presenta una tendencia creciente. Por otro lado, se observa un comportamiento cíclico que se repite año tras año (*estacionalidad*), con un número mayor de pasajeros durante los meses de verano. La longitud del ciclo estacional es $c = 12$. 

# En este ejemplo, la estacionalidad de la serie se observa claramente en el gráfico temporal. No obstante, el diagrama de cajas por mes nos permite también valorar la presencia de estacionalidad.


Mes_ord <- factor(Mes[121:T],levels = c("Enero","Febrero","Marzo","Abril",
                                        "Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"))
# Ordenamos los meses para que los represente por orden temporal

boxplot(Total[121:T] ~ Mes_ord, ylab="", xlab="")

#Una gráfica estacional es similar a una gráfica temporal, excepto que los datos se dibujan contra las “estaciones” individuales en las que se observaron los datos. A continuación se ofrece un ejemplo:
library('ggplot2')
ggseasonplot(Pasajeros_ts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Num Pasajeros") +
  ggtitle("Seasonal plot: número de pasajeros en aeorpuerto Valencia")



insample <- window(Pasajeros_ts,start=c(2010,1),end=c(2018,12))  
# ajuste desde enero de 2010 hasta diciembre de 2018
outsample <- window(Pasajeros_ts,start=c(2019,1),end=c(2019,12)) 
# reservamos 2019 para valorar la predicción


# Análisis de la serie mediante la metodología Box-Jenkins

# Dadas las características de la serie temporal: tendencia y estacionalidad, el primer paso del análisis es determinar la transformación estacionaria de la serie.

# Calculamos un diferencia estacional ($D = 1$):


d12insample <- diff(insample,12)
plot(d12insample)

#Parece que hemos quitado la estacionalidad, pero todavía queda la tendencia. Calculamos pues una diferencia regular ($d = 1$):

dd12insample <- diff(d12insample)
plot(dd12insample)


# plot squared values to see if variance is constant
plot(dd12insample^2)
# goldfeld-quandt
library(lmtest)
?gqtest
lm( dd12insample~.)
gqtest( lm(x~.,data=as.data.frame(dd12insample)) )
# large p-value we don't reject null hypothesis of homoscedasticity


#Podemos asumir que la serie diferenciada con $d = 1$ y $D = 1$ ya es estacionaria. Pasamos a examinar el correlograma y el correlograma parcial:

acf(dd12insample,lag.max=50)
pacf(dd12insample,lag.max=50)


# Si nos fijamos en los primeros retardos, podemos pensar:

# La función de autocorrelación tiene el primer coeficiente significativo, mientras que la función de autocorrelación parcial muestra decrecimiento: (p,d,q) = (0,1,1)

# La función de autocorrelación decrece y la función de autocorrelación parcial tiene el primer coeficiente significativo: (p,d,q) = (1,1,0)

# Las dos funciones muestran decrecimiento a partir del primer coeficiente: (p,d,q) = (1,1,1)

# Si nos fijamos en los retardos estacionales (Lag = 1, 2, 3, 4 ciclos estacionales), podemos pensar:

# No hay ningún coeficiente significativo: (P,D,Q) = (0,1,0)

# La función de autocorrelación tiene el primer coeficiente significativo, mientras que la función de autocorrelación parcial muestra decrecimiento: (P,D,Q) = (0,1,1)

# Veamos el ajuste proporcionado por los distintos modelos:
?arima

Pasajeros_model1 <- arima(insample, order=c(0,1,1), seasonal=list(order=c(0,1,0), period=12))
Pasajeros_model1

Pasajeros_model2 <- arima(insample, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12))
Pasajeros_model2

Pasajeros_model3 <- arima(insample, order=c(1,1,0), seasonal=list(order=c(0,1,0), period=12))
Pasajeros_model3

Pasajeros_model4 <- arima(insample, order=c(1,1,0), seasonal=list(order=c(0,1,1), period=12))
Pasajeros_model4

Pasajeros_model5 <- arima(insample, order=c(1,1,1), seasonal=list(order=c(0,1,0), period=12))
Pasajeros_model5

Pasajeros_model6 <- arima(insample, order=c(1,1,1), seasonal=list(order=c(0,1,1), period=12))
Pasajeros_model6

?auto.arima
# notar que checara todos los modelos con p<=5, q<=5, P<=2, Q<=2, d<=2, D=1, tal que 
# p+q+P+Q<=5 porque stepwise = FALSE, si no usa una heuristica
# approximation=FALSE ->
Pasajeros_model <- auto.arima(insample, stepwise=FALSE, approximation=FALSE)
Pasajeros_model

accuracy(Pasajeros_model)


# El modelo de menor AIC es (p,d,q)(P,D,Q) = (0,1,1)(0,1,1), que coincide con el modelo proporcionado por la función <code>auto.arima</code>. El MAPE asociado a este modelo es 2.4599. La ecuación del modelo es: 

#Veamos a continuación la representación gráfica del ajuste obtenido. Línea negra: valores reales, línea roja: valores ajustados. 


fitval <- Pasajeros_model$fitted # Valores ajustados

plot(insample,ylab="Num Pasajeros")
lines(fitval,col="red")


# Antes de pasar a la predicción, comprobamos que el modelo es válido. Como muestran las siguientes salidas, los residuos del modelo pueden considerarse ruido blanco. 

?checkresiduals
checkresiduals(Pasajeros_model,plot=TRUE)
# contraste ljung box, H0: no autocorrelation, large p-value we dont reject

#La predicción obtenida para los 12 meses de 2019 junto con el error de predicción vienen dados por:

pred <- forecast(Pasajeros_model,h=12)$mean
pred # Predicción puntual

plot(forecast(Pasajeros_model,h=12))

rmse_pred <- sqrt(mean((outsample-pred)^2))
mape_pred <- 100*mean(abs(outsample-pred)/outsample)
rmse_pred;mape_pred

# podriamos comparar con el siguiente mejor modelo segun AIC
pred4 <- forecast(Pasajeros_model4,h=12)$mean
pred4 # Predicción puntual

plot(forecast(Pasajeros_model4,h=12))

rmse_pred4 <- sqrt(mean((outsample-pred4)^2))
mape_pred4 <- 100*mean(abs(outsample-pred4)/outsample)
rmse_pred4;mape_pred4
rmse_pred;mape_pred
# asi que (0,1,1)(0,1,1) mejor que (1,1,0)(0,1,1)


# Finalmente, representamos gráficamente los valores reales de 2019 que habíamos reservado junto con la predicción puntual:


plot(pred, col="red",xaxt="n",xlab="Año 2019")
points(outsample,pch=19)

