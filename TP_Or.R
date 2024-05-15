#TP1 - Modélisation de séries temporelles
# Emilie CAILLERIE
# Véronique DEMIANENKO


rm(list=ls())

library(quantmod)
library(tseries)
library(forecast)
library(zoo)

# Partie 1 : Analyse descriptive
#1)
debut <- as.Date("2020-01-01")
fin <- as.Date("2022-12-31")

getSymbols("GOLD", from = debut, to = fin)
View(GOLD)


#2)
plot(GOLD$GOLD.Close, xlab = "Date", ylab = "Prix de clôture de l'or")

# Analyse descriptive :
mean(GOLD$GOLD.Close,na.rm= TRUE)
sd(GOLD$GOLD.Close,na.rm= TRUE)
summary(GOLD$GOLD.Close)
boxplot(GOLD$GOLD.Close)



# Partie 2 : Modélisation

# On divise les données en deux parties
fin2 <- as.Date("2022-10-01")
getSymbols("GOLD", from = debut, to = fin2)

apprentissage <- na.omit(GOLD$GOLD.Close)
plot(apprentissage)

#1a)
#ACF
acf_result <- acf(apprentissage, lag.max = 50) 

## Sur le graphe ACF on observe une décroissance régulière, ce qui indique une 
# dépendance à court terme de la série chronologique.

#PACF
pacf_result <- pacf(apprentissage, lag.max = 50)

## On observe sur le graphe PACF l'existence d'un seul pic significatif, suivi 
# de corrélations non significatives. Cela indique donc un terme AR d'ordre 1.

#1b)
model = auto.arima(apprentissage)
model

#1c)
# Extraction de la série résiduelle
residu <- residuals(model)
plot(residu)

# Test d'indépendance
ljung_box_test <- Box.test(residu, lag = 5, type = "Ljung-Box") 
ljung_box_test 
ljung_box_test$method


# Test de stationnarité
adf_test <- adf.test(residu)
adf_test

#2)

getSymbols("GOLD", from = fin2, to = fin)

test <- na.omit(GOLD$GOLD.Close)
plot(test)

# On test le modèle ARIMA trouvé précédemment sur l'ensemble de données test
# (de octobre à décembre)
arima(test, c(0,1,0))

forecast(test)
plot(forecast(test))
forecast <- na.omit(forecast)

# Les graphes sont extrêmement similaires, le modèle est donc bien adapté.

# Calcul de l'erreur quadratique moyenne

eqm <- sqrt(mean((forecast(test)$mean - residu)^2))
eqm

# Calcul de l'erreur absolue moyenne
eam <- mean(abs(forecast(test)$mean - residu))
eam

