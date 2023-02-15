#Kamil Baran Projekt Szeregi czasowe
library(fpp2)
library(forecast)

#wczytanie danych
zarobki <- read.csv("C:/Users/Kamil/Desktop/projekt szeregi/zarobki.csv")

colnames(zarobki) <- c("Data", "Ilosc_dolarow_na_tydzien")

domy <- read.csv("C:/Users/Kamil/Desktop/projekt szeregi/domy.csv")

colnames(domy) <- c("Data", "Ilosc_domow")
View(zarobki)
View(domy)

#wybieramy 2 kolumne z kazdej z wczytanych danych
zarobki2 <- zarobki[,2]
domy2 <- domy[,2]

#stworzenie szeregow czasowych
(domys <- ts(domy2, start = c(1988,1), frequency = 12))
(zarobkis <- ts(zarobki2, start = c(1968,1), frequency = 12))


#glowne cechy analizowanych szeregow----
monthplot(domys)
monthplot(zarobkis)

boxplot(domys ~ cycle(domys), main = "Domy", xlab = "Miesiace", ylab = "Ilosc")
boxplot(zarobkis ~ cycle(zarobkis), main = "Srednie tygodniowe zarobki pracownikow", xlab = "Miesiace", ylab = "Ilosc dolarow na tydzien")


lag.plot(domys, lags = 12)
lag.plot(zarobkis, lags = 12)

acf(domys)
acf(zarobkis)

pacf(domys)
pacf(zarobkis)

seasonplot(domys, col = rainbow(6))
seasonplot(zarobkis, col = rainbow(6))

#dekompozycja----
#I dane
adddomy <- decompose(domys, type = "additive")
plot(adddomy)
asdd<-diff(domys, lag.max = 1)
asdAD <- decompose(asdd, type="additive")
plot(asdAD)
plot(asdd)
tsdisplay(asdAD$seasonal)

#II dane
zarobkim <- decompose(zarobkis, type = "multiplicative")
plot(zarobkim)
adzarobki <- decompose(zarobkis, type = "additive")
plot(adzarobki)


#dekompozycja na podstawie modelu regresji
#I dane
reg <- tslm(domys ~ trend)
regts <- tslm(domys ~ trend+season)
plot(domys, main = "Dekompozycja modelu regresji danych dla domów", ylab=c("Jednostki"))
lines(fitted(reg), col="blue", lty = 2)
lines(fitted(regts), col="red", lty = 2)
legend("topright", c("Trend","Trend i sezonowosc"), col = c("blue","red"), lty = 1:2, cex = 0.8)

#IIdane
regz <- tslm(zarobkis ~ trend)
regzts <- tslm(zarobkis ~ trend+season)
plot(zarobkis, main = "Dekompozycja model regresji dla zarobkow", ylab=c("Srednie zarobki"))
lines(fitted(regz), col="blue", lty = 2)
lines(fitted(regzts), col="red", lty = 2)
legend("topleft",c("Trend","Trend i sezonowosc"), col = c("blue","red"), lty = 1:2)


#usuwanie trendu oraz sezonowosci----
#Idane
domy_sez <- diff(domys, lag = 12)
tsdisplay(domy_sez)
domy_sez_trend <- diff(domy_sez, lag = 1)
tsdisplay(domy_sez_trend, main = "Trend")

#IIdane
zarobki_sez <- diff(zarobkis, lag = 12)
tsdisplay(zarobki_sez)
zarobki_sez_trend <- diff(zarobki_sez, lag = 1)
tsdisplay(zarobki_sez_trend)

#Stworzenie szeregów stacjonarnych----
#I dane
tsdisplay(domys)
domyst <- BoxCox(domys, lambda = 0)
tsdisplay(domyst)
domyst2 <- diff(domyst, lag = 12)
tsdisplay(domyst2)
domyst3 <- diff(domyst2, lag = 1)
tsdisplay(domyst3)

#IIdane
tsdisplay(zarobkis)
zarobst <- BoxCox(zarobkis, lambda = 0)
tsdisplay(zarobst)
zarobst2 <- diff(zarobst, lag = 12)
tsdisplay(zarobst2)
zarobst3 <- diff(zarobst2, lag = 1)
tsdisplay(zarobst3)

#Sprawdzamy czy jestrealizacja szumu bialego 
#Idane
Acf(domyst3, lag.max = 100)
#szereg nie jest realizacja szumu bialego

#IIdane
Acf(zarobst3, lag.max = 100)
#szereg nie jest realizacja szumu bialego



#wyznaczanie rzędów dla modeli af i ma
#ar(domyst3,aic = TRUE, order.max = 100) #p=62
Acf(domyst3, lag.max = 100) #q=12
Pacf(domyst3, lag.max = 100) #p=62

#ar(zarobst3,aic = TRUE, order.max = 100)
Acf(zarobst3, lag.max = 100) #q=12
Pacf(zarobst3, lag.max = 100) #p=60



#Wspolczynnik modelu AR----
#Idane
(domyar <- ar(domyst3 , aic = FALSE, order.max = 62, method = "yule-walker"))
(domyar2 <- ar(domyst3, aic = FALSE, order.max = 62, method = "burg"))

(domyarauto <- ar(domyst3, aic = TRUE, order.max = 100))
#IIdane
(zarar <- ar(zarobst3 , aic = FALSE, order.max = 60, method = "yule-walker"))
(zarar2 <- ar(zarobst3, aic = FALSE, order.max = 60, method = "burg"))

(zarar2rauto <- ar(zarobst3, aic = TRUE, order.max = 100))


#Wspolczynnik modelu MA----
#dane1
wspama_domy <- Arima(domys, order = c(0,0,12))
summary(wspama_domy)

#dane2
wspama_zarobki <- Arima(zarobkis, order =c(0,0,12))
summary(wspama_zarobki)


#wyznaczanie modeli z uzyciem autoarima----
#dane1
(ar1 <- auto.arima(domyst3, ic ="aicc"))
(ar2 <- auto.arima(domyst3, ic ="aic"))
(ar3 <- auto.arima(domyst3, ic ="bic"))
#dane2
(ar4 <- auto.arima(zarobst3 , ic ="aicc"))
(ar5 <- auto.arima(zarobst3 , ic ="aic"))
(ar6 <- auto.arima(zarobst3 , ic ="bic"))


#prognozowanie z wykorzystaniem metod naiwnych----
#Na podstawie sredniej dane 1
domysmeanf <- meanf(domys, 12)
plot(domysmeanf)
#metoda naive dane 1
domysnaive <- naive(domys, 12)
plot(domysnaive)
#metoda snaive dane 1
domyssnaive <- snaive(domys,12)
plot(domyssnaive)
#z uzwgl?dnieniem dryfu dane 2
domysdryf <- rwf(domys, 12, drift = TRUE)
plot(domysdryf)


#Na podstawie sredniej dane 2
zarobkimeanf <- meanf(zarobkis, 12)
plot(domysmeanf)
#metoda naive dane 2
zarobkinaive <- naive(zarobkis, 12)
plot(domysnaive)
#metoda snaive dane 2
zarobkisnaive <- snaive(zarobkis,12)
plot(domyssnaive)
#z uzwgl?dnieniem dryfu dane 2
zarobkidryf <- rwf(zarobkis, 12, drift = TRUE)
plot(domysdryf)

#Wybor najlepszej metody----
#dane 1
(accuracy(domysmeanf))
(accuracy(domysnaive))
(accuracy(domyssnaive))
(accuracy(domysdryf))

#dane 2
(accuracy(zarobkimeanf))
(accuracy(zarobkinaive))
(accuracy(zarobkisnaive))
(accuracy(zarobkidryf))



