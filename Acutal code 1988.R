setwd("/Users/jonasschernich/Uni/6. Semester/Bachelorarbeit/InferenceForRanks")
library(tidyverse)
library(dplyr)
library(csranks)
library(lubridate)
library(sandwich)
library(lmtest)
library(csranks)

######

# Benenne die Spalten um

######
yield <- read_csv2("Yields.csv")
other_indices <- read.csv2("AndereIndices.csv")
pacific <- read.csv2("Pacific.csv")
gold <- read.csv2("Gold.csv")
gold <- na.omit(gold)
Nikkei <- read.csv2("Nikkei.csv")
pureValue <- read.csv2("purevalue.csv")
indices_isin <- read.csv2("Indizes.csv")
colnames(indices_isin) <- c("date", "WORLD", "EM", "FTSEDIV", "FTSEPER", "FTSE", "EURODIV", "EUROPER", "EURO", "NASDAQ", "SP500", "DAX", "SPSMALL600VALUE", "HANGSENGDIV", "HANGSENGPER", "HANGSENG", "SP500VALUE", "SP500GROWTH")
indices<- indices_isin[-c(1:4696),c(1, 2, 3, 6, 9, 10, 11, 12, 16, 17, 18)]
indices$Yield <- yield$Yield / 100
indices$dailyyield <- exp(log(1 + indices$Yield) / 365) - 1
indices$date <- as.Date(strptime(indices$date, format = "%d.%m.%y"))
indices$Year <- as.numeric(format(indices$date, "%Y"))
indices$Nikkei <- Nikkei[(nrow(Nikkei) - nrow(indices) + 1):nrow(Nikkei),2]

indices$cumYield <- rep( 0,nrow(indices))
indices$cumYield[1] <- 100
for(i in 2:nrow(indices)) {
  indices$cumYield[i] <- indices$cumYield[i - 1]*(1+indices$dailyyield[i-1])
}





indices[,4] <- gsub(",", ".", indices[,4] )
indices[,4] <- as.numeric(indices[,4])
indices[,6] <- gsub(",", ".", indices[,6] )
indices[,6] <- as.numeric(indices[,6] )
indices[,7] <- gsub(",", ".", indices[,7] )
indices[,7] <- as.numeric(indices[,7] )
indices[,8] <- gsub(",", ".", indices[,8] )
indices[,8] <- as.numeric(indices[,8] )
indices[,9] <- gsub(",", ".", indices[,9] )
indices[,9] <- as.numeric(indices[,9] )
indices[,10] <- gsub(",", ".", indices[,10] )
indices[,10] <- as.numeric(indices[,10] )
indices[,11] <- gsub(",", ".", indices[,11] )
indices[,11] <- as.numeric(indices[,11] )

# Datenframes zusammenführen und fehlende Werte mit "NA" markieren
indices <- merge(indices, lowBetaPortfolio, by = "date", all.x = TRUE)
colnames(indices)[which(colnames(indices) == "kurs")] <- "Low Beta"
std_prev <- sd(indices$`Low Beta`, na.rm = TRUE)
#replacing NAs with the last documented stock price
for(i in 1:nrow(indices)) {
  if(is.na(indices$`Low Beta`[i])) {
    indices$`Low Beta`[i] <- indices$`Low Beta`[i - 1]
  }
}
std_ratio <- 1 - sd(indices$`Low Beta`)/std_prev

###########
SP <- indices[c(1,7)]
sp_value <- indices$SP500
SPmoving_avrg <- rep(TRUE, length(sp_value))
# Calculating S&P 200day moving average dates
for(i in 201 : length(sp_value)) {
  if(sp_value[i] < mean(sp_value[(i - 200):(i - 1)])) {
    SPmoving_avrg[i] <- FALSE
  }
}
# Initialize empty vectors
under <- c()
over <- c()

# Identify positions of first FALSE after TRUE and first TRUE after FALSE
for (i in 2:length(SPmoving_avrg)) {
  if (SPmoving_avrg[i] == FALSE && SPmoving_avrg[i-1] == TRUE) {
    under <- c(under, i)
  }
  if (SPmoving_avrg[i] == TRUE && SPmoving_avrg[i-1] == FALSE) {
    over <- c(over, i)
  }
}

SP_return <- SP
cSP <- SP
for(i in 1:nrow(SP)) {
  SP_return$SP500[i] <- (SP$SP500[i+ 1] - SP$SP500[i])/SP$SP500[i]
}
SP_return <- SP_return[-nrow(SP_return),]
SP_return$return3x <- SP_return$SP500*3
#### Gold unter 200sma
gold$return <- c(rep(0, length(gold$X.NAME.)))
#Gold Returns berechnen
for(i in 2:nrow(gold)) {
  gold$return[i] <- (gold$Gold.Bullion.LBM...t.oz.DELAY[i+ 1] - gold$Gold.Bullion.LBM...t.oz.DELAY[i])/gold$Gold.Bullion.LBM...t.oz.DELAY[i]
}
#Gold Länge anpassen
gold_1987 <- gold[-c(1:5216, nrow(gold)),]
for(i in 1:length(over)) {
  SP_return$return3x[under[i]:over[i]] <- gold_1987$return[under[i]:over[i]]
}
SP_return$return3x[under[length(under)]:nrow(SP_return)] <- gold_1987$return[under[length(under)]:nrow(SP_return)]

#leverage 3x
SP_return<- SP_return[-1,]
SP <- rep(100, nrow(SP_return) + 1)
SP3x <- rep(100, nrow(SP_return) + 1)
for(i in 1:(nrow(SP_return))) {
  SP[i + 1] <- SP[i] * (1 + SP_return$SP500[i])
  SP3x[i + 1] <- SP3x[i] * (1 + SP_return$return3x[i])
}
indices$SP3xLev <- c(100, SP3x)
indices$Gold <- gold$Gold.Bullion.LBM...t.oz.DELAY[-c(1:5216)]



#### Cash unter 200sma
cSP_return <- cSP

for(i in 1:nrow(cSP)) {
  cSP_return$SP500[i] <- (cSP$SP500[i+ 1] - cSP$SP500[i])/cSP$SP500[i]
}
cSP_return <- cSP_return[-nrow(cSP_return),]
cSP_return$return3x <- cSP_return$SP500*3
#Gold Länge anpassen
for(i in 1:length(over)) {
  cSP_return$return3x[under[i]:over[i]] <- 0
}
cSP_return$return3x[under[length(under)]:nrow(cSP_return)] <-0

#leverage 3x
cSP_return <- cSP_return[-1,]
cSP <- rep(100, nrow(cSP_return) + 1)
cSP3x <- rep(100, nrow(cSP_return) + 1)
for(i in 1:(nrow(cSP_return))) {
  cSP[i + 1] <- cSP[i] * (1 + cSP_return$SP500[i])
  cSP3x[i + 1] <- cSP3x[i] * (1 + cSP_return$return3x[i])
}
indices$cSP3xLev <- c(100, cSP3x)

########################
#Evolutionary Portfolio Theory
# Kurs erstellen
evo_stocks <- indices$WORLD/indices$WORLD[1]*100*0.87 + indices$EM/indices$EM[1]*100*0.13 
evo_fixedIncome <- indices$dailyyield
# Regimewechsel bestimmen
low20 <- c()
low40 <- c()
ath <- c()

for (i in 1:length(evo_stocks)) {
  if (evo_stocks[i] == max(evo_stocks[1:i])) {
    ath <- c(ath, i)
  }
  if (i > 1 && evo_stocks[i] < 0.8 * max(evo_stocks[1:(i-1)]) && all(evo_stocks[which(evo_stocks == evo_stocks[tail(ath, n = 1L)]) : (i - 1)] >= 0.8 * max(evo_stocks[1:(i-1)]))) {
    low20 <- c(low20, i)
  }
  if (i > 1 && evo_stocks[i] < 0.6 * max(evo_stocks[1:(i-1)]) && all(evo_stocks[which(evo_stocks == evo_stocks[tail(ath, n = 1L)]) : (i - 1)] >= 0.6 * max(evo_stocks[1:(i-1)]))) {
    low40 <- c(low40, i)
  }
}

low20  # Stellen mit mehr als 20% Tiefpunkt
low40  # Stellen mit mehr als 40% Tiefpunkt
ath    # Stellen mit neuen Allzeithochs

# die neuen ATHs nach 20% drops bestimmen
ath_store <- c()
for(i in 1:length(low20)) {
  ath_store <- c(ath_store, ath[which(sort(c(ath, low20)) == low20[i]) + 1])
}
evo_stock_returns <- rep(1, length(evo_stocks))
for (i in 1:(length(evo_stock_returns) - 1)) {
  evo_stock_returns[i] <-  (evo_stocks[i + 1] - evo_stocks[i])/evo_stocks[i]
}
evo_stock_returns[length(evo_stock_returns)] <- (evo_stocks[length(evo_stocks)]-evo_stocks[length(evo_stocks) - 1]) / evo_stocks[length(evo_stocks) - 1]
evo_portfolio <- c(100, rep(0, nrow(indices) - 1))

#Funktion um die verschiedenen Regime anzuwenden
apply_returns <- function(start, end, regime, evo_portfolio) {
  if(regime == 1) {
    for(i in (start + 1) : end) {
      evo_portfolio[i] <- evo_portfolio[i - 1] * (1 + (evo_stock_returns[i]*0.8 + evo_fixedIncome[i] * 0.2))
    }
  }
  if(regime == 2) {
    for(i in (start + 1) : end) {
      evo_portfolio[i] <- evo_portfolio[i - 1] * (1 + evo_stock_returns[i])
    }
  }
  if(regime == 3) {
    for(i in (start + 1) : end) {
      evo_portfolio[i] <- evo_portfolio[i - 1] * (1 + (evo_stock_returns[i]*1.2 - evo_fixedIncome[i] * 0.2))
    }
  }
  return(evo_portfolio)
}


#Funktion händisch Auf die verschiedenen Phasen anwenden
evo_portfolio <- apply_returns(1, low20[1], 1, evo_portfolio)
evo_portfolio <- apply_returns(low20[1], ath_store[1], 2, evo_portfolio)
evo_portfolio <- apply_returns(ath_store[1], low20[2], 1, evo_portfolio)
evo_portfolio <- apply_returns(low20[2], ath_store[2], 2, evo_portfolio)
evo_portfolio <- apply_returns(ath_store[2], low20[3], 1, evo_portfolio)
evo_portfolio <- apply_returns(low20[3], low40[1], 2, evo_portfolio)
evo_portfolio <- apply_returns(low40[1], ath_store[3], 3, evo_portfolio)
evo_portfolio <- apply_returns(ath_store[3], low20[4], 1, evo_portfolio)
evo_portfolio <- apply_returns(low20[4], low40[2], 2, evo_portfolio)
evo_portfolio <- apply_returns(low40[2], ath_store[4], 3, evo_portfolio)
evo_portfolio <- apply_returns(ath_store[4], low20[5], 1, evo_portfolio)
evo_portfolio <- apply_returns(low20[5], ath_store[5], 2, evo_portfolio)
evo_portfolio <- apply_returns(ath_store[5], low20[6], 1, evo_portfolio)
evo_portfolio <- apply_returns(low20[6], ath_store[6], 2, evo_portfolio)
evo_portfolio <- apply_returns(ath_store[6], low20[7], 1, evo_portfolio)
evo_portfolio <- apply_returns(low20[6], length(evo_portfolio), 2, evo_portfolio)

#evoPortfolio zu indices hinzufügen
indices$evoPortfolio <- evo_portfolio

########################
## Jährliche Returns

# Filtern der Zeilen für den letzten Tag eines Jahres
Indices_last_day <- indices %>%
  group_by(Year) %>%
  filter(date == max(date)) %>%
  ungroup()

# Neue Datenframe für Renditen erstellen
Returns <- data.frame(Year = Indices_last_day$Year)

# Schleife über alle Spalten außer der ersten (Jahres-Spalte)
for (i in 2:ncol(Indices_last_day)) {
  col_name <- colnames(Indices_last_day)[i]  # Name der aktuellen Spalte
  Returns[col_name] <- NA  # Neue Spalte in "Returns" für die Renditen erstellen
  
  # Schleife über alle Jahre
  for (j in 2:nrow(Indices_last_day)) {
    current_year <- Indices_last_day$Year[j]  # Aktuelles Jahr
    previous_year <- current_year - 1  # Vorheriges Jahr
    
    # Kurs vom aktuellen und vorherigen Jahr abrufen
    current_price <- Indices_last_day[j, col_name]
    previous_price <- Indices_last_day[Indices_last_day$Year == previous_year, col_name]
    
    # Rendite berechnen und in "Returns" speichern
    Returns[j, col_name] <- (current_price - previous_price) / previous_price
  }
}
Returns <- Returns[-1,]
Returns$Year <- c(1988:2022)
# Ergebnis anzeigen
View(Returns)

YearlyReturns1988 <- Returns[-c(1, 12, 13)]
YearlyReturns1996 <- Returns[-c(1:8), -c(12, 13, 15)]






##############
## Monatlich
# Monatliche Returns
# Neue Spalte für das Jahr und den Monat erstellen
indices$Year <- as.numeric(format(indices$date, "%Y"))
indices$Month <- as.numeric(format(indices$date, "%m"))

# Filtern der Zeilen für den letzten Tag eines Monats
Indices_last_day <- indices %>%
  group_by(Year, Month) %>%
  filter(date == max(date)) %>%
  ungroup()

# Neue Datenframe für Renditen erstellen
Returns <- data.frame(Year = Indices_last_day$Year, Month = Indices_last_day$Month)

# Schleife über alle Spalten außer den ersten beiden (Jahr und Monat)
for (i in 2:ncol(Indices_last_day)) {
  col_name <- colnames(Indices_last_day)[i]  # Name der aktuellen Spalte
  Returns[col_name] <- NA  # Neue Spalte in "Returns" für die Renditen erstellen
  
  # Schleife über alle Zeilen
  for (j in 2:nrow(Indices_last_day)) {
    current_year <- Indices_last_day$Year[j]  # Aktuelles Jahr
    current_month <- Indices_last_day$Month[j]  # Aktueller Monat
    previous_year <- ifelse(current_month == 1, current_year - 1, current_year)  # Vorheriges Jahr
    previous_month <- ifelse(current_month == 1, 12, current_month - 1)  # Vorheriger Monat
    
    # Kurs vom aktuellen und vorherigen Jahr und Monat abrufen
    current_price <- Indices_last_day[j, col_name]
    previous_price <- Indices_last_day[Indices_last_day$Year == previous_year & Indices_last_day$Month == previous_month, col_name]
    
    # Rendite berechnen und in "Returns" speichern
    Returns[j, col_name] <- (current_price - previous_price) / previous_price
  }
}

Returns <- Returns[-1,]
Returns$Year <- rep(1988:2022, each = 12)
Returns$Month <- c(1:12)
View(Returns)


MonthlyReturns1988 <- Returns[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 17, 18, 19, 20, 21)]
MonthlyReturns1996 <- Returns[-c(1:(8*12)),c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 17, 18, 19, 20, 21)]
########
#pureValue
pureValue <- na.omit(pureValue[-c(1 : (which(pureValue$X.NAME. == "29.12.95") - 1), (which(pureValue$X.NAME. == "02.01.23")) : length(pureValue$X.NAME.)),c(1,3)])
colnames(pureValue) <- c("date", "pureValue")
indices1996 <- cbind(indices[-c(1:(which(indices$date == "1996-01-01") - 1)),], pureValue$pureValue[-1])

#yearly
## Jährliche Returns
pureValue$date <- as.Date(pureValue$date, format = "%d.%m.%y")
pureValue$Year <- as.numeric(format(pureValue$date, "%Y"))
# Filtern der Zeilen für den letzten Tag eines Jahres
Indices_last_day <- pureValue %>%
  group_by(Year) %>%
  filter(date == max(date)) %>%
  ungroup()

# Neue Datenframe für Renditen erstellen
Returns <- data.frame(Year = Indices_last_day$Year)

# Schleife über alle Spalten außer der ersten (Jahres-Spalte)
for (i in 2:ncol(Indices_last_day)) {
  col_name <- colnames(Indices_last_day)[i]  # Name der aktuellen Spalte
  Returns[col_name] <- NA  # Neue Spalte in "Returns" für die Renditen erstellen
  
  # Schleife über alle Jahre
  for (j in 2:nrow(Indices_last_day)) {
    current_year <- Indices_last_day$Year[j]  # Aktuelles Jahr
    previous_year <- current_year - 1  # Vorheriges Jahr
    
    # Kurs vom aktuellen und vorherigen Jahr abrufen
    current_price <- Indices_last_day[j, col_name]
    previous_price <- Indices_last_day[Indices_last_day$Year == previous_year, col_name]
    
    # Rendite berechnen und in "Returns" speichern
    Returns[j, col_name] <- (current_price - previous_price) / previous_price
  }
}
Returns <- Returns[-1,]
Returns$Year <- c(1996:2022)
# Ergebnis anzeigen
View(Returns)

YearlyReturns1996 <- cbind(YearlyReturns1996[-1], Returns[2])


##Monatliche Returns
pureValue <- read.csv2("purevalue.csv")
pureValue <- na.omit(pureValue[-c(1 : (which(pureValue$X.NAME. == "29.12.95") - 1)),c(1,3)])
colnames(pureValue) <- c("date", "pureValue")
pureValue <- pureValue[c(1:which(pureValue$date == "30.12.22")),]
pureValue$date <- as.Date(pureValue$date, format = "%d.%m.%y")
pureValue$Year <- as.numeric(format(pureValue$date, "%Y"))
pureValue$Month <- as.numeric(format(pureValue$date, "%m"))

# Filtern der Zeilen für den letzten Tag eines Monats
Indices_last_day <- pureValue %>%
  group_by(Year, Month) %>%
  filter(date == max(date)) %>%
  ungroup()

# Neue Datenframe für Renditen erstellen
Returns <- data.frame(Year = Indices_last_day$Year, Month = Indices_last_day$Month)

# Schleife über alle Spalten außer den ersten beiden (Jahr und Monat)
for (i in 2:ncol(Indices_last_day)) {
  col_name <- colnames(Indices_last_day)[i]  # Name der aktuellen Spalte
  Returns[col_name] <- NA  # Neue Spalte in "Returns" für die Renditen erstellen
  
  # Schleife über alle Zeilen
  for (j in 2:nrow(Indices_last_day)) {
    current_year <- Indices_last_day$Year[j]  # Aktuelles Jahr
    current_month <- Indices_last_day$Month[j]  # Aktueller Monat
    previous_year <- ifelse(current_month == 1, current_year - 1, current_year)  # Vorheriges Jahr
    previous_month <- ifelse(current_month == 1, 12, current_month - 1)  # Vorheriger Monat
    
    # Kurs vom aktuellen und vorherigen Jahr und Monat abrufen
    current_price <- Indices_last_day[j, col_name]
    previous_price <- Indices_last_day[Indices_last_day$Year == previous_year & Indices_last_day$Month == previous_month, col_name]
    
    # Rendite berechnen und in "Returns" speichern
    Returns[j, col_name] <- (current_price - previous_price) / previous_price
  }
}

Returns <- Returns[-1,]
Returns$Year <- rep(1996:2022, each = 12)
Returns$Month <- c(1:12)
View(Returns)

MonthlyReturns1996 <- cbind(MonthlyReturns1996 , Returns$pureValue)






