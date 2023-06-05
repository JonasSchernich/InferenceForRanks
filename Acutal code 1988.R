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
#Jährliche Returns
# Neue Spalte für das Jahr erstellen
indices_store <- indices
indices <- indices_store






# Filtern der Zeilen für den letzten Tag eines Jahres
Indices_last_day <- indices %>%
  group_by(Year) %>%
  filter(date == max(date)) %>%
  ungroup()

# Ergebnis anzeigen
#View(Indices_last_day)

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




########################
# Returns Cov Matrix
cov_matrix <- cov(Returns[-c(1)])



# Durchschnitt der Renditen berechnen
average_returns <- lapply(Returns[-c(1)], mean)



# Varianz des Durchschnitts berechnen
#variance_of_average <- t(average_returns) %*% cov_matrix %*% average_returns

# Ergebnis ausgeben
#print(variance_of_average)




#######################
#HAC ZEUG
HACvar <- function(vector) {
  return(vcovHAC(lm(vector ~ 1))[1,1])
}
hacdf <- data.frame(mean = colMeans(Returns[-c(1,2)]), HACVar =  unname(unlist(lapply(Returns[-c(1, 2)], function(x) HACvar(x)))))
hacdf
cov_matrix[1, 1] <- hacdf$HACVar[1]



########

return_estimates_transposed <- data.frame(mean=unlist(lapply(Returns[-c(1)], mean)), variance = diag(cov_matrix))
return_estimates_transposed <- cbind(jurisdiction = rownames(return_estimates_transposed), return_estimates_transposed)
rownames(return_estimates_transposed) <- NULL
irank(return_estimates_transposed$mean)

return_cov_mat <- diag(return_estimates_transposed$variance^2)
CS_marg <- csranks(return_estimates_transposed$mean, cov_matrix/nrow(Returns), coverage=0.95, simul=FALSE, R=1000, seed=101)
return_rankL_marg <- CS_marg$L
return_rankU_marg <- CS_marg$U

grid::current.viewport()

plotmarg <- plot(CS_marg, popnames = return_estimates_transposed$jurisdiction, title = "Ranking of expected Portfolio Returns", 
                 subtitle = "(with 95% marginal confidence sets)", colorbins=4)
plotmarg

CS_simul <- csranks(return_estimates_transposed$mean, cov_matrix, coverage=0.95, simul=TRUE, R=1000, seed=101)
math_rankL_simul <- CS_simul$L
math_rankU_simul <- CS_simul$U


grid::current.viewport()

plotsimul <- plot(CS_simul, popnames = return_estimates_transposed$jurisdiction, title="Ranking of expected Portfolio Returns", 
                  subtitle="(with 95% simultaneous confidence sets)", colorbins=4)

plotsimul



##############
#Monatlich
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
Returns$Year <- c(1988:2022)
Returns$Month <- c(1:12)
View(Returns)


Returns <- Returns[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 17, 18, 19, 20)]

cov_matrix <- cov(Returns)

# Durchschnitt der Renditen berechnen
average_returns <- lapply(Returns, mean)




########

return_estimates_transposed <- data.frame(mean=unlist(lapply(Returns, mean)), variance = diag(cov_matrix))
return_estimates_transposed <- cbind(jurisdiction = rownames(return_estimates_transposed), return_estimates_transposed)
rownames(return_estimates_transposed) <- NULL
irank(return_estimates_transposed$mean)

return_cov_mat <- diag(return_estimates_transposed$variance^2)
CS_marg <- csranks(return_estimates_transposed$mean, cov_matrix/nrow(Returns), coverage=0.95, simul=FALSE, R=1000, seed=101)
return_rankL_marg <- CS_marg$L
return_rankU_marg <- CS_marg$U

grid::current.viewport()

plotmarg <- plot(CS_marg, popnames = return_estimates_transposed$jurisdiction, title = "Ranking of expected Portfolio Returns", 
                 subtitle = "(with 95% marginal confidence sets)", colorbins=4)
plotmarg

CS_simul <- csranks(return_estimates_transposed$mean, cov_matrix, coverage=0.95, simul=TRUE, R=1000, seed=101)
math_rankL_simul <- CS_simul$L
math_rankU_simul <- CS_simul$U


grid::current.viewport()

plotsimul <- plot(CS_simul, popnames = return_estimates_transposed$jurisdiction, title="Ranking of expected Portfolio Returns", 
                  subtitle="(with 95% simultaneous confidence sets)", colorbins=4)

plotsimul


