library(tidyverse)
library(dplyr)
library(csranks)
other_indices <- read.csv2("AndereIndices.csv")
pacific <- read.csv2("Pacific.csv")
gold <- read.csv2("Gold.csv")
gold <- na.omit(gold)
Nikkei <- read.csv2("Nikkei.csv")
pureValue <- read.csv2("purevalue.csv")
indices_isin <- read.csv2("Indizes.csv")
colnames(indices_isin) <- c("date", "WORLD", "EM", "FTSEDIV", "FTSEPER", "FTSE", "EURODIV", "EUROPER", "EURO", "NASDAQ", "SP500", "DAX", "SPSMALL600VALUE", "HANGSENGDIV", "HANGSENGPER", "HANGSENG", "SP500VALUE", "SP500GROWTH")
indices<- indices_isin[-c(1:4696),c(1, 2, 3, 6, 9, 10, 11, 12, 16, 17, 18)]

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
for(i in 1:nrow(SP)) {
  SP_return$SP500[i] <- (SP$SP500[i+ 1] - SP$SP500[i])/SP$SP500[i]
}
SP_return <- SP_return[-nrow(SP_return),]
gold$return <- c(rep(0, length(gold$X.NAME.)))
for(i in 2:nrow(gold)) {
  gold$return[i] <- (gold$Gold.Bullion.LBM...t.oz.DELAY[i+ 1] - gold$Gold.Bullion.LBM...t.oz.DELAY[i])/gold$Gold.Bullion.LBM...t.oz.DELAY[i]
}
gold_1987 <- gold[-c(1:5216, nrow(gold)),]
for(i in 1:length(over)) {
  SP_return$SP500[under[i]:over[i]] <- gold_1987$return[under[i]:over[i]]
}
SP_return$SP500[under[length(under)]:nrow(SP_return)] <- gold_1987$return[under[length(under)]:nrow(SP_return)]


#leverage 3x
SP_return$return3x <- SP_return$SP500*3
SP_return <- SP_return[-1,]

SP <- rep(100, nrow(SP_return) + 1)
SP3x <- rep(100, nrow(SP_return) + 1)
for(i in 1:(nrow(SP_return))) {
  SP[i + 1] <- SP[i] * (1 + SP_return$SP500[i])
  SP3x[i + 1] <- SP3x[i] * (1 + SP_return$return3x[i])
}
indices$SP3xLev <- c(100, SP3x)
indices$Gold <- gold$Gold.Bullion.LBM...t.oz.DELAY[-c(1:5216)]

####################################
##Anzahl Tage pro Monat
# Beispiel-Vektor mit Daten
indices$date <- as.Date(indices$date, format = "%d.%m.%y")
dates_vec <- indices$date

#Kurse auf 100 normieren
for(i in 2:ncol(indices)) {
  indices[[i]] <- indices[[i]]/indices[[i]][1]*100
}


########################
#Plots
plot(indices$date, log(indices$SP3xLev), type = "l", col = "blue", xlab = "Datum", ylab = "Aktienkurs",
     main = "Aktienkurse")
lines(indices$date, log(indices$Gold), col = "red")
lines(indices$date, log(indices$WORLD), col = "green")
legend("topleft", legend = c("3xLev", "Gold", "World"), col = c("blue", "red"), lty = 1)



########################
#Jährliche Returns
#Jährliche Returns
# Neue Spalte für das Jahr erstellen
indices$Year <- as.numeric(format(indices$date, "%Y"))

# Filtern der Zeilen für den letzten Tag eines Jahres
Indices_last_day <- indices %>%
  group_by(Year) %>%
  filter(date == max(date)) %>%
  ungroup()

# Ergebnis anzeigen
View(Indices_last_day)

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


#######################################





# Erstelle ein data.frame mit einer Spalte für die Daten
dates_df <- data.frame(date = as.Date(dates_vec))

# Gruppiere nach Jahr und Monat, zähle die Anzahl der Tage in jedem Monat und zeige das Ergebnis an
month_length <- dates_df %>%
  group_by(year = lubridate::year(date), month = lubridate::month(date, label = TRUE)) %>%
  summarise(num_days = n()) %>%
  ungroup()

# Jahre und Monate erstellen
years <- 1988:2022
months <- 1:12

# Leeren DataFrame erstellen
total_monthly_returns <- data.frame(Year = integer(), Month = integer(), WORLD = numeric(), EM = numeric(),
                                    FTSE = numeric(), EURO = numeric(), NASDAQ = numeric(), SP500 = numeric(),
                                    DAX = numeric(),  HANGSENG = numeric(),
                                    SP500VALUE = numeric(), SP500GROWTH = numeric())

# Schleife über Jahre und Monate
for (year in years) {
  for (month in months) {
    # Neue Zeile hinzufügen
    total_monthly_returns <- rbind(total_monthly_returns, c(year, month, rep(0, 11)))
  }
}

# Spaltennamen festlegen
colnames(total_monthly_returns) <- c("Year", "Month", colnames(indices[2:11]))
total_monthly_returns <- total_monthly_returns[-13]

# Liste mit allen Indizes
index_names <-colnames(indices[-1])

# Iteration über alle Indizes
for (index in index_names) {
  
  # Berechnung der monatlichen Renditen
  z <- cumsum(month_length$num_days)
  y <- c()
  for (i in 1:(length(month_length$num_days) - 1)) {
    
    y <- append(y, (indices[[index]][z[i + 1]] - indices[[index]][z[i]])/indices[[index]][z[i]])
  }
  
  
  total_monthly_returns[index] <- y
}

total_monthly_returns[3:ncol(total_monthly_returns)] <- total_monthly_returns[3:ncol(total_monthly_returns)]*10



######
# Beispiel-Daten: Liste von Renditen
returns <- list(World = total_monthly_returns$WORLD,
                EM = total_monthly_returns$EM,
                FTSE = total_monthly_returns$FTSE,
                EURO = total_monthly_returns$EURO,
                NASDAQ = total_monthly_returns$NASDAQ,
                SP500 = total_monthly_returns$SP500, 
                DAX = total_monthly_returns$DAX,
                Hangseng = total_monthly_returns$HANGSENG, 
                Value = total_monthly_returns$SP500VALUE,
                Growth = total_monthly_returns$SP500GROWTH)

# Durchschnitt der Renditen berechnen
average_returns <- sapply(returns, mean)

# Kovarianzmatrix unter Berücksichtigung der Abhängigkeitsstruktur schätzen
cov_matrix <- vcovHAC(returns, method = "white")

# Varianz des Durchschnitts berechnen
variance_of_average <- t(average_returns) %*% cov_matrix %*% average_returns

# Ergebnis ausgeben
print(variance_of_average)



returns_df <- data.frame(year = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), DAX = c(2, 3, 4, 2, 3, 4, 2, 3, 4, 2), SP500 = rep(1, 10), FTSE = rep(1.3, 10))




# Annahme: Du hast einen Dataframe namens "returns_df" mit den Spalten Year, Dax, S&P500, FTSE.

# Lade die erforderlichen Pakete (falls sie noch nicht installiert sind)
# install.packages(c("sandwich", "lmtest"))
library(sandwich)
library(lmtest)
HACvar <- function(vector) {
  return(vcovHAC(lm(vector ~ 1))[1,1])
}
hacdf <- data.frame(mean = colMeans(Returns[-1]), HACVar =  unname(unlist(lapply(Returns[-1], function(x) HACvar(x)))))
hacdf

return_estimates_transposed <-hacdf
return_estimates_transposed <- cbind(jurisdiction = rownames(return_estimates_transposed), return_estimates_transposed[c(1, 2)])
rownames(return_estimates_transposed) <- NULL
irank(return_estimates_transposed$mean)

return_cov_mat <- diag(return_estimates_transposed$HACVar^2)
CS_marg <- csranks(return_estimates_transposed$mean, return_cov_mat, coverage=0.95, simul=FALSE, R=1000, seed=101)
return_rankL_marg <- CS_marg$L
return_rankU_marg <- CS_marg$U

grid::current.viewport()

plotmarg <- plot(CS_marg, popnames = return_estimates_transposed$jurisdiction, title = "Ranking of expected Portfolio Returns", 
                 subtitle = "(with 95% marginal confidence sets)", colorbins=4)
plotmarg

CS_simul <- csranks(return_estimates_transposed$mean, return_cov_mat, coverage=0.95, simul=TRUE, R=1000, seed=101)
math_rankL_simul <- CS_simul$L
math_rankU_simul <- CS_simul$U


grid::current.viewport()

plotsimul <- plot(CS_simul, popnames = return_estimates_transposed$jurisdiction, title="Ranking of expected Portfolio Returns", 
                  subtitle="(with 95% simultaneous confidence sets)", colorbins=4)

plotsimul



