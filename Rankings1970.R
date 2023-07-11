setwd("/Users/jonasschernich/Uni/6. Semester/Bachelorarbeit/InferenceForRanks")
library(tidyverse)
library(tidyr)
library(dplyr)
library(csranks)
#library(lubridate)
#library(sandwich)
library(lmtest)
library(csranks)
#Einlesen
Nikkei70 <- read.csv2("Nikkei.csv")[-c(1:5218),]
indices70 <- cbind(read.csv2("Indizes.csv")[-1, c(1, 2, 11, 12, 16)], Nikkei70$NIKKEI.225.STOCK.AVERAGE...PRICE.INDEX)
gold <- read.csv2("Gold.csv")
indices70$gold <- na.omit(gold)[-c(1:521),2]
colnames(indices70) <- c("date", "World", "SP500", "DAX", "HANGSENG", "Nikkei", "Gold")
indices70$date <- as.Date(strptime(indices70$date, format = "%d.%m.%y"))


for (i in 1:nrow(indices70)) {
  indices70$SP500[i] <- gsub(",", ".", indices70$SP500[i] )
  indices70$DAX[i] <- gsub(",", ".", indices70$DAX[i] )
  indices70$HANGSENG[i] <- gsub(",", ".", indices70$HANGSENG[i] )
}

indices70$SP500 <- as.numeric(indices70$SP500)
indices70$DAX <- as.numeric(indices70$DAX)
indices70$HANGSENG <- as.numeric(indices70$HANGSENG)


# Normieren 
for(i in 2 : ncol(indices70)) {
  indices70[i] <- indices70[i] / indices70[1, i] * 100
}

########
#SP Lev
SP <- indices70[c(1,3)]
sp_value <- indices70$SP500
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
for (i in 2 : length(SPmoving_avrg)) {
  if (SPmoving_avrg[i] == FALSE && SPmoving_avrg[i-1] == TRUE) {
    under <- c(under, i)
  }
  if (SPmoving_avrg[i] == TRUE && SPmoving_avrg[i-1] == FALSE) {
    over <- c(over, i)
  }
}

SP_return <- SP
cSP <- SP
for(i in 1 : nrow(SP)) {
  SP_return$SP500[i] <- (SP$SP500[i+ 1] - SP$SP500[i])/SP$SP500[i]
}
SP_return <- SP_return[-nrow(SP_return),]
SP_return$return3x <- SP_return$SP500 * 3
#### Gold unter 200sma
gold$return <- c(rep(0, length(gold$X.NAME.)))
#Gold Returns berechnen
for(i in 2 : nrow(gold)) {
  gold$return[i] <- (gold$Gold.Bullion.LBM...t.oz.DELAY[i+ 1] - gold$Gold.Bullion.LBM...t.oz.DELAY[i])/gold$Gold.Bullion.LBM...t.oz.DELAY[i]
}
#Gold Länge anpassen
gold_1970 <- gold[-c(1 : 5216, nrow(gold)),]
for(i in 1 : length(over)) {
  SP_return$return3x[under[i]:over[i]] <- gold_1970$return[under[i] : over[i]]
}
SP_return$return3x[under[length(under)] : nrow(SP_return)] <- gold_1970$return[under[length(under)]:nrow(SP_return)]

#leverage 3x
SP_return<- SP_return[-1,]
SP <- rep(100, nrow(SP_return) + 1)
SP3x <- rep(100, nrow(SP_return) + 1)
for(i in 1:(nrow(SP_return))) {
  SP[i + 1] <- SP[i] * (1 + SP_return$SP500[i])
  SP3x[i + 1] <- SP3x[i] * (1 + SP_return$return3x[i])
}
indices70$SP3xLev <- c(100, SP3x)




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
indices70$cSP3xLev <- c(100, cSP3x)

########################
indices70first <- indices70[1,]
indices70first$date <- indices70first$date - 1
indices70 <- rbind(indices70first, indices70)
indices70$Year <- as.numeric(format(indices70$date, "%Y"))
indices70$Month <- as.numeric(format(indices70$date, "%m"))
# Jährliche Returns
yearlast <- indices70[!duplicated(indices70$Year, fromLast = TRUE), ]
YearlyReturns1970 <- yearlast
for(i in 2 : 9) {
  for(j in 2 : nrow(yearlast)) {
    YearlyReturns1970[j, i] <- (yearlast[j, i] -  yearlast[j - 1, i])/yearlast[j - 1,i]
  }
}
YearlyReturns1970 <- YearlyReturns1970[-1,-c(10:11)]
YearlyReturns1970$date <- 1970:2022
colnames(YearlyReturns1970)[1] <- "Year"




# Monatsweise letzte Zeilen extrahieren
monthlast <- indices70 %>%
  group_by(Year, Month) %>%
  filter(date == max(date)) %>%
  ungroup()

MonthlyReturns1970 <- monthlast
for(i in 2 : 9) {
  for(j in 2 : nrow(monthlast)) {
    MonthlyReturns1970[j, i] <- (monthlast[j, i] -  monthlast[j - 1, i])/monthlast[j - 1,i]
  }
}

MonthlyReturns1970 <- MonthlyReturns1970[-1, -c(10:11)]
MonthlyReturns1970$date <- c(1:nrow(MonthlyReturns1970))
colnames(MonthlyReturns1970)[1] <- "Month"


weeklast <- indices70[seq(1, nrow(indices70), by = 5), ]
WeeklyReturns1970 <- weeklast
for(i in 2 : 9) {
  for(j in 2 : nrow(WeeklyReturns1970)) {
    WeeklyReturns1970[j, i] <- (weeklast[j, i] -  weeklast[j - 1, i])/weeklast[j - 1,i]
  }
}

WeeklyReturns1970 <- WeeklyReturns1970[-1, -c(10:11)]
WeeklyReturns1970$date <- c(1:nrow(WeeklyReturns1970))
colnames(WeeklyReturns1970)[1] <- "Week"




















