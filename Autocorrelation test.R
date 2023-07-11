library(car)
library(stats)
#Creating a list with all the time series
timeseries <- list(Monthly_Return_MSCI_World_1970 =  MonthlyReturns1970$World, Monthly_Return_SP_500 = MonthlyReturns1970$SP500, 
                   Monthly_Return_DAX_1970 = MonthlyReturns1970$DAX, Monthly_Return_HANG_SENG_1970 = MonthlyReturns1970$HANGSENG,
                   Monthly_Return_Nikkei_225_1970 = MonthlyReturns1970$Nikkei, Monthly_Return_Gold_1970 = MonthlyReturns1970$Gold,
                   Monthly_Return_SP500_Lev_Gold_1970 = MonthlyReturns1970$SP3xLev, Monthly_Return_SP500_Lev_Cash_1970 = YearlyReturns1970$cSP3xLev,
                   Yearly_Return_MSCI_World_1970 =  YearlyReturns1970$World, Yearly_Return_SP500_1970 = YearlyReturns1970$SP500, 
                   Yearly_Return_DAX_1970 = YearlyReturns1970$DAX, Yearly_Return_HANG_SENG_1970 = YearlyReturns1970$HANGSENG,
                   Yearly_Return_Nikkei_225_1970 = YearlyReturns1970$Nikkei, Yearly_Return_Gold_1970 = YearlyReturns1970$Gold,
                   Yearly_Return_SP500_Lev_Gold_1970 = YearlyReturns1970$SP3xLev, Yearly_Return_SP500_Lev_Cash_1970 = YearlyReturns1970$cSP3xLev,
                   Monthly_Return_MSCI_World_1988 = MonthlyReturns1988$WORLD, Monthly_Return_EM_1988 = MonthlyReturns1988$EM, 
                   Monthly_Return_FTSE_1988 = MonthlyReturns1988$FTSE, Monthly_Return_EURO_1988 = MonthlyReturns1988$EURO, 
                   Monthly_Return_NASDAQ_1988 = MonthlyReturns1988$NASDAQ, Monthly_Return_SP5001988 = MonthlyReturns1988$SP500,
                   Monthly_Return_DAX_1988 = MonthlyReturns1988$DAX, Monthly_Return_HANG_SENG_1988 = MonthlyReturns1988$HANGSENG,
                   Monthly_Return_SP500_Value_1988 = MonthlyReturns1988$SP500VALUE, Monthly_Return_SP500_Growth_1988 = MonthlyReturns1988$SP500GROWTH,
                   Monthly_Return_Nikkei_225_1988 = MonthlyReturns1988$Nikkei, Monthly_Return_Low_Beta_1988 = MonthlyReturns1988$`Low Beta`,
                   Monthly_Return_SP500_Lev_Gold_1988 = MonthlyReturns1988$SP3xLev,  Monthly_Return_Gold_1988 = MonthlyReturns1988$Gold,
                   Monthly_Return_SP500_Lev_Cash_1988 = MonthlyReturns1988$cSP3xLev, Monthly_Return_Evolutionary_Portfolio_1988 = MonthlyReturns1988$evoPortfolio,
                   Yearly_Return_MSCI_World_1988 = YearlyReturns1988$WORLD, Yearly_Return_EM_1988 = YearlyReturns1988$EM, 
                   Yearly_Return_FTSE_1988 = YearlyReturns1988$FTSE, Yearly_Return_EURO_1988 = YearlyReturns1988$EURO, 
                   Yearly_Return_NASDAQ_1988 = YearlyReturns1988$NASDAQ, Yearly_Return_SP5001988 = YearlyReturns1988$SP500,
                   Yearly_Return_DAX_1988 = YearlyReturns1988$DAX, Yearly_Return_HANG_SENG_1988 = YearlyReturns1988$HANGSENG,
                   Yearly_Return_SP500_Value_1988 = YearlyReturns1988$SP500VALUE, Yearly_Return_SP500_Growth_1988 = YearlyReturns1988$SP500GROWTH,
                   Yearly_Return_Nikkei_225_1988 = YearlyReturns1988$Nikkei, Yearly_Return_Low_Beta_1988 = YearlyReturns1988$`Low Beta`,
                   Yearly_Return_SP500_Lev_Gold_1988 = YearlyReturns1988$SP3xLev, Yearly_Return_Gold_1988 = YearlyReturns1988$Gold,
                   Yearly_Return_SP500_Lev_Cash_1988 = YearlyReturns1988$cSP3xLev, Yearly_Return_Evolutionary_Portfolio_1988 = YearlyReturns1988$evoPortfolio,
                   Monthly_Return_MSCI_World_1996 = MonthlyReturns1996$WORLD, Monthly_Return_EM_1996 = MonthlyReturns1996$EM, 
                   Monthly_Return_FTSE_1996 = MonthlyReturns1996$FTSE, Monthly_Return_EURO_1996 = MonthlyReturns1996$EURO, 
                   Monthly_Return_NASDAQ_1996 = MonthlyReturns1996$NASDAQ, Monthly_Return_SP5001996 = MonthlyReturns1996$SP500,
                   Monthly_Return_DAX_1996 = MonthlyReturns1996$DAX, Monthly_Return_HANG_SENG_1996 = MonthlyReturns1996$HANGSENG,
                   Monthly_Return_SP500_Value_1996 = MonthlyReturns1996$SP500VALUE, Monthly_Return_SP500_Growth_1996 = MonthlyReturns1996$SP500GROWTH,
                   Monthly_Return_Nikkei_225_1996 = MonthlyReturns1996$Nikkei, Monthly_Return_Low_Beta_1996 = MonthlyReturns1996$`Low Beta`,
                   Monthly_Return_SP500_Lev_Gold_1996 = MonthlyReturns1996$SP3xLev, Monthly_Return_Gold_1996 = MonthlyReturns1996$Gold,
                   Monthly_Return_SP500_Lev_Cash_1996 = MonthlyReturns1996$cSP3xLev, Monthly_Return_Evolutionary_Portfolio_1996 = MonthlyReturns1996$evoPortfolio, 
                   Monthly_Return_SP500_Small_Cap_Pure_Value_1996 = MonthlyReturns1996$`Returns$pureValue`, Yearly_Return_MSCI_World_1996 = YearlyReturns1996$WORLD, 
                   Yearly_Return_EM_1996 = YearlyReturns1996$EM, 
                   Yearly_Return_FTSE_1996 = YearlyReturns1996$FTSE, Yearly_Return_EURO_1996 = YearlyReturns1996$EURO, 
                   Yearly_Return_NASDAQ_1996 = YearlyReturns1996$NASDAQ, Yearly_Return_SP5001996 = YearlyReturns1996$SP500,
                   Yearly_Return_DAX_1996 = YearlyReturns1996$DAX, Yearly_Return_HANG_SENG_1996 = YearlyReturns1996$HANGSENG,
                   Yearly_Return_SP500_Value_1996 = YearlyReturns1996$SP500VALUE, Yearly_Return_SP500_Growth_1996 = YearlyReturns1996$SP500GROWTH,
                   Yearly_Return_Nikkei_225_1996 = YearlyReturns1996$Nikkei, Yearly_Return_Low_Beta_1996 = YearlyReturns1996$`Low Beta`,
                   Yearly_Return_SP500_Lev_Gold_1996 = YearlyReturns1996$SP3xLev, Yearly_Return_Gold_1996 = YearlyReturns1996$Gold,
                   Yearly_Return_SP500_Lev_Cash_1996 = YearlyReturns1996$cSP3xLev, Yearly_Return_Evolutionary_Portfolio_1996 = YearlyReturns1996$evoPortfolio, 
                   Yearly_Return_SP500_Small_Cap_Pure_Value_1996 = YearlyReturns1996$pureValue)
#Creating Dataframes for the results
ljung_box_results <- data.frame(Portfolio = character(), P.Value = numeric())
durbin_watson_results <- data.frame(Portfolio = character(), DW_Statistic = numeric(), P.Value = numeric())
for(i in seq_along(timeseries)) {
  ##Ljung-Box Test
  # Creating AR1 ARIMA Modell for
  model <- arima(timeseries[[i]], order = c(1, 0, 0))  # Hier verwenden wir ein AR(1)-Modell, aber du kannst die Ordnung entsprechend anpassen
  
  # Testing for the residuals and storing results
  ljung_box <- Box.test(model$residuals, lag = 5, type = "Ljung-Box")
  ljung_box_results <- rbind(ljung_box_results, c(names(timeseries[i]), ljung_box$p.value))
  
  ## Durbin Watson Test
  # Creating lagged portfolio return
  stock_prices_lag <- c(NA,timeseries[[i]][-length(timeseries[[i]])])
  
  # creating data frame with both time series
  df <- data.frame(timeseries[[i]], stock_prices_lag)
  
  # eliminating NAs
  df <- na.omit(df)
  
  # Regression
  model <- lm(timeseries[[i]][-1] ~ stock_prices_lag, data = df)
  
  # Storing results
  dw_result <- durbinWatsonTest(model)
  durbin_watson_results <- rbind(durbin_watson_results, c(names(timeseries[i]), round(as.numeric(dw_result$dw),3), round(as.numeric(dw_result$p), 4)))
}
#Renaming
colnames(ljung_box_results) <- c("Portfolio", "P-Value")
colnames(durbin_watson_results) <- c("Portfolio", "DW_Statistic","P-Value")
autocorr_results <- durbin_watson_results
autocorr_results$P_Value_Ljung_Box <-round(as.numeric(ljung_box_results$`P-Value`), 3)






