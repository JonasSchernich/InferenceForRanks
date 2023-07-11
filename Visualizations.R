library(ggplot2)
library(matrixStats)
library(gridExtra)

###### finishing up the data sets
MonthlyReturns1970 <- MonthlyReturns1970[-1]
YearlyReturns1970 <- YearlyReturns1970[-1]
YearlyReturns1988 <- YearlyReturns1988[-12]
colnames(MonthlyReturns1970) <- c("MSCI World", "S&P 500", "DAX", "Hang Seng", "Nikkei 225", "Gold", "S&P 500 3x & Gold", "S&P 500 3x & Cash")
colnames(YearlyReturns1970) <- c("MSCI World", "S&P 500", "DAX", "Hang Seng", "Nikkei 225", "Gold", "S&P 500 3x & Gold", "S&P 500 3x & Cash")
colnames(MonthlyReturns1988) <- c("MSCI World", "MSCI EM", "FTSE 100", "MSCI Europe", "NASDAQ-100", "S&P 500", "DAX", "Hang Seng", "S&P 500 Value", "S&P 500 Growth", "Nikkei 225", "Low Beta","S&P 500 3x & Gold", "Gold", "S&P 500 3x + Cash", "Evolutionary Portfolio")
colnames(YearlyReturns1988) <- c("MSCI World", "MSCI EM", "FTSE 100", "MSCI Europe", "NASDAQ-100", "S&P 500", "DAX", "Hang Seng", "S&P 500 Value", "S&P 500 Growth", "Nikkei 225", "Low Beta","S&P 500 3x & Gold", "Gold", "S&P 500 3x + Cash", "Evolutionary Portfolio")  
colnames(YearlyReturns1996) <- c("MSCI World", "MSCI EM", "FTSE 100", "MSCI Europe", "NASDAQ-100", "S&P 500", "DAX", "Hang Seng", "S&P 500 Value", "S&P 500 Growth", "Nikkei 225", "Low Beta","S&P 500 3x & Gold", "Gold", "S&P 500 3x + Cash", "Evolutionary Portfolio", "S&P Small Cap Pure Value")  
colnames(YearlyReturns1996) <- c("MSCI World", "MSCI EM", "FTSE 100", "MSCI Europe", "NASDAQ-100", "S&P 500", "DAX", "Hang Seng", "S&P 500 Value", "S&P 500 Growth", "Nikkei 225", "Low Beta","S&P 500 3x & Gold", "Gold", "S&P 500 3x + Cash", "Evolutionary Portfolio", "S&P Small Cap Pure Value")  
###########

#Functions
marg_CI <- function(Returns, title, conflevel) {
  return_estimates_transposed <- data.frame(mean=unlist(lapply(Returns, mean)), variance = diag(cov(Returns)/nrow(Returns)))
  return_estimates_transposed <- cbind(jurisdiction = rownames(return_estimates_transposed), return_estimates_transposed)
  rownames(return_estimates_transposed) <- NULL
  irank(return_estimates_transposed$mean)
  
  return_cov_mat <- diag(return_estimates_transposed$variance^2)
  CS_marg <- csranks(return_estimates_transposed$mean, cov(Returns)/nrow(Returns), coverage=conflevel, simul=FALSE, R=1000, seed=101)
  return_rankL_marg <- CS_marg$L
  return_rankU_marg <- CS_marg$U
  
  grid::current.viewport()
  
  plotmarg <- plot(CS_marg, popnames = return_estimates_transposed$jurisdiction, title = title, 
                   subtitle = "(with 5% marginal confidence sets)", colorbins=4)
  return(plotmarg)
}

sim_CI <- function(Returns, title, conflevel) {
  return_estimates_transposed <- data.frame(mean=unlist(lapply(Returns, mean)), variance = diag(cov(Returns) / nrow(Returns)))
  return_estimates_transposed <- cbind(jurisdiction = rownames(return_estimates_transposed), return_estimates_transposed)
  rownames(return_estimates_transposed) <- NULL
  irank(return_estimates_transposed$mean)
  return_cov_mat <- diag(return_estimates_transposed$variance^2)
  CS_simul <- csranks(return_estimates_transposed$mean, cov(Returns)/nrow(Returns), coverage=conflevel, simul=TRUE, R=1000, seed=101)
  math_rankL_simul <- CS_simul$L
  math_rankU_simul <- CS_simul$U
  grid::current.viewport()
  plotsimul <- plot(CS_simul, popnames = return_estimates_transposed$jurisdiction, title=title, 
                    subtitle="(with 5% simultaneous confidence sets)", colorbins=4)
  return(plotsimul)
}

###########
#1970

plot1970 <- data.frame(
  x = indices70$date,
  y1 = indices70$World,
  y2 = indices70$SP500,
  y3 = indices70$DAX,
  y4 = indices70$HANGSENG,
  y5 = indices70$Nikkei,
  y6 = indices70$Gold,
  y7 = indices70$SP3xLev,
  y8 = indices70$cSP3xLev
)


ggplot(plot1970, aes(x = x)) +
  geom_line(aes(y = y1, color = "Graph 1"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y2, color = "Graph 2"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y3, color = "Graph 3"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y4, color = "Graph 4"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y5, color = "Graph 5"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y6, color = "Graph 6"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y7, color = "Graph 7"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y8, color = "Graph 8"), linetype = "solid", linewidth = 1) +
  labs(x = "Year", y = "Portfolio Value", title = "Portfolios since 1970") +
  scale_y_continuous(trans = "log10", breaks = c(100, 1000, 10000, 100000, 1000000, 3000000), 
                     labels = c("100", "1,000", "10,000", "100,000", "1,000,000", "3,000,000")) +
  scale_color_manual(
    values = c("Graph 1" = "blue", "Graph 2" = "red", "Graph 3" = "green", "Graph 4" = "orange", "Graph 5" = "pink", "Graph 6" = "yellow", "Graph 7" = "black", "Graph 8" = "turquoise"),
    labels = c("MSCI World", "S&P 500", "DAX", "Hang Seng", "Nikkei 225", "Gold", "S&P 500 3x & Gold", "S&P 500 3x & Cash")
  ) +
  guides(color = guide_legend(title = "Portfolios"))+
  theme_minimal()

# Avrg performance and standard deviation bar charts
## Yearly 
#Mittelwerte erstellen
year1970mean <- colMeans(YearlyReturns1970[2:9])
names(year1970mean) <- c("MSCI World", "S&P 500", "DAX", "Hang Seng", "Nikkei 225", "Gold", "S&P 500 3x & Gold", "S&P 500 3x & Cash")
# Erstellen des Datenrahmens
year1970data <- data.frame(Kategorie = paste0("Portfolio ", 1:8), Mittelwert = year1970mean)
#sd erstellen
year1970sd <- colSds(as.matrix(YearlyReturns1970[2:9]))
year1970data$sd <- year1970sd
# Plotten des Balkendiagramms
ggplot(year1970data, aes(x = Kategorie, y = Mittelwert)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Portfolios", y = "Average Return", title = "Yearly Average Portfolio Returns") +
  theme_minimal()+
  scale_x_discrete(labels = names(year1970mean))

ggplot(year1970data, aes(x = Kategorie, y = sd)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Portfolios", y = "Standard Deviation", title = "Standard Deviation of Yearly Portfolio Returns") +
  theme_minimal()+
  scale_x_discrete(labels = names(year1970sd))

## Monthly 
month1970mean <- colMeans(MonthlyReturns1970[2:9])
names(month1970mean) <- c("MSCI World", "S&P 500", "DAX", "Hang Seng", "Nikkei 225", "Gold", "S&P 500 3x & Gold", "S&P 500 3x & Cash")
# Erstellen des Datenrahmens
month1970data <- data.frame(Kategorie = paste0("Portfolio ", 1:8), Mittelwert = month1970mean)
#sd erstellen
month1970sd <- colSds(as.matrix(MonthlyReturns1970[2:9]))
month1970data$sd <- month1970sd

# Plotten des Balkendiagramms
ggplot(month1970data, aes(x = Kategorie, y = Mittelwert)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Portfolios", y = "Average Returns", title = "Monthly Average Portfolio Returns") +
  theme_minimal()+
  scale_x_discrete(labels = names(month1970mean))

ggplot(month1970data, aes(x = Kategorie, y = sd)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Portfolios", y = "Standard Deviations", title = "Standard Deviations of Monthly Portfolio Returns") +
  theme_minimal()+
  scale_x_discrete(labels = names(month1970sd))








###########
# 1988
plot_indices <- indices
for(i in 2:ncol(indices)) {
  plot_indices[i] <- plot_indices[i] / plot_indices[1, i]*100
}
plot1988 <- data.frame(
  x = plot_indices$date,
  y1 = plot_indices$WORLD,
  y2 = plot_indices$EM,
  y3 = plot_indices$FTSE,
  y4 = plot_indices$EURO,
  y5 = plot_indices$NASDAQ,
  y6 = plot_indices$SP500,
  y7 = plot_indices$DAX,
  y8 = plot_indices$SP500VALUE,
  y9 = plot_indices$SP500GROWTH,
  y10 = plot_indices$HANGSENG,
  y11 = plot_indices$Nikkei,
  y12 = plot_indices$Gold,
  y13 = plot_indices$SP3xLev,
  y14 = plot_indices$cumYield,
  y15 = plot_indices$evoPortfolio,
  y16 = plot_indices$`Low Beta`
)

plot1988indices <- data.frame(
  x = plot_indices$date,
  y1 = plot_indices$WORLD,
  y2 = plot_indices$EM,
  y3 = plot_indices$FTSE,
  y4 = plot_indices$EURO,
  y5 = plot_indices$NASDAQ,
  y6 = plot_indices$SP500,
  y7 = plot_indices$DAX,
  y8 = plot_indices$HANGSENG,
  y9 = plot_indices$Nikkei
)


ggplot(plot1988indices, aes(x = x)) +
  geom_line(aes(y = y1, color = "Graph 1"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y2, color = "Graph 2"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y3, color = "Graph 3"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y4, color = "Graph 4"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y5, color = "Graph 5"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y6, color = "Graph 6"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y7, color = "Graph 7"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y8, color = "Graph 8"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y9, color = "Graph 9"), linetype = "solid", linewidth = 1) +
  labs(x = "Year", y = "Portfolio Value", title = "Index Portfolios since 1988") +
  scale_y_continuous(trans = "log10", breaks = c(50, 100, 500, 1000, 5000), 
                     labels = c("50", "100", "500", "1,000", "5,000")) +
  scale_color_manual(
    values = c("Graph 1" = "blue", "Graph 2" = "red", "Graph 3" = "green", "Graph 4" = "orange",
               "Graph 5" = "pink", "Graph 6" = "yellow", "Graph 7" = "black",
               "Graph 8" = "cyan", "Graph 9" = "magenta"),
    labels = c("MSCI World", "MSCI EM", "FTSE 100", "MSCI Europe", "NASDAQ-100", "S&P 500", "DAX", "Hang Seng", "Nikkei 225")
  ) +
  guides(color = guide_legend(title = "Portfolios"))+
  theme_minimal()


plot1988SPs <- data.frame(
  x = plot_indices$date,
  y1 = plot_indices$SP500,
  y2 = plot_indices$SP500VALUE,
  y3 = plot_indices$SP500GROWTH
)


ggplot(plot1988SPs, aes(x = x)) +
  geom_line(aes(y = y1, color = "Graph 1"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y2, color = "Graph 2"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y3, color = "Graph 3"), linetype = "solid", linewidth = 1) +
  labs(x = "Year", y = "Portfolio Value", title = "Factor Portfolios since 1988") +
  scale_y_continuous(trans = "log10", breaks = c(100, 300, 1000, 3000), 
                     labels = c("100", "300", "1,000", "3,000")) +
  scale_color_manual(
    values = c("Graph 1" = "blue", "Graph 2" = "red", "Graph 3" = "green"),
    labels = c("S&P 500", "S&P 500 Value", "S&P 500 Growth")
  ) +
  guides(color = guide_legend(title = "Portfolios"))+
  theme_minimal()

plot1988others <- data.frame(
  x = plot_indices$date,
  y1 = plot_indices$SP500,
  y2 = plot_indices$evoPortfolio,
  y3 = plot_indices$SP3xLev,
  y4 = plot_indices$Gold,
  y5 = plot_indices$cSP3xLev,
  y6 =  plot_indices$`Low Beta`
)
ggplot(plot1988others, aes(x = x)) +
  geom_line(aes(y = y1, color = "Graph 1"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y2, color = "Graph 2"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y3, color = "Graph 3"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y4, color = "Graph 4"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y5, color = "Graph 5"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y6, color = "Graph 6"), linetype = "solid", linewidth = 1) +
  labs(x = "Year", y = "Portfolio Value", title = "Other Portfolios since 1988") +
  scale_y_continuous(trans = "log10", breaks = c(50, 100, 1000, 10000, 100000), 
                     labels = c("50", "100","1,000", "10,000", "100,000")) +
  scale_color_manual(
    values = c("Graph 1" = "blue", "Graph 2" = "red", "Graph 3" = "green", "Graph 4" = "orange",
               "Graph 5" = "pink", "Graph 6" = "yellow"),
    labels = c("S&P 500", "Evolutionary Portfolio", "S&P 500 3x & Gold", "Gold", "S&P 500 3x + Cash", "Low Beta")
  ) +
  guides(color = guide_legend(title = "Portfolios"))+
  theme_minimal()


# Avrg performance bar chart
## Yearly 
# Beispielwerte für die Mittelwerte
yearly_mean_values1988 <- colMeans(YearlyReturns1988[-12])  
names(yearly_mean_values1988) <- c("MSCI World", "MSCI EM", "FTSE 100", "MSCI Europe", "NASDAQ-100", 
             "S&P 500", "DAX", "Hang Seng", "S&P 500 Value", "S&P 500 Growth", 
             "Nikkei 225", "Low Beta", "S&P 500 3x & Gold", "Gold", "S&P 500 3x & Cash", 
             "Evolutionary Portfolio")
# Erstellung des Dataframes für den Barchart
mean_df <- data.frame(
  Variable = names(yearly_mean_values1988),
  Mean = yearly_mean_values1988
)
yearly_sd_values1988 <- colSds(as.matrix(YearlyReturns1988[-12]))
mean_df$sd <- yearly_sd_values1988
# Erstellung des Barcharts
#avrg Return
ggplot(mean_df, aes(x = Variable, y = Mean)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Portfolios", y = "Average Returns", title = "Yearly Average Returns") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# sd
ggplot(mean_df, aes(x = Variable, y = sd)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Portfolios", y = "Standard Deviations", title = "Standard Deviations of Yearly Portfolio Returns") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Monthly 
# Beispielwerte für die Mittelwerte
monthly_mean_values1988 <- colMeans(MonthlyReturns1988)
names(monthly_mean_values1988) <- c("MSCI World", "MSCI EM", "FTSE 100", "MSCI Europe", "NASDAQ-100", 
                                   "S&P 500", "DAX", "Hang Seng", "S&P 500 Value", "S&P 500 Growth", 
                                   "Nikkei 225", "S&P 500 3x & Gold", "Gold", "S&P 500 3x & Cash", 
                                   "Evolutionary Portfolio")
# Erstellung des Dataframes für den Barchart
monthly_mean_df1988 <- data.frame(
  Variable = names(monthly_mean_values1988),
  Mean = monthly_mean_values1988
)
#sd
monthly_sd_values1988 <- colSds(as.matrix(MonthlyReturns1988))
monthly_mean_df1988$sd <- monthly_sd_values1988
# Erstellung des Barcharts
#avrg Returns
ggplot(monthly_mean_df1988, aes(x = Variable, y = Mean)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Portfolios", y = "Average Returns", title = "Monthly Average Returns") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#sd
ggplot(monthly_mean_df1988, aes(x = Variable, y = sd)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Portfolios", y = "Standard Deviations", title = "Standard Deviations of Monthly Portfolio Returns") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#########
# 1996
#adding graph with pure value

plot_indices1996 <- indices1996
for(i in 2:ncol(plot_indices1996)) {
  plot_indices1996[i] <- plot_indices1996[i] / plot_indices1996[1, i]*100
}
plot1996 <- data.frame(
  x = plot_indices1996$date,
  y1 = plot_indices1996$WORLD,
  y2 = plot_indices1996$EM,
  y3 = plot_indices1996$FTSE,
  y4 = plot_indices1996$EURO,
  y5 = plot_indices1996$NASDAQ,
  y6 = plot_indices1996$SP500,
  y7 = plot_indices1996$DAX,
  y8 = plot_indices1996$SP500VALUE,
  y9 = plot_indices1996$SP500GROWTH,
  y10 = plot_indices1996$HANGSENG,
  y11 = plot_indices1996$Nikkei,
  y12 = plot_indices1996$Gold,
  y13 = plot_indices1996$SP3xLev,
  y14 = plot_indices1996$evoPortfolio,
  y15 = plot_indices1996$cumYield,
  y16 = plot_indices1996$`pureValue$pureValue[-1]`
)

plot1996indices <- data.frame(
  x = plot_indices1996$date,
  y1 = plot_indices1996$WORLD,
  y2 = plot_indices1996$EM,
  y3 = plot_indices1996$FTSE,
  y4 = plot_indices1996$EURO,
  y5 = plot_indices1996$NASDAQ,
  y6 = plot_indices1996$SP500,
  y7 = plot_indices1996$DAX,
  y8 = plot_indices1996$HANGSENG,
  y9 = plot_indices1996$Nikkei
)


ggplot(plot1996indices, aes(x = x)) +
  geom_line(aes(y = y1, color = "Graph 1"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y2, color = "Graph 2"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y3, color = "Graph 3"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y4, color = "Graph 4"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y5, color = "Graph 5"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y6, color = "Graph 6"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y7, color = "Graph 7"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y8, color = "Graph 8"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y9, color = "Graph 9"), linetype = "solid", linewidth = 1) +
  labs(x = "Year", y = "Portfolio Value", title = "Index Portfolios since 1996") +
  scale_y_continuous(trans = "log10", breaks = c(30, 100, 300, 1000, 3000, 10000), 
                     labels = c("30", "100", "300", "1000", "3000", "10000")) +
  scale_color_manual(
    values = c("Graph 1" = "blue", "Graph 2" = "red", "Graph 3" = "green", "Graph 4" = "orange",
               "Graph 5" = "pink", "Graph 6" = "yellow", "Graph 7" = "black",
               "Graph 8" = "cyan", "Graph 9" = "magenta"),
    labels = c("MSCI World", "MSCI EM", "FTSE 100", "MSCI Europe", "NASDAQ-100", "SP 500", "DAX", "Hang Seng", "Nikkei 225")
  ) +
  guides(color = guide_legend(title = "Portfolios"))+
  theme_minimal()


plot1996SPs <- data.frame(
  x = plot_indices1996$date,
  y1 = plot_indices1996$SP500,
  y2 = plot_indices1996$SP500VALUE,
  y3 = plot_indices1996$SP500GROWTH,
  y4 = plot_indices1996$`pureValue$pureValue[-1]`
)


ggplot(plot1996SPs, aes(x = x)) +
  geom_line(aes(y = y1, color = "Graph 1"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y2, color = "Graph 2"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y3, color = "Graph 3"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y4, color = "Graph 4"), linetype = "solid", linewidth = 1) +
  labs(x = "Year", y = "Portfolio Value", title = "Factor Portfolios since 1996") +
  scale_y_continuous(trans = "log10", breaks = c(50, 100, 300, 1000), 
                     labels = c("50", "100", "300", "1000")) +
  scale_color_manual(
    values = c("Graph 1" = "yellow", "Graph 2" = "red", "Graph 3" = "green", "Graph 4" = "blue"),
    labels = c("S&P 500", "S&P 500 Value", "S&P 500 Growth", "S&P Small Cap Pure Value")
  ) +
  guides(color = guide_legend(title = "Portfolios"))+
  theme_minimal()

plot1996other <- data.frame(
  x = plot_indices1996$date,
  y1 = plot_indices1996$SP500,
  y2 = plot_indices1996$evoPortfolio,
  y3 = plot_indices1996$SP3xLev,
  y4 = plot_indices1996$Gold,
  y5 = plot_indices1996$cSP3xLev
)
ggplot(plot1996other, aes(x = x)) +
  geom_line(aes(y = y1, color = "Graph 1"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y2, color = "Graph 2"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y3, color = "Graph 3"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y4, color = "Graph 4"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = y5, color = "Graph 5"), linetype = "solid", linewidth = 1) +
  labs(x = "Year", y = "Portfolio Value", title = "Other Portfolios since 1996") +
  scale_y_continuous(trans = "log10", breaks = c(50, 100, 300, 1000, 3000, 10000), 
                     labels = c("50", "100", "300", "1000", "3000",  "10000")) +
  scale_color_manual(
    values = c("Graph 1" = "blue", "Graph 2" = "red", "Graph 3" = "green", "Graph 4" = "orange",
               "Graph 5" = "pink", "Graph 6" = "yellow"),
    labels = c("S&P 500",  "Evolutionary Portfolio", "S&P 500 3x + Gold", "Gold", "S&P 500 3x + Cash")
  ) +
  guides(color = guide_legend(title = "Portfolios"))+
  theme_minimal()


# Avrg performance bar chart
## Yearly 
# Beispielwerte für die Mittelwerte
yearly_mean_values1996 <- colMeans(YearlyReturns1996)  
names(yearly_mean_values1996) <- c("MSCI World", "MSCI EM", "FTSE 100", "MSCI Europe", "NASDAQ-100", 
                                   "S&P 500", "DAX", "Hang Seng", "S&P 500 Value", "S&P 500 Growth", 
                                   "Nikkei 225", "Low Beta", "S&P 500 3x & Gold", "Gold", "S&P 500 3x & Cash", 
                                   "Evolutionary Portfolio", "S&P Small Cap Pure Value")

# Erstellung des Dataframes für den Barchart
mean_df1996 <- data.frame(
  Variable = names(yearly_mean_values1996),
  Mean = yearly_mean_values1996
)
#sd
yearly_sd_values1996 <- colSds(as.matrix(YearlyReturns1996))
mean_df1996$sd <- yearly_sd_values1996
# Erstellung des Barcharts
#avrg Returns
ggplot(mean_df1996, aes(x = Variable, y = Mean)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Portfolios", y = "Average Returns", title = "Yearly Average Returns") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#sd
ggplot(mean_df1996, aes(x = Variable, y = sd)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Portfolios", y = "Standard Deviations", title = "Standard Deviations of Yearly Portfolio Returns") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Monthly 
# Beispielwerte für die Mittelwerte
monthly_mean_values1996 <- colMeans(MonthlyReturns1996)
names(monthly_mean_values1996) <- c("MSCI World", "MSCI EM", "FTSE 100", "MSCI Europe", "NASDAQ-100", 
                                    "S&P 500", "DAX", "Hang Seng", "S&P 500 Value", "S&P 500 Growth", 
                                    "Nikkei 225", "Low Beta", "S&P 500 3x & Gold", "Gold", "S&P 500 3x & Cash", 
                                    "Evolutionary Portfolio", "S&P Small Cap Pure Value")
# Erstellung des Dataframes für den Barchart
monthly_mean_df1996 <- data.frame(
  Variable = names(monthly_mean_values1996),
  Mean = monthly_mean_values1996
)
#sd
monthly_sd_values1996 <- colSds(as.matrix(MonthlyReturns1996))
monthly_mean_df1996$sd <- monthly_sd_values1996
# Erstellung des Barcharts
#avrg Returns
ggplot(monthly_mean_df1996, aes(x = Variable, y = Mean)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Portfolios", y = "Average Returns", title = "Monthly Average Returns") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#sd
ggplot(monthly_mean_df1996, aes(x = Variable, y = sd)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Portfolios", y = "Standard Deviations", title = "Standard Deviations of Yearly Portfolio Returns") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#######
##Sharp Ratios
#1970
year1970data$sharp <- year1970data$Mittelwert / year1970data$sd
month1970data$sharp <-month1970data$Mittelwert / month1970data$sd


ggplot(year1970data, aes(x = rownames(year1970data), y = sharp)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Portfolios", y = "Sharp Ratio", title = "Sharp Ratios of Yearly Returns since 1970") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(month1970data, aes(x = rownames(month1970data), y = sharp)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Portfolios", y = "Sharp Ratio", title = "Sharp Ratios of Monthly Returns since 1970") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#1988
mean_df$sharp <- mean_df$Mean / mean_df$sd
monthly_mean_df1988$sharp <- monthly_mean_df1988$Mean / monthly_mean_df1988$sd


ggplot(mean_df, aes(x = Variable, y = sharp)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Portfolios", y = "Sharp Ratio", title = "Sharp Ratios of Yearly Returns since 1988") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(monthly_mean_df1988, aes(x = Variable, y = sharp)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Portfolios", y = "Sharp Ratio", title = "Sharp Ratios of Monthly Returns since 1988") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#1996
mean_df1996$sharp <- mean_df1996$Mean / mean_df1996$sd
monthly_mean_df1996$sharp <- monthly_mean_df1996$Mean /monthly_mean_df1996$sd


ggplot(mean_df1996, aes(x = Variable, y = sharp)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Portfolios", y = "Sharp Ratio", title = "Sharp Ratios of Yearly Returns since 1996") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(monthly_mean_df1996, aes(x = Variable, y = sharp)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Portfolios", y = "Sharp Ratio", title = "Sharp Ratios of Monthly Returns since 1996") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




### Rankings
## Bootstrap rankings
ranking10samples <- marg_CI(samples10, "Ranking with 10 Resampling Iterations", 0.95)
ranking1000samples <- marg_CI(samples1000, "Ranking with 1000 Resampling Iterations", 0.95)
grid.arrange(ranking10samples, ranking1000samples, ncol = 2)

## Rankings 1970 0.95
# Monthly
marg1970monthlyCI95 <- marg_CI(MonthlyReturns1970, "Ranking of avrg. Monthly Returns since 1970 with marg. CI", 0.95)
sim1970monthlyCI95 <- sim_CI(MonthlyReturns1970, "Ranking of avrg. Monthly Returns since 1970 with sim. CI", 0.95)
grid.arrange(marg1970monthlyCI95, sim1970monthlyCI95, ncol = 2)
# Yearly
marg1970yearlyCI95 <- marg_CI(YearlyReturns1970, "Ranking of avrg. Yearly Returns since 1970 with marg. CI", 0.95)
sim1970yearlyCI95 <- sim_CI(YearlyReturns1970, "Ranking of avrg. Yearly Returns since 1970 with sim. CI", 0.95)
grid.arrange(marg1970yearlyCI95, sim1970yearlyCI95, ncol = 2)

## Rankings 1988 0.95
# Monthly
marg1988monthlyCI95 <- marg_CI(MonthlyReturns1988, "Ranking of avrg. Monthly Returns since 1988 with marg. CI", 0.95)
sim1988monthlyCI95 <- sim_CI(MonthlyReturns1988, "Ranking of avrg. Monthly Returns since 1988 with sim. CI", 0.95)
grid.arrange(marg1988monthlyCI95, sim1988monthlyCI95, ncol = 2)
# Yearly
marg1988yearlyCI95 <- marg_CI(YearlyReturns1988, "Ranking of avrg. Yearly Returns since 1988 with marg. CI", 0.95)
sim1988yearlyCI95 <- sim_CI(YearlyReturns1988, "Ranking of avrg. Yearly Returns since 1988 with sim. CI", 0.95)
grid.arrange(marg1988yearlyCI95, sim1988yearlyCI95, ncol = 2)

## Rankings 1996 0.95
# Monthly
marg1996monthlyCI95 <- marg_CI(MonthlyReturns1996, "Ranking of avrg. Monthly Returns since 1996 with marg. CI", 0.95)
sim1996monthlyCI95 <- sim_CI(MonthlyReturns1996, "Ranking of avrg. Monthly Returns since 1996 with sim. CI", 0.95)
grid.arrange(marg1996monthlyCI95, sim1996monthlyCI95, ncol = 2)
# Yearly
marg1996yearlyCI95 <- marg_CI(YearlyReturns1996, "Ranking of avrg. Yearly Returns since 1996 with marg. CI", 0.95)
sim1996yearlyCI95 <- sim_CI(YearlyReturns1996, "Ranking of avrg. Yearly Returns since 1996 with sim. CI", 0.95)
grid.arrange(marg1996yearlyCI95, sim1996yearlyCI95, ncol = 2)

## Rankings 1970 0.5
# Monthly
marg1970monthlyCI50 <- marg_CI(MonthlyReturns1970, "Ranking of avrg. Monthly Returns since 1970 with marg. CI", 0.5)
sim1970monthlyCI50 <- sim_CI(MonthlyReturns1970, "Ranking of avrg. Monthly Returns since 1970 with sim. CI", 0.5)
grid.arrange(marg1970monthlyCI50, sim1970monthlyCI50, ncol = 2)
# Yearly
marg1970yearlyCI50 <- marg_CI(YearlyReturns1970, "Ranking of avrg. Yearly Returns since 1970 with marg. CI", 0.5)
sim1970yearlyCI50 <- sim_CI(YearlyReturns1970, "Ranking of avrg. Yearly Returns since 1970 with sim. CI", 0.5)
grid.arrange(marg1970yearlyCI50, sim1970yearlyCI50, ncol = 2)

## Rankings 1988 0.5
# Monthly
marg1988monthlyCI50 <- marg_CI(MonthlyReturns1988, "Ranking of avrg. Monthly Returns since 1988 with marg. CI", 0.5)
sim1988monthlyCI50 <- sim_CI(MonthlyReturns1988, "Ranking of avrg. Monthly Returns since 1988 with sim. CI", 0.5)
grid.arrange(marg1988monthlyCI50, sim1988monthlyCI50, ncol = 2)
# Yearly
marg1988yearlyCI50 <- marg_CI(YearlyReturns1988, "Ranking of avrg. Yearly Returns since 1988 with marg. CI", 0.5)
sim1988yearlyCI50 <- sim_CI(YearlyReturns1988, "Ranking of avrg. Yearly Returns since 1988 with sim. CI", 0.5)
grid.arrange(marg1988yearlyCI50, sim1988yearlyCI50, ncol = 2)

## Rankings 1996 0.5
# Monthly
marg1996monthlyCI50 <- marg_CI(MonthlyReturns1996, "Ranking of avrg. Monthly Returns since 1996 with marg. CI", 0.5)
sim1996monthlyCI50 <- sim_CI(MonthlyReturns1996, "Ranking of avrg. Monthly Returns since 1996 with sim. CI", 0.5)
grid.arrange(marg1996monthlyCI50, sim1996monthlyCI50, ncol = 2)
# Yearly
marg1996yearlyCI50 <- marg_CI(YearlyReturns1996, "Ranking of avrg. Yearly Returns since 1996 with marg. CI", 0.5)
sim1996yearlyCI50 <- sim_CI(YearlyReturns1996, "Ranking of avrg. Yearly Returns since 1996 with sim. CI", 0.5)
grid.arrange(marg1996yearlyCI50, sim1996yearlyCI50, ncol = 2)

## Rankings 1970 0.05
# Monthly
marg1970monthlyCI05 <- marg_CI(MonthlyReturns1970, "Ranking of avrg. Monthly Returns since 1970 with marg. CI", 0.05)
sim1970monthlyCI05 <- sim_CI(MonthlyReturns1970, "Ranking of avrg. Monthly Returns since 1970 with sim. CI", 0.05)
grid.arrange(marg1970monthlyCI05, sim1970monthlyCI05, ncol = 2)
# Yearly
marg1970yearlyCI05 <- marg_CI(YearlyReturns1970, "Ranking of avrg. Yearly Returns since 1970 with marg. CI", 0.05)
sim1970yearlyCI05 <- sim_CI(YearlyReturns1970, "Ranking of avrg. Yearly Returns since 1970 with sim. CI", 0.05)
grid.arrange(marg1970yearlyCI05, sim1970yearlyCI05, ncol = 2)

## Rankings 1988 0.05
# Monthly
marg1988monthlyCI05 <- marg_CI(MonthlyReturns1988, "Ranking of avrg. Monthly Returns since 1988 with marg. CI", 0.05)
sim1988monthlyCI05 <- sim_CI(MonthlyReturns1988, "Ranking of avrg. Monthly Returns since 1988 with sim. CI", 0.05)
grid.arrange(marg1988monthlyCI05, sim1988monthlyCI05, ncol = 2)
# Yearly
marg1988yearlyCI05 <- marg_CI(YearlyReturns1988, "Ranking of avrg. Yearly Returns since 1988 with marg. CI", 0.05)
sim1988yearlyCI05 <- sim_CI(YearlyReturns1988, "Ranking of avrg. Yearly Returns since 1988 with sim. CI", 0.05)
grid.arrange(marg1988yearlyCI05, sim1988yearlyCI05, ncol = 2)

## Rankings 1996 0.05
# Monthly
marg1996monthlyCI05 <- marg_CI(MonthlyReturns1996, "Ranking of avrg. Monthly Returns since 1996 with marg. CI", 0.05)
sim1996monthlyCI05 <- sim_CI(MonthlyReturns1996, "Ranking of avrg. Monthly Returns since 1996 with sim. CI", 0.05)
grid.arrange(marg1996monthlyCI05, sim1996monthlyCI05, ncol = 2)
# Yearly
marg1996yearlyCI05 <- marg_CI(YearlyReturns1996, "Ranking of avrg. Yearly Returns since 1996 with marg. CI", 0.05)
sim1996yearlyCI05 <- sim_CI(YearlyReturns1996, "Ranking of avrg. Yearly Returns since 1996 with sim. CI", 0.05)
grid.arrange(marg1996yearlyCI05, sim1996yearlyCI05, ncol = 2)


colMeans(YearlyReturns1970)



