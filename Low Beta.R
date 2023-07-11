setwd("/Users/jonasschernich/Uni/6. Semester/Bachelorarbeit/Aktien")
cola <- read.csv2("KO.csv")
duke <- read.csv2("duk.csv")
ed <- read.csv2("ed.csv")
jnj <- read.csv2("jnj.csv")
kmb <- read.csv2("kmb.csv")
mcd <- read.csv2("mcd.csv")
pep <- read.csv2("pep.csv")
pg <- read.csv2("pg.csv")
vz <- read.csv2("vz.csv")
wmt <- read.csv2("wmt.csv")


SP500 <- data.frame(date = indices_isin$date[4697:length(indices_isin$SP500)], Kurs = indices_isin$SP500[4697:length(indices_isin$SP500)])



stocks <- list(Cola = cola, Duke = duke, Edison = ed, JnJ = jnj, Clark = kmb, McDonalds=mcd, Pepsi=pep, Procter=pg, Verizon=vz, Walmart=wmt)
## Aktien dfs fixen und auf 1 standardisieren
for(i in 1:length(stocks)) {
  stocks[[i]] <- data.frame(
    date = sub(",.*", "", stocks[[i]]$Date.Open.High.Low.Close.Adj.Close.Volume),
    kurs = sub(".*,(.*),(.*),(.*),(.*),.*", "\\2", stocks[[i]]$Date.Open.High.Low.Close.Adj.Close.Volume),
    stringsAsFactors = FALSE)
  stocks[[i]]$kurs <- as.numeric(stocks[[i]]$kurs)
  stocks[[i]]$kurs <- stocks[[i]]$kurs / stocks[[i]][1,2]
  stocks[[i]]$kurs <- stocks[[i]]$kurs * 10
}

lowBetaPortfolio <- stocks[[1]]
for(i in 2:10) {
  lowBetaPortfolio$kurs <- lowBetaPortfolio$kurs + stocks[[i]]$kurs
}
# bis ende 22
lowBetaPortfolio <- lowBetaPortfolio[1:8821,]




#datum und Handelstage angleichen
SP500overlap <- c(2, 33, 67, 108, 133, 178, 236, 258, 263, 298, 322, 368, 394, 438, 496, 518, 523, 558, 597, 628, 655, 698, 756, 779, 784, 818, 847, 888, 916, 958, 1021, 1040, 1045, 1078, 1122, 1148, 1177, 1223, 1281, 1302, 1307, 1338, 1377, 1413, 1438, 1483, 1541, 1562, 1603, 1632, 1650, 1673, 1698, 1743, 1801, 1823, 1828, 1863, 1902, 1933, 1959, 2003, 2061, 2083, 2088, 2123, 2157, 2193, 2221, 2263, 2326, 2345, 2350, 2383, 2412, 2453, 2482, 2523, 2586, 2606, 2611, 2623, 2643, 2682, 2713, 2742, 2788, 2846, 2867, 2872, 2883, 2903, 2937, 2978, 3003, 3048, 3106, 3127, 3143, 3168, 3212, 3238, 3264, 3308, 3366, 3388, 3393, 3403, 3428, 3467, 3498, 3525, 3568, 3574, 3575, 3576, 3577, 3626, 3649, 3654, 3668, 3688, 3717, 3758, 3786, 3828, 3891, 3910, 3915, 3928, 3948, 3992, 4018, 4047, 4088, 4151, 4171, 4176, 4188, 4208, 4247, 4283, 4292, 4308, 4353, 4411, 4432, 4448, 4473, 4497, 4543, 4568, 4613, 4671, 4693, 4698, 4708, 4733, 4772, 4803, 4829, 4873, 4931, 4953, 4958, 4959, 4968, 4993, 5027, 5063, 5090, 5133, 5191, 5214, 5219, 5233, 5253, 5277, 5323, 5352, 5393, 5456, 5476, 5481, 5493, 5513, 5552, 5583, 5612, 5658, 5716, 5737, 5742, 5753, 5773, 5807, 5848, 5873, 5918, 5976, 5997, 6013, 6038, 6082, 6108, 6133, 6178, 6236, 6258, 6263, 6273, 6298, 6332, 6368, 6395, 6438, 6478, 6479, 6496, 6519, 6524, 6538, 6558, 6587, 6628, 6656, 6698, 6761, 6780, 6785, 6798, 6818, 6862, 6888, 6917, 6958, 7021, 7041, 7046, 7058, 7078, 7112, 7148, 7177, 7223, 7281, 7302, 7307, 7318, 7338, 7367, 7413, 7438, 7483, 7541, 7563, 7568, 7578, 7603, 7642, 7673, 7699, 7743, 7801, 7823, 7828, 7838, 7863, 7892, 7933, 7960, 8003, 8061, 8070, 8084, 8089, 8103, 8123, 8167, 8193, 8221, 8263, 8326, 8345, 8350, 8363, 8383, 8422, 8453, 8482, 8528, 8586, 8607, 8612, 8623, 8643, 8677, 8718, 8743, 8788, 8846, 8867, 8883, 8908, 8947, 8978, 8993, 9003, 9048, 9106, 9128)

SP500 <- SP500[-SP500overlap,]
SP500$Kurs <- gsub(",", ".", SP500$Kurs )
SP500$Kurs <- as.numeric(SP500$Kurs)
SP500$Kurs <- SP500$Kurs/SP500$Kurs[1] * 100
# Liniengrafik erstellen
plot(as.Date(lowBetaPortfolio$date), lowBetaPortfolio$kurs, type = "l", col = "blue", xlab = "Day", ylab = "Aktienkurs",
     main = "Aktienkurse")
lines(as.Date(lowBetaPortfolio$date), SP500$Kurs, col = "green")
legend("topleft", legend = c("Low Beta Portfolio","S&P 500"), col = c("blue", "red"), lty = 1)

# var nochmal mit HAC machen
beta <- cov(lowBetaPortfolio$kurs, SP500$Kurs)/var(SP500$Kurs)
beta







