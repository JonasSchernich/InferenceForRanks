# Moving Block Bootstrap Funktion
mbb <- function(data, blockl){
  samples <- list()
  for (i in 1 : (nrow(data) - blockl + 1)) {
    samples[[i]] <- as.data.frame(data[c(i: (i + blockl - 1)),])
  }
  return(samples)
}

mbb_sampling <- function(data, blockl, samplenum) {
  samplesource <- mbb(data, blockl)
  blocknum <- floor(nrow(data)/blockl)
  bootstrapped_DFs <- list()
  for(i in 1:samplenum) {
    store_df <- as.data.frame(sample(samplesource, 1))
    for(j in 1 : blocknum) {
      store_df <- rbind(store_df, as.data.frame(sample(samplesource, 1)))
    }
    bootstrapped_DFs[[i]] <- store_df
  }
  return(bootstrapped_DFs)
}

samples10 <- mbb_sampling(YearlyReturns1996, 3, 10)
samples1000 <- mbb_sampling(YearlyReturns1996, 3, 1000)

##Crating the data frame with new means for 
#10 Samples
means_df <- data.frame(replicate(ncol(YearlyReturns1996), rep(0, length(samples10))))
names(means_df) <- names(YearlyReturns1996)

for(i in 1:length(samples10)) {
  for(j in 1:ncol(YearlyReturns1996)) {
    means_df[i, j] <- mean(samples10[[i]][[j]])
  }
}
samples10 <- means_df
#1000 samples
means_df <- data.frame(replicate(ncol(YearlyReturns1996), rep(0, length(samples1000))))
names(means_df) <- names(YearlyReturns1996)

for(i in 1:length(samples1000)) {
  for(j in 1:ncol(YearlyReturns1996)) {
    means_df[i, j] <- mean(samples1000[[i]][[j]])
  }
}
samples1000 <- means_df





