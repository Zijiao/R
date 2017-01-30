# Used in analysis of clientCluster-v2

hist.data = hist(cluster$height, plot=F)

# > hist.data$counts [12] and [14] were 0, for logrization, manually assigning 1
 hist.data$counts[12] <- 1
 hist.data$counts[14] <- 1

hist.data$counts = log(hist.data$counts, 2)
plot(hist.data)

y <- hist.data$counts
x <- hist.data$mids

