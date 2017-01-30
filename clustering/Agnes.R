# R code for clustering

library(cluster)

# ================== Declare Paths =====================

# distance matrix output by Python
distMatrixFilePath = "~/Desktop/FlightSafety/IPythonNotebook/distForR.csv"

# clustering outcome
# used in customer clustering
clusteringOutputPath = "/Users/Zijiao/Desktop/FlightSafety/IPythonNotebook/clusterFromR.csv"

# used in client clustering
clusterAmountPath = "/Users/Zijiao/Desktop/FlightSafety/IPythonNotebook/clusterAmount.csv"

# used in client clustering
mergeProcedureOutputPath = "/Users/Zijiao/Desktop/FlightSafety/IPythonNotebook/merge.csv"

# @Zijiao: these two paths should be consistant with their counterparts in Python code

# =======================================================

dist <- read.table(distMatrixFilePath, colClasses=c(rep("factor", 1)), quote="\"")

# run the agnes()
cluster <- agnes(dist, diss = TRUE)

# then plot
#plot(cluster)
# sample as pltree(cluster_q1)

# first group by cluster number or height
cut_k <- cutree(as.hclust(cluster), h = 1.7) 

unique(cut_k)  

# mergeProcedure is for analysis usage
write.table(cluster$merge, mergeProcedureOutputPath)

# write cluster output (for customer clustering)
write.csv(cut_k, clusteringOutputPath)

# wirte cluster total amount (for client clustering)
write.csv(unique(cut_k), clusterAmountPath)



