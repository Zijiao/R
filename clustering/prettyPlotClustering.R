
# 8 colors
labelColors = c("#CD5B45", "#EE7621", "#FFD700", "#66CD00", "#8EE5EE", "#0000CD", "#9A32CD", "#CD1076")

# 5 colors
#labelColors = c("#CD5B45", "#EE7621", "#FFD700", "#66CD00", "#8EE5EE")
clusMember = cutree(as.hclust(cluster), k = 5)

hcd = as.dendrogram(cluster)
# function to get color labels
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}
# using dendrapply
clusDendro = dendrapply(hcd, colLab)
# make plot

#png(filename="CustomerClustering.png", )
plot(clusDendro, main = "Customer Clustering")
#dev.off()




