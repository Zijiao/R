
# @ Yi Zhang

require(graphics)

hc <- hclust(dist(USArrests), "ave")

plot(hc)

v <- hc$order

names(v) <- (1:length(v))

x <- as.numeric(names(v))

names(x) <- -v

nst <- nrow(hc$merge)

xx <- rep(NA,nst)

names(xx) <- 1:nst

x <- c(x,xx)



for (i in 1:nst)
  
{
  
  ith <- as.character(i)
  
  x[ith] = mean(x[as.character(hc$merge[i,])])
  
  y = hc$height[i]
  
  z = i
  
  text(x[ith],y,z)
  
}

