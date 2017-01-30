#Time series and predict part:
cpp1<-read.csv("~/Documents/data2 final project/CPP1.csv") cpp1.ts<-cpp1[,13]
plot(cpp1.ts,cex=0.3)
cpp1.ord=ar(cpp1.ts,method="mle")
cpp1.ord$order
cppm=arima(cpp1.ts[1:81870],order=c(12,1,1))
cpp.pred=predict(cppm)
cppm.ts=ts(cpp1.ts[81800:1024641])
plot(cppm.ts,cex=0.05,col="red") points(cppm.ts,pch=19,col="red",cex=0.3) cppp.pred=ts(c(cpp1.ts[81800:1024641],cpp.pred$pred)) cppp.pred.up=ts(c(cpp1.ts[81800:1024641],cpp.pred$pred+2*cpp.pred$se))
14
cppp.pred.lw=ts(c(cpp1.ts[81800:1024641],cpp.pred$pred-2*cpp.pred$se)) points(cppp.pred,pch=21,col="green",cex=0.6)
lines(cppp.pred.up,lty=3)
lines(cppp.pred.lw,lty=3)

# Clustering part:
CPP <- read.csv("~/Documents/CPP.csv") tt<-table(CPP$Linkage.Id,CPP$Department) tt1<-table(CPP$Order.Type.Code,CPP$Department) tt2<-table(CPP$Requester.WWID,CPP$Department) c11=cor(tt)
quantile(c11,c(0:100)/100) c11[c11<0.05]=0
c12=c11
dimnames(c21)=NULL pairs(c21,pcl=”.”,col=2) c31=sqrt(1-c21^2) hi=hclust(as.dist(c31),method="ward") plot(hi1)
c21=cor(tt)
quantile(c21,c(0:100)/100)
c21[c21<0.05]=0
c22=c21
dimnames(c22)=NULL
pairs(c22,pcl=”.”,col=2)
c32=sqrt(1-c22^2) hi=hclust(as.dist(c32),method="ward")
plot(hi2)
c31=cor(tt)
quantile(c31,c(0:100)/100)
c31[c31<0.05]=0
c32=c31
dimnames(c31)=NULL
pairs(c31,pcl=”.”,col=2)
c33=sqrt(1-c31^2) hi=hclust(as.dist(c33),method="ward")
plot(hi3)

# Network part:
# Network for all department: tt11=apply(tt1,2,function(x)x/max(x)) u1=mmpc(data.frame(tt11))
plot(u1) tt2=table(CPP$Order.Type.Code,CPP$Department) tt22=apply(tt2,2,function(x)x/max(x)) u2=mmpc(data.frame(tt22))
15
plot(u2)
tt3=table(CPP$Account.Code,CPP$Department)
tt33=apply(tt3,2,function(x)x/max(x))
u3=mmpc(data.frame(tt33))
plot(u3)
# network for each cluster order.cluster3<-rbind(cpp.Cordis,cpp.Depuy,cpp.Diabetes,cpp.Ethicon,cpp.HCS,cpp.OCD,cpp.Vis takon)
order.cluster3.table<-table(order.cluster3$Order.Type.Code,order.cluster3$Department) order.cluster3.table.sta=apply(order.cluster3.table,2,function(x)x/max(x))# order.u3=mmpc(data.frame(order.cluster3.table.sta))
plot(order.u3)
