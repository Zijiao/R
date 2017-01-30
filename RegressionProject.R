library(MASS)
library(nnet)
library(car)
library(leaps)

data<-read.table("F:\\Dropbox\\Courses\\semester 1__562__Regression\\final project\\DataSet_A.txt",header=T)
attach(data)
pairs(data)
residual1=resid(model1)
fit1=predict(model1)
plot(fit1,residual1,main="residual1 against fit1")
qqnorm(residual1)
qqline(residual1)

residual2=resid(model2)
fit2=predict(model2)
plot(fit2,residual2,main="residual2 against fit2")
qqnorm(residual2)
qqline(residual2)
studentized_residual2<-rstudent(model2)
plot(fit2,studentized_residual2,main="studentized residuals against fitted values for model2")

#outliers

leverage<-hatvalues(model2)
plot(leverage, ylab="leverage", main="leverage plot",type="b",sub="2p/n=0.055")
identify(1:506,leverage)
text(leverage,cex=.7)
abline(h=0.055)
h_l=2*14/506


leverage[leverage>0.055]



cook<-cooks.distance(model2)
plot(cook,ylab="cook's distance",type="b",main="Cook's Distance")
text(cook,labels=1:dim(data)[1])




dff=dffits(model2)
dffcut=2*sqrt(14/506)
dff>-dffcut
## 18 23 36


DFBETAS<-dfbetas(model2)
DFBETAS[abs(DFBETAS)>0.089]
plot(DFBETAS,ylab="DFBETAS",type="b",main="DFBETAS")
text(DFBETAS,labels=1:dim(data)[1])
abline(h=0.089)

DFFITS<-dffits(model2)
DFFITS[abs(DFFITS)>0.333]
plot(DFFITS,ylab="DFFITS",type="b",main="DFFITS")
text(DFFITS,labels=1:dim(data)[1])
abline(h=0.333)

COVRATIO<-covratio(model2)
COVRATIO[COVRATIO<1-3*14/506]
plot(COVRATIO,ylab="COVRATIO",type="b",main="COVRATIO")
text(COVRATIO,labels=1:dim(data)[1])
abline(h=1+3*14/506)

newdata<-data[-c(215,368,369),]

model3<-lm(log(MEDV) ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO + BKPOP + LSTAT,data=newdata)

model0<-lm(log(MEDV)~1,data=newdata)
step(model0,scope=list(lower=model0,upper=model3),direction="forward")
step(model3,direction="backward")
step(model0,scope=list(upper=model3),direction="both")


 qqnorm(resid(model4))
qqline(resid(model4))
plot(predict(model4),resid(model4),main="residual plot against fitted value for model 4")


X<-cbind(CRIM,ZN,INDUS,CHAS,NOX,RM,AGE,DIS,RAD,TAX,PTRATIO,BKPOP,LSTAT)
b<-regsubsets(as.matrix(X),log(MEDV))
rs<-summary(b)
rs$adjr2
rs$cp
subsets(b,statistic=c("cp"))
subsets(b,statistic=c("adjr2")
leaps(as.matrix(X),log(y),method=c("Cp"))
c<-leaps(as.matrix(X),log(MEDV),method=c("Cp"))
cps<-c$Cp
plot(c$size,cps,main="Cp plot",xlab="p")
abline(0,1)
a<-leaps(as.matrix(X),log(MEDV),method=c("adjr2"))
adjr2<-a$adjr2
plot(c$size,adjr2,xlab="p",main="adjust R-square")

all.poss.regs(log(MEDV) ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO + BKPOP + LSTAT,data=newdata,best=5)





modelK<-lm(log(MEDV) ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO + BKPOP + LSTAT,data=newdata)

summary(modelK)$adj.r.squared


reduce1<-lm(log(MEDV) ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO + BKPOP + LSTAT,data=newdata)




cor1<-cbind(CRIM,ZN,INDUS,CHAS,NOX,RM,AGE,DIS,RAD,TAX,PTRATIO,BKPOP,LSTAT)
cor(as.matrix(cor1))

reduce2<-lm(log(MEDV) ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO + BKPOP + LSTAT,data=newdata)



reduce3<-lm(log(MEDV) ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + TAX + PTRATIO + BKPOP + LSTAT,data=newdata)


reduce4<-lm(log(MEDV) ~ CRIM + ZN +  CHAS + NOX + RM +  DIS + RAD + PTRATIO + BKPOP + LSTAT,data=newdata)



cor2<-cbind(CRIM,ZN,CHAS,NOX,RM,DIS,RAD,PTRATIO,BKPOP,LSTAT)
cor(as.matrix(cor2))

step(reduce4,direction="backward")


reduce5<-lm(log(MEDV) ~ CRIM + CHAS + NOX + RM +  DIS + RAD + PTRATIO + BKPOP + LSTAT,data=newdata)


r<-rstudent(reduce4)
plot(reduce4$fitted.value,r,xlab="fitted value",ylab="studentized residual",main="residual plot")



model5<-lm(log(MEDV)~CRIM+ZN+CHAS+NOX+RM+DIS+RAD+TAX+PTRATIO+BKPOP+LSTAT,data=newdata)

covariancematrix<-cbind(CRIM,CHAS,NOX,RM,DIS,RAD,TAX,PTRATIO,BKPOP,LSTAT)
cor(as.matrix(covariancematrix))



residual7=resid(model8)
fit7=predict(model8)
plot(fit7,residual7,main="residual7 against fit7")
qqnorm(residual7)
qqline(residual7)
