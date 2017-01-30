# preprocess
wdbc <- read.csv("~/Dropbox/COURSES/Semester 3/588__Data Mining/project/wdbc.data", header=F)
w<-wdbc[,3:32]
pca_unscaled<-prcomp(w)
pca_scaled<-prcomp(w,scale=TRUE)
plot(pca_scaled$sdev)
plot(pca_scaled$sdev)
w_pcarotated<-pca_scaled$x
response<-wdbc[,2]
realnum_response<-wdbc[,4]
for(i in 1:569){if(response[i]=='B'){realnum_response[i]=0} else{realnum_response[i]=1}}
w_pcafull<-cbind(realnum_response,w_pcarotated)
d=sort(sample(nrow(w_pcafull),nrow(w_pcafull)*.8)) # 80% train 20% test
w_train=w_pcafull[d,]
w_test=w_pcafull[-d,]
w_traindata<-as.data.frame(w_train)
w_testdata<-as.data.frame(w_test)
colnames(w_traindata)[1]<-"y"
colnames(w_testdata)[1]<-"y"

#logistics regression

logic_train_error<-array(0,30)
logic_test_error<-array(0,30)

logic_model1<-glm(y~PC1,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[1]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[1]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[2]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[2]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[3]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[3]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[4]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[4]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[5]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[5]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[6]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[6]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[7]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[7]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[8]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[8]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[9]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[9]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[10]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[10]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[11]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[11]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[12]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[12]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[13]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[13]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[14]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[14]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[15]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[15]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[16]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[16]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[17]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[17]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[18]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[18]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[19]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[19]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[20]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[20]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[21]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[21]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[22]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[22]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22+PC23,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[23]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[23]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22+PC23+PC24,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[24]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[24]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22+PC23+PC24+PC25,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[25]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[25]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22+PC23+PC24+PC25+PC26,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[26]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[26]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22+PC23+PC24+PC25+PC26+PC27,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[27]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[27]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22+PC23+PC24+PC25+PC26+PC27+PC28,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[28]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[28]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22+PC23+PC24+PC25+PC26+PC27+PC28+PC29,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[29]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[29]=1-sum(diag(classfication))/sum(classfication)

logic_model1<-glm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22+PC23+PC24+PC25+PC26+PC27+PC28+PC29+PC30,family=binomial(link="logit"),data=w_traindata)
a=predict(logic_model1,w_traindata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_traindata$y)
logic_train_error[30]=1-sum(diag(classfication))/sum(classfication)
a=predict(logic_model1,w_testdata,type="link")
p=exp(a)/(exp(a)+1)
classfication <- table(p>.5,w_testdata$y)
logic_test_error[30]=1-sum(diag(classfication))/sum(classfication)

plot(logic_train_error,col="red")
points(logic_test_error,col="black")
 
# from the plot I found that the first 2 PCs lead to the smallest testing error,
#thus I will choose 2 PCs from now on for other methods

#LDA

library(MASS)
lda_model<-lda(y~PC1+PC2,data=w_traindata)
#confusion table
library(SDMTools)
confusion.matrix(w_traindata$y,predict(lda_model,w_traindata)$class)
#    obs
#pred   0   1
#0 280  35
#1   2 138
#attr(,"class")
#[1] "confusion.matrix"
# the training error rate of LDA is (2+35)/(2+35+138+280)=0.08131868

confusion.matrix(w_testdata$y,predict(lda_model,w_testdata)$class)
#    obs
#pred  0  1
#0 74  3
#1  1 36
#attr(,"class")
#[1] "confusion.matrix"
# testig error is 0.03508772

#QDA
qda_model<-qda(y~PC1+PC2,data=w_traindata)
confusion.matrix(w_traindata$y,predict(qda_model,w_traindata)$class)
#    obs
#pred   0   1
#0 269  23
#1  13 150
#attr(,"class")
#[1] "confusion.matrix"
# training error of QDA is (13+23)/(269+23+13+150)=0.07912088
confusion.matrix(w_testdata$y,predict(qda_model,w_testdata)$class)
#    obs
#pred  0  1
#0 74  2
#1  1 37
#attr(,"class")
#[1] "confusion.matrix"
# testing error of QDA is 0.02631579
# Graphics for LDA and QDA
library(klaR)
w_testdata$y<-factor(w_testdata$y)
partimat(y~PC1+PC2,data=w_testdata,method="lda")
partimat(y~PC1+PC2,data=w_testdata,method="qda")

# Decision Tree (useless)
library(rpart)
rpart_model<-rpart(y~PC1+PC2,data=w_traindata)
plot(rpart_model)
text(rpart_model)
printcp(rpart_model)
#Regression tree:
#  rpart(formula = y ~ PC1 + PC2, data = w_traindata)
#
#Variables actually used in tree construction:
#  [1] PC1 PC2
#
#Root node error: 107.22/455 = 0.23565
#
#n= 455 
#
#CP nsplit rel error  xerror     xstd
#1 0.655331      0   1.00000 1.00332 0.023245
#2 0.057718      1   0.34467 0.37328 0.048310
#3 0.035028      3   0.22923 0.28490 0.043701
#4 0.010000      5   0.15918 0.25545 0.040908
rpart_model.cp <- rpart_model$cptable[which.min(rpart_model$cptable[, "xerror"]), "CP"]
rpart_model.prune <- prune(rpart_model,rpart_model.cp)
class.pred<-table(predict(rpart_model.prune,type="class"), w_traindata$y) # wrong 
1-sum(diag(class.pred))/sum(class.pred)

# Dicision Tree (not done yet)
library(rpart)
w_rawdata<-wdbc[,2:32] # for decistion tree, we use raw data instead of rotated data
w_rawdata<-as.data.frame(w_rawdata)
colnames(w_rawdata)[1]<-"y"
for(i in 1:569){if(w_rawdata$y[i]=='B'){w_rawdata$y[i]=0} else{w_rawdata$y[i]=1}}
d=sort(sample(nrow(w_rawdata),nrow(w_rawdata)*.8)) # 80% train 20% test
w_rawtrain=w_rawdata[d,]
w_rawtest=w_rawdata[-d,]
w_rawtraindata<-as.data.frame(w_rawtrain)
w_rawtestdata<-as.data.frame(w_rawtest)
rpart_model1<-rpart(y~V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+V31+V32,data=w_rawtraindata)
plot(rpart_model1)
text(rpart_model1)

# Random Forest
library(randomForest)
y_rf<-w_traindata$y
x_rf<-w_traindata[,2:3]
y_rf_test<-w_testdata$y
x_rf_test<-w_testdata[,2:3]
rf_model<-randomForest(y_rf~.,data=x_rf,importance=TRUE,proximity=TRUE)
print(rf_model)
importance(rf_model)
plot(rf_model)
rf_pred=predict(rf_model,cbind(y_rf_test,x_rf_test),type="response",norm.votes=TRUE,predict.all=FALSE) 
rf_result_comparison<-cbind(rf_pred,w_testdata$y)
inco=0;for(i in 1:114){if(abs(rf_result_comparison[i,1]-rf_result_comparison[i,2])>=0.5){inco=inco+1}}
# testing error is inco/114=7/114=0.06140350877

# SVM
library(e1071)
svm_model=svm(as.factor(w_traindata$y)~.,data=w_traindata[,2:3])
pred <- fitted(svm_model)
table(pred, w_traindata$y)
#pred   0   1
#0 271  18
#1  11 155
# training error rate is 29/(29+271+155)=0.06373626
pred_test <- predict(svm_model, w_testdata[,2:3])
table(pred_test, w_testdata$y)
#pred_test  0  1
#0 68  5
#1  1 40
# testing error rate is 0.05263158

# SVM plot
svm_testfactor=factor(w_testdata$y)
svm_testdata=data.frame(PC1=w_testdata[,2],PC2=w_testdata[,3],y=svm_testfactor)
svm_trainfactor=factor(w_traindata$y)
svm_traindata=data.frame(PC1=w_traindata[,2],PC2=w_traindata[,3],y=svm_trainfactor)
svm_model_for_plot<-svm(y~PC1+PC2,data=svm_traindata)
plot(svm_model_for_plot,svm_testdata)

# neural network
library(neuralnet)
w_traindata$y<-as.numeric(w_traindata$y)  # I changed "y" into factor to dram some plot before, and it has to be numeric to call neuralnet()
for(i in 1:455){w_traindata$y[i]=w_traindata$y[i]-1} # Every value is added by 1(now it is (1,2)) somehow, I changed it back to (0,1)
neural_model<-neuralnet(y~PC1+PC2,data=w_traindata[,1:3],hidden=2)
neural_result<-compute(neural_model,w_testdata[,2:3])$net.result
neural_result_comparison<-cbind(neural_result,w_testdata$y)
inco=0;for(i in 1:114){if(abs(neural_result_comparison[i,1]-neural_result_comparison[i,2])>=0.5){inco=inco+1}} # to compute the testing error rate
 # test error rate is inco/114=2/114=0.01754385965