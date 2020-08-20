datax<-read.csv("redatax.csv")
datay<-read.csv("redatay.csv")
datax$G1 <- as.factor(datax$G1)
datax$G2 <- as.factor(datax$G2)
datax$G3 <- as.factor(datax$G3)
datax$Medu <- as.factor(datax$Medu)
datax$Fedu <- as.factor(datax$Fedu)
datax$famrel <- as.factor(datax$famrel)
datax$freetime <- as.factor(datax$freetime)
datax$goout <- as.factor(datax$goout)
datax$Dalc <- as.factor(datax$Dalc)
datax$Walc <- as.factor(datax$Walc)
datax$health <- as.factor(datax$health)
x0<-data.frame(datax[,2:14],datax$G1)
#x3<-data.frame(datax[,2:33])
#x2<-data.frame(datax[,2:32])
#x1<-data.frame(datax[,2:31])

datay$G1 <- as.factor(datay$G1)
datay$G2 <- as.factor(datay$G2)
datay$G3 <- as.factor(datay$G3)
datay$Medu <- as.factor(datay$Medu)
datay$Fedu <- as.factor(datay$Fedu)
datay$famrel <- as.factor(datay$famrel)
datay$freetime <- as.factor(datay$freetime)
datay$goout <- as.factor(datay$goout)
datay$Dalc <- as.factor(datay$Dalc)
datay$Walc <- as.factor(datay$Walc)
datay$health <- as.factor(datay$health)
y3<-data.frame(datay[,2:33])
y2<-data.frame(datay[,2:32])
y1<-data.frame(datay[,2:31])


install.packages("rpart")
library(rpart)
set.seed(1234)
ind=sample(2,nrow(x0),replace = T,prob = c(0.7,0.3))
traintset1<-x0[ind==1,]
testset1<-x0[ind==2,]
rp<-rpart(datax.G1~.,data = traintset1)
printcp(rp)
plot(rp,margin =0.1)
text(rp,all = F,use.n = F)
pred = predict(rp,testset1,type = "class")
treetab = table(pred,testset1$datax.G1)
treeacc = sum(diag(treetab))/sum(treetab)

ind=sample(2,nrow(x2),replace = T,prob = c(0.7,0.3))
traintset2<-x2[ind==1,]
testset2<-x2[ind==2,]
rp2<-rpart(G2~.,data = traintset2)
printcp(rp2)
plot(rp2,margin =0.1)
text(rp2,all = F,use.n = F)
pred2 = predict(rp2,testset2,type = "class")
treetab2 = table(pred2,testset2$G2)
treeacc2 = sum(diag(treetab2))/sum(treetab2)

ind=sample(2,nrow(x3),replace = T,prob = c(0.7,0.3))
traintset3<-x3[ind==1,]
testset3<-x3[ind==2,]
rp3<-rpart(G3~.,data = traintset3)
printcp(rp3)
plot(rp3,margin =0.1)
text(rp3,all = F,use.n = F)

#try multinomial glm
library(nnet)
g1<-multinom(G1~Dalc+failures+freetime+goout+health+Mjob+reason+sex,
             data = traintset1)
p<-predict(g1, testset1)
tab1<-table(p,testset1$G1)
acc1<-sum(diag(tab1))/sum(tab1)

#3 cate
g1<-multinom(G1~absences+failures+Fedu+Fjob+freetime+health+Medu+Mjob+paid+reason+schoolsup, 
             data = traintset1)
p1<-predict(g1_step, testset1)
tab1<-table(p1,testset1$G1)
acc1<-sum(diag(tab))/sum(tab)
####################################################
g2<-multinom(G2~G1+Walc,
             data = traintset2)
p2<-predict(g2,testset2)
tab2<-table(p2,testset2$G2)
tab2
acc2<-sum(diag(tab2))/sum(tab2)
#3 cate
g2<-multinom(G2~G1,
             data = traintset2)
p2<-predict(g2,testset2)
tab2<-table(p2,testset2$G2)
tab2
acc2<-sum(diag(tab2))/sum(tab2)
####################################################
g3<-multinom(G3~absences+Fedu+G2,
             data = traintset3)
p3<-predict(g3,testset3)
tab3<-table(p3,testset3$G3)
acc3<-sum(diag(tab3))/sum(tab3)

#3 cate
g3<-multinom(G3~absences+Fedu+G2,
             data = traintset3)
p3<-predict(g3,testset3)
tab3<-table(p3,testset3$G3)
acc3<-sum(diag(tab3))/sum(tab3)

