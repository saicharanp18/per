# Question 1

rm(list=ls())
library("ALL"); data(ALL)
IsB <- ALL$BT %in% c("B","B1","B2","B3", "B4")

# Question 1.b
IsB1 <- factor(IsB, labels = c("T", "B"))
prob.name <-c("39317_at", "38018_g_at")
expr.data <- exprs(ALL)[prob.name,]

require(rpart)
B.stage<- factor(IsB)
c.tr <-rpart(B.stage ~ ., data = data.frame(t(expr.data)))
rpartpred<- predict(c.tr, type="class")
table(rpartpred, B.stage)
plot(c.tr, branch=0, margin=0.1);
text(c.tr, digits=3)

library(ROCR)
data.lgr <- data.frame(IsB, t(expr.data))
fit.lgr <- glm(IsB~., family=binomial(link='logit'), data=data.lgr)
pred.prob <-  predict(fit.lgr, data=data.lgr[,-1], type="response")
pred <- prediction(pred.prob, IsB)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Question 1.c
performance(pred,"auc")

# Question 1.d
require(caret)
n<-dim(data.lgr)[1]
index<-1:n
K<-10
flds<- createFolds(index, k=K)
mcr.cv.raw<-rep(NA, K)
for (i in 1:K) {
  testID<-flds[[i]]
  data.tr<-data.lgr[-testID,]
  data.test<-data.lgr[testID,]
  fit.lgr<-glm(IsB~., family=binomial(link='logit'), data=data.tr)
  pred.prob<-predict(fit.lgr, newdata=data.test, type="response")
  pred.B<-(pred.prob> 0.5)
  mcr.cv.raw[i]<- sum(pred.B!="B" & IsB =="B")/sum(IsB =="B") 
  mcr.cv.raw[i] <- sum(pred.B!=data.test$IsB)/length(pred.B)
}
mean(mcr.cv.raw)

# Question 1.e
rm(list=ls())
library("ALL"); data(ALL)
IsB <- ALL$BT %in% c("B","B1","B2","B3", "B4")
prob.name <-c("39317_at", "38018_g_at")
y <- as.numeric(IsB)
expr.data <- exprs(ALL)[prob.name,]
reg.lgr <- glm(y~t(expr.data), family=binomial(link='logit'))
pred.prob <- predict(reg.lgr, data=reg.lgr$expr.data, type="response")
pred.B <- factor(pred.prob> 0.5, levels=c(TRUE,FALSE), labels=c("B","T"))
IsB1 <- factor(IsB, levels=c(TRUE,FALSE), labels=c("B","T"))
table(pred.B, IsB1)
confint(reg.lgr, level=0.8)

# Question 1.f
data.lgr <- data.frame(IsB, t(expr.data))
n<-dim(data.lgr)[1]
mcr.cv.raw<-rep(NA, n) 
for (i in 1:n) {
  data.tr<-data.lgr[-i,]
  data.test<-data.lgr[i,]
  fit.lgr<-glm(IsB~., family=binomial(link='logit'), data=data.tr)
  pred.prob<-predict(fit.lgr, newdata=data.test, type="response")
  pred.B<-(pred.prob> 0.5)
  mcr.cv.raw[i]<-sum(pred.B!=data.test$IsB)/length(pred.B)
}
mean(mcr.cv.raw)

# Question 1.g
rm(list=ls())
library("ALL"); data(ALL)
Alldata <- t(exprs(ALL))
x <- prcomp(Alldata, scale. = TRUE)
PropVar<-summary(x)$importance[2,]
plot(1:length(PropVar), PropVar, xlab='number of principal components', ylab='proportion of variance explained')

# Question 1.h
IsB <- ALL$BT %in% c("B","B1","B2","B3", "B4")
patients <- factor(IsB, labels = c("T", "B"))
data.pca<-x$x[,1:5]
n<-length(patients)
All.svm<- svm(data.pca, patients, type = "C-classification", kernel = "linear")
svmpred<- predict(All.svm , data.pca)
mcr.svm <-mean(svmpred!=patients)
mcr.svm

# Question 1.i
mcr.cv.raw<-rep(NA, n)
for (i in 1:n) {
  svmest <-svm(data.pca[-i,], patients[-i], type = "C-classification", kernel = "linear")
  svmpred <-predict(svmest, t(data.pca[i,]))
  mcr.cv.raw[i] <-mean(svmpred!=patients[i])
}
mean(mcr.cv.raw)

# Question 2

#For tree method
rm(list=ls())
mcr.tr.tree <- rep(NA, 4)
mcr.cv.tree <- rep(NA, 4)
require(rpart)
pca.iris<-prcomp(iris[,1:4], scale=TRUE)
Species<-iris$Species
n<-length(Species)
for (k in 1:4){
  data.pca<-pca.iris$x[,1:k]
  iris2<-data.frame(Species, data.pca)
  fit<- rpart(Species ~ ., data = iris2, method = "class")
  pred.tr<-predict(fit, iris2, type = "class")
  mcr.tr.tree[k] <-mean(pred.tr!=Species)
  mcr.cv.raw<-rep(NA, n)
  for (i in 1:n) {
    fit.tr <-rpart(Species ~ ., data = iris2[-i,], method = "class")
    pred <-predict(fit.tr, iris2[i,], type = "class")
    mcr.cv.raw[i]<- mean(pred!=Species[i])
  }
  mcr.cv.tree[k] <- mean(mcr.cv.raw)
}
mcr.cv.tree
mcr.tr.tree

# For logistic method
mcr.cv.log <- rep(NA, 4)
mcr.lgr.log <- rep(NA, 4)
for (k in 2:4){
  data.pca<-pca.iris$x[,1:k]
  iris2<-data.frame(Species, data.pca)
  iris2.lgr <-vglm(Species~., family=multinomial, data=iris2)
  pred.prob <- predict(iris2.lgr, iris2[,-1], type="response")
  pred.lgr <-apply(pred.prob, 1, which.max) 
  pred.lgr<- factor(pred.lgr, levels=c("1","2","3"), labels=levels(iris2$Species))
  mcr.lgr.log[k] <-mean(pred.lgr!=iris2$Species)
  mcr.cv.raw<-rep(NA, n)
  for (i in 1:n) {
    fit.lgr <-vglm(Species~., family=multinomial, data=iris2[-i,])
    pred.prob<- predict(fit.lgr, iris2[i,-1], type="response")
    pred <-apply(pred.prob, 1, which.max)
    pred <-factor(pred, levels=c("1","2","3"), labels=levels(iris2$Species))
    mcr.cv.raw[i]<-mean(pred!=Species[i])
  }
  mcr.cv.log[k] <- mean(mcr.cv.raw)
}
#logistic for K = 1
data.pca<-pca.iris$x[,1]
iris2<-data.frame(Species, data.pca)
iris2.lgr <-vglm(Species~., family=multinomial, data=iris2)
pred.prob <- predict(iris2.lgr, iris2[,-1], type="response")
pred.lgr <-apply(pred.prob, 1, which.max) 
pred.lgr<- factor(pred.lgr, levels=c("1","2","3"), labels=levels(iris2$Species))
mcr.lgr.log[1] <-mean(pred.lgr!=iris2$Species)
mcr.cv.raw<-rep(NA, n)
for (i in 1:n) {
  fit.lgr <-vglm(Species~., family=multinomial, data=iris2[-i,])
  pred.prob<- predict(fit.lgr, iris2[i,-1], type="response")
  pred <-apply(pred.prob, 1, which.max)
  pred <-factor(pred, levels=c("1","2","3"), labels=levels(iris2$Species))
  mcr.cv.raw[i]<-mean(pred!=Species[i])
}
mcr.cv.log[1] <- mean(mcr.cv.raw)

mcr.cv.log
mcr.lgr.log

# For VCM method
mcr.svm.vcm <- rep(NA, 4)
mcr.cv.vcm <- rep(NA, 4)
for (k in 2:4) {
  data.pca<-pca.iris$x[,1:k]
  iris2<-data.frame(Species, data.pca)
  iris2.svm<- svm(data.pca, Species, type = "C-classification", kernel = "linear")
  svmpred<- predict(iris2.svm , data.pca)
  mcr.svm.vcm[k] <-mean(svmpred!=Species)
  mcr.cv.raw<-rep(NA, n)
  for (i in 1:n) {
    svmest <-svm(data.pca[-i,], Species[-i], type = "C-classification", kernel = "linear")
    svmpred <-predict(svmest, t(data.pca[i,]))
    mcr.cv.raw[i] <-mean(svmpred!=Species[i])
  }
  mcr.cv.vcm[k] <-mean(mcr.cv.raw)
}
# SVM for K = 1
data.pca<-pca.iris$x[,1]
iris2<-data.frame(Species, data.pca)
iris2.svm<- svm(data.pca, Species, type = "C-classification", kernel = "linear")
svmpred<- predict(iris2.svm , data.pca)
mcr.svm.vcm[1] <-mean(svmpred!=Species)
mcr.cv.raw<-rep(NA, n)
for (i in 1:n) {
  svmest <-svm(data.pca[-i], Species[-i], type = "C-classification", kernel = "linear")
  svmpred <-predict(svmest, t(data.pca[i]))
  mcr.cv.raw[i] <-mean(svmpred!=Species[i])
}
mcr.cv.vcm[1] <-mean(mcr.cv.raw)

mcr.cv.vcm
mcr.svm.vcm
