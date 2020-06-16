credit <- `creditcard`

head(credit, 10)
sum(is.na(credit))

logit<-glm(card ~ .,family= "binomial",data=credit)
summary(logit)


prob=predict(logit,type=c("response"),credit)
prob
confusion<-table(prob>0.5,credit$card)
confusion


Accuracy<-sum(diag(confusion))/sum(confusion)
Accuracy


library(ROCR)
library(pROC)

rocrpred<-prediction(prob,credit$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
auc <- auc(credit$card ~ prob)
auc

