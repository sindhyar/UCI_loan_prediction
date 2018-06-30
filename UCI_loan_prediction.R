rm(list=ls())
Credit_Default<-read.csv("C://Users/sindhya/Documents/CS513/R scripts/UCI_Credit_Card.csv")

# build a new data frame to use subset of the original data
Credit_Default$EDUCATION[Credit_Default$EDUCATION==0] <- NA
Credit_Default$EDUCATION[Credit_Default$EDUCATION==5] <- NA
Credit_Default$EDUCATION[Credit_Default$EDUCATION==6] <- NA
Credit_Default$MARRIAGE[Credit_Default$MARRIAGE==0] <- NA
Credit_Default<-na.omit(Credit_Default)

Credit_Default = Credit_Default[-1]
datafactor <-c(2,3,4,6:11,24)
Credit_Default[,datafactor]<-lapply(Credit_Default[,datafactor],factor)

set.seed(12345)
split = sample.split(Credit_Default$default.payment.next.month, SplitRatio = 0.7)
training_set = subset(Credit_Default, split == TRUE)
test_set = subset(Credit_Default, split == FALSE)

training_set <- training_set[,c('PAY_0', 'PAY_2', 'PAY_4','PAY_3','PAY_5','PAY_6',
                                'BILL_AMT1', 'LIMIT_BAL','EDUCATION','BILL_AMT2',
                                'PAY_AMT5','PAY_AMT6','PAY_AMT2','PAY_AMT4','PAY_AMT3','AGE','default.payment.next.month')]

training_set <- training_set[,c('PAY_0', 'BILL_AMT1', 'AGE', 'BILL_AMT2',
                                'BILL_AMT3', 'PAY_AMT1','LIMIT_BAL','BILL_AMT4',
                                'BILL_AMT6',
                                'BILL_AMT5','PAY_AMT2',
                                'PAY_AMT3','PAY_AMT6','default.payment.next.month')]

test_set<-test_set[,c('PAY_0', 'PAY_2', 'PAY_4','PAY_3','PAY_5','PAY_6',
                      'BILL_AMT1', 'LIMIT_BAL','EDUCATION','BILL_AMT2',
                      'PAY_AMT5','PAY_AMT6','PAY_AMT2','PAY_AMT4','PAY_AMT3','AGE','default.payment.next.month')]


#######Decision Tree- C5.0###################
library(C50)
tree_model<-C5.0(x=training_set[,-17],y=training_set$default.payment.next.month,trials =3)
tree_model
summary(tree_model)
plot(tree_model)
test1<-predict(tree_model,test_set)
result1<-cbind(test_set$default.payment.next.month,test1)


t<-table(result1[,1],result1[,2])
sum(diag(t))/sum(t)
accuracy <- sum(test1 == test_set[,17])/nrow(test_set) * 100
accuracy
cfm <- confusionMatrix(test1, test_set$default.payment.next.month)
cfm
install.packages("pROC")
library("pROC")
pred_prob<-predict(tree_model,test_set,type='prob')
auc<-auc(test_set$default.payment.next.month,pred_prob[,2])
plot(roc(test_set$default.payment.next.month,pred_prob[,2], main="ROC curve"))
legend(.6,.2, auc, title="AUC")


#ROC for random forest
library(randomForest)
rf_fit <- randomForest(default.payment.next.month~., data = training_set, 
                       importance= TRUE,ntree=1000)
rf_fit
importance(rf_fit)
varImpPlot(rf_fit)


prediction <- predict(rf_fit, test_set)
table(actual = test_set$default.payment.next.month, prediction)
accuracy <- sum(prediction == test_set$default.payment.next.month)/nrow(test_set)
accuracy

install.packages("pROC")
library("pROC")
pred_prob<-predict(rf_fit,test_set,type='prob')
auc<-auc(test_set$default.payment.next.month,pred_prob[,2])
plot(roc(test_set$default.payment.next.month,pred_prob[,2]))


###Naive bayes model##############3
install.packages('e1071', dependencies = TRUE)
library('e1071')

##Applying naive-bayes model
nbayes_dsn<- naiveBayes(default.payment.next.month ~.,data=Credit_Default) ## train the model with training dataset considering all columns
category_dsn<-predict(nbayes_dsn,Credit_Default)
category_dsn
table(Nbayes_train=category_dsn,Credit_Default$default.payment.next.month)
NB_wrong_dsn<-sum(category_dsn!=Credit_Default$default.payment.next.month)

NB_error_rate_dsn<-NB_wrong_dsn/length(category_dsn)
NB_error_rate_dsn

#Calculating accuracy for Naive Bayes model
prediction <- predict(nbayes_dsn, test_set)
?table()

t<-table(actual = test_set[,24], prediction)
accuracy <- sum(prediction == test_set$default.payment.next.month)/nrow(test_set) *100
accuracy

cfm <- confusionMatrix(prediction, test_set$default.payment.next.month)
cfm
#Plot ROC curve & find AUC

library(ROCR)
pred_test_naive<-predict(nbayes_dsn, newdata = test_set, type="raw")
p_test_naive<-prediction(pred_test_naive[,2], test_set$default.payment.next.month)
perf_naive<-performance(p_test_naive, "tpr", "fpr")
plot(perf_naive)
performance(p_test_naive, "auc")@y.values

