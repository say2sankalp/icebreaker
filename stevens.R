rm(list= ls(all = TRUE))
setwd("~/anedge")
getwd()
stevens<- read.csv("stevens.csv")
str(stevens)
library(caTools)
set.seed(3000)
spl<- sample.split(stevens$Reverse,SplitRatio = 0.7)
train<- subset(stevens,spl== TRUE)
test<- subset(stevens,spl==FALSE)
#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
stree<- rpart(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data = train,method = "class",minbucket=5)
prp(stree)
predictcart<- predict(stree,newdata = test ,type = "class")
table(test$Reverse,predictcart)
library(ROCR)
predictROC<- predict(stree,newdata = test)
predictROC
pred<- prediction(predictROC[,2],test$Reverse)
perf<- performance(pred,"tpr","fpr")
plot(perf,colorize= TRUE)
as.numeric(performance(pred,"auc")@y.values)

# random forest
#install.packages("randomForest")
library(randomForest)
sforest<- randomForest(Reverse ~ Circuit+Issue+Petitioner+ Respondent +LowerCourt+Unconst,data = train,nodesize= 25,ntrees=200)
train$Reverse <- as.factor(train$Reverse)
test$Reverse<- as.factor(test$Reverse)
set.seed(100)
sforest1<- randomForest(Reverse ~ Circuit+Issue+Petitioner+ Respondent +LowerCourt+Unconst,data = train,nodesize= 25,ntrees=200)
predforest<- predict(sforest1,newdata = test)
table(test$Reverse,predforest)
#install.packages("caret")
library(caret)
#install.packages("e1071")
library(e1071)
# k fold validation 
numfold<- trainControl(method = "cv",number = 10)
#started creatign something i dont knwo 
cpfold<- expand.grid(.cp=seq(0.01,0.5,0.01))
train(Reverse ~ Circuit +Issue+Petitioner+ Respondent+LowerCourt+Unconst,data = train,method= "rpart",trControl= numfold,tuneGrid= cpfold )
streecv<- rpart(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data = train,method= "class",cp= 0.18)
predictcv<- predict(streecv,newdata = test,type = "class")
table(test$Reverse,predictcv)
prp(streecv)
