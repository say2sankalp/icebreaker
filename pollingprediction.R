rm(list = ls(all= TRUE) )
getwd()
poll<- read.csv("~/anedge/PollingData.csv")
str(poll)
library(corrgram)
corrgram(poll)
summary(poll)
plot(poll)
library(mice)
simple<- poll[c("SurveyUSA","Rasmussen","PropR","DiffCount")]
str(simple)
summary(simple)
set.seed(144)
imputed<- complete(mice(simple))
poll
poll$Rasmussen<- imputed$Rasmussen
poll$SurveyUSA<- imputed$SurveyUSA
train<- subset(poll,Year ==2004| Year==2012)
test<- subset(poll,Year== 2012)
table(train$Republican)
# predicting model variable
table(sign(train$Rasmussen))
table(sign(train$Republican))
table(train$Republican ,sign(train$Rasmussen))
cor(train[c("Rasmussen","SurveyUSA","PropR","DiffCount","Republican")])
model1<- glm(Republican~ PropR,data = train,family = "binomial")
summary(model1)
pred1<- predict(model1,type = "response")
table(train$Republican,pred1 >= 0.5)
model2<- glm(Republican ~ SurveyUSA,data= train,family = "binomial")
summary(model2)
pred2<- predict(model2,type = "response") 
table(train$Republican, pred2> 0.5)
table(test$Republican,sign(test$Rasmussen))
testpred1<- predict(model2,newdata = test,family= "response")
table(test$Republican,testpred1 >= 0.5)
testpred1
test$Republican
