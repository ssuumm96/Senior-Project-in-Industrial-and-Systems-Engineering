library("e1071")
library(party)
library(datasets)

setwd("C:/Users/AtivBook/Downloads")
data <- read.csv("churn.csv")
attach(data)

train<-data[1:1000,]
test<-data[1001:1450,]

##로지스틱회귀
model<-glm(churn ~ ., data= train, family=binomial())
pred=predict(model,newdata=test,type="response")>0.5
table(pred,test$churn)
summary(model)
coef(model)

##후진소거
out <- step(model,direction = "backward")
summary(out)
pred=predict(out,newdata=test,type="response")>0.5
table(pred,test$churn)

##전진선택
model.s  <- glm(churn~1, data=train)
out <- step(model.s,scope=list(lower=model.s,upper=model),direction = "forward")
pred=predict(out,newdata=test,type="response")>0.5
table(pred,test$churn)





##svm
svm_model <-svm(churn ~.,data=train)
summary(svm_model)
pred = predict(svm_model,test)>0.5
table(pred,test$churn)

##의사결정트리
customer_ctree <- ctree(churn~., data=train)
pred = predict(customer_ctree,test)>0.5
table(pred,test$churn)
