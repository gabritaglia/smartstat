#Random Forest esempio
require(randomForest)


load("work/d")
load("work/tt")
train1<-tt$train1
covariate<-tt$covariate
test1<-tt$test1
###########################################
#RANDOM FOREST REGRESSIONE
###########################################
outcome1<-d[,"promo_redemption"]
pred.matrix<-d[,covariate]
set.seed(10)
rf11<-randomForest(pred.matrix[train1,], 
                   outcome1[train1],
                   ntree=1000, 
                   importance=TRUE,
                   proximity=T)
load("rf/rf11")
str(rf11)

rf11$call

rf11$predicted[1:10]

imp<-rf11$importance
imp<-imp[order(-imp[,2]),]
imp[1:10,]

rf11$proximity[1:5,1:5]

predicted.test<-predict(rf11, d[test1,variabili])
actual.test<-d[test1,"promo_redemption"]

predicted.train<-rf11$predicted
actual.train<-d[train1,"promo_redemption"]


X.VarExp<-function(y, yhat){
  rss<-sum((y-yhat)^2)
  tss<-sum((y-mean(y))^2)
  perc<-(1-rss/tss)*100
  return(perc)
}

X.VarExp(y=actual.train, yhat=predicted.train)
X.VarExp(y=actual.test, yhat=predicted.test)

###########################################
#QUANTILE RANDOM FOREST
###########################################
require(quantregForest)
db.qrf<-d[train1,covariate]
db.test<-d[test1,covariate]
outcome.qrf<-d$promo_redemption[train1]
outcome.test<-d$promo_redemption[test1]
#Limite delle quantregForest
#Can not handle categorical predictors with more than 32 categories.

qrf <- quantregForest(x=db.qrf, y=outcome.qrf)
# NB. La foresta casuale e la foresta "quantile" devono essere costruite sullo stesso set di variabili

load("rf/qrf")

qrf


#stimo i centili per tutti i dati di test
quart<-c(0.25,0.5,0.75)
prc<-predict(qrf, newdata= db.qrf, quart)
prc[1:10,]
