#try logistic regression instead of Random Forest

##############################################################################
#funzioni logit e "antilogit" e variance explained
##############################################################################
logit<-function(x) log(x/(1-x))
antilogit<-function(x)1*exp(x)/(1+exp(x))

X.VarExp<-function(y, yhat){
  rss<-sum((y-yhat)^2)
  tss<-sum((y-mean(y))^2)
  perc<-(1-rss/tss)*100
  return(perc)
}
X.VarRes<-function(y, yhat){
  rss<-sum((y-yhat)^2)
  tss<-sum((y-mean(y))^2)
  perc<-rss/tss*100
  return(perc)
}
##############################################################################
##############################################################################
load("dati/promo")
names(promo)
#mydata<-promo[-which(promo$promo_redemtption%in%c(0,100)),] #tolgo gli 0 100
mydata<-mydata[,-c(1,43:256)]

names(mydata)
dim(mydata)
####### LOGIT REGRESSION ######################
red<-mydata$promo_redemption/100
summary(red)
qred<-mydata$buoni_qta_redenta
summary(qred)
mydata<-mydata[, -which(names(mydata)%in% c("promo_redemption",
                                            "buoni_qta_redenta"))]

mylm<-lm( logit(red)~., data=mydata)


plot(antilogit(mylm$fitted.values))
plot(antilogit(mydata$y))

X.VarExp(y=antilogit(mydata$y),
         yhat=antilogit(mylm$fitted.values))


summary(mylm)
names(mylm)

yhat<-predict(mylogit, mydata)

X.VarExp(y=mydata$y, yhat=yhat)


#modello con outcome qta_buoni_redenti
mydata$y<-d$promo_redemption/100*d$buoni_qta_emessa

mylm<-lm(y~., data=mydata)
summary(mylm)

X.VarExp(mydata$y, mylm$fitted.values)



#poisson model or binomiale negativa

mydata$y<-round(d$promo_redemption/100*d$buoni_qta_emessa,0)
summary(mydata$y)
bp<-boxplot(mydata$y) #cerco gli outliers
mydata<-mydata[-bp$out,]
hist(mydata$y, breaks=20)
curve(dpois(x, lambda=mean(mydata$y)))
var(mydata$y)
mean(mydata$y)
library(MASS)
mydata1<-mydata[,which(names(mydata)%in%c("cliente_sottosettore"))]

negbin<-glm.nb(mydata$y ~., data=mydata1)
names(negbin)
summary(negbin$fitted.values)
summary(mydata$y)

X.VarExp(mydata$y, negbin$fitted.values)


fit <- glm(y ~., data=mydata, family=poisson())
summary(fit)
names(fit)
summary(fit$fitted.values)
X.VarExp(mydata$y, fit$fitted.values)

plot(mydata$y, fit$fitted.values,xlim=c(0,1000),ylim=c(0,1000))
cor(mydata$y, fit$fitted.values)

summary(d$promo_redemption/100)
redhat<-fit$fitted.values/mydata$buoni_qta_emessa
summary(redhat)
cor(redhat,d$promo_redemption/100)
hist(redhat)

summary(redhat)
plot(redhat,d$promo_redemption/100,xlim=c(0,1),ylim=c(0,1))

X.VarExp(d$promo_redemption[-bp$out]/100, redhat)







