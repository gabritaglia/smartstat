#Modello di Regressione Logistico

#1 - popolazione navigatori outcome acquisto si/no
#2 - popolazione carrellisti outcome acquisto si/no
#1 - popolazione navigatori outcome carrello si/no

setwd("/home/stefano/LAVORO/SKY1")

#popolazione: prendo il file cookies2 che contiene 687.960 di cookies
library(dplyr)

load("sample2.1/cookies")

var_navi<-c("n_visite", "n_days_visite", "n_pagine_viste","n_entry_pages_distinte","n_entry_pages_totali")
          var_day<-names(cookies)[c(171,172,174:177)] #escluso mercoledì
           var_we<-names(cookies)[179] #escludo wd
       var_fascia<-names(cookies)[c(180:182,184:186)] p_visite_afte
     var_fasciawe<-names(cookies)[187:200]
 var_fasciasabdom<-names(cookies)[202:221] #escludo p_visite_prelav_dom
  var_piattaforma<-names(cookies)[c(228:231,233)]
       var_mobile<-names(cookies)[232]
  var_pagesgiorno<-names(cookies)[235:240]
    var_pagesArea<-names(cookies)[c(242:244,246:258)]
        var_video<-names(cookies)[c(259,308)]
  var_commerciale<-names(cookies)[c(245,260:267,294,309:316)]
     var_pagesSem<-names(cookies)[268:289]
    var_entryArea<-names(cookies)[c(291:293,295:307)]
     var_entrySem<-names(cookies)[317:338]
      var_insPack<-names(cookies)[354:365]
          var_log<-names(cookies)[401]

subset<-cookies[,c(
                var_day,          #7
                var_we,           #2
#                 var_fascia,       #7
#                 var_fasciawe,     #14
                var_fasciasabdom, #21
#                 var_piattaforma,  #5
                var_mobile,       #1
                var_pagesgiorno,  #6
                var_pagesArea,    #16
                var_video,        #2
#                 var_commerciale,  #18
                var_pagesSem,     #22
                var_entryArea,    #16
                var_entrySem,     #22
#                 var_insPack,      #12
                var_log          #1
                )]

rownames(subset)<-cookies$visitorID


#logaritmizzo tutto
for( i in names(subset)){
  subset[,i]<-log(subset[,i]+1)
}

subset$outcome<-factor(cookies$soggetto_calcio)
levels(subset$outcome)[levels(subset$outcome)=="compra"]<-"carrello"
levels(subset$outcome)<-c(1,0)
subset$outcome<-as.numeric(as.character(subset$outcome))

#ripartiziono il campione 10:1
sottoinsieme<-sample(which(subset$outcome==0),sum(subset$outcome==1)*10)
sottoinsieme<-c(sottoinsieme, which(subset$outcome==1))

#prova
data<-subset[sottoinsieme,]

modello<-glm(outcome~.,family="binomial",data=data) 

pre<-predict(modello)

pred<-antilogit(a)
summary(pred)

pred1<-dicotomizza(pred,0.1)

table(pred1)

Performances(data$outcome, pred1)

str(modello)

modello$coefficients

library(relaimpo)
calc.relimp(modello,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Stepwise Regression
library(MASS)
step <- stepAIC(modello, direction="forward")
step$anova # display results 













dicotomizza<-function(x,soglia=0.5){
  x[x>=soglia]<-1
  x[x<soglia]<-0
  return(x)
}  
logit<-function(x){
  y=log(x/(1-x))
  return(y)
}
antilogit<-function(y){
  x=(exp(y))/(1+exp(y))
  return(x)
}
  
Performances<-function(actual,predicted,beta=0.5){
    t<-table(actual,predicted)
    TN<-t[1,1]
    TP<-t[2,2]
    FN<-t[2,1]
    FP<-t[1,2]
    p=list()
    p$ConfusionMatrix<-t
    p$ClassError<-c(t[1,2], t[2,1])/margin.table(t,1)
    p$TrueNegativeRate<-TN/(TN+FP)
    p$TruePositiveRate<-TP/(TP+FN)
    p$GMean<-sqrt(p$TrueNegativeRate*p$TruePositiveRate)
    p$WeightedAccuracy<-beta*p$TrueNegativeRate+beta*p$TrueNegativeRate
    p$Precision<-TP/(TP+FP)
    p$Recall=p$TruePositiveRate
    p$FMeasure<-(2*p$Precision*p$Recall)/(p$Precision+p$Recall)
    return(p)
  }



# # ------ OUTCOME FORESTA NC
# subset$outcome<-factor(cookies$soggetto)
# levels(subset$outcome)[levels(subset$outcome)=="compra"]<-"carrello"
# mydata<-subset %>%
#   filter(visitorID%in%nav | visitorID%in%car | visitorID%in%acq)%>%
#   select(-(visitorID))
# ------ OUTCOME FORESTA NC-CALCIO
subset$outcome<-factor(cookies$soggetto_calcio)
levels(subset$outcome)[levels(subset$outcome)=="compra"]<-"carrello"
mydata<-subset %>%
  filter(visitorID%in%nav | visitorID%in%car | visitorID%in%acq)%>%
  select(-(visitorID))

# # ------ OUTCOME FORESTA CA
# subset$outcome<-factor(cookies$soggetto)
# mydata<-subset %>%
#   filter(visitorID%in%car | visitorID%in%acq)%>%
#   select(-(visitorID))
# mydata$outcome<-factor(mydata$outcome)
# # ------ OUTCOME FORESTA CA-CALCIO
# subset$outcome<-factor(cookies$soggetto_calcio)
# mydata<-subset %>%
#   filter(visitorID%in%car | visitorID%in%acq_calcio)%>%
#   select(-(visitorID))
# levels(mydata$outcome)<-c("carrello","compra", "carrello")
# # ------ OUTCOME FORESTA CA-CINEMA
# subset$outcome<-factor(cookies$soggetto_cinema)
# mydata<-subset %>%
#   filter(visitorID%in%car | visitorID%in%acq_cinema)%>%
#   select(-(visitorID))
# levels(mydata$outcome)<-c("carrello","compra", "carrello")


