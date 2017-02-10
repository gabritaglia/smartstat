#Modello di Regressione Logistico


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




setwd("/home/gabriele/skyFTP")

#popolazione: prendo il file cookies2 che contiene 687.960 di cookies
library(dplyr)

load("sample2.1/cookies")

var_navi<-c("n_visite", "n_days_visite", "n_pagine_viste","n_entry_pages_distinte","n_entry_pages_totali")
var_day<-names(cookies)[c(171,172,174:177)] #no mercoledì
var_we<-names(cookies)[179] #no wd
var_fascia<-names(cookies)[c(180:182,184:186)] #no p_visite_afte
var_fasciawe<-names(cookies)[c(187,189:200)]  #no p_visite_prelav_we
var_fasciasabdom<-names(cookies)[202:221] #no p_visite_prelav_dom
var_piattaforma<-names(cookies)[c(228:231,233)] 
var_mobile<-names(cookies)[232]
var_pagesgiorno<-names(cookies)[c(235,238,240)]
var_pagesgiorno2<-names(cookies)[c(236,237,239)]
var_pagesArea<-names(cookies)[c(242:244,246:258)]
var_video<-names(cookies)[c(259,308)]
var_commerciale<-names(cookies)[c(245,260:262,264:267,294,309:311,313:316)] #noskytv
var_pagesSem<-names(cookies)[268:288] #notuttoilresto
var_entryArea<-names(cookies)[c(291:293,295:307)]
var_entrySem<-names(cookies)[317:337] #notuttoilresto
var_insPack<-names(cookies)[354:365]
var_log<-names(cookies)[401]

subset<-cookies[,c(
  #                     var_navi,
  var_day,          #7
  var_we,           #2
  #                 var_fascia,       #7
  #                 var_fasciawe,     #14
  var_fasciasabdom, #21
  #                 var_piattaforma,  #5
  var_mobile,        #1
  var_pagesgiorno,   #3
  #                 var_pagesgiorno2,  #3
  var_pagesArea,    #16
  #                 var_video,        #2
  #                 var_commerciale,  #18
  var_pagesSem,     #22
  var_entryArea,    #16
  var_entrySem,     #22
  var_log      #1
#   var_insPack      #12
)]

rownames(subset)<-cookies$visitorID


#logaritmizzo tutto
for( i in names(subset)){
  subset[is.na(subset[,i]),i] <- 0
  subset[,i]<-log(subset[,i]+1)
}


nomi<-c("globale","calcio", "sport", "cinema", "famiglia", "HD","tecnologiasky")
soggetti<-c("soggetto","soggetto_calcio", "soggetto_sport", "soggetto_cinema", "soggetto_famiglia","soggetto_HD","soggetto_tecnologiasky")

modelliEnzoCiCj<-list()


#Prima cosa, seleziono i cookies che hanno messo nel carrello il pacchetto X. Su questi scopro quali sono le determinanti della scelta del pacchetto Y
x=6
carrelloX<-cookies[,soggetti[x]]!="naviga"
soggetti[x]
sum(carrelloX)
#modificare ricorsivamente i
soggetti
i=5
soggetti[i]
# for (i in 1:length(soggetti)){
  
  subset$outcome<-factor(cookies[,soggetti[i]])
  table(subset$outcome)
  data<-subset[carrelloX,]
levels(data$outcome)
  levels(data$outcome)[levels(data$outcome)=="compra"]<-"carrello"
levels(data$outcome)  
  levels(data$outcome)<-c(1,0) #carrello naviga
  data$outcome<-as.numeric(as.character(data$outcome))
  table(data$outcome)
  
    
  modello<-glm(outcome~.,family="binomial",data=data) 
  
  pred<-predict(modello)
  pred<-antilogit(pred)
  summary(pred)
  
  
prop.table(table(data$outcome))
  pred1<-dicotomizza(pred,0.35)
  
  
  Performances(data$outcome, pred1)
  
  table(data$outcome)
  cof<-data.frame(modello$coefficients)
  
  summary(modello)
  
  modelliEnzoCiCj[[i]]<-modello
  names(modelliEnzoCiCj)[i]<-soggetti[i]
}


save("output/modelliEnzoCiCj")









