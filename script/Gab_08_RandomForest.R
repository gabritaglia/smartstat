#random forest
#3 step:
#1 - popolazione navigatori outcome acquisto si/no
#2 - popolazione carrellisti outcome acquisto si/no
#1 - popolazione navigatori outcome carrello si/no

setwd("/home/stefano/LAVORO/SKY1")

#popolazione: prendo il file cookies2 che contiene 687.960 di cookies
library(dplyr)
library(randomForest)
library(doMC)

load("sample2/cookies")
#aggiustamenti
cookies$n_entry_pages_totali[is.na(cookies$n_entry_pages_totali)]<-1
cookies[is.na(cookies)]<-0
cookies$mobile.vendor<-factor(paste0(cookies$piattaforma,".",cookies$vendor))

nav<-cookies %>%
  filter(compra==0 & carrello==0)%>%
  select(visitorID)
nav<-nav[,1]
nav<-sample(nav,length(nav)/4)

# nav_tanto<-cookies %>%
#   #filter(compra==0 & carrello==0)%>%
#   filter( n_visite>quantile(n_visite, 0.25)
#           | n_days_visite>quantile(n_days_visite, 0.25)
#           | n_pagine_viste>quantile(n_pagine_viste, 0.25)
#           #           & n_entry_pages_distinte>mean(n_entry_pages_distinte)
#           #           & n_entry_pages_totali>mean(n_entry_pages_totali)
#   )%>%
#   select(visitorID)
# nav_tanto<-nav_tanto[,1]
# 
# cookies$nav_tanto<-0
# cookies$nav_tanto[cookies$visitorID %in% nav_tanto]<-1
# sum(cookies$nav_tanto)

acq<-cookies %>%
  filter(compra==1)%>%
  select(visitorID)
acq<-acq[,1]

car<-cookies %>%
  filter(carrello==1)%>%
  select(visitorID)
car<-car[,1]

acq_calcio<-cookies %>%
  filter(compra==1 & PacchettoCalcio==1)%>%
  select(visitorID)
acq_calcio<-acq_calcio[,1]

acq_sport<-cookies %>%
  filter(compra==1 & PacchettoSport==1)%>%
  select(visitorID)
acq_sport<-acq_sport[,1]

acq_cinema<-cookies %>%
  filter(compra==1 & PacchettoCinema==1)%>%
  select(visitorID)
acq_cinema<-acq_cinema[,1]
#NOTA SULLE VARIABILI DELLA RANDOM FOREST
#PER RENDERE PIU' AGEVOLI I CONTI INIZIO AD ESCLUDERE TALUNE VARIABILI RIDONDANTI O TROPPO DETTAGLIATE.
#suddivido le variabili in gruppi:
#VARIABILI DI NAVIGAZIONE DETTAGLIO
#   X1Monday.00:X7Sunday.23
#VARIABILI NAVIGAZIONE
#   n_visite, n_days_visite, n_pagine_viste, n_entry_pages_distinte,n_entry_pages_totali
#   p_visite_Lun:p_visite_Dom
#   p_visite_we, p_visite_wd
#   p_visite_prelav:p_visite_nigh
#   p_visite_prelav_wd:p_visite_nigh_we
#VARIABILI NON CATEGORIALI (GIORNI)
#   first_day, last_day, purchase_day, basket_day
#PIATTAFORMA
#   old, vendor, browser, mobile, mobile.vendor
#CONTENUTO DI NAVIGAZIONE PAGES
#   pages_video:pages_sport
#   pages_acquisto_cinema:pages_problemi_tecnici
#CONTENUTO DI NAVIGAZIONE ENTRY
#   entry_video:entry_sport
#   entry_acquisto_cinema:entry_problemi_tecnici
#LOG
#   log
#VARIABILI LEGATE ALL'ACQUISTO  
#   days_before_purchase, days_before_basket, days_basket_purchase, n_pacchetti
#VARIABILI LEGATE AL SOGGETTO
#   compra, carrello,vede_promo,soggetto,
#   soggetto_calcio, soggetto_cinema, soggetto_sport
#   PacchettoCalcio, PacchettoCinema,PacchettoSport


subset<- cookies %>%
  select(visitorID,n_visite, n_days_visite, n_pagine_viste,n_entry_pages_distinte,n_entry_pages_totali,
         p_visite_Mar:p_visite_Dom,    #escludo la collineare
         p_visite_wd,                  #escludo la collineare
         p_visite_morn:p_visite_nigh,  #escludo la collineare
#          p_visite_morn_wd:p_visite_nigh_we, #escludo la collineare
         old, vendor, browser, mobile, mobile.vendor,
         pages_video:pages_sport,
         entry_video:entry_sport,
#          entry_acquisto_cinema:entry_problemi_tecnici,
#          pages_acquisto_cinema:pages_problemi_tecnici,
#          n_pacchetti, days_before_basket,
#          PacchettoCinema, PacchettoSport, PacchettoCalcio,
         log) %>%
  select(-c(entry_commerciale,pages_commerciale)#ovvietà
         )

# subset<- cookies %>%
#   select(visitorID,n_visite, n_days_visite, n_pagine_viste,n_entry_pages_distinte,n_entry_pages_totali,
#          p_visite_Lun:p_visite_Dom,    #escludo la collineare
#          p_visite_wd,p_visite_we,                 #escludo la collineare
#          p_visite_prelav:p_visite_nigh,  #escludo la collineare
#          #          p_visite_morn_wd:p_visite_nigh_we, #escludo la collineare
#          old, vendor, browser, mobile, mobile.vendor,
#          pages_video:pages_sport,
#          entry_video:entry_sport,
#          #          entry_acquisto_cinema:entry_problemi_tecnici,
#          #          pages_acquisto_cinema:pages_problemi_tecnici,
# #          n_pacchetti, days_before_basket,
# #          PacchettoCinema, PacchettoSport, PacchettoCalcio,
#          log
#   )

rownames(subset)<-cookies$visitorID
subset$n_entry_pages_totali[is.na(subset$n_entry_pages_totali)]<-1
subset[is.na(subset)]<-0
subset[,is.character(subset)]<-apply(subset[,is.character(subset)], 2, factor)

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




numeroProcessi<-4
# cutoff=1-c(prop.table(table(mydata$outcome)))
# classwt=c(prop.table(table(mydata$outcome)))
cutoff=rep(1/length(levels(mydata$outcome)),length(levels(mydata$outcome)))
# sampsize=c(1000,100)
nodesize=50
registerDoMC(numeroProcessi) #-- numero di processi
rf <- foreach(ntree=rep(150,numeroProcessi),
              .combine='combine',
              .packages='randomForest',
              .multicombine=FALSE) %dopar%
              randomForest(outcome~. ,data=mydata,
                          ntree=ntree,importance=TRUE,
                           proximity=FALSE,
#                            classwt=classwt,
#                            cutoff=cutoff,
#                            sampsize=c(10,10), strata=mydata$outcome,
                           nodesize=nodesize)

Performances(rf)

# rf
# save(rf,file="rf/rf_NC_600tree_40var_noOvvio")


# load("rf/rf_NC_600tree_42var")



a<-importanzaVarClass(rf, "carrello")
round(a,2)


a<-rf$importance
round(a,2)




#########-------------------#########-------------------------#########
Performances<-function(rf, outcome=mydata$outcome,beta=0.5){
  t<-table(outcome, rf$predicted)
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

importanzaVarClass<-function(rf, namecol="MeanDecreaseGini"){
  ord.var<-order(rf$importance[,namecol], decreasing=TRUE)
  var.rf<-data.frame(rf$importance[ord.var,namecol])
  names(var.rf)<-"MeanDecreaseGini"
  print(plot(var.rf[,1], type="b", pch=19, col="blue", ylab="MeanDecreaseGini"))
  text(var.rf[,1],label=row.names(var.rf),cex=0.7)
  return(var.rf)
}

X.VarExp<-function(y, yhat){
  rss<-sum((y-yhat)^2)
  tss<-sum((y-mean(y))^2)
  perc<-(1-rss/tss)*100
  return(perc)
}
