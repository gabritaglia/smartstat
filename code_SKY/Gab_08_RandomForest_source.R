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
numeroProcessi<-5


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

cookies$nav_tanto<-0
cookies$nav_tanto[cookies$visitorID %in% nav_tanto]<-1
sum(cookies$nav_tanto)


acq<-cookies %>%
  filter(compra==1)%>%
  select(visitorID)
acq<-acq[,1]

car<-cookies %>%
  filter(carrello==1)%>%
  select(visitorID)
car<-car[,1]
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
         p_visite_Mar:p_visite_Dom,
         p_visite_wd,
         p_visite_morn:p_visite_nigh,
         #         p_visite_morn_wd:p_visite_nigh_we,
         old, vendor, browser, mobile, mobile.vendor)


rownames(subset)<-cookies$visitorID

subset$n_entry_pages_totali[is.na(subset$n_entry_pages_totali)]<-1
subset[is.na(subset)]<-0

mydata<-subset %>%
  filter(visitorID%in%nav | visitorID%in%car | visitorID%in%acq)%>%
  select(-(visitorID))

outcome<-cookies %>%
  filter(visitorID%in%nav | visitorID%in%car | visitorID%in%acq)%>%
  select(soggetto)
outcome<-factor(outcome[,1])

numeroProcessi<-numeroProcessi
cutoff=c(prop.table(table(outcome)))
registerDoMC(numeroProcessi) #-- numero di processi
rf <- foreach(ntree=rep(30,numeroProcessi),.combine='combine',.packages='randomForest',.multicombine=FALSE) %dopar%
  randomForest(mydata,outcome,ntree=ntree,importance=TRUE,proximity=FALSE,nodesize=10,
               cutoff=cutoff)
# rf
save(rf,file="rf/rf")


t<-table(outcome, rf$predicted)
sum(diag(prop.table(t)))



importanzaVar<-function(rf){
  ord.var<-order(rf$importance[,1], decreasing=TRUE)
  var.rf<-data.frame(rf$importance[ord.var,1])
  print(plot(var.rf[,1], type="b", pch=19, col="blue", ylab="%incMSE"))
  text(var.rf[,1],label=row.names(var.rf),cex=0.7)
  return(var.rf)
}

X.VarExp<-function(y, yhat){
  rss<-sum((y-yhat)^2)
  tss<-sum((y-mean(y))^2)
  perc<-(1-rss/tss)*100
  return(perc)
}
