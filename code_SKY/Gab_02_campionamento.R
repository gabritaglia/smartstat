#CAMPIONAMENTO

load("VisitorID_PurchaseID_VisitN_ReturnFreq_Citta_totale")
visite <- VisitorID_PurchaseID_VisitN_ReturnFreq_Citta_totale
names(visite) <- c("hour","visitorID","purchaseID","visit_number","last_visit","city")


id_cookies<-unique(visite$visitorID)
length(id_cookies)
numerosita_campione<-100000
campione<-sample(1:length(id_cookies), numerosita_campione)

id_cookies_campione<-id_cookies[campione]
length(id_cookies_campione)

rm(visite,VisitorID_PurchaseID_VisitN_ReturnFreq_Citta_totale)
save(file="sample/id_cookies_campione", id_cookies_campione)
#carico tutti le tabelle su cui facciamo il sample
load("id_cookies_campione")

#1_visite
visite<-read.csv(file="visite.csv")
visite_sample<-visite[visite$visitorID%in%id_cookies_campione,]
save(file="sample/visite_sample",visite_sample )

#2_piattaforma
piattaforma<-read.csv(file="piattaforma.csv")
piattaforma_sample<-piattaforma[piattaforma$visitorID%in%id_cookies_campione,]
save(file="sample/piattaforma_sample",piattaforma_sample )

#3_pagine
load("VisitorID_EntryP_Page_PV")
pagine_sample<-VisitorID_EntryP_Page_PV[VisitorID_EntryP_Page_PV$Visitor_ID%in%id_cookies_campione,]
save(file="sample/pagine_sample",pagine_sample )
#4_prodotti
prodotti<-read.csv(file="prodotti.csv")
prodotti_sample<-prodotti[prodotti$visitorID%in%id_cookies_campione,]
save(file="sample/prodotti_sample",prodotti_sample )
#5_log
log<-read.csv(file="log.csv")
log_sample<-log[log$visitorID%in%id_cookies_campione,]
save(file="sample/log_sample",log_sample )
