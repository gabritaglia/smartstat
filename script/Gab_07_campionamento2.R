#CAMPIONAMENTO

# load("VisitorID_PurchaseID_VisitN_ReturnFreq_Citta_totale")
# visite <- VisitorID_PurchaseID_VisitN_ReturnFreq_Citta_totale
# names(visite) <- c("hour","visitorID","purchaseID","visit_number","last_visit","city")

#carico il file visite
prodotti<-read.csv(file="prodotti.csv")

cookiesID<-as.character(unique(prodotti$visitorID))
length(cookiesID)
#compra
compratoreID<-as.character((prodotti$visitorID[!is.na(prodotti$purchaseID)]))
compratoreID<-unique(compratoreID)
#carrello
carrelloID<-as.character((prodotti$visitorID[which(prodotti$products!="")]))
carrelloID<-unique(carrelloID)
CarrelloCompratoreID<-unique(c(compratoreID, carrelloID))
length(CarrelloCompratoreID)
rm(carrelloID, compratoreID)
navigatoreID<-cookiesID[!cookiesID%in%CarrelloCompratoreID]
length(navigatoreID)
#tengo tutti i compratoreID e i carrelloID
#poi inizio a prendere un campione di navigatoreID pari a 0 volte la somma di compratoreID e carrelloID (poi quando andrò a fare la foresta riduco le numerosità)
numerosita_campione<-length(CarrelloCompratoreID)*0
navigatoreID_campione<-sample(navigatoreID,numerosita_campione)

cookiesID_campione<-c(CarrelloCompratoreID, navigatoreID_campione)

#creo la nuova cartella dove contenere il campione da 1.000.000 di cookies
# dir.create("sample")
save(file="sample2/cookiesID_campione", cookiesID_campione)
#carico tutti le tabelle su cui facciamo il sample
load("sample2/cookiesID_campione")


#4_prodotti
# prodotti<-read.csv(file="prodotti.csv")
prodotti_sample<-prodotti[prodotti$visitorID%in%cookiesID_campione,]
save(file="sample2/prodotti_sample",prodotti_sample )
rm(prodotti, prodotti_sample)
#1_visite
visite<-read.csv(file="visite.csv")
visite_sample<-visite[visite$visitorID%in%cookiesID_campione,]
save(file="sample2/visite_sample",visite_sample )
rm(visite, visite_sample)
#_piattaforma
piattaforma<-read.csv(file="piattaforma.csv")
piattaforma_sample<-piattaforma[piattaforma$visitorID%in%cookiesID_campione,]
save(file="sample2/piattaforma_sample",piattaforma_sample )
rm(piattaforma, piattaforma_sample)
#3_pagine
load("VisitorID_EntryP_Page_PV")
pagine_sample<-VisitorID_EntryP_Page_PV[VisitorID_EntryP_Page_PV$Visitor_ID%in%cookiesID_campione,]
save(file="sample2/pagine_sample",pagine_sample )
rm(VisitorID_EntryP_Page_PV, pagine_sample)
#5_log
log<-read.csv(file="log.csv")
log_sample<-log[log$visitorID%in%cookiesID_campione,]
save(file="sample2/log_sample",log_sample )
rm(log, log_sample)