#aggiustamenti

load("dati/promo")
load("cartaceo")
load("printhome")
dati<-read.csv2("Dati 19 dicembre 2014.csv")

dates<-dati[,c("promo_id","buoni_inizio_validita")]
dates$buoni_inizio_validita<-as.character(dates$buoni_inizio_validita)
a<-1:nrow(dates)
class(a)<-"Date"
a
for(i in 1:length(a)){
a[i]<-as.Date(dates$buoni_inizio_validita[i], "%d/%m/%Y")}
a
dates$buoni_inizio_validita<-a

promo<-promo[,which(names(promo)!="buoni_inizio_validita")]
promo<-merge(promo, dates, by="promo_id")

cartaceo<-cartaceo[,which(names(cartaceo)!="buoni_inizio_validita")]
cartaceo<-merge(cartaceo, dates, by="promo_id")

printhome<-printhome[,which(names(printhome)!="buoni_inizio_validita")]
printhome<-merge(printhome, dates, by="promo_id")


#tengo anche buoni_qta_redenta

qta<-dati[,c("promo_id","buoni_qta_redenta")]
promo<-merge(promo, qta, by="promo_id")
cartaceo<-merge(cartaceo, qta, by="promo_id")
printhome<-merge(printhome, qta, by="promo_id")


save(promo,file="dati/promo")
save(cartaceo,file="cartaceo")
save(printhome, file="printhome")
