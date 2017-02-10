###################################################
#TABELLA 1 -- VISITE
load("VisitorID_PurchaseID_VisitN_ReturnFreq_Citta_totale")
visite <- VisitorID_PurchaseID_VisitN_ReturnFreq_Citta_totale
names(visite) <- c("hour","visitorID","purchaseID","visit_number","last_visit","city")
visite1<-visite #copia di sicurezza
rm(VisitorID_PurchaseID_VisitN_ReturnFreq_Citta_totale)

#distinguo il timestamp in giorno/ora/weekend
visite$ts<- strptime(visite$hour,"%b %d, %Y, Hour %H")
visite$giorno <- format(visite$ts,"%Y-%m-%d")
visite$ora <- format(visite$ts,"%H")
visite$day <- weekdays(visite$ts)
visite$we <- 0
visite$we[which(visite$day%in%c("Saturday","Sunday"))] <- 1

start_date<-as.Date(min(visite$giorno))
end_date<-as.Date(max(visite$giorno))
end_date-start_date

ore_prelavoro<-c("06","07","08")
ore_mattina<-c("09","10","11")
ore_pranzo<-c("12","13","14")
ore_pome<-c("15","16","17","18")
ore_sera<-c("19","20","21","22","23")
ore_notte<-c("00","01","02","03","04","05")
#fascia_oraria
visite$fascia_oraria<-""
visite$fascia_oraria[which(visite$ora %in% ore_prelavoro)]<-"0_prelavoro"
visite$fascia_oraria[which(visite$ora %in% ore_mattina)]<-"1_morning"
visite$fascia_oraria[which(visite$ora %in% ore_pranzo)]<-"2_lunch"
visite$fascia_oraria[which(visite$ora %in% ore_pome)]<-"3_afternoon"
visite$fascia_oraria[which(visite$ora %in% ore_sera)]<-"4_evening"
visite$fascia_oraria[which(visite$ora %in% ore_notte)]<-"5_night"

#fine aggiunte al file visite
#salvo visite.csv
write.csv(visite, file="visite.csv")

tab<-table(visite$fascia_oraria)
tab
tab1<-table(visite$ora)
png("/home/gabriele/SKY/images/Barplot_Visite_ora.png", width=1024, height=800)
barplot(tab1,ylab="Frequenza",xlab="giorni", ylim=c(0,max(tab1)*1.2))
box()
dev.off()

tab2<-table(visite$fascia_oraria, visite$we)
prop.table(tab2,margin=2)
tab2a<-matrix(tab2, ncol=2)

tab3<-table(visite$fascia_oraria, visite$day)
tab32<-prop.table(tab3,margin=2)
tab3a<-matrix(tab3, ncol=7)

#variabile visit_number
#Numero di visite al sito fino ad un certo istante temporale
tab4<-table(table(visite$visit_number))

#numero di visite al sito da parte del singolo cookie
tab5<-aggregate(visite$visitorID,by=list(visite$visitorID),length)    
names(tab5)<-c("visitorID", "NumeroVisite")
tab5<-table(visite$visitorID)
tab6<-table(tab5)

length(which(tab5>=2 &tab5<10))


hist(tab6, breaks=10)
summary(as.numeric(tab6))


###################################################
#TABELLA 2 -- PIATTAFORMA
load("VisitorID_OpSist_BT")
piattaforma <- VisitorID_OpSist_BT
names(piattaforma) <- c("hour","visitorID","os","browser")

piattaforma$ts <- strptime(piattaforma$hour,"%b %d, %Y, Hour %H")
piattaforma$giorno <- format(piattaforma$ts,"%d/%m/%y")
piattaforma$ora <- format(piattaforma$ts,"%H")
piattaforma$day <- weekdays(piattaforma$ts)
piattaforma$we <- 0
piattaforma$we[which(piattaforma$day%in%c("Saturday","Sunday"))] <- 1

aggr <- read.csv("os.csv",sep=";")
piattaforma <- merge(piattaforma,aggr,by="os",all=TRUE)
head(piattaforma)

# table(piattaforma$os,piattaforma$piattaforma)
# write.csv(piattaforma,"piattaforma.csv")

h<-head(piattaforma)

tab.day<-table(piattaforma$day)
tab.we<-table(piattaforma$we)


tab.piat<-table(piattaforma$piattaforma)
a<-data.frame(tab.piat)


piattaforma$os<-factor(piattaforma$os)
tab.os<-table(piattaforma$os)
tab.os

piattaforma$browser<-factor(piattaforma$browser)
tab.browser<-table(piattaforma$browser)
a<-data.frame(tab.browser)

###################################################
#TABELLA 3 -- PAGINE
load("pagine_comp")
pagine_comp<-pagine_comp[order(pagine_comp$freq),]


###################################################
#TABELLA 4 -- PRODOTTI
prodotti<-read.csv("prodotti.csv")

length(which(prodotti$promo==""))
class(prodotti$purchaseID)
sum(!is.na(prodotti$purchaseID))
length(which(prodotti$purchaseID)
       length(which(prodotti$venduto==1))
prodotti_sel<-prodotti[-which(prodotti$promo==""),]
pluto<-aggregate(prodotti_sel$visitorID,by=list(prodotti_sel$visitorID,prodotti_sel$promo),length)
names(pluto) <- c("visitorID","promoID","freq")
tab <- table(table(paste(pluto$visitorID)))
tab<-data.frame(tab)
prodotti_sel2<-prodotti_sel[!is.na(prodotti$purchaseID),]
pluto$freq
pippo<-aggregate(prodotti$visitorID,by=list(prodotti$visitorID,prodotti$purchaseID),length)
names(pluto) <- c("visitorID","promoID","freq")
Head(pluto)
       
###################################################
#TABELLA 5 -- LOGGATI
# load("VisitorID_ExternalID")
# log <- VisitorID_ExternalID
# names(log) <- c("hour","visitorID","log")
# 
log$dummy <- ""
log$dummy[which(log$log!="")] <- "loggati"
log$dummy[which(log$log=="")] <- "non loggati"
# 
# data.frame(table(log$dummy))
# length(unique(log$visitorID[log$dummy=="loggati"]))
# length(unique(log$visitorID[log$dummy=="non loggati"]))
# 
# length(unique(log$visitorID))
# length(unique(log$log))
# 
# t <- table(log$visitorID)
# t <- data.frame(t)
# t <- t[order(t[,2],decreasing=TRUE),]
# 
# table(t)
# 
# sel <- subset(log,log!="")
# sel <- sel[order(sel$visitorID),]
# length(unique(sel$visitorID))
# 
# t <- table(sel$visitorID)
# t <- data.frame(t)
# t <- t[order(t[,2],decreasing=TRUE),]
# 
# 
# subset(log,log=="20392d305ad79f31dc4e7c013e7a2dbe77e1fef9e682a34c")
# subset(log,visitorID=="3010558992664307895_4611687343424889763")
# 
# 
# log$ts <- strptime(log$hour,"%b %d, %Y, Hour %H")
# log$giorno <- format(log$ts,"%d/%m/%y")
# log$ora <- format(log$ts,"%H")
# log$day <- weekdays(log$ts)
# log$we <- 0
# log$we[which(log$day%in%c("Saturday","Sunday"))] <- 1
# 
# head(log)
# write.csv(log,"log.csv")


log<-read.csv("log.csv")
names(log)

log_sel <- subset(log,log!="")
pluto<-aggregate(log_sel$visitorID, by=list(log_sel$visitorID,log_sel$log),length)
names(pluto) <- c("visitorID","logID","freq")
head(pluto)

aggr_visitorID <- aggregate(pluto$freq,list(pluto$visito))

tab <- table(table(paste(pluto$visitorID)))
tab<-data.frame(tab)

tab2 <- table(table(paste(pluto$logID)))
tab2<-data.frame(tab2)

