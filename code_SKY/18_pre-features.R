#DA FAR GIRARE PRIMA DI INIZIARE LO SCRIPT DELLE FEATURES 
#TIRA SU I DATI E METTE I NOMI DELLE VARIABILI GIUSTI

dir_input="Sample_2013_1sem"
dir_output="Sample_2013_1sem/dati_ordinati"
dir_output_pages="Sample_stagioni/pages"

file_visite<-"sample2013.1_visite"
file_piattaforma<-"sample2013.1_piattaforma"
file_pagine<-"sample2013.1_pagine"
file_prodotti<-"sample2013.1_prodotti"
file_log<-"sample2013.1_log"
nome_file_pages<-"pages_2013.1"
nome_file_entrypages<-"entrypages_2013.1"

#---------------------------------------------------------------------
# visite <- read.table("Sample_2013_1sem/sample2013.1_visite.csv",header=FALSE,sep=";")
#---------------------------------------------------------------------
#VISITE
visite <- read.table(paste0(dir_input,"/",file_visite,".csv"),header=FALSE,sep=";",quote="")
names(visite)<-c("X","Y","hour","visitorID","purchaseID","visit_number","last_visit","city")
visite<-visite[,-c(1,2)]
#distinguo il timestamp in giorno/ora/weekend
visite$ts<- strptime(visite$hour,"%b %d, %Y, Hour %H")
visite$giorno <- format(visite$ts,"%Y-%m-%d")
visite$ora <- format(visite$ts,"%H")
visite$day <- weekdays(visite$ts)
visite$we <- 0
visite$we[which(visite$day%in%c("Saturday","Sunday"))] <- 1
head(visite)
save(visite, file=paste0(dir_output,"/",file_visite,"_ordinato"))

#---------------------------------------------------------------------
# piattaforma <- read.table("Sample_2013_1sem/sample2013.1_piattaforma.csv",header=FALSE,sep=";")
#---------------------------------------------------------------------
#PIATTAFORMA
piattaforma<-read.table(paste0(dir_input,"/",file_piattaforma,".csv"),header=FALSE,sep=";")
names(piattaforma)<-c("X","Y","hour","visitorID","os","browser")
piattaforma<-piattaforma[,-c(1,2)]
#distinguo il timestamp in giorno/ora/weekend
piattaforma$ts<- strptime(piattaforma$hour,"%b %d, %Y, Hour %H")
piattaforma$giorno <- format(piattaforma$ts,"%Y-%m-%d")
piattaforma$ora <- format(piattaforma$ts,"%H")
piattaforma$day <- weekdays(piattaforma$ts)
piattaforma$we <- 0
piattaforma$we[which(piattaforma$day%in%c("Saturday","Sunday"))] <- 1

head(piattaforma)
save(piattaforma, file=paste0(dir_output,"/",file_piattaforma,"_ordinato"))

#---------------------------------------------------------------------
# pagine <- read.table("Sample_2013_1sem/sample2013.1_pagine.csv",header=FALSE,sep=";")
#---------------------------------------------------------------------
#PAGINE
pagine<-read.table(paste0(dir_input,"/",file_pagine,".csv"),header=FALSE,sep=";")
names(pagine)<-c("X","hour", "visitorID", "entry_pages", "pages", "channels", "page_views")
pagine<-pagine[,-c(1)]
#distinguo il timestamp in giorno/ora/weekend
pagine$ts<- strptime(pagine$hour,"%b %d, %Y, Hour %H")
pagine$giorno <- format(pagine$ts,"%Y-%m-%d")
pagine$ora <- format(pagine$ts,"%H")
pagine$day <- weekdays(pagine$ts)
pagine$we <- 0
pagine$we[which(pagine$day%in%c("Saturday","Sunday"))] <- 1
head(pagine)
save(pagine, file=paste0(dir_output,"/",file_pagine,"_ordinato"))


#salvo la tabella con gli URL delle pagine per l'analisi semantica
pages<-unique(pagine$pages) 
pages<-pages[pages!=""]
pages<-as.character(pages)
write.csv(pagine,file=paste0(dir_output_pages,"/",nome_file_pages,".csv"))
#salvo la tabella con gli URL delle entry pages per l'analisi semantica
entry_pages<-unique(pagine$entry_pages) 
entry_pages<-entry_pages[entry_pages!=""]
entry_pages<-as.character(entry_pages)
write.csv(pagine,file=paste0(dir_output_pages,"/",nome_file_entrypages,".csv"))





#---------------------------------------------------------------------
# prodotti <- read.table("Sample_2013_1sem/sample2013.1_prodotti.csv",header=FALSE,sep=";")
load("sample2/prodotti_sample")
#---------------------------------------------------------------------
#PRODOTTI
prodotti <- read.table(paste0(dir_input,"/",file_prodotti,".csv"),header=FALSE,sep=";")
names(prodotti)<-c("X","Y","hour", "visitorID", "purchaseID", "promo", "products")
prodotti<-prodotti[,-c(1,2)]
#distinguo il timestamp in giorno/ora/weekend
prodotti$ts<- strptime(prodotti$hour,"%b %d, %Y, Hour %H")
prodotti$giorno <- format(prodotti$ts,"%Y-%m-%d")
prodotti$ora <- format(prodotti$ts,"%H")
prodotti$day <- weekdays(prodotti$ts)
prodotti$we <- 0
prodotti$we[which(prodotti$day%in%c("Saturday","Sunday"))] <- 1
head(prodotti)
save(prodotti, file=paste0(dir_output,"/",file_prodotti,"_ordinato"))

#---------------------------------------------------------------------
# log <- read.table("Sample_2013_1sem/sample2013.1_log.csv",header=FALSE,sep=";")
#---------------------------------------------------------------------
#LOG
log <- read.table(paste0(dir_input,"/",file_log,".csv"),header=FALSE,sep=";")
names(log)<-c("X","Y","hour", "visitorID", "log")
log<-log[,-c(1,2)]
#distinguo il timestamp in giorno/ora/weekend
log$ts<- strptime(log$hour,"%b %d, %Y, Hour %H")
log$giorno <- format(log$ts,"%Y-%m-%d")
log$ora <- format(log$ts,"%H")
log$day <- weekdays(log$ts)
log$we <- 0
log$we[which(log$day%in%c("Saturday","Sunday"))] <- 1
head(log)
save(log, file=paste0(dir_output,"/",file_log,"_ordinato"))

#---------------------------------------------------------------------




# #salvo la tabella di frequenze del sistema operativo
# #PIATTAFORMA
# piattaforma<-read.table(paste0(dir_input,"/",file_piattaforma,".csv"),header=FALSE,sep=";")
# names(piattaforma)<-c("X","Y","hour","visitorID","os","browser")
# piattaforma<-piattaforma[,-c(1,2)]
# table(piattaforma$os)
# a<-data.frame(table(p$os))
