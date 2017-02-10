#DETERMINAZIONE DELLE FEATURES PER I COOKIES DI SKY
###############################
list.files("sample")


##------------------------------------------------------------------------------------
##--- VISITE_SAMPLE
load("sample/visite_sample")
v<-visite_sample
v<-v[,-1]
rm(visite_sample)
names(v)
cookies<-data.frame(visitorID=unique(v$visitorID))
cookies$visitorID<-as.character(cookies$visitorID)
#n_visite
tab<-table(as.character(v$visitorID))
tab1<-data.frame(cbind(visitorID=row.names(tab),n_visite=tab))
cookies<-merge(cookies, tab1, by="visitorID", all=T)
cookies$n_visite<-as.numeric(cookies$n_visite)
rm(tab1)
tab2<-table(tab)
t<-cbind(as.numeric(row.names(tab2)),tab2,prop.table(tab2), cumsum(prop.table(tab2)))
t<-as.data.frame(t)
#compra
compratoreID<-as.character((v$visitorID[!is.na(v$purchaseID)]))
cookies[,"compra"]<-0
cookies[cookies$visitorID%in%compratoreID,"compra"]<-1
#p_visite_fascia_we/wd
v[v$we==1 & v$fascia_oraria=="1_morning", "fascia_oraria_we"]<-"1_morning_we"
v[v$we==1 & v$fascia_oraria=="2_lunch", "fascia_oraria_we"]<-"1_lunch_we"
v[v$we==1 & v$fascia_oraria=="3_afternoon", "fascia_oraria_we"]<-"3_afternoon_we"
v[v$we==1 & v$fascia_oraria=="4_evening", "fascia_oraria_we"]<-"4_evening_we"
v[v$we==1 & v$fascia_oraria=="5_night", "fascia_oraria_we"]<-"5_night_we"
v[v$we==0 & v$fascia_oraria=="1_morning", "fascia_oraria_we"]<-"1_morning_wd"
v[v$we==0 & v$fascia_oraria=="2_lunch", "fascia_oraria_we"]<-"1_lunch_wd"
v[v$we==0 & v$fascia_oraria=="3_afternoon", "fascia_oraria_we"]<-"3_afternoon_wd"
v[v$we==0 & v$fascia_oraria=="4_evening", "fascia_oraria_we"]<-"4_evening_wd"
v[v$we==0 & v$fascia_oraria=="5_night", "fascia_oraria_we"]<-"5_night_wd"
#p_visite_we
tab2<-table(as.character(v$visitorID), v$we)
tab3<-prop.table(tab2,1)
tab3<-data.frame(visitorID=cbind(row.names(tab3)),p_visite_we=tab3[,2])
cookies<-merge(cookies, tab3, by="visitorID", all=T)
cookies[,"p_visite_wd"]<-1-cookies[,"p_visite_we"]
rm(tab2, tab3)
#p_visite_matt/pra/pome/sera/notte
tab2<-table(as.character(v$visitorID), v$fascia_oraria)
tab2<-prop.table(tab2,1)
tab2<-data.frame(visitorID=cbind(row.names(tab2)),
                 p_visite_morn=tab2[,1],
                 p_visite_lunc=tab2[,2],
                 p_visite_afte=tab2[,3],
                 p_visite_even=tab2[,4],
                 p_visite_nigh=tab2[,5]
)
cookies<-merge(cookies, tab2, by="visitorID", all=T)
rm(tab2)
#p_visite_matt/pra/pome/sera/notte_we/wd
tab2<-table(as.character(v$visitorID), v$fascia_oraria_we)
tab2<-prop.table(tab2,1)
tab2<-data.frame(visitorID=cbind(row.names(tab2)),
                 p_visite_morn_wd=tab2[,1],
                 p_visite_lunc_wd=tab2[,3],
                 p_visite_afte_wd=tab2[,5],
                 p_visite_even_wd=tab2[,7],
                 p_visite_nigh_wd=tab2[,9],
                 p_visite_morn_we=tab2[,2],
                 p_visite_lunc_we=tab2[,4],
                 p_visite_afte_we=tab2[,6],
                 p_visite_even_we=tab2[,8],
                 p_visite_nigh_we=tab2[,10]
)
cookies<-merge(cookies, tab2, by="visitorID", all=T)
rm(tab2)
#quanti giorni n_days_visite
t<-unique(v[,c("visitorID","giorno")])
t$giorno<-as.Date(t$giorno)
t2<-table(as.character(t$visitorID))
t3<-as.data.frame(t2)
names(t3)<-c("visitorID", "n_days_visite")
cookies<-merge(cookies, t3, by="visitorID", all=T)
rm(t2,t3)
#trovo l'intervallo temporale globale di osservazione in giorni
v$giorno<-as.Date(v$giorno)
intervallo_giorni<-as.numeric(max(v$giorno)-min(v$giorno))
cookies$p_days_visiteSUperiodo<-cookies$n_days_visite/intervallo_giorni
first_day<-aggregate(t$giorno, by=list(t$visitorID), min)
last_day<-aggregate(t$giorno, by=list(t$visitorID), max)
d<-merge(first_day, last_day, by="Group.1")
names(d)<-c("visitorID","first_day","last_day")
d[, "int_days_life"]<-as.numeric(d[,"last_day"]-d[,"first_day"])+1 #la somma di 1 serve per indicare anche la visita singola. Se 1 significa che l'intervallo di visita è un giorno
cookies<-merge(cookies, d, by="visitorID", all=T)
cookies$p_days_visiteSUlife<-cookies$n_days_visite/cookies$int_days_life
rm(t, intervallo_giorni,first_day, last_day, d)
#rimarrebbe da sfruttare la variabile last_visit ma è categoriale e non saprei adesso come fare ad associarla al singolo cookie
rm(v)
save(file="sample/cookies", cookies)


##------------------------------------------------------------------------------------
##--- PIATTAFORMA_SAMPLE
load("sample/piattaforma_sample")
p<-piattaforma_sample
p<-p[,-1]
rm(piattaforma_sample)
os<-read.csv2("os2.csv")
p<- merge(p,os,by="os",all.x=TRUE)
p<-p[,!names(p)%in%c("piattaforma.x")]
names(p)[names(p)=="piattaforma.y"]<-"piattaforma"
#p_visite_mobile  questo è una cagata perché il cookie è un oggetto riferito ad un browser e di conseguenza ad un os!
# tab2<-table(as.character(p$visitorID), p$piattaforma)
# tab3<-prop.table(tab2,1)
# tab3<-data.frame(visitorID=cbind(row.names(tab3)),
#                  p_visite_mob=tab3[,1],
#                  p_visite_pc=tab3[,2],
#                  p_visite_altro=tab3[,3]
# )
table(p$old)
table(p$vendor)
table(p$vendor, p$piattaforma)
length(unique(c(p$visitorID,p$browser)))
length(unique(c(p$visitorID,p$piattaforma)))
levels(p$piattaforma)[1]<-"nonspec" #rinomino ALTRO in non specificato
p$visitorID<-as.character(p$visitorID)
p_merge<-unique(p[,c("visitorID", "piattaforma","old","vendor", "browser")])
# u<-unique(p_merge[,c("visitorID", "piattaforma","old","vendor", "browser")])
#Qui sorge il problema della non unicità del browser e os (e delle variabili conseguenti) al visitorID, sono pochi casi (88 massimo) che potrebbero essere errori. 
#Verificare che siano effettivamente errori
#PROBLEMA IL BROWSER E OS NON E' UNIVOCO PER I COOKIES. COME FARE? CHIEDERE A SILVIO
#Per ovviare questa faccenda ora io elimino d'ufficio i duplicati
dup<-duplicated(p_merge$visitorID)
p_merge<-p_merge[!dup,]
cookies<-merge(cookies, p_merge, by="visitorID", all.x=T, all.y=F)
rm(os,p_merge, dup)
rm(p)
save(file="sample/cookies", cookies)


# load("sample/cookies")
##------------------------------------------------------------------------------------
##--- PAGINE_SAMPLE
load("sample/pagine_sample")
pg<-pagine_sample
rm(pagine_sample)
names(pg)<-c("hour", "visitorID", "entry_pages", "pages", "channels", "page_views")
pg$visitorID<-as.character(pg$visitorID)
#numero di pagine viste dal cookie nel periodo
tab<-table(pg$visitorID)
tab1<-data.frame(cbind(visitorID=row.names(tab),n_pagine_viste=tab))
cookies<-merge(cookies, tab1, by="visitorID", all=T)
#inizio esplorazioni
#tabellla di frequenze  entry page
tab<-table(as.character(pg$entry_page))
tab1<-data.frame(cbind(entry_page=as.character(row.names(tab)),Freq=as.numeric(tab)))
str(tab1)
tab1$entry_page<-as.character(tab1$entry_page)
tab1$Freq<-as.numeric(as.character(tab1$Freq))
tab1<-tab1[order(tab1$Freq, decreasing=T),]
summary(tab1$Freq)
#tabellla di frequenze  channels
tab<-table(as.character(pg$channels))
tab2<-data.frame(cbind(entry_page=as.character(row.names(tab)),Freq=as.numeric(tab)))
str(tab2)
tab2$entry_page<-as.character(tab2$entry_page)
tab2$Freq<-as.numeric(as.character(tab2$Freq))
str(tab2)
tab2<-tab2[order(tab2$Freq, decreasing=T),]
summary(tab2$Freq)
rm(tab,tab1,tab2)
#fine esplorazioni
#numero di entry page per cookies
library(data.table)
pgt <- data.table(pg)
pgt<-pgt[, list(n_entry_pages = length(unique("entry_pages"))), by = c("visitorID", "entry_pages")]
t2<-table(pgt$visitorID)
t2<-as.data.frame(t2)
names(t2)<-c("visitorID","n_entry_pages")
cookies<-merge(cookies,t2,by="visitorID")
rm(pgt,t2)
#lavoro sulle PAGES
pages<-paste(unique(pg$pages))
t<-table(pg$pages)
t<-t[!row.names(t)==""]
t<-sort(t, decreasing=T)
pages_SKY<-data.frame(cbind(pages=row.names(t), Freq=t, Perc=round(prop.table(t)*100,2)))
write.csv2(pages_SKY, file="output/pages_SKY.csv", row.names=F)
rm(t)
#######################################################
#PAGES SECONDO BARBERIS_TAGLIABUE
url <- strsplit(pages,"http%3A//|http://")
url[[1]]
url <- sapply(url,function(x)x[2])
url[[1]]
url <- strsplit(url,"/")
url[[1]]
# url <- sapply(url,function(x) c(x[1],x[2],x[3],x[4],simplify=FALSE))
URL<-as.data.frame(pages)
URL[,"l1"]<- sapply(url,function(x)x[1])
URL[,"l2"]<- sapply(url,function(x)x[2])
URL[,"l3"]<- sapply(url,function(x)x[3])
URL[,"l4"]<- sapply(url,function(x)x[4])

t<-table(URL$l1)
t<-sort(t, decreasing=T)
livello1_pages<-data.frame(cbind(pages=row.names(t), Freq=t, Perc=round(prop.table(t)*100,2), CumPerc=round(cumsum(t)*100/sum(t),2)))
write.csv2(livello1_pages, file="output/livello1_pages.csv", row.names=F)
rm(t)

URL[,"info"]<-""
URL[which(URL$l1=="tg24.sky.it"),"info"]<-URL$l3[which(URL$l1=="tg24.sky.it")]
URL[which(URL$l1=="video.sky.it"),"info"]<-URL$l2[which(URL$l1=="video.sky.it")]
URL[which(URL$l1=="sport.sky.it"),"info"]<-URL$l3[which(URL$l1=="sport.sky.it")]
URL[which(URL$l1=="charitystars.sport.sky.it"),"info"]<-URL$l1[which(URL$l1=="charitystars.sport.sky.it")]
URL[which(URL$l3=="ledirettediskysport.html"),"info"]<-URL$l3[which(URL$l3=="ledirettediskysport.html")]
URL[which(URL$l1=="mag.sky.it"),"info"]<-URL$l3[which(URL$l1=="mag.sky.it")]
URL[which(URL$l1=="guidatv.sky.it"),"info"]<-URL$l3[which(URL$l1=="guidatv.sky.it")]
URL[which(URL$l1=="cinema.sky.it"),"info"]<-URL$l1[which(URL$l1=="cinema.sky.it")]
URL[which(URL$l1=="oroscopo.sky.it"),"info"]<-URL$l1[which(URL$l1=="oroscopo.sky.it")]
URL[which(URL$l1=="meteo.sky.it"),"info"]<-URL$l1[which(URL$l1=="meteo.sky.it")]
URL[which(URL$l1=="arte.sky.it"),"info"]<-URL$l2[which(URL$l1=="arte.sky.it")]
URL[which(URL$l1=="fantascudetto.sky.it"),"info"]<-URL$l1[which(URL$l1=="fantascudetto.sky.it")]
URL[which(URL$l1=="fantamondiale.sky.it"),"info"]<-URL$l1[which(URL$l1=="fantamondiale.sky.it")]
URL[which(URL$l1=="fantacampioni.sky.it"),"info"]<-URL$l1[which(URL$l1=="fantacampioni.sky.it")]
URL[which(URL$l1=="fantagp.sky.it"),"info"]<-URL$l1[which(URL$l1=="fantagp.sky.it")]
URL[which(URL$l1=="business.sky.it"),"info"]<-URL$l1[which(URL$l1=="business.sky.it")]
URL[which(URL$l1=="forum.sky.it"),"info"]<-URL$l1[which(URL$l1=="forum.sky.it")]
URL[which(URL$l1=="abbonamento.sky.it"),"info"]<-URL$l1[which(URL$l1=="abbonamento.sky.it")]
URL[which(URL$l1=="collam.abbonamento.sky.it"),"info"]<-URL$l1[which(URL$l1=="collam.abbonamento.sky.it")]
URL[which(URL$l1=="dev.geocms.it"),"info"]<-URL$l1[which(URL$l1=="dev.geocms.it")]
URL_merge<-URL[,c("pages", "l1","info")]
pg<-merge(pg, URL_merge, by="pages")
#associo ai cookies n variabili derivate dal primo livello dell'url che indicano il numero di volte in cui il cookie ha visitato una pagine con quel livello
t<-table(URL$l1)
t<-sort(t, decreasing=T)
t<-cbind(row.names(t),t, round(prop.table(t),3),cumsum(round(prop.table(t),3)))
t<-as.data.frame(t)
names(t)<-c("l1","Freq","FreqRel","FreqRelCum")
t$FreqRelCum<-as.numeric(as.character(t$FreqRelCum))
write.csv(file="output/Freq_pages_l1.csv",t)
l1_scelta<-as.character(t[which(t$FreqRelCum<0.9),"l1"])
pg[which(pg$l1%in%l1_scelta), "l1_scelta"]<-pg$l1[which(pg$l1%in%l1_scelta)]
t<-table(pg$visitorID, pg$l1_scelta)
t<-as.data.frame(cbind(visitorID=row.names(t),t))
names(t)[2:length(names(t))]<-paste0("pages_l1_",names(t)[2:length(names(t))])
cookies<-merge(cookies, t, by="visitorID")
rm(t, l1_scelta)
#associo ai cookies n variabili derivate dall'informazione semantica dell'url (ottenuta dalle regole) che indicano il numero di volte in cui il cookie ha visitato una pagine con quell'informazione semantica
t2<-table(URL$info)
t2<-sort(t2, decreasing=T)
t2<-cbind(row.names(t2),t2, round(prop.table(t2),3),cumsum(round(prop.table(t2),3)))
t2<-as.data.frame(t2)
names(t2)<-c("info","Freq","FreqRel","FreqRelCum")
t2$FreqRelCum<-as.numeric(as.character(t2$FreqRelCum))
write.csv(file="output/Freq_pages_info.csv",t2)
info_scelta<-as.character(t2[which(t2$FreqRelCum<0.9),"info"])
pg[which(pg$info%in%info_scelta), "info_scelta"]<-pg$info[which(pg$info%in%info_scelta)]
t2<-table(pg$visitorID, pg$info_scelta)
t2<-as.data.frame(cbind(visitorID=row.names(t2),t2))
names(t2)[2:length(names(t2))]<-paste0("pages_info_",names(t2)[2:length(names(t2))])
cookies<-merge(cookies, t2, by="visitorID")
rm(t2, info_scelta)
rm(URL, URL_merge,url, pages)

#lavoro sulle ENTRY_PAGES
entry_pages<-paste(unique(pg$entry_pages))
t<-table(pg$entry_pages)
t<-sort(t, decreasing=T)
entry_pages_SKY<-data.frame(cbind(pages=row.names(t), Freq=t, Perc=round(prop.table(t)*100,2)))
write.csv2(entry_pages_SKY, file="output/entry_pages_SKY.csv", row.names=F)
rm(t)
#

# entry_pages<-entry_pages[entry_pages!=""]
url <- strsplit(entry_pages,"http%3A//|http://")
url[[100]]
url <- sapply(url,function(x)x[2])
url[[100]]
url <- strsplit(url,"/")
url[[100]]
# url <- sapply(url,function(x) c(x[1],x[2],x[3],x[4],simplify=FALSE))
URL<-as.data.frame(entry_pages)
URL[,"entry_l1"]<- sapply(url,function(x)x[1])
URL[,"entry_l2"]<- sapply(url,function(x)x[2])
URL[,"entry_l3"]<- sapply(url,function(x)x[3])
URL[,"entry_l4"]<- sapply(url,function(x)x[4])
URL[,"entry_info"]<-""
URL[which(URL$entry_l1=="tg24.sky.it"),"entry_info"]<-URL$entry_l3[which(URL$entry_l1=="tg24.sky.it")]
URL[which(URL$entry_l1=="video.sky.it"),"entry_info"]<-URL$entry_l2[which(URL$entry_l1=="video.sky.it")]
URL[which(URL$entry_l1=="sport.sky.it"),"entry_info"]<-URL$entry_l3[which(URL$entry_l1=="sport.sky.it")]
URL[which(URL$entry_l1=="charitystars.sport.sky.it"),"entry_info"]<-URL$entry_l1[which(URL$entry_l1=="charitystars.sport.sky.it")]
URL[which(URL$entry_l3=="ledirettediskysport.html"),"entry_info"]<-URL$entry_l3[which(URL$entry_l3=="ledirettediskysport.html")]
URL[which(URL$entry_l1=="mag.sky.it"),"entry_info"]<-URL$entry_l3[which(URL$entry_l1=="mag.sky.it")]
URL[which(URL$entry_l1=="guidatv.sky.it"),"entry_info"]<-URL$entry_l3[which(URL$entry_l1=="guidatv.sky.it")]
URL[which(URL$entry_l1=="cinema.sky.it"),"entry_info"]<-URL$entry_l1[which(URL$entry_l1=="cinema.sky.it")]
URL[which(URL$entry_l1=="oroscopo.sky.it"),"entry_info"]<-URL$entry_l1[which(URL$entry_l1=="oroscopo.sky.it")]
URL[which(URL$entry_l1=="meteo.sky.it"),"entry_info"]<-URL$entry_l1[which(URL$entry_l1=="meteo.sky.it")]
URL[which(URL$entry_l1=="arte.sky.it"),"entry_info"]<-URL$entry_l2[which(URL$entry_l1=="arte.sky.it")]
URL[which(URL$entry_l1=="fantascudetto.sky.it"),"entry_info"]<-URL$entry_l1[which(URL$entry_l1=="fantascudetto.sky.it")]
URL[which(URL$entry_l1=="fantamondiale.sky.it"),"entry_info"]<-URL$entry_l1[which(URL$entry_l1=="fantamondiale.sky.it")]
URL[which(URL$entry_l1=="fantacampioni.sky.it"),"entry_info"]<-URL$entry_l1[which(URL$entry_l1=="fantacampioni.sky.it")]
URL[which(URL$entry_l1=="fantagp.sky.it"),"entry_info"]<-URL$entry_l1[which(URL$entry_l1=="fantagp.sky.it")]
URL[which(URL$entry_l1=="business.sky.it"),"entry_info"]<-URL$entry_l1[which(URL$entry_l1=="business.sky.it")]
URL[which(URL$entry_l1=="forum.sky.it"),"entry_info"]<-URL$entry_l1[which(URL$entry_l1=="forum.sky.it")]
URL[which(URL$entry_l1=="abbonamento.sky.it"),"entry_info"]<-URL$entry_l1[which(URL$entry_l1=="abbonamento.sky.it")]
URL[which(URL$entry_l1=="collam.abbonamento.sky.it"),"entry_info"]<-URL$entry_l1[which(URL$entry_l1=="collam.abbonamento.sky.it")]
URL[which(URL$entry_l1=="dev.geocms.it"),"entry_info"]<-URL$entry_l1[which(URL$entry_l1=="dev.geocms.it")]
URL_merge<-URL[,c("entry_pages", "entry_l1","entry_info")]
pg<-merge(pg, URL_merge, by="entry_pages")
#associo ai cookies n variabili derivate dal primo livello dell'url della entry_pages che indicano il numero di volte in cui il cookie ha visitato una pagine con quel livello
t<-table(URL$entry_l1)
t<-sort(t, decreasing=T)
t<-cbind(row.names(t),t, round(prop.table(t),3),cumsum(round(prop.table(t),3)))
t<-as.data.frame(t)
names(t)<-c("entry_l1","Freq","FreqRel","FreqRelCum")
t$FreqRelCum<-as.numeric(as.character(t$FreqRelCum))
write.csv(file="output/Freq_entry_l1.csv",t)
entry_l1_scelta<-as.character(t[which(t$FreqRelCum<0.9),"entry_l1"])
pg[which(pg$entry_l1%in%entry_l1_scelta), "entry_l1_scelta"]<-pg$entry_l1[which(pg$entry_l1%in%entry_l1_scelta)]
t<-table(pg$visitorID, pg$entry_l1_scelta)
t<-as.data.frame(cbind(visitorID=row.names(t),t))
names(t)[2:length(names(t))]<-paste0("entry_l1_",names(t)[2:length(names(t))])
cookies<-merge(cookies, t, by="visitorID")
rm(t, entry_l1_scelta)
#associo ai cookies n variabili derivate ddall'informazione semantica dell'url della entry_pages (ottenuta dalle regole) che indicano il numero di volte in cui il cookie ha visitato una pagine con quell'informazione semantica
t2<-table(URL$entry_info)
t2<-sort(t2, decreasing=T)
t2<-cbind(row.names(t2),t2, round(prop.table(t2),3),cumsum(round(prop.table(t2),3)))
t2<-as.data.frame(t2)
names(t2)<-c("entry_info","Freq","FreqRel","FreqRelCum")
t2$FreqRelCum<-as.numeric(as.character(t2$FreqRelCum))
write.csv(file="output/Freq_entry_info.csv",t2)
entry_info_scelta<-as.character(t2[which(t2$FreqRelCum<0.9),"entry_info"])
pg[which(pg$entry_info%in%entry_info_scelta), "entry_info_scelta"]<-pg$entry_info[which(pg$entry_info%in%entry_info_scelta)]
t2<-table(pg$visitorID, pg$entry_info_scelta)
t2<-as.data.frame(cbind(visitorID=row.names(t2),t2))
names(t2)[2:length(names(t2))]<-paste0("entry_info_",names(t2)[2:length(names(t2))])
cookies<-merge(cookies, t2, by="visitorID")
rm(t2,entry_info_scelta)
rm(URL, URL_merge,url, entry_pages)
# #durata della visita in ore (Me e range interquartile) #non è possibile ricavare l'ora di uscita 
# pg$ts<- strptime(pg$hour,"%b %d, %Y, Hour %H")
# pg$giorno <- format(pg$ts,"%Y-%m-%d")
# pg$ora <- format(pg$ts,"%H")
# 
# pg1<-pg[order(pg$visitorID, pg$hour,pg$entry_pages),]

save(file="sample/cookies", cookies)
rm(pg)

# load("sample/cookies")
##------------------------------------------------------------------------------------
##--- PRODOTTI_SAMPLE
load("sample/prodotti_sample")
pd<-prodotti_sample
rm(prodotti_sample)
pd<-pd[,-1]

#carrello
summary(pd$products)
length(which(pd$products!=""))
carrelloID<-as.character((pd$visitorID[which(pd$products!="")]))
cookies[,"carrello"]<-0
cookies[cookies$visitorID%in%carrelloID,"carrello"]<-1

#vede_promo
summary(pd$promo)
length(which(pd$promo!=""))
vede_promoID<-as.character((pd$visitorID[pd$promo!=""]))
cookies[,"vede_promo"]<-0
cookies[cookies$visitorID%in%vede_promoID,"vede_promo"]<-1

#n_pacchetti_carrello
products<-as.character(unique(pd$products))
pacchetti <- strsplit(products, split="\\+")
n_pacchetti<-as.numeric(as.character(sapply(pacchetti, length)))
products<-cbind(products, n_pacchetti)
pd<-merge(pd,products, by="products")
pd_subset<-pd[which(pd$visitorID%in%carrelloID),]
t<-aggregate(as.numeric(as.character(pd_subset$n_pacchetti)), by=list(pd_subset$visitorID), max)
names(t)<-c("visitorID", "n_pacchetti")
cookies<-merge(cookies, t, by="visitorID", all.x=T)
cookies[is.na(cookies$n_pacchetti),"n_pacchetti"]<-0

rm(pd, pd_subset,t, carrelloID, vede_promoID, n_pacchetti, pacchetti, products)
save(file="sample/cookies", cookies)

# load("sample/cookies")
##------------------------------------------------------------------------------------
##--- LOG_SAMPLE
load("sample/log_sample")
log<-log_sample
rm(log_sample)
log<-log[,-1]

length(which(log$log!=""))
logID<-as.character((log$visitorID[which(log$log!="")]))
cookies[,"log"]<-0
cookies[cookies$visitorID%in%logID,"log"]<-1

save(file="sample/cookies", cookies)
rm(log, logID)

# load("sample/cookies")
##------------------------------------------------------------------------------------


######################################################
#perché le tabelle visite e piattaforma hanno numerosità differenti??
table(table(unique(c(v$ts,v$visitorID))))
table(table(unique(c(p$ts,p$visitorID))))

###-- SUBSET COMPRATORI --###
com<-cookies[cookies$compra==1,]
