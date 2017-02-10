#DETERMINAZIONE DELLE FEATURES PER I COOKIES DI SKY
###############################
list.files("sample")

tableFattaBene<-function(x){
  t<-sort(table(x), decreasing=T)
  t<-data.frame(cbind(x=row.names(t), Freq=t, Perc=round(prop.table(t)*100,2),
                      CumPerc=round(cumsum(t)*100/sum(t),2)),row.names=NULL)
  return(t)
}
##------------------------------------------------------------------------------------
##--- VISITE_SAMPLE
load("sample/visite_sample")
v<-visite_sample
v<-v[,-1]
rm(visite_sample)
names(v)
cookies<-data.frame(visitorID=unique(v$visitorID))
cookies$visitorID<-as.character(cookies$visitorID)


ore_prelavoro<-c(6,7,8)
ore_mattina<-c(9,10,11)
ore_pranzo<-c(12,13,14)
ore_pome<-c(15,16,17,18)
ore_sera<-c(19,20,21,22,23)
ore_notte<-c(0,1,2,3,4,5)
#fascia_oraria
v$fascia_oraria<-""
v$fascia_oraria[which(v$ora %in% ore_prelavoro)]<-"0_prelavoro"
v$fascia_oraria[which(v$ora %in% ore_mattina)]<-"1_morning"
v$fascia_oraria[which(v$ora %in% ore_pranzo)]<-"2_lunch"
v$fascia_oraria[which(v$ora %in% ore_pome)]<-"3_afternoon"
v$fascia_oraria[which(v$ora %in% ore_sera)]<-"4_evening"
v$fascia_oraria[which(v$ora %in% ore_notte)]<-"5_night"

rm(ore_prelavoro,ore_mattina, ore_pranzo, ore_pome, ore_sera, ore_notte)
#n_visite per giorno-ora
giorno<-v$day
levels(giorno)<-c("5Friday","1Monday","6Saturday","7Sunday","4Thursday","2Tuesday","3Wednesday")
ora<-factor(v$ora)
levels(ora)<-c("00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23")
giorno.ora<-paste(giorno, ora)
t<-table(as.character(v$visitorID),giorno.ora)
t<-data.frame(cbind(visitorID=row.names(t),n_visite=t))
cookies<-merge(cookies, t, by="visitorID", all=T)
rm(giorno, ora, giorno.ora)
#n_visite
tab<-table(as.character(v$visitorID))
tab1<-data.frame(cbind(visitorID=row.names(tab),n_visite=tab))
cookies<-merge(cookies, tab1, by="visitorID", all=T)
cookies$n_visite<-as.numeric(cookies$n_visite)
rm(tab1)
# tab2<-table(tab)
# t<-cbind(as.numeric(row.names(tab2)),tab2,prop.table(tab2), cumsum(prop.table(tab2)))
# t<-as.data.frame(t)
# rm(tab2)
rm(tab)
#p_visite_day
t<-table(as.character(v$visitorID),v$day)
t<-prop.table(t,1)
t<-as.data.frame.matrix(t)
t<-data.frame(visitorID=cbind(row.names(t)),
                 p_visite_Lun=t[,1],
                 p_visite_Mar=t[,2],
                 p_visite_Mer=t[,3],
                 p_visite_Gio=t[,4],
                 p_visite_Ven=t[,5],
                 p_visite_Sab=t[,6],
                 p_visite_Dom=t[,7]
)
cookies<-merge(cookies, t, by="visitorID", all=T)
rm(t)
#p_visite_fascia_we/wd
v[v$we==1 & v$fascia_oraria=="0_prelavoro", "fascia_oraria_we"]<-"0_prelavoro_we"
v[v$we==1 & v$fascia_oraria=="1_morning", "fascia_oraria_we"]<-"1_morning_we"
v[v$we==1 & v$fascia_oraria=="1_morning", "fascia_oraria_we"]<-"1_morning_we"
v[v$we==1 & v$fascia_oraria=="2_lunch", "fascia_oraria_we"]<-"1_lunch_we"
v[v$we==1 & v$fascia_oraria=="3_afternoon", "fascia_oraria_we"]<-"3_afternoon_we"
v[v$we==1 & v$fascia_oraria=="4_evening", "fascia_oraria_we"]<-"4_evening_we"
v[v$we==1 & v$fascia_oraria=="5_night", "fascia_oraria_we"]<-"5_night_we"
v[v$we==0 & v$fascia_oraria=="0_prelavoro", "fascia_oraria_we"]<-"0_prelavoro_wd"
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
                 p_visite_prelav=tab2[,1],
                 p_visite_morn=tab2[,2],
                 p_visite_lunc=tab2[,3],
                 p_visite_afte=tab2[,4],
                 p_visite_even=tab2[,5],
                 p_visite_nigh=tab2[,6]
)
cookies<-merge(cookies, tab2, by="visitorID", all=T)
rm(tab2)
#p_visite_matt/pra/pome/sera/notte_we/wd
tab2<-table(as.character(v$visitorID), v$fascia_oraria_we)
tab2<-prop.table(tab2,1)
tab2<-data.frame(visitorID=cbind(row.names(tab2)),
                 p_visite_prelav_wd=tab2[,1],
                 p_visite_morn_wd=tab2[,3],
                 p_visite_lunc_wd=tab2[,5],
                 p_visite_afte_wd=tab2[,7],
                 p_visite_even_wd=tab2[,9],
                 p_visite_nigh_wd=tab2[,11],
                 p_visite_prelav_we=tab2[,2],
                 p_visite_morn_we=tab2[,4],
                 p_visite_lunc_we=tab2[,6],
                 p_visite_afte_we=tab2[,8],
                 p_visite_even_we=tab2[,10],
                 p_visite_nigh_we=tab2[,12]
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
# table(p$old)
# table(p$vendor)
# table(p$vendor, p$piattaforma)
# length(unique(c(p$visitorID,p$browser)))
# length(unique(c(p$visitorID,p$piattaforma)))
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
#variabile mobile dummy (i nonspec sono considerati d'ufficio pc)
p_merge$mobile<-0
p_merge$mobile[p_merge$piattaforma=="mobile"]<-1
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
# #inizio esplorazioni
# #tabellla di frequenze  entry page
# tab<-table(as.character(pg$entry_page))
# tab1<-data.frame(cbind(entry_page=as.character(row.names(tab)),Freq=as.numeric(tab)))
# str(tab1)
# tab1$entry_page<-as.character(tab1$entry_page)
# tab1$Freq<-as.numeric(as.character(tab1$Freq))
# tab1<-tab1[order(tab1$Freq, decreasing=T),]
# summary(tab1$Freq)
# #tabella di frequenze  channels
# tab<-table(as.character(pg$channels))
# tab2<-data.frame(cbind(entry_page=as.character(row.names(tab)),Freq=as.numeric(tab)))
# str(tab2)
# tab2$entry_page<-as.character(tab2$entry_page)
# tab2$Freq<-as.numeric(as.character(tab2$Freq))
# str(tab2)
# tab2<-tab2[order(tab2$Freq, decreasing=T),]
# summary(tab2$Freq)
# rm(tab,tab1,tab2)
# #fine esplorazioni
#numero di entry page per cookies
library(data.table)
pgt <- data.table(pg)
pgt<-pgt[, list(n_entry_pages = length(unique("entry_pages"))), by = c("visitorID", "entry_pages")]
t2<-table(pgt$visitorID)
t2<-as.data.frame(t2)
names(t2)<-c("visitorID","n_entry_pages_distinte")
cookies<-merge(cookies,t2,by="visitorID")
rm(pgt,t2)
#lavoro sulle PAGES
pages<-paste(unique(pg$pages))
t<-table(pg$pages)
t<-t[!row.names(t)==""]
t<-sort(t, decreasing=T)
pages_SKY<-data.frame(cbind(pages=row.names(t), Freq=t, Perc=round(prop.table(t)*100,2)))
write.csv2(pages_SKY, file="output/pages_SKY.csv", row.names=F)
rm(t, pages_SKY)
#frequenza parole entro area
ricavaParole<-function(pages,paroleNO=NULL){
  library(stringr)
  library(tm)
  url <- strsplit(pages,"http%3A//|http://")
  url[[1]]
  url <- sapply(url,function(x)x[2])
  url[[1]]
  url1<-str_replace_all(url, "[^[:alpha:]]", " ")
  url1[[1]] #per ogni url la stringa contenente le parole
  url1[[1]]
  parole<-unlist(strsplit(url1," "))
  parole
  parole<-parole[!parole%in%c(stopwords("italian"),"htm","html","vid","sky","it","shtml","php","pls","","www",letters,NA, "sh", "sht",paroleNO)]
  parole
  return(parole)
}
#freqParole
freqParole<-function(area, paroleNO=NULL, data=pg1){
  pages<-unique(data$pages[data$area==area])
  parole<-ricavaParole(pages,paroleNO)
  t<-sort(table(parole), decreasing=T)
  t<-data.frame(cbind(parole=row.names(t), Freq=t, Perc=round(prop.table(t),2),
                      CumPerc=round(cumsum(t)/sum(t),2)))
  return(t)}
#PAGES SECONDO NUOVO APPROCCIO
url <- strsplit(pages,"http%3A//|http://")
# url[[1]]
url <- sapply(url,function(x)x[2])
# url[[1]]
url <- strsplit(url,"/")
# url[[1]]
URL<-as.data.frame(pages)
URL[,"l1"]<- sapply(url,function(x)x[1])
URL[,"l2"]<- sapply(url,function(x)x[2])
#commerciale
URL[(URL$l1%in%c("soloperte.sky.it","abbonamento.sky.it")) | (URL$l1=="www.sky.it" & URL$l2%in%c("prodotti-sky","acquista","abbonarsi")),"area"]<-"pages_commerciale"
#assist_e_cont_clienti
URL[(URL$l1%in%c("guidatv.sky.it","skygo.sky.it","skyid.sky.it","hotclub.sky.it")) | (URL$l1=="www.sky.it" & URL$l2%in%c("assistenza-e-supporto","area-clienti"))
    ,"area"]<-"pages_assist_e_cont_clienti"
#news
URL[(URL$l1%in%c("tg24.sky.it")) | (URL$l1=="video.sky.it" & URL$l2%in%c("news"))
    ,"area"]<-"pages_news"
#meteo
URL[(URL$l1%in%c("meteo.sky.it"))
    ,"area"]<-"pages_meteo"
#sport
URL[(URL$l1%in%c("sport.sky.it","fantascudetto.sky.it","liveblog.sport.sky.it",
                 "fantascudetto.com","fantascudetto.net","fantacampioni.sky.it",
                 "charitystars.sport.sky.it", "fantamondiale.sky.it",
                 "liveblog.sport.sky.it")) | (URL$l1=="video.sky.it" & URL$l2%in%c("sport"))
    ,"area"]<-"pages_sport"
#intrattenimento
URL[(URL$l1%in%c("arte.sky.it","mag.sky.it","skyatlantic.sky.it","skyuno.sky.it",
                 "cinema.sky.it","xfactor.sky.it","masterchef.sky.it",
                 "hellskitchen.sky.it","juniormasterchef.sky.it",
                 "theapprentice.sky.it")) | (URL$l1=="video.sky.it" & URL$l2%in%c("mag","skyatlantic","skyuno","cinema","xfactor","masterchef","hellskitchen","theapprentice"))
    ,"area"]<-"pages_intrattenimento"
#contenuti_generico
URL[(URL$l1%in%c("oroscopo.sky.it","forum.sky.it"))
    ,"area"]<-"pages_contenuti_generico"
#non_definito
URL[is.na(URL$area),"area"]<-"pages_nondef_home"

# tableFattaBene(URL$area)

pg1<-merge(pg, URL, by="pages")
# tableFattaBene(pg1$area)


#pages_comm
pages_comm<-freqParole("pages_commerciale")
#pages_ass
pages_ass<-freqParole("pages_assist_e_cont_clienti") 
#pages_news
pages_news<-freqParole("pages_news",paroleNO=c("tg","video","news"))
#pages_meteo
pages_meteo<-freqParole("pages_meteo",paroleNO=c("meteo"))
#pages_sport
pages_sport<-freqParole("pages_sport", paroleNO=c("sport","video"))
#pages_intrattenimento
pages_intrattenimento<-freqParole("pages_intrattenimento")
#pages_contenuti_generico
pages_contenuti_generico<-freqParole("pages_contenuti_generico")
#pages_nondef_home
pages_nondef_home<-freqParole("pages_nondef_home")
# 
# write.csv(file="output/pages_comm.csv", pages_comm, row.names=F)
# write.csv(file="output/pages_ass.csv", pages_ass, row.names=F)
# write.csv(file="output/pages_news.csv", pages_news, row.names=F)
# write.csv(file="output/pages_meteo.csv", pages_meteo, row.names=F)
# write.csv(file="output/pages_sport.csv", pages_sport, row.names=F)
# write.csv(file="output/pages_intrattenimento.csv", pages_intrattenimento, row.names=F)
# write.csv(file="output/pages_contenuti_generico.csv", pages_contenuti_generico, row.names=F)
# write.csv(file="output/pages_nondef_home.csv", pages_nondef_home, row.names=F)

rm(pages_comm,pages_ass,pages_news,pages_meteo,pages_sport, pages_intrattenimento, pages_contenuti_generico, pages_nondef_home)

#creazione delle relative features:
#elenco features:
url2character<-function(pages){
  library(stringr)
  library(tm)
  url <- strsplit(pages,"http%3A//|http://")
  url <- sapply(url,function(x)x[2])
  url1<-str_replace_all(url, "[^[:alpha:]]", " ")
  return(url1) #per ogni url la stringa contenente le parole
}

URL$pages<-as.character(URL$pages)
URL$url_stringa<-url2character(URL$pages)
creaFeature<-function(area,pattern,data=URL){
  pattern<-c(pattern)
  yes<-sapply(pattern, grepl, data$url_stringa, ignore.case=TRUE)
  yes<-apply(yes,1,any)
  a<-rep(0,nrow(data))
  a[data$area%in%area & yes]<-1
  return(a)
}
#contenuto video
URL[,"pages_video"]<-0
URL[URL$l1%in%c("video.sky.it"), "pages_video"]<-1
#features di area
t<-as.data.frame.matrix(table(URL$pages, URL$area))
URL<-merge(URL, t, by.x="pages",by.y="row.names")
#area commerciale
URL$pages_acquisto_cinema<-creaFeature("pages_commerciale","cinema")
URL$pages_acquisto_calcio<-creaFeature("pages_commerciale","calcio")
URL$pages_acquisto_soloperte<-creaFeature("pages_commerciale","soloperte")
#area assistenza e clienti
URL$pages_visione_sport<-creaFeature("pages_assist_e_cont_clienti",c("sport","calcio"))
URL$pages_visione_cinema<-creaFeature("pages_assist_e_cont_clienti",c("cinema","film","commedia","drammatico","azione","thriller"))
URL$pages_visione_serie<-creaFeature("pages_assist_e_cont_clienti",c("serie","film"))
#area news
URL$pages_cronaca<-creaFeature("pages_news","cronaca")
URL$pages_mondo<-creaFeature("pages_news","mondo")
URL$pages_politica<-creaFeature("pages_news","politica")
URL$pages_economia<-creaFeature("pages_news","economia")
URL$pages_spettacolo<-creaFeature("pages_news","spettacolo")
#area meteo
URL$pages_previsioni_we<-creaFeature("pages_meteo",c("sab","dom"))
URL$pages_previsioniNord<-creaFeature("pages_meteo",c("piemonte","aosta","lombardia","veneto","friuli","trentino","adige","emilia","liguria"))
URL$pages_previsioniCentro<-creaFeature("pages_meteo",c("toscana","umbria","lazio","marche","friuli","trentino","adige","emilia"))
URL$pages_previsioniSud<-creaFeature("pages_meteo",c("campania","basilicata","puglia","molise","calabria","sicilia","sardegna"))
URL$pages_previsioni_villeggiatura<-creaFeature("pages_meteo",c("montagna","mare"))
#area sport
URL$pages_calcio<-creaFeature("pages_sport",c("calcio","italiano","calciomercato","mondiale","estero","champions","gol","fantascudetto","inter", "milan","juventus","real"))
URL$pages_formula1<-creaFeature("pages_sport",c("formula","motori"))
URL$pages_motogp<-creaFeature("pages_sport",c("motogp","motori","moto"))
URL$pages_ciclismo<-creaFeature("pages_sport",c("ciclismo","tour","france"))
URL$pages_basket<-creaFeature("pages_sport",c("nba"))
#area intrattenimento
URL$pages_programmitv<-creaFeature("pages_intrattenimento",c("masterchef","xfactor","serie","hellskitchen","juniormasterchef"))
URL$pages_cinema<-creaFeature("pages_intrattenimento",c("cinema","film", "trailer"))
URL$pages_gossip<-creaFeature("pages_intrattenimento",c("gossip","sexy","style","life","calendario"))
#area contenuti generico
URL$pages_forum_calcio<-creaFeature("pages_contenuti_generico",c("milan","calciomercato","calcio","juventus","inter", "champions"))
URL$pages_oroscopo<-creaFeature("pages_contenuti_generico",c("oroscopo"))
URL$pages_problemi_tecnici<-creaFeature("pages_contenuti_generico",c("decoder","abbonamento","mysky"))


pg1<-merge(pg, URL, by="pages")
# names(pg1)
pg1$visitorID<-factor(pg1$visitorID)
t<-aggregate(pg1[11:ncol(pg1)], by=list(pg1$visitorID), sum)
names(t)[1]<-"visitorID"
cookies<-merge(cookies,t, by="visitorID")

# pt<-t
# n<-as.numeric(cookies$n_pagine_viste)
# t2[,2:ncol(t2)]<-apply(t2[,2:ncol(t2)],2,function(x)x/n) 
# names(t2)[1]<-"visitorID"
# names(t2)[2:ncol(t2)]
save(file="sample/pg1", pg1)
rm(pages)
save(file="sample/cookies", cookies)

###################################################################
#lavoro sulle ENTRY_PAGES
entry_pages<-paste(unique(pg$entry_pages))
entry_pages_SKY<-tableFattaBene(pg$entry_pages)
write.csv2(entry_pages_SKY, file="output/entry_pages_SKY.csv", row.names=F)
rm(entry_pages_SKY)
#Entry_pages SECONDO NUOVO APPROCCIO
entryurl <- strsplit(entry_pages,"http%3A//|http://")
entryurl[[1]]
entryurl <- sapply(entryurl,function(x)x[2])
entryurl[[1]]
entryurl <- strsplit(entryurl,"/")
entryurl[[1]]
entryURL<-as.data.frame(entry_pages)
entryURL[,"l1"]<- sapply(entryurl,function(x)x[1])
entryURL[,"l2"]<- sapply(entryurl,function(x)x[2])
#commerciale
entryURL[(entryURL$l1%in%c("soloperte.sky.it","abbonamento.sky.it")) | (entryURL$l1=="www.sky.it" & entryURL$l2%in%c("prodotti-sky","acquista","abbonarsi")),"area"]<-"entry_commerciale"
#assist_e_cont_clienti
entryURL[(entryURL$l1%in%c("guidatv.sky.it","skygo.sky.it","skyid.sky.it","hotclub.sky.it")) | (entryURL$l1=="www.sky.it" & entryURL$l2%in%c("assistenza-e-supporto","area-clienti"))
    ,"area"]<-"entry_assist_e_cont_clienti"
#news
entryURL[(entryURL$l1%in%c("tg24.sky.it")) | (entryURL$l1=="video.sky.it" & entryURL$l2%in%c("news"))
    ,"area"]<-"entry_news"
#meteo
entryURL[(entryURL$l1%in%c("meteo.sky.it"))
    ,"area"]<-"entry_meteo"
#sport
entryURL[(entryURL$l1%in%c("sport.sky.it","fantascudetto.sky.it","liveblog.sport.sky.it",
                 "fantascudetto.com","fantascudetto.net","fantacampioni.sky.it",
                 "charitystars.sport.sky.it", "fantamondiale.sky.it",
                 "liveblog.sport.sky.it")) | (entryURL$l1=="video.sky.it" & entryURL$l2%in%c("sport"))
    ,"area"]<-"entry_sport"
#intrattenimento
entryURL[(entryURL$l1%in%c("arte.sky.it","mag.sky.it","skyatlantic.sky.it","skyuno.sky.it",
                 "cinema.sky.it","xfactor.sky.it","masterchef.sky.it",
                 "hellskitchen.sky.it","juniormasterchef.sky.it",
                 "theapprentice.sky.it")) | (entryURL$l1=="video.sky.it" & entryURL$l2%in%c("mag","skyatlantic","skyuno","cinema","xfactor","masterchef","hellskitchen","theapprentice"))
    ,"area"]<-"entry_intrattenimento"
#contenuti_generico
entryURL[(entryURL$l1%in%c("oroscopo.sky.it","forum.sky.it"))
    ,"area"]<-"entry_contenuti_generico"
#non_definito
entryURL[is.na(entryURL$area),"area"]<-"entry_nondef_home"

tableFattaBene(entryURL$area)

pg2<-merge(pg, entryURL, by="entry_pages")
tableFattaBene(pg2$area)

# 
# #entry_pages_comm
# entry_pages_comm<-freqParole("entry_commerciale", data=pg2)
# #entry_pages_ass
# entry_pages_ass<-freqParole("entry_assist_e_cont_clienti",data=pg2) 
# #entry_pages_news
# entry_pages_news<-freqParole("entry_news",paroleNO=c("tg","video","news"), data=pg2)
# #entry_pages_meteo
# entry_pages_meteo<-freqParole("entry_meteo",paroleNO=c("meteo"), data=pg2)
# #entry_pages_sport
# entry_pages_sport<-freqParole("entry_sport", paroleNO=c("sport","video"), data=pg2)
# #entry_pages_intrattenimento
# entry_pages_intrattenimento<-freqParole("entry_intrattenimento", data=pg2)
# #entry_pages_contenuti_generico
# entry_pages_contenuti_generico<-freqParole("entry_contenuti_generico", data=pg2)
# #entry_pages_nondef_home
# entry_pages_nondef_home<-freqParole("entry_nondef_home", data=pg2)
# 
# write.csv(file="output/entry_pages_comm.csv", entry_pages_comm, row.names=F)
# write.csv(file="output/entry_pages_ass.csv", entry_pages_ass, row.names=F)
# write.csv(file="output/entry_pages_news.csv", entry_pages_news, row.names=F)
# write.csv(file="output/entry_pages_meteo.csv", entry_pages_meteo, row.names=F)
# write.csv(file="output/entry_pages_sport.csv", entry_pages_sport, row.names=F)
# write.csv(file="output/entry_pages_intrattenimento.csv", entry_pages_intrattenimento, row.names=F)
# write.csv(file="output/entry_pages_contenuti_generico.csv", entry_pages_contenuti_generico, row.names=F)
# write.csv(file="output/entry_pages_nondef_home.csv", entry_pages_nondef_home, row.names=F)

rm(entry_pages_comm,entry_pages_ass,entry_pages_news,entry_pages_meteo,entry_pages_sport, entry_pages_intrattenimento, entry_pages_contenuti_generico, entry_pages_nondef_home)

#creazione delle relative features:
  #elenco features:
  url2character<-function(pages){
    library(stringr)
    library(tm)
    url <- strsplit(pages,"http%3A//|http://")
    url <- sapply(url,function(x)x[2])
    url1<-str_replace_all(url, "[^[:alpha:]]", " ")
    return(url1) #per ogni url la stringa contenente le parole
  }

entryURL$entry_pages<-as.character(entryURL$entry_pages)
entryURL$url_stringa<-url2character(entryURL$entry_pages)
creaFeature<-function(area,pattern,data=entryURL){
  pattern<-c(pattern)
  yes<-sapply(pattern, grepl, data$url_stringa, ignore.case=TRUE)
  yes<-apply(yes,1,any)
  a<-rep(0,nrow(data))
  a[data$area%in%area & yes]<-1
  return(a)
}
#contenuto video
entryURL[,"entry_video"]<-0
entryURL[entryURL$l1%in%c("video.sky.it"), "entry_video"]<-1
#features di area
t<-as.data.frame.matrix(table(entryURL$entry_pages, entryURL$area))
entryURL<-merge(entryURL, t, by.x="entry_pages",by.y="row.names")
#area commerciale
entryURL$entry_acquisto_cinema<-creaFeature("entry_commerciale","cinema")
entryURL$entry_acquisto_calcio<-creaFeature("entry_commerciale","calcio")
entryURL$entry_acquisto_soloperte<-creaFeature("entry_commerciale","soloperte")
#area assistenza e clienti
entryURL$entry_visione_sport<-creaFeature("entry_assist_e_cont_clienti",c("sport","calcio"))
entryURL$entry_visione_cinema<-creaFeature("entry_assist_e_cont_clienti",c("cinema","film","commedia","drammatico","azione","thriller"))
entryURL$entry_visione_serie<-creaFeature("entry_assist_e_cont_clienti",c("serie"))
#area news
entryURL$entry_cronaca<-creaFeature("entry_news","cronaca")
entryURL$entry_mondo<-creaFeature("entry_news","mondo")
entryURL$entry_politica<-creaFeature("entry_news","politica")
entryURL$entry_economia<-creaFeature("entry_news","economia")
entryURL$entry_spettacolo<-creaFeature("entry_news","spettacolo")
#area meteo
entryURL$entry_previsioni_we<-creaFeature("entry_meteo",c("sab","dom"))
entryURL$entry_previsioniNord<-creaFeature("entry_meteo",c("piemonte","aosta","lombardia","veneto","friuli","trentino","adige","emilia","liguria"))
entryURL$entry_previsioniCentro<-creaFeature("entry_meteo",c("toscana","umbria","lazio","marche","friuli","trentino","adige","emilia"))
entryURL$entry_previsioniSud<-creaFeature("entry_meteo",c("campania","basilicata","puglia","molise","calabria","sicilia","sardegna"))
entryURL$entry_previsioni_villeggiatura<-creaFeature("entry_meteo",c("montagna","mare"))
#area sport
entryURL$entry_calcio<-creaFeature("entry_sport",c("calcio","italiano","calciomercato","mondiale","estero","champions","gol","fantascudetto","inter", "milan","juventus","real"))
entryURL$entry_formula1<-creaFeature("entry_sport",c("formula","motori"))
entryURL$entry_motogp<-creaFeature("entry_sport",c("motogp","motori","moto"))
entryURL$entry_ciclismo<-creaFeature("entry_sport",c("ciclismo","tour","france"))
entryURL$entry_basket<-creaFeature("entry_sport",c("nba"))
#area intrattenimento
entryURL$entry_programmitv<-creaFeature("entry_intrattenimento",c("masterchef","xfactor","serie"))
entryURL$entry_cinema<-creaFeature("entry_intrattenimento",c("cinema","film"))
entryURL$entry_gossip<-creaFeature("entry_intrattenimento",c("gossip","sexy","style","life","calendario"))
#area contenuti generico
entryURL$entry_forum_calcio<-creaFeature("entry_contenuti_generico",c("milan","calciomercato","calcio","juventus"))
entryURL$entry_oroscopo<-creaFeature("entry_contenuti_generico",c("oroscopo"))
entryURL$entry_problemi_tecnici<-creaFeature("entry_contenuti_generico",c("decoder","abbonamento","mysky"))

pg2<-merge(pg, entryURL, by="entry_pages")
names(pg2)
pg2$visitorID<-factor(pg2$visitorID)
t<-aggregate(pg2[11:ncol(pg2)], by=list(pg2$visitorID), sum)
# t<-apply(t,2,function(x)x/)
names(t)[1]<-"visitorID"
cookies<-merge(cookies,t, by="visitorID")

#numero totale di entry pages
cookies$n_entry_pages_totali<-apply(cookies[,c("entry_assist_e_cont_clienti",
                                             "entry_commerciale",
                                             "entry_contenuti_generico",
                                             "entry_intrattenimento",
                                             "entry_meteo",
                                             "entry_news",
                                             "entry_nondef_home",
                                             "entry_sport")],1,sum)

save(file="sample/pg2", pg2)
save(file="sample/cookies", cookies)


rm(pg,pg1,pg2,t, url, URL,entryURL, entry_pages, entryurl)

# load("sample/cookies")
##------------------------------------------------------------------------------------
##--- PRODOTTI_SAMPLE
# load("sample/prodotti_sample")
# pd<-prodotti_sample
# pd<-pd[,-1]
# rm(prodotti_sample)
# #operazioni fatte una tantum con system locale ENGLISH
# pd$hour<-as.character(pd$hour)
# pd$ts<- strptime(pd$hour,"%b %d, %Y, Hour %H")
# pd$giorno <- format(pd$ts,"%Y-%m-%d")
# save(pd, file="sample/prodotti_sample2")
load("sample/prodotti_sample2")

#compra
compratoreID<-as.character((pd$visitorID[!is.na(pd$purchaseID)]))
cookies[,"compra"]<-0
cookies[cookies$visitorID%in%compratoreID,"compra"]<-1
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
qualepacchetto<-function(nome){
  yes<-sapply(nome, grepl, pacchetti, ignore.case=TRUE)
  yes<-apply(yes,1,any)
  yes<-as.numeric(yes)
  return(yes)
}
PacchettoCalcio<-qualepacchetto("Calcio")
PacchettoCinema<-qualepacchetto("Cinema")
PacchettoSport<-qualepacchetto("Sport")
products<-cbind(products, n_pacchetti, PacchettoCalcio, PacchettoCinema, PacchettoSport)
pd<-merge(pd,products, by="products")
pd_subset<-pd[which(pd$visitorID%in%carrelloID),]
names(pd_subset)
pd_subset[,9:12]<-apply(pd_subset[,9:12],2,function(x)as.numeric(as.character(x)))
t<-aggregate(pd_subset[,9:12], by=list(pd_subset$visitorID), max)
names(t)[1]<-c("visitorID")
cookies<-merge(cookies, t, by="visitorID", all.x=T)
cookies[is.na(cookies$n_pacchetti),"n_pacchetti"]<-0
cookies[is.na(cookies$PacchettoCalcio),"PacchettoCalcio"]<-0
cookies[is.na(cookies$PacchettoCinema),"PacchettoCinema"]<-0
cookies[is.na(cookies$PacchettoSport),"PacchettoSport"]<-0
# compra calcio/cinema/sport
#variabile comportamento
cookies$soggetto<-"naviga"
cookies$soggetto[cookies$carrello==1]<-"carrello"
cookies$soggetto[cookies$compra==1]<-"compra"

cookies$soggetto_calcio<-"naviga"
cookies$soggetto_calcio[cookies$carrello==1 & cookies$PacchettoCalcio==1]<-"carrello"
cookies$soggetto_calcio[cookies$compra==1 & cookies$PacchettoCalcio==1]<-"compra"

cookies$soggetto_cinema<-"naviga"
cookies$soggetto_cinema[cookies$carrello==1 & cookies$PacchettoCinema==1]<-"carrello"
cookies$soggetto_cinema[cookies$compra==1 & cookies$PacchettoCinema==1]<-"compra"

cookies$soggetto_sport<-"naviga"
cookies$soggetto_sport[cookies$carrello==1 & cookies$PacchettoSport==1]<-"carrello"
cookies$soggetto_sport[cookies$compra==1 & cookies$PacchettoSport==1]<-"compra"

#days_before_purchase
pd$giorno<-as.Date(pd$giorno)
a<-pd[!is.na(pd$purchaseID),c("visitorID", "giorno","purchaseID", "products")]
purchase_day<-aggregate(a$giorno,by=list(a$visitorID), min)
names(purchase_day)<-c("visitorID", "purchase_day")
cookies<-merge(cookies, purchase_day, by="visitorID", all.x=T)
cookies$purchase_day<-as.Date(cookies$purchase_day)
cookies$days_before_purchase<-as.numeric(cookies$purchase_day-cookies$first_day)+1
#days_before_basket
pd$giorno<-as.Date(pd$giorno)
a<-pd[!is.na(pd$products),c("visitorID", "giorno","purchaseID", "products")]
basket_day<-aggregate(a$giorno,by=list(a$visitorID), min)
names(basket_day)<-c("visitorID", "basket_day")
cookies<-merge(cookies, basket_day, by="visitorID", all.x=T)
cookies$basket_day<-as.Date(cookies$basket_day)
cookies$days_before_basket<-as.numeric(cookies$basket_day-cookies$first_day)+1
#days_basket_purchase
cookies$days_basket_purchase<-as.numeric(cookies$purchase_day-cookies$basket_day)+1


rm(pd, pd_subset,t, carrelloID, vede_promoID, n_pacchetti, pacchetti, products, PacchettoCalcio, PacchettoCinema, PacchettoSport, purchase_day, basket_day,a)
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




save(file="sample/cookies", cookies)
# load("sample/cookies")
##------------------------------------------------------------------------------------


# ######################################################
# #perché le tabelle visite e piattaforma hanno numerosità differenti??
# table(table(unique(c(v$ts,v$visitorID))))
# table(table(unique(c(p$ts,p$visitorID))))
# 
# ###-- SUBSET COMPRATORI --###
# com<-cookies[cookies$compra==1,]




